
\begin{code}

module Compiler.TypeUnification where

import Control.Monad
import Control.Monad.Writer

import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Maybe

import Compiler.BaseTypes
import Compiler.Qtypes
import Compiler.SemTypes
import Compiler.UnificationErrors

\end{code}

\subsubsection{Unification comments and notes}
The whole type unification thing!

First - Need to treat type variable in declarations differently from those
that are created as we go.  Type declaration vars are created as
|TypeVariable| types, Function declaration vars are created as
|RigidVariable| types.  Anything created as we go will be a
|MalleableVariable|.

So, the type declaration:

    qdata Either a b = {Left (a) | Right (b)}

treats both |a| and |b| as |TypeVariables| and stores them that way
in the global symbol table. This means there will be three entries:

   Either - ["a","b"] ["Left", "Right"]

   Left -> input:[TypeVariable "a"],
           output :[Either (TypeVariable "a")(TypeVariable "b")]
   Right-> input:[TypeVariable "b"],
           output :[Either (TypeVariable "a")(TypeVariable "b")]

Note this means, for example that |Right 0| would return a type of
|Either (MalleableVariable ti) (INT)|.

This is the only way |MalleableVariable|s are introduced.

The function declaration:

   eswitch::(ein:Either(a,b); eout:Either(b,a))={...}

treats the |a| and |b| here as |RigidVariables|. The symbol table entry for
|eswitch| will indicate an input argument type of
\[DeclaredType "Either" [RigidVariable "a", RigidVariable "b"]\]
and the output will be similar:
\[DeclaredType "Either" [RigidVariable "b", RigidVariable "a"]\]

Assuming the code for |eswitch| is as follows:
{ case ein of
     Left(lef) =>
       {eout = Right(lef)}
     Right(rit) =>
       {eout = Left(rit)}
}
the following typechecking and unifications would take place:
\subsubsection{Function declaration and entry}
At the declaration a number of symbol table entries are committed.
First, the global symbol table for the function itself as indicated above.
This is not actually used further in the body of a non-recursive function.

Then, when processing the parameters, the variable |ein| is placed in the
linear symbol table with a type of
 |Either (RigidVariable "a") (RigidVariable "b")|. For the output parameter,
|eout|, a constraint is added to the constraint table requiring |eout| to
have the type |Either (RigidVariable "b") (RigidVariable "a")|.
This constraint is removed at the end of the function, when |eout|
will not longer be in scope.

\subsubsection{First, at |case ein of|}
The variable ein will be evaluated and its type saved. In this case it
is a variable of type as above. Then the case clauses are checked.

\subsubsection{|Left| clause}
The constructor |Left| is checked in the global symbol table to ensure
it is a constructor of |ein|'s type. Then, the type of |lef| is determined
by a reverse construction of |ein|'s type. That is, what type would |lef|
have to have to give |ein| its type. This sets the type of |lef| to
|RigidVariable "a"|
\subsubsection{First assignment to |eout|}.
The statement |eout = Right(lef)| does the following: First, the expression
|Right (lef)| is evaluated. |Right| is found in the global symbol table. It
takes one argument of type |TypeVariable "b"| and returns the type
|Either (TypeVariable "a") (TypeVariable "b")|. The |TypeVariable b| is
matched to |lef|'s type, giving that $b \rightarrow RigidVariable a$.
When evaluating the return type of the constructor, we will not find a binding
for the typevariable "a" in the constructors arguments. Therefore, we will
create a new |MalleableVariable| for that one, giving a return type of
|Either (MalleableVariable "tv0") (RigidVariable "a").

Upon assignment to |eout|, this will be compared to the constraint of
|eout -> Either (RigidVariable "b") (RigidVariable "a")|. This will
give us the equation |MalleableVariable "tv0" = RigidVariable "b"|.

\subsubsection{Other side of case (|Left|)}
Conversely, the other case gives us a typing of
|eout -> Either (RigidVariable "b") (MalleableVariable "tv1")| and
the equation |MalleableVariable "tv1" = RigidVariable "a"|

At the end of the case, each of the cases will have created an entry
in the linear symbol table. These must now be merged.

The first step is to apply any equations that give values for the
|MalleableVariables|.  As illustrated above, we have two and the result
will be that both entries for |eout| will show a type of
|Either (RigidVariable "b") (RigidVariable "a")|. Therefore, we have no issues.


\subsection{Invalid return type}
Consider, however, the code:
  badsw::{ein:Either (a,b); eout : Either (b,a) =
{ case ein of Left(lef) => {eout = Left(lef)}
              Right(righ) => {eout = Right(righ)} }

In this code, the routine declaration generates the same symbol table entries
and constraints as the correct one. However the code in the first clause
will give a typing for |eout| of
|Either (RigidVariable "a") (MaleableVariable "tv0")|. This gives an
immediate error when checking the constraint as trying to equate
|RigidVariable "a"| and |RigidVariable "b"| leads to an immediate error.



The function instanceOf checks to see if |t1| is an instance of
|t2|, given that |t2| may be (or contain) type variables (of either
the |TypeVariable| or |RigidVariable| kind) and these may point to
any one thing. (i.e., having them already in the map is an error,
unless the entry is already the same). Other type checking is done
at the same time.

\subsection{Unifying types}
Next, consider the statement

   c = Cons(0,Nil)
The first part of the processing generates a type for the $Nil$
constructor. As above, this will be given the type
  \[  List (MalleableVariable tv0)\]
The constant $0$ has the type $INT$.

When we try to check the argument types of the $Cons$, we will get to
a point where the typevariable "a" must be both an INT and a
MalleableVariable tv0.  This must be added as a type relation for the
rest of the program and used to replace any uses of tv0 in the future.

\begin{code}

instanceOf :: (Qtype -> Bool) ->Qtype -> Qtype -> Map Identifier Qtype ->
         WriterT CompilerLogs SemStateMonad (Map Identifier Qtype)
instanceOf varType t1 t2 un
    | t1 == t2 = return un
    | ((isBaseType t1 && isBaseType t2) && (t1 /= t2))   ||
      (isBaseType t1 && isDeclType t2) ||
      (isBaseType t2 && isDeclType t1)
          = fail $ unequalDataType  (show t1) (show t2)
    | isProc t1 || isProc t2  = fail noFuncTypes
    | varType t1 = fail $ illegalLHSType (show t1) (show t2)
    | varType t2 = instanceOfVar (getTypeVar t2) t1 un
    | isMalleableTypeVar t1
        = instanceOfVar (getTypeVar t1) t2 un
    | isMalleableTypeVar t2
        = instanceOfVar (getTypeVar t2) t1 un
    | isTypeVar t1 && not (varType t1)       --Allow typevar when hating rigid types :)
        = instanceOfVar (getTypeVar t1) t2 un
    | isTypeVar t2 && not (varType t2)
        = instanceOfVar (getTypeVar t2) t1 un
    | isDeclType t1 && isDeclType t2
        = if typeId t1 == typeId t2
            then instanceOfList varType (zip (typeParms t1) (typeParms t2)) un
            else fail $ uncontainableDataType (show t1) (show t2)
    | otherwise = fail $ unequalDataType (show t1) (show t2)

instanceOfVar :: Maybe Identifier -> Qtype -> Map Identifier Qtype ->
                 WriterT CompilerLogs SemStateMonad(Map Identifier Qtype)
instanceOfVar Nothing t _
    = fail $ patternMissing "instanceOfVar" "No variable" (show t)
instanceOfVar (Just a) t un
    = case Map.lookup a un of
           Just telse ->
               if t == telse then return un -- un' <- unify t telse un
               else do  t' <- unify t telse
                        return $ Map.insert a t' un
           Nothing ->
                 return $ Map.insert a t un

instanceOfList :: (Qtype -> Bool) ->[(Qtype,Qtype)] ->
                  Map Identifier Qtype ->
                  WriterT CompilerLogs SemStateMonad(Map Identifier Qtype)
instanceOfList vt [] un = return un
instanceOfList vt ((a,b):ts) un
          = instanceOf vt a b un >>= instanceOfList vt ts

\end{code}

\subsubsection{|restriction|}

Thoughts behind restriction - This is a unification that
pays attentions to any contained in stuff, then when
t2 is "some" kind of type var, maps the typevar to the first type.

Not sure if this is needed anymore.

\begin{code}

restriction :: Qtype -> Qtype -> WriterT CompilerLogs SemStateMonad (Map Identifier Qtype)
restriction t1 t2
    = do containedInOrFail t1 t2 -- OK - can restrict
         restrict t1 t2 Map.empty

restrict:: Qtype -> Qtype -> Map Identifier Qtype
        -> WriterT CompilerLogs SemStateMonad (Map Identifier Qtype)
restrict t1 t2 mp
   | t1 == t2      =  return mp
   | isTypeVar t2  =  return $ Map.insert (fromJust $ getTypeVar t2) t1 mp
   | isDeclType t1 && isDeclType t2 =
                      do  let t1s = getDeclTypes t1
                              t2s = getDeclTypes t2
                          restrictList (zip t1s t2s) mp
   | otherwise     = fail $ patternMissing  "restrict" (show t1)  (show t2)

restrictList :: [(Qtype,Qtype)] -> Map Identifier Qtype ->
                WriterT CompilerLogs SemStateMonad (Map Identifier Qtype)
restrictList [] mp = return mp
restrictList (t1t2:ts) mp
    =  uncurry restrict t1t2 mp >>= restrictList ts

\end{code}

The |containedInOrFail| stuff is there to check that the right
hand argument is a rigid type variable (or only contains rigid ones)
when the left hand is not the same as the right hand.  So
 List (QUBIT) List(a) would be ok

but
 List(b) List (a) would not.

\begin{code}

containedInOrFailm :: Qtype -> Maybe Qtype -> WriterT CompilerLogs SemStateMonad ()
containedInOrFailm _ Nothing = return ()
containedInOrFailm t1 (Just t2) = containedInOrFail t1 t2

containedInOrFail :: Qtype -> Qtype -> WriterT CompilerLogs SemStateMonad ()
containedInOrFail t1 t2
    | t1 == t2 = do  semLog semLLDebug  "ciof types are equal"
                     return ()
    | (isBaseType t1 && isBaseType t2) ||
      (isBaseType t1 && isDeclType t2) ||
      (isBaseType t2 && isDeclType t1)
           = fail $ uncontainableDataType (show t1) (show t2)
   | isRigidTypeVar t1 && isRigidTypeVar t2
        = fail $ nocontainOfRigidVariables (show t1) (show t2)
   | isRigidTypeVar t2 || isTypeVar t2
       =  semLog semLLDebug $ "Contained In or fail with tv on right " ++
          show t1 ++ show t2
   | isDeclType t1 && isDeclType t2
       = if getDeclTypeId t1 == getDeclTypeId t2 then
                do  semLog semLLDebug  $ "Ciof with mapping " ++ show t1 ++ show t2
                    mapM_  (uncurry containedInOrFail) $
                            zip (getDeclTypes t1) (getDeclTypes t2)
         else  fail $ uncontainableDataType (show t1) (show t2)
   | isMalleableTypeVar t2
       = do   semLog semLLDebug $ "Ciof with t2 malleable " ++ show t2
              unifyVar (fromMaybe "" $ getTypeVar t2) t1
              return ()
   | isMalleableTypeVar t1
       = do  semLog semLLDebug $ "Ciof with t1 malleable " ++ show t1
             unifyVar (fromMaybe "" $ getTypeVar t1) t2
             return ()
   | otherwise = fail $ patternMissing "containedInOrFail" (show t1)  (show t2)

checkTypesInMapPairs :: Map String (SymEntryLinear,SymEntryLinear) ->
                        WriterT CompilerLogs SemStateMonad SymbolTableLinear
checkTypesInMapPairs sts =
    do let (eqPrs, uneqPrs) = Map.partition (uncurry (==)) sts
           okitems = Map.map fst eqPrs
           checkingitems = Map.toList uneqPrs
       converted <-  checkTypes checkingitems Map.empty
       return $ Map.union okitems converted

checkTypes :: [(String,(SymEntryLinear,SymEntryLinear))] ->
              Map String Qtype ->
              WriterT CompilerLogs SemStateMonad SymbolTableLinear
checkTypes [] _ = return Map.empty
checkTypes ((k,(ent1,ent2)):entries) assigns =
    do (assigns', entry) <- checkType ent1 ent2 assigns
       symtab <- checkTypes entries assigns'
       return $ Map.insert k entry symtab

checkType :: SymEntryLinear -> SymEntryLinear -> Map String Qtype ->
             WriterT CompilerLogs SemStateMonad (Map String Qtype, SymEntryLinear)
checkType st1 st2 assigns =
    do unify (qtype st1) (qtype st2)
       t' <-  deriveType isMalleableTypeVar assigns (qtype st1) --Fix malleables?
       return (assigns, st1{qtype = t'})


--tupleizeTE :: TypeRelation -> (Qtype,Qtype)
--tupleizeTE (ContainedIn qt1 qt2) = (qt1, qt2)

--unifyTE :: TypeRelation -> Map Identifier Qtype ->
--           WriterT CompilerLogs SemStateMonad (Map Identifier Qtype)
--unifyTE (ContainedIn qt1 qt2) = unify qt1 qt2
--
--unifyTESet :: Set TypeRelation -> Map Identifier Qtype ->
--              WriterT CompilerLogs SemStateMonad(Map Identifier Qtype)
--unifyTESet setTE
--    = unifyList $ Set.toList $ Set.map tupleizeTE setTE


\end{code}

I'm not sure if this is usable  or needed at all anymore

\begin{code}

unify :: Qtype -> Qtype -> WriterT CompilerLogs SemStateMonad Qtype
unify t1 t2  | t1 == t2 = return t1
unify t1 t2
   | ((isBaseType t1 && isBaseType t2) && (t1 /= t2))   ||
     (isBaseType t1 && isDeclType t2) ||
     (isBaseType t2 && isDeclType t1)
       = fail $ unequalDataType  ("from Unify"++show t1) (show t2)

unify (Qproc _ _) _   = fail noFuncTypes
unify _ (Qproc _ _)   = fail noFuncTypes

unify (MalleableVariable a) t --a must be a "t"
    = unifyVar a t

unify t (MalleableVariable a)  -- a must be at least "t"
    = unifyVar a t

unify (DeclaredType tid ts) (DeclaredType pid ps)
   | tid == pid = do typs <- unifyList (zip ts ps)
                     return $ DeclaredType tid typs
   | otherwise = fail $ unequalDataType tid pid

unify t1 t2  = fail $ patternMissing "unify" (show t1) (show t2)

unifyVar :: Identifier -> Qtype ->
            WriterT CompilerLogs SemStateMonad Qtype
unifyVar a t
    = do trel <- getTypeRelations
         case Map.lookup a trel of
           Just telse -> unify t telse
           Nothing -> do addTypeRelation a t
                         return t

unifyList :: [(Qtype,Qtype)] -> WriterT CompilerLogs SemStateMonad [Qtype]
unifyList = mapM (uncurry unify)

\end{code}

|deriveTypes| is used when calling a cons or proc. The rigid or typevariables in the
declarations are "specified" by the input arguments.

Deriving will then replace the rigids with their respective items, or new
malleable type vars if not specified.

\begin{code}

deriveTypes :: (Qtype -> Bool) -> Map Identifier Qtype -> [Qtype] ->
               WriterT CompilerLogs SemStateMonad [Qtype]
deriveTypes vt  = mapM . deriveType vt

deriveType :: (Qtype -> Bool) -> Map Identifier Qtype ->
              Qtype -> WriterT CompilerLogs SemStateMonad Qtype
deriveType vt mp t
    | isDeclType t =
        do let dtypes = getDeclTypes t
           dtypes' <- deriveTypes vt mp dtypes
           return $ setDeclTypes t dtypes'
    | vt t = let Just tv = getTypeVar t
                 tval = Map.lookup tv mp
             in case tval of
                  Nothing -> do ntv <- newTypeVar
                                return (MalleableVariable ntv)
                  (Just t') -> return t'
    | otherwise = return t

fixMalleables :: [Qtype] -> WriterT CompilerLogs SemStateMonad [Qtype]
fixMalleables   = mapM fixMalleable

fixMalleable :: Qtype -> WriterT CompilerLogs SemStateMonad Qtype
fixMalleable  t@(MalleableVariable a)
    = do trmap <- getTypeRelations
         case Map.lookup a trmap of
            Nothing -> return t
            Just t' -> return t'
fixMalleable  t@(DeclaredType tid tparms)
    = do tparms' <- fixMalleables tparms
         return $ DeclaredType tid tparms'
fixMalleable t = return t

fixMalleablesInMap :: Map Identifier Qtype ->
                      WriterT CompilerLogs SemStateMonad (Map Identifier Qtype)
fixMalleablesInMap mp
    = do let (keys,vals) = unzip $ Map.toList mp
         vals' <- fixMalleables vals
         return $ Map.fromList $ zip keys vals'


\end{code}

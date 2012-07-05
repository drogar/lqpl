\incsec{The semantic checking}
\label{incsec:semantic}
%if false
\begin{code}
module Compiler.Semantic where

import Control.Arrow
import Control.Monad.State as State

import Control.Monad.Writer


import Data.List as List
import Data.Bits
import Data.Map as Map
import Data.Set as Set

import Compiler.BaseTypes
import Compiler.CompSupport(hasDuplicates)
import Compiler.IrTypes
import Compiler.SemTypes
import Compiler.SemanticErrors
import Compiler.SymbolTable
import Compiler.TypeUnification
import Compiler.Qtypes

import Data.Stack
import Data.Tuples

\end{code}
%endif
We create the IR for a program as a list of the sizes of the global variables,
then two lists of functions and statements. This is a multi pass process in
that we do the following in order:
%if false
 TODO - update this with what really happens.
%endif
\bi
\item{} Update the symboltable with the types that are declared.
(\haskfunc{updateSymTab})
\item{} Get the sizes of all variables.
\item{} Update the symbol table with the variables. (\haskfunc{updateSymTab} and
\haskfunc{updateSymTabList})
\item{} Create the list of functions that are declared.
\item{} Finally, create the intermediate representation
 for each of the statments. (\haskfunc{stmtListIr})
\ei

All of the transformation functions use the \hasktype{SemStateMonad} to
output. See \fullref{incsec:semantic}
\incsubsec{Tranformation of the program type}
\label{incsec:transprogram}
\incsubsubsec{\haskfuncnoref{block progIr} and \haskfuncnoref{progIr}}
\label{haskellfunction:blockprogIr}
\index{Compiler Functions!semantics!blockprogIr}\label{haskellfunction:progIr}
\index{Compiler Functions!semantics!progIr}
This function will transform the parsed form of the program into an IR.

\CodeResetNumbers
\begin{code}

progIr :: Program-> WriterT CompilerLogs SemStateMonad  Iprog
progIr  gds
     = do  semLog semLLTrace "iring prog"
           addTypesToSt gds
           addProcsToSt gds
           semLog semLLTrace "progir - added procs to st"
           dumpsts semLLDebug3
           let procs = filterProcs gds
           iprocs <- mapM procIr procs
           return $ Iprog $ Map.fromList $ zip (List.map procnm procs) iprocs
\end{code}

Type unifiying of procs:
We want:
   Somehow add output types as type equations "to be", for use when
doing the semantic and ir gen of the statements in the proc.

The type of the input items are unified to the type of the actual args
upon calling. However, we also would want to do unifying at this stage.
For example, suppose the proc is declared with a List(a) input, but
we then pass the head of the list to a proc expecting an (Either x y).

We then have a type equation of a == Either x y which could / should be
used for further unification.

\begin{code}


procIr :: Procedure ->  WriterT CompilerLogs SemStateMonad Iproc
procIr (Procedure procnm incparms inqparms outqparms outcparms procstmts)
     = do  stl<-getSymTabLinear
           stc<-getSymTabClassical
           tcrs <- getTypeConstraints
           procent <- stgloballookup procnm
           semLog semLLDebug1 $ show procent
           lvl <- getSemLvl
           setSemLvl $ incFunc lvl
           let  pinqtypes = qargtypes procent
                cinqtypes = cargtypes procent
                poutqtypes = qrettypes procent
                inargs = zip inqparms $ List.map Just pinqtypes
                outargs = zip (List.map parmId outqparms) poutqtypes
           updateStList inargs
           addTypeConstraints  outargs
           pushOffset --Classical stack locns
           updateClassicalListP incparms
           updateClassicalListP outcparms
           outentries <- mapM classicalStlookup $ List.map parmId outcparms
           --dumpst
           stsir <- stmtListIr procstmts
           checkInArgsDestroyed $ List.map (parmId . fst) inargs List.\\ List.map  fst outargs
           checkOutArgsCreated $ List.map  fst outargs
           setTypeConstraints tcrs
           setSymTabLinear stl
           setSymTabClassical stc
           setSemLvl lvl
           popOffset
           return $  Iproc procnm (cdlabel procent)
                      (List.map parmType incparms)
                      (List.map parmType inqparms)
                      (List.map parmType outqparms)
                      (List.map (coffset &&& ctype) outentries) stsir

\end{code}
\incsubsec{Transformation of statements}
\label{incsec:transstatement}
\incsubsubsec{\haskfuncnoref{stmtListIr}}
\label{haskellfunction:stmtListIr}
\index{Compiler Functions!semantics!stmtListIr}
We first set up a simple function that will go through a list of statements,
producing the accompanying IR for each of them.
\begin{code}
stmtListIr :: [Statement]-> WriterT CompilerLogs SemStateMonad [Istmt]
stmtListIr [] = return []
stmtListIr (s:ss) = do
      stmt <- stmtIr s
      semLog semLLDebug3  $ "Processed : " ++ show s
      dumpst semLLDebug3
      semLog semLLTrace "Done "
      semLog semLLTrace " "
      stmts <- stmtListIr ss
      return $ stmt ++ stmts

\end{code}
\incsubsubsec{\haskfuncnoref{stmtIr}}
\label{haskellfunction:stmtIr}
\index{Compiler Functions!semantics!stmtIr}
This function pattern matches based upon the \hasktype{QPLstmt} constructors and
produces the appropriate IR.
\begin{code}
stmtIr :: Statement-> WriterT CompilerLogs SemStateMonad [Istmt]

\end{code}
\incsubsubsec{Assignment Statements.} This will produce a \haskcons{IZero} or
\haskcons{IOne}
IR, translating the identifier to its offset and level.
\CodeContinueNumbers
\begin{code}

stmtIr stmt@(Assignment id exp)
   = do  (irexp, irt, classical) <- exprIr exp
         cvar <- maybeLookupClassical id
         case cvar of
           Nothing ->
            do  updateSt stmt (Just irt)
                sentry <- stlookup id
                flatirexp <- flattenIrExp irexp
                let ecqexp = if classical
                              then Left flatirexp
                              else Right flatirexp
                return [Iassign (name sentry)  ecqexp]
           Just clsclVar -> --remove form clscl side after
            if classical then
               do  flatirexp <- flattenIrExp irexp
                   warnssm $ convertClassToLin id (show exp)
                   updateSt stmt (Just irt)
                   sentry <- stlookup id
                   return [Iassign (name sentry) (Left flatirexp)]
             else
               fail $ qToCError (show exp) id

\end{code}


\incsubsubsec{Looping.} The \haskcons{QPLwhile} is translated to either an
\haskcons{IWhile} or it fails if the test was not of a \bit.
The identifier is replaced with the offset and level.
While now gone.

stmtIr  (While exp s)
  = do irs <- stmtListIr s
       (irexp, irt) <- exprIr exp
       if  BIT == irt
           then return ([Iwhile irexp irs] )
           else fail "While must test a bit."
\begin{code}
stmtIr  (CaseSt exp clauses)
  = do  (irexp, irt, classical) <- exprIr exp
        if classical
         then  fail "Can not case a classical value"
         else  do  irc <- clauseListIr irt clauses
                   return [Icase irexp  irc]


\end{code}

\incsubsubsec{Conditional statements.} The \haskcons{QPLcond} or \haskcons{QPLmeas}
 is translated to one of
\haskcons{Icond} or \haskcons{Imeas}. It may fail if the identifier was not of the
correct type.
The identifier is replaced with the offset and level.
Cond now gone.

stmtIr (Cond exp s1 s2)
     = do  irs1 <- stmtListIr  s1
           irs2 <- stmtListIr  s2
           (irexp, irt) <- exprIr exp
           if BIT == irt
              then return [Icond irexp irs1 irs2]
              else fail "If expression must be of type BIT."
\begin{code}

stmtIr (Measure exp s1 s2)
     = do  (irexp, irt, classical) <- exprIr exp
           stl <- getSymTabLinear
           [Iblock irs1] <- stmtIr (BlockStatement s1)
           st1 <- getSymTabLinear
           setSymTabLinear stl
           [Iblock irs2] <- stmtIr (BlockStatement s2)
           st2 <- getSymTabLinear
           (exir1, exir2, st) <- symTabMerge st1 st2
           setSymTabLinear st
           --t <- semanticUnify irt
           if QUBIT == irt --t
              then return [Imeas irexp (irs1 ++ exir1)  (irs2 ++ exir2)]
              else fail $ measureNotQubit (show exp) $ show irt

\end{code}

\incsubsubsec{The block statement.} Here, we must handle the possiblity of
new identifiers being used in this block. To do this, we save the symbol table
and the amount of storage in use at the beginning of the block.
 We then add the
statements in the block to the symbol table and compute the
 amount of storage used
in this block. The IR for the statements is computed recursively
after they have
been added to the symbol table and both items are returned.
\begin{code}


stmtIr (BlockStatement stmtlist)
     = do  semLog semLLTrace $ "Iring block statments "++ showList stmtlist ""
           copyAndPushOffset
           stc <- getSymTabClassical
           istmts <- stmtListIr stmtlist
           setSymTabClassical stc
           popOffset
           return [Iblock  istmts]

\end{code}

\incsubsubsec{Local functions.} This combines the complexity of a block with
numerous others. The primary tasks to be done here are:
\bi
\item Compute the storage.
\item Create IR for the statements in the procedure definition.
\item Create IR for the result statement.
\item Determine if there are any \qubit usage errors. That is, are there any
cases where a \qubit is illegally duplicated?
\item Determine if we are a recursive procedure or not.
\ei
The first two items are similar to that done by the block translation. The third
is nothing new, just another recursive call to \haskfuncnoref{stmtIr}.  The code
for determining illegal duplication is contained in the functions
\haskfunc{addQubitUsage} and \haskfunc{updateQubitdups}. These work similarly to
a shadow symbol table where they add the \qubit usage. This can then be inspected
to determine if there is illegal duplication. This is intended to catch abstruse
errors such as having a \qubit declared in an outer block, then used in
a prodedure definition in a transformation with a parameter and then the
result statement calling that procedure with the parameter being the global qubit.
See the examples \texttt{caught.qpl} and \texttt{caught2.qpl} in
\fullref{sec:invalidsemantics} .

\incsubsubsec{Procedure calling}
We first start with the standards of any language that calls a procedure. We lookup the
name, the names of the arguments. The first difference is that we then look up this
procedure in our shadow symbol table that is used to determine if parameters are duplicated.
The remainder of the code checks for gross duplications such as calling the procedure with the
same name in the parameter list and determines if this is a recursive call.
\begin{code}

stmtIr (Call nm cexps qexps qids cids)
    = do  semLog semLLTrace $ "iring call " ++ nm
          fentry <- stgloballookup nm
          ceirt <- mapM exprIr cexps
          qeirt <- mapM exprIr qexps
          let  (ceir, cetypes, cclassicals) = unzip3 ceirt
               (qeir, qetypes, qclassicals) = unzip3 qeirt
          qmpfinal <- checkCallArgs cetypes cclassicals qetypes fentry
          dtypes <- deriveTypes isRigidTypeVar qmpfinal $ qrettypes fentry
          addNewLinearEntries qids dtypes SeVar
                     -- Need to actually unify based on inputs....
          lvl <- getSemLvl
          ctypes <- deriveTypes isRigidTypeVar qmpfinal $ crettypes fentry
          addNewClassicalEntries [0..] cids ctypes (SeCVar lvl )
          return [Icall (transform fentry)
                            (callingLabel fentry)
                            (parmnames fentry)
                            ceir qeir
                        (zip qids (qrettypes fentry))
                                    (zip cids (crettypes fentry)) ]


stmtIr (UseAssign id exp)
    = do  (irexp, irt,classical) <- exprIr exp
          lvl <- getSemLvl
          offset <- getOffset
          decOffset
          let sec = SeUse id offset lvl irt
          modSymTabClassical (Map.insert id sec)
          flatirexp <- flattenIrExp irexp
          semLog semLLTrace $ " Doing UseAssign " ++ id ++ " --> " ++ show exp
          dumpst semLLDebug3
          let ecqexp = if classical then Left flatirexp else Right flatirexp
          return [IuseAssign id ecqexp]

stmtIr (UseFromHere [])
       = return []

stmtIr (UseFromHere (id:ids))
    = do  first <- stmtIr (UseAssign id (Evar id))
          rest <- stmtIr $ UseFromHere ids
          return $ first ++ rest


stmtIr (Use ids stmts)
    = do  ventry <- mapM stlookup ids
          lvl <- getSemLvl
          setSemLvl $ incUse lvl
          copyAndPushOffset
          offset <- getOffset
          let useids = makeUseEntries offset lvl ventry
          decOffsetByN (length ids)
          stc <- getSymTabClassical
          let  lindel:: [SymbolTableLinear -> SymbolTableLinear]
               lindel = List.map Map.delete ids
          modSymTabLinear (List.foldl (.) id lindel)
          modSymTabClassical (List.foldl (.) id $
                             List.map (uncurry Map.insert) $
                             zip ids useids)
--         dumpst
          stirs <- stmtListIr stmts
          setSymTabClassical stc
          popOffset
          setSemLvl lvl
          return [Iuse [name v |v<-ventry]
                           stirs]



stmtIr (Guard gcs)
    = do  gcirs <- gcListIr gcs
          return [Iguard gcirs]
\end{code}

\begin{code}
stmtIr (Discard ids)
    = do  semLog semLLTrace  "In discard"
          dumpst semLLDebug3
          idents <- mapM stlookup ids
          let dels = Map.fromList $ zip ids idents
          modSymTabLinear (flip Map.difference dels)
          return [ Idiscard $ List.map name idents ]

stmtIr (ControlledBy stmt cids)
    = do  let (ids, recontrols) = unzip $ List.map splitControl cids
          semLog semLLTrace  "Controlled by "
          if hasDuplicates ids then fail $ dupInList ids
             else do  idents <- mapM stlookup ids
                      let dels = Map.fromList $ zip ids idents
                      modSymTabLinear (flip Map.difference dels)
                      istmt <- stmtIr stmt
                      semLog semLLDebug $ show dels;
                      semLog semLLDebug3 "Symbol table before control"
                      dumpst semLLDebug3
                      currst <- getSymTabLinear
                      let overlaps = ids `List.intersect` Map.keys currst
                      ctrlScopeErr overlaps
                      modSymTabLinear (Map.union dels) -- put them back
                      semLog semLLDebug3 "Symbol table after control"
                      dumpst semLLDebug3
                      return [ IcontrolledBy istmt $
                               zipWith ($) recontrols $
                               List.map name idents ]

stmtIr (Skip)
        =  return []

stmtIr (ZeroStack)
        =  return [IzeroStack]

\end{code}

\begin{code}

gcListIr :: [GuardClause] ->  WriterT CompilerLogs SemStateMonad [(IrExpression, [Istmt])]
gcListIr [] = return []
gcListIr (gc:[])
    = do  (ire,irs) <- guardClauseIr gc
          return [(ire,irs)]
gcListIr (gc:gcs)
   = do  ststart <- getSymTabLinear
         (ire0,irstmts0) <- guardClauseIr gc
         st0 <- getSymTabLinear
         setSymTabLinear ststart
         irrest <- gcListIr gcs
         stRest <- getSymTabLinear
         (exir0,exirRest,st) <- symTabMerge st0 stRest
         setSymTabLinear st
         let  irstmts0' = irstmts0 ++ exir0
              irrest' = List.map (app2of2 (++exirRest)) irrest
         return $ (ire0,irstmts0') : irrest'

guardClauseIr :: GuardClause ->  WriterT CompilerLogs SemStateMonad (IrExpression, [Istmt])
guardClauseIr (GuardClause e ss)
    = do  (ire, gct,classical) <- exprIr e
          if gct == BOOL && classical
            then do  [Iblock irss] <- stmtIr (BlockStatement ss)
                     return (ire, irss)
            else fail $ guardClauseType  (show gct)

semanticUnify :: Qtype ->  WriterT CompilerLogs SemStateMonad (Qtype)
semanticUnify
    = return
{-
   = do tes <- getTypeRelations
        un <- unifyTESet tes Map.empty
        return $ appunify t un
-}

consTypesOK :: Qtype -> [Qtype] ->Bool --Check that first is instance of all snds
consTypesOK _ _ = True

clauseListIr :: Qtype ->
                [(CaseClause, [Statement])] ->
                 WriterT CompilerLogs SemStateMonad [IrCaseClause]
clauseListIr _ [] = return []
clauseListIr ctype (cclause:[])
    = do  irc1 <- clauseIr ctype cclause
          return [irc1]

clauseListIr ctype (cclause:clauses)
    = do  st0 <- getSymTabLinear
          [irc1] <- clauseListIr  ctype [cclause]
          st1 <- getSymTabLinear
          setSymTabLinear st0
          ircRest <- clauseListIr ctype clauses
          st2 <- getSymTabLinear
          semLog semLLTrace "About to merge"
          (exir1, exir2, st) <- symTabMerge st1 st2
          setSymTabLinear st
          let  irc1' = appendStatementsToClause exir1 irc1
               ircRest' = List.map (appendStatementsToClause exir2) ircRest
          return $ irc1' : ircRest'

clauseIr ::  Qtype ->
             (CaseClause, [Statement])->
              WriterT CompilerLogs SemStateMonad (IrCaseClause)
clauseIr casetype ((CaseClause cid ids),stmts)
    = do  semLog semLLTrace $ "In case clause " ++ cid
          ccentry <- stgloballookup cid
          let clausetype = head $ rettypes ccentry
         --TODO add casetype check with , head $ rettypes ccentry
          semLog semLLDebug  $ " Unifying casetype '"++show casetype ++
                " with ret type of the ccentr '"++ show  clausetype
          restrictMap <- restriction casetype clausetype
          clstypes <-  deriveTypes isTypeVar restrictMap $
                      argtypes ccentry
--             newentries = zip ids $
--               List.map (\ (o,(idn,typ)) -> SeCaseId o idn typ) $
--                   zip [0..] $ zip ids $ clstypes

          semLog semLLTrace  $ "Adding " ++ showList ids ""
          addNewLinearEntries ids clstypes (SeCaseId 0)
          dumpst semLLDebug2
          [Iblock sirs] <- stmtIr (BlockStatement stmts)
          dumpst semLLDebug2
          semLog semLLDebug3 $ "removing " ++ showList ids ""
          rmirs <- removeIdListLinear "Case pattern" ids
          semLog semLLDebug3  "Popped after case clause"
          return (IrCaseClause cid ids $ sirs++rmirs)



\end{code}

\begin{code}

noWarnRemoveIdLinear = removeIdLinear Nothing

removeIdLinear :: (Maybe String) ->String ->  WriterT CompilerLogs SemStateMonad ([Istmt])
removeIdLinear kind id
    = do  ste <- maybeLookup id
          case ste of
             Just _ ->
                 do  modSymTabLinear (Map.delete id)
                     case  kind of
                           Just kd ->
                               do  warnssm $ idsNotUsed kd id
                                   return [Idiscard [id]]
                           Nothing ->
                               return []
             Nothing ->
                 return []

removeIdListLinear :: String -> [String] ->  WriterT CompilerLogs SemStateMonad ([Istmt])
removeIdListLinear _ [] = return []
removeIdListLinear kind ("_":ids)
    =  removeIdListLinear kind ids
removeIdListLinear kind (id:ids)
    = do  is1 <- removeIdLinear (Just kind) id
          isrest <-removeIdListLinear kind ids
          return $ is1 ++ isrest


checkCallArgs :: [Qtype] -> [Bool] -> [ Qtype]->
                 SymEntryGlobal ->
                  WriterT CompilerLogs SemStateMonad (Map Identifier Qtype)
checkCallArgs ctypes ctypsok qtypes fentry
    = do  semLog semLLTrace $ "checking Call Args (c) "++(show ctypes)++" (q) "++(show qtypes)
          checkClassicals ctypsok
          let cargs = cargtypes fentry
          argCountOK ctypes cargs
          cmp <- argTypesOK isRigidTypeVar Map.empty ctypes cargs
          let qargs = qargtypes fentry
          argCountOK qtypes qargs
          cmp' <- argTypesOK isRigidTypeVar cmp qtypes qargs
          fixMalleablesInMap cmp'

checkConsArgs :: [Qtype] -> SymEntryGlobal ->
                  WriterT CompilerLogs SemStateMonad (Map Identifier Qtype)
checkConsArgs  qtypes consentry
    = do  semLog semLLTrace $ "checking Cons Args (q) "++(show qtypes)
          let qargs = argtypes consentry
          argCountOK qtypes qargs
          cmp' <- argTypesOK isTypeVar Map.empty qtypes qargs
          semLog semLLDebug3 $ "Resulting map is "++show cmp'
          fixMalleablesInMap cmp'

checkClassicals :: [Bool] ->  WriterT CompilerLogs SemStateMonad ()
checkClassicals clscltypeOK =
    unless(and clscltypeOK) $
           fail "At least one classical parameter is actually quantum"

argCountOK :: [Qtype] ->  [Qtype] ->
               WriterT CompilerLogs SemStateMonad ()
argCountOK   qs rs
           =  let  lqs = length qs
                   lrs = length rs
              in  unless (lqs == lrs) $
                    fail $ (if lqs > lrs then "Too many " else "Too few ") ++
                             "arguments supplied in function call. Expected "++ show rs ++ " but got " ++ show qs


argTypesOK :: (Qtype -> Bool) ->Map Identifier Qtype->[Qtype] ->
              [Qtype] ->
               WriterT CompilerLogs SemStateMonad (Map Identifier Qtype)
argTypesOK vartype mp qs rs
           = do semLog semLLDebug3 $ "checking Arg types: "++(show qs)++" to "++(show rs)
                instanceOfList vartype (zip qs rs) mp



exprIr :: Expression ->  WriterT CompilerLogs SemStateMonad (IrExpression, Qtype, Bool)

exprIr (Eminus e)
     = do (ie, irt, b) <- exprIr e
          case irt of
             INT  ->  return (IrMinus ie , INT, b)
             _    ->  fail $ illegalMinusType (show e) (show irt)

exprIr (Enot e)
     = do (ie, irt, b) <- exprIr e
          case irt of
             BIT   ->  return (IrNot ie , BIT, False)
             BOOL  ->  return (IrNot ie, BOOL, b)
             _     ->  fail $ illegalNotType (show e) (show irt)

exprIr (EQubit q) = return (IrQubit q , QUBIT, False)

exprIr (Ebool b) = return (IrBool b, BOOL, True)

--exprIr (Ebracket e) = exprIr e

exprIr ec@(Econs cid es)
   = do  semLog semLLTrace "In cons expression"
         eirs <- mapM exprIr es
         let (expirs, etypes, classicals) = unzip3 eirs
         consent <- stgloballookup cid
         semLog semLLDebug $ "Checking args in cons, " ++
                showList ( argtypes consent)  " vs. " ++ showList etypes ""
         qmpfinal <- checkConsArgs etypes consent
         semLog semLLDebug3 $ "unified map is " ++ show qmpfinal
         semLog semLLDebug3 $ "Return type is " ++ show (rettypes consent)
         dtype <-  deriveType isTypeVar qmpfinal $ head $ rettypes consent
         semLog semLLDebug1 $ "   Derived -> " ++ show dtype
         -- Unify and return the right type.
         return (  IrCons cid expirs,
                   dtype,
                   False)

exprIr (Ecall nm cexps qexps qids)
     = do  fentry <- stgloballookup nm
           ceirt <- mapM exprIr cexps
           qeirt <- mapM exprIr qexps
           let  (ceir, cetypes, cclassicals) = unzip3 ceirt
                (qeir, qetypes, qclassicals) = unzip3 qeirt
           case (qrettypes fentry) of
             []      -> fail $ callIllegalRets nm
             rtypes  ->
               do  qmpfinal <- checkCallArgs cetypes cclassicals qetypes fentry
                   dtypes <- deriveTypes isRigidTypeVar qmpfinal $ qrettypes fentry
                   addNewLinearEntries qids dtypes SeVar
                   return (  IrExpCall  (callingLabel  fentry) (parmnames fentry)
                                        ceir qeir $ zip qids rtypes,
                             (last dtypes),
                             False)

exprIr (Enum i) = return (IrNum  i, INT, True)
exprIr (Evar id )
        = do  sentry <- lookupClassicalOrLinear id
              case sentry of
                 Right (centry) ->
                     return (  IrCvar id  (coffset centry)
                                          (level centry)
                                          (ctype centry),
                               (ctype centry),
                               True)
                 Left (lentry) ->
                     do noWarnRemoveIdLinear id --modSymTabLinear (Map.delete id)
                        return (  IrVar (name lentry) (qtype lentry),
                                  qtype lentry,
                                  False)

exprIr (Eapply op exp1 exp2 )
        = do  (iexp1, ietype1, b1) <- exprIr exp1
              (iexp2, ietype2, b2) <- exprIr exp2
              unift1 <- semanticUnify ietype1
              unift2 <- semanticUnify ietype2
              if b1 && b2 && binopTypesOK op unift1 unift2
                then  return (Apply op iexp1 iexp2, (optype op), True)
                else  fail $ "Invalid types for operation " ++
                             show op ++ ('(':show (fromEnum op)) ++ ") " ++
                             show ietype1 ++ (':':show b1) ++
                             ", " ++ show ietype2 ++ (':':show b2)

binopTypesOK :: BinOp -> Qtype -> Qtype -> Bool
binopTypesOK  = binopTypesOK' . fromEnum

binopTypesOK' :: Int -> Qtype -> Qtype -> Bool
binopTypesOK' b INT INT    = b < 9 || b > 12
binopTypesOK' b BOOL BOOL  = b >= 9 || b <= 14
binopTypesOK' _ _ _        = False

optype :: BinOp -> Qtype
optype  = optype' . fromEnum

optype' i
      | i < 9      = INT
      | otherwise  = BOOL

flattenIrExp :: IrExpression ->   WriterT CompilerLogs SemStateMonad IrExpression
flattenIrExp (IrNot e)  = flattenIrExp e >>= applyNot
flattenIrExp (Apply op e1 e2 )
                        = do  fe1 <- flattenIrExp e1
                              fe2 <- flattenIrExp e2
                              applyOp op fe1 fe2
flattenIrExp e          = return e

applyNot :: IrExpression ->  WriterT CompilerLogs SemStateMonad IrExpression
applyNot (IrNum  i)  = return (IrNum  (complement i))
applyNot fe          = return (IrNot fe)

applyOp :: BinOp -> IrExpression -> IrExpression ->  WriterT CompilerLogs SemStateMonad IrExpression
applyOp op (IrNum  i1) (IrNum  i2)
     = return (case op of
                         Add -> (IrNum (i1 + i2))
                         Sub -> (IrNum (i1 - i2))
                         Mul -> (IrNum (i1 * i2))
                         Div -> (IrNum (div i1  i2))
                         Rem -> (IrNum (rem i1  i2))
                         Mod -> (IrNum (mod i1 i2))
                         And -> (IrNum (i1 .&. i2))
                         Or -> (IrNum ( i1 .|. i2))
                         Xor -> (IrNum (xor i1 i2))
                         Opeq -> (IrBool $ i1 == i2)
                         Opneq -> (IrBool $ i1 == i2)
                         Oplt -> (IrBool $ i1 < i2)
                         Opgt -> (IrBool $ i1 > i2)
                         Ople -> (IrBool $ i1 <= i2)
                         Opge -> (IrBool $ i1 >= i2)
                         Oplshift -> (IrNum (shift i1 i2))
                         Oprshift -> (IrNum (shift i1 (-i2)))
                         _ -> Apply op (IrNum i1) (IrNum i2))

applyOp op e1 e2 = return (Apply op e1 e2)

\end{code}


\incsubsec{\haskfuncnoref{makeIr}}
\label{haskellfunction:makeIr}
\index{Compiler Functions!semantics!makeIr}
This function will actually run the syntax of the program through the translation process.
Its derivative function \haskfuncdef{ioMakeIr}{Compiler}{semantics}
 applies this to a base state with an empty
symbol table.
\begin{code}
makeIr :: Program -> WriterT CompilerLogs SemStateMonad Iprog
makeIr prog
     = do  emptySymbolTables
           setSemLvl (Lvl 0 0 0 0)
           programir <- progIr prog
           te <- getTypeRelations
           return programir




makeIrWithLL :: Program -> Int -> WriterT CompilerLogs SemStateMonad Iprog
makeIrWithLL prog ll
     = do  emptySymbolTables
           setSemLvl (Lvl 0 0 0 0)
           setLL ll
           programir <- progIr prog
           te <- getTypeRelations
           return programir
           --liftIO $ print te
           --      let pir = subProgTypes  (solve te) programir
           --   return (pir )
{-
solve :: Set TypeRelation -> Map String Qtype
solve ste
    = Map.unions $ Set.elems $ Set.map solve1 ste


solve1 :: TypeRelation -> Map String Qtype
solve1 (ContainedIn (MalleableVariable s) t) = Map.singleton s t
solve1 _ = Map.empty

-}
removeState :: Int -> SemStateMonad (Iprog, CompilerLogs) -> IO (Iprog, CompilerLogs)
removeState loglevel qprogIR
   = do ((a,cl),s)  <-  runStateT qprogIR
          ( SemState (Lvl 0 0 0 0)
            Map.empty Map.empty Map.empty
            0 0 0
            Map.empty
            Map.empty [] (push 0 emptyStack) loglevel )
        return (a,cl++(warnings s))

{-
ioMakeIr :: Program ->Int-> WriterT CompilerLogs IO Iprog
ioMakeIr qprog loglevel
     = do (a,s) <-  runStateT (makeIr qprog)
          ( SemState (Lvl 0 0 0 0)
            Map.empty Map.empty Map.empty
            0 0 0
                        Map.empty
                        Map.empty [] (push 0 emptyStack) loglevel )
          case (warnings s) of
             []     -> tell ["No warnings"]
             warns  -> tell $ reverse warns
    return a

-}

\end{code}
\begin{code}
{-
subProgTypes ::Map String Qtype -> Iprog -> Iprog
subProgTypes mse (Iprog pmap)
             = Iprog $ Map.map (subptypes mse) pmap
subptypes :: Map String Qtype -> Iproc -> Iproc
subptypes mse (Iproc s lbl cins tins touts couts stmts)
  = Iproc s lbl cins tins touts couts $ List.map (substypes mse) stmts

substypes :: Map String Qtype -> Istmt -> Istmt
substypes mse (IuseAssign nn (Left iexp))
  = IuseAssign nn (Left $ subetypes mse iexp)

substypes mse (IuseAssign nn (Right iexp))
  = IuseAssign nn (Right $ subetypes mse iexp)

substypes mse (Iassign nn (Right iexp))
  = Iassign nn (Right $ subetypes mse iexp)
substypes mse (Iassign nn (Left iexp))
  = Iassign nn (Left $ subetypes mse iexp)
substypes mse (Imeas ie s1s s2s)
  = Imeas  (subetypes mse ie) (List.map (substypes mse) s1s) $
              List.map (substypes mse) s2s
substypes mse (Icase ie icc)
  = Icase (subetypes mse ie) $ List.map (subctypes mse) icc
substypes mse (Icall ut s nms ies1 ies2 parms1 parms2)
  = Icall ut s nms (List.map (subetypes mse) ies1)
      (List.map (subetypes mse) ies2)
      (List.map (app2of2 $ subtype mse) parms1)
      (List.map (app2of2 $ subtype mse) parms2)
substypes mse (Ialloc nn t)
  = Ialloc nn $ subtype mse t
substypes mse (Iblock stmts)
  = Iblock (List.map (substypes mse) stmts)
substypes mse s = s

subctypes :: Map String Qtype -> IrCaseClause -> IrCaseClause
subctypes mse (IrCaseClause c ids stmts)
  = IrCaseClause c ids $ List.map (substypes mse) stmts


subetypes :: Map String Qtype -> IrExpression -> IrExpression
subetypes mse (Apply b e1 e2)
          = Apply b (subetypes mse e1) $ subetypes mse e2
subetypes mse (IrNot e)
          = IrNot $ subetypes mse e
subetypes mse (IrVar nn t)
          = IrVar nn $ subtype mse t
subetypes mse (IrExpCall  s nms ices iqes parms)
  = IrExpCall s nms (List.map (subetypes mse) ices)
        (List.map (subetypes mse) iqes) $
         List.map (app2of2 $ subtype mse)  parms
subetypes mse (IrCons c es)
          = IrCons c $ List.map (subetypes mse) es
subetypes mse e = e

subtype ::  Map String Qtype -> Qtype -> Qtype
subtype mse t@(TYPEVAR s)
        = findWithDefault t s mse

subtype _ t = t
-}

addNewLinearEntries ::  [NodeName] ->
                        [Qtype] ->
                        (NodeName -> Qtype -> SymEntryLinear) ->
                         WriterT CompilerLogs SemStateMonad()
addNewLinearEntries names types f
    = do  let  addpairs = List.filter ((/= "_") . fst) $ zip names types
               entries = List.map (uncurry f) addpairs
               skipnames = List.filter (/= "_") names
          semLog semLLDebug $ "addLinEntries "++ showList addpairs "."
          mapM_ addEntry $ zip skipnames entries

addNewClassicalEntries ::  [Int]->
                           [NodeName] ->
                           [Qtype] ->
                           (Int -> NodeName -> Qtype -> SymEntryClassical) ->
                            WriterT CompilerLogs SemStateMonad()
addNewClassicalEntries offsets names types f
    = do  let entries = List.map (uncurry3 f) $ zip3 offsets names types
          mapM_ addClassicalEntry $ zip names entries


\end{code}
\begin{code}


symTabMerge :: SymbolTableLinear -> SymbolTableLinear ->
                WriterT CompilerLogs SemStateMonad ([Istmt], [Istmt], SymbolTableLinear)
symTabMerge st1 st2
    = do  tes <- getTypeRelations
          semLog semLLTrace "merge st - getting map difference"
          semLog semLLDebug2  "First for merge"
          semLog semLLDebug2 $ show st1
          semLog semLLDebug2 $ show "Second for merge"
          semLog semLLDebug2 $ show st2
          -- First, check intersection is OK.
          let intersectmap = Map.intersectionWith (,) st1 st2
          combined <- checkTypesInMapPairs intersectmap
--         apmap <- unifyTESet tes Map.empty
          let  s1minuss2 = Map.difference st1 st2
               s2minuss1 = Map.difference st2 st1
               --s1only = Map.map (updateStTypes apmap) s1minuss2
               --s2only = Map.map (updateStTypes apmap) s2minuss1

          stmts1 <- mapM (uncurry makeDiscard) $ Map.toList s1minuss2 --s1only
          stmts2 <- mapM (uncurry makeDiscard) $ Map.toList s2minuss1 --s2only
          mapM_ (uncurry warnDelete) $ Map.toList  s1minuss2 --s1only
          mapM_ (uncurry warnDelete) $ Map.toList  s2minuss1 --s2only
          --let newst =  Map.intersection st1 st2
          return (stmts1, stmts2, combined) --newst)


warnDelete :: String -> SymEntryLinear ->  WriterT CompilerLogs SemStateMonad()
warnDelete id
    = warnssm . unbalancedCreation id . show . qtype


makeDiscard :: String -> SymEntryLinear ->  WriterT CompilerLogs SemStateMonad (Istmt)
makeDiscard id (SeVar n t)    = return $ Idiscard [n]
makeDiscard id _              = return $ Idiscard []



\end{code}
\begin{code}
checkInArgsDestroyed ::[Identifier]->
                        WriterT CompilerLogs SemStateMonad ()
checkInArgsDestroyed = mapM_ checkNameIsDestroyed


checkNameIsDestroyed :: Identifier ->  WriterT CompilerLogs SemStateMonad()
checkNameIsDestroyed name
    = do  stEntry <- maybeLookup name
          case stEntry of
           Just e   -> fail $ parmInFunctionNotRemoved name
           Nothing  -> return ()

checkOutArgsCreated ::[Identifier]->
                        WriterT CompilerLogs SemStateMonad ()
checkOutArgsCreated = mapM_ checkNameIsCreated


checkNameIsCreated :: Identifier ->  WriterT CompilerLogs SemStateMonad()
checkNameIsCreated  name
    = do  stEntry <- maybeLookup name
          case stEntry of
           Nothing  -> fail $ parmInFunctionNotCreated name
           Just e   -> return ()

\end{code}


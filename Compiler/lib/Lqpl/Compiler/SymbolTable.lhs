8\incsec{Symbol Table}
\label{incsec:symboltable}
Our symbol table structure is comprised of an associative array, matching
the variables hash value with an index and an array.
%if false
\begin{code}
module Lqpl.Compiler.SymbolTable where

import Control.Arrow
import Control.Monad.State

import Control.Monad.Writer

import Data.Char(toUpper, toLower)
import Data.List as List  (map, concat)
import Data.Map as Map

import Lqpl.Compiler.BaseTypes
import Lqpl.Compiler.CompSupport(find,uncapitalize,hasDuplicates, mkflabel)
import Lqpl.Compiler.IrTypes
import Lqpl.Compiler.Qtypes
import Lqpl.Compiler.SemanticErrors
import Lqpl.Compiler.SemTypes
import Lqpl.Compiler.SymbolTableGlobals
import Lqpl.Compiler.TypeUnification

\end{code}
%endif
\incsubsubsec{\haskfuncnoref{makeAnEmptySymbolTable}}
\label{haskellfunction:makeAnEmptySymbolTable}
\index{Compiler Functions!Symbol table!makeAnEmptySymbolTable}
Our first function creates an empty symbol table and sets our state to it.

\begin{code}
emptySymbolTables :: WriterT CompilerLogs SemStateMonad ()
emptySymbolTables =
    do setSymTabLinear Map.empty
       setSymTabClassical Map.empty
       addGlobalTransforms
\end{code}
\incsubsubsec{Standard Symbol Table functions.}
We follow up with the standard symbol table functions. The symbol table
lookup is encapsulated in \haskfuncdef{stlookup}{Compiler}{symbol table}, which uses the helper function
\haskfunc{find} and in turn is used by
\haskfuncdef{idlook}{Compiler}{symbol table}. Entries are stored as a
list of entries in a  map, with the head of the list being
the one currently in scope.

\begin{code}
maybeLookup :: String->WriterT CompilerLogs SemStateMonad (Maybe SymEntryLinear)
maybeLookup name
  = do symtab <- getSymTabLinear
       return $ Map.lookup name symtab

maybeLookupClassical :: String->WriterT CompilerLogs SemStateMonad (Maybe SymEntryClassical)
maybeLookupClassical name
  = do symtab <- getSymTabClassical
       return $ Map.lookup name symtab

lookupClassicalOrLinear :: String ->
                           WriterT CompilerLogs SemStateMonad (Either SymEntryLinear
                                                 SymEntryClassical)
lookupClassicalOrLinear name
    = do llin <- maybeLookup name
         case llin of
           (Just e) -> return $ Left e
           Nothing ->
               do lclassical <- maybeLookupClassical name
                  case lclassical of
                       (Just c) -> return $ Right c
                       Nothing -> fail $ notfound name

dropWithType ::Qtype-> String->WriterT CompilerLogs SemStateMonad ([Istmt], SymEntryLinear)
dropWithType tv id
   = do mentry <- maybeLookup id
        case mentry of
          Just ent -> return ([Idiscard [name ent]],ent)
          Nothing -> do lvl <- getSemLvl
                        let ent =  SeVar id tv
                        addEntry (id, ent)
                        return ([], ent) --Will be created by rename.

classicalStlookup :: String -> WriterT CompilerLogs SemStateMonad SymEntryClassical
classicalStlookup name = do
  sentry <- maybeLookupClassical name
  case sentry of
    (Just sent) -> return sent
    Nothing -> fail $ notfound name

stlookup :: String->WriterT CompilerLogs SemStateMonad SymEntryLinear
stlookup name = do
  sentry <- maybeLookup name
  case sentry of
    (Just sent) -> return sent
    Nothing -> fail $ notfound name

idlook :: String->WriterT CompilerLogs SemStateMonad (NodeName,Qtype)
idlook id
    = do sentry <- stlookup id
         return (name sentry, qtype sentry)

stgloballookup :: String->WriterT CompilerLogs SemStateMonad SymEntryGlobal
stgloballookup name
  = do symtab <- getSymTabGlobal
       let sentry = Map.lookup name symtab
       case sentry of
         (Just sent) -> return sent
         Nothing ->
           let modAndNames = splitTransformMods name
               (mods, name') = (init &&& last) modAndNames
               sentry' = Map.lookup name' symtab
           in  case sentry' of
                  (Just sent') -> return $ appTMods mods sent'
                  Nothing -> fail $ glblNotFound name

splitTransformMods :: String -> [String]
splitTransformMods s
   = if '-' `elem` s
      then let (f, '-':s') = span (/= '-') s
               s'' = splitTransformMods s'
           in (f:s'')
      else [s]

appTMods :: [String] -> SymEntryGlobal -> SymEntryGlobal
appTMods ("Inv":rest) se
   = let se' = appTMods rest se
     in appInvMod se
appTMods ("C":rest) se
   = let se' = appTMods rest se
     in appCMod se
appTMods _ se = se

appInvMod :: SymEntryGlobal -> SymEntryGlobal
appInvMod (SeFun nm dc1 (Just g) dc2 dc3 inqb outqb dc4)
          = SeFun ("Inv-" ++ nm) dc1 (Just (Inverse g)) dc2 dc3 outqb inqb dc4

appInvMod x = x

appCMod :: SymEntryGlobal -> SymEntryGlobal
appCMod (SeFun nm dc1 (Just g) dc2 dc3 inqb outqb dc4)
          = SeFun ("C-" ++ nm) dc1 (Just (Controlled g)) dc2 dc3
                (QUBIT:inqb) (QUBIT:outqb) dc4

appCMod x = x

\end{code}

\incsubsec{Adding to the symbol table.}
We add all of the entries for a scope to the symbol table at once.
\incsubsubsec{\haskfuncnoref{addToSymTab}}
\label{haskellfunction:addToSymTab}
\index{Compiler Functions!symbol table!addToSymTab}
Modifies a symbol table by adding the given list of symbol table entries
to the current entry for an identifier.

\begin{code}
--addToSymTab :: SymbolTable->String->SymEntry->SymbolTable
--addToSymTab st key entries
--     =Map.unionWith (++) st (Map.singleton key entries)
\end{code}
\incsubsubsec{\haskfuncnoref{addEntriesList}}
\label{haskellfunction:addEntriesList}
\index{Compiler Functions!symbol table!addEntriesList}
Working within the monad, takes a list of pairs of identifiers and corresponding
symbol table entries and adds them to the current symbol table.

\begin{code}
{-
addKeyValPairToMapC :: Ord k => (a -> a -> a) ->  Map.Map k a -> (k,a)
              -> Map.Map k a
addKeyValPairToMapC f mp (key,val) = Map.insertWith f key val mp

addListToMapC :: Ord k => (a -> a -> a) ->  Map.Map k a -> [(k,a)]
              -> Map.Map k a
addListToMapC f mp [] = mp
addListToMapC f mp (entry:entries)
     =  addListToMapC f (addKeyValPairToMapC f mp entry) entries
-}
addEntriesList  :: [(String,SymEntryLinear)]->WriterT CompilerLogs SemStateMonad()
addEntriesList  = modSymTabLinear . Map.union . Map.fromList

addClassicalEntriesList  :: [(String,SymEntryClassical)]->WriterT CompilerLogs SemStateMonad()
addClassicalEntriesList  = modSymTabClassical . Map.union . Map.fromList
\end{code}
\incsubsubsec{\haskfuncnoref{addEntry}}
\label{haskellfunction:addEntry}
\index{Compiler Functions!symbol table!addEntry}
Again, working withing the monad, adds a single entry to the symbol table.

\begin{code}
addEntry :: (String,SymEntryLinear)->WriterT CompilerLogs SemStateMonad()
addEntry (key, entry)
    = modSymTabLinear (Map.insert key entry)

addClassicalEntry :: (String,SymEntryClassical)->WriterT CompilerLogs SemStateMonad()
addClassicalEntry (key, entry) = modSymTabClassical (Map.insert key entry)

\end{code}

addNewType ::Statement ->WriterT CompilerLogs SemStateMonad()
addNewType (DataDeclaration dd)
    = addTypeEntry dd

addNewType _ = return ()

\incsubsec{\haskclassnoref{SymTabEntry}}
\label{haskellclass:SymTabEntry}
\index{Compiler Classes!Symbol table!SymTabEntry}
We create a class to encapsulate the chores of being able to add entries
for each of the
syntactic data types.

\begin{code}
class SymTabEntry a where
\end{code}
\incsubsubsec{\haskfuncnoref{updateSymTab}}
\label{haskellfunction:updateSymTab}
\index{Compiler Functions!symbol table!updateSymTab}
Makes and adds a new entry.

\begin{code}
  updateSt :: a->Maybe Qtype -> WriterT CompilerLogs SemStateMonad ()
  updateSt a t = makeEntry a t >>= addEntry

\end{code}
\incsubsubsec{\haskfuncnoref{updateSymTabList}}
\label{haskellfunction:updateSymTabList}
\index{Compiler Functions!symbol table!updateSymTabList}
Make and add entries for a list of syntactic elements.

\begin{code}
  updateStList :: [(a, Maybe Qtype)]->WriterT CompilerLogs SemStateMonad ()
  updateStList elts = makeEntriesList elts >>=  addEntriesList

\end{code}
\incsubsubsec{\haskfuncnoref{makeEntry}}
\label{haskellfunction:makeEntry}
\index{Compiler Functions!symbol table!makeEntry}
Actually create the entry information and the string that will
be used to key on it.

\begin{code}
  makeEntry :: a->Maybe Qtype->WriterT CompilerLogs SemStateMonad (String, SymEntryLinear)

\end{code}
\incsubsubsec{\haskfuncnoref{makeEntries}}
\label{haskellfunction:makeEntries}
\index{Compiler Functions!symbol table!makeEntries}
Make multiple entries for a single syntactic entry.
This can be used for those items,
such as blocks, which are containers of multiple items.


Then the process is repeated for the scope 2, where |x| will be placed in scope.
The calling of these for this program can be traced as follows:
\begin{verbatim}
makeEntries on the program
...makeEntriesList on the statements
...makeEntries on the first statement (proc)
.....makeEntry on the proc
\end{verbatim}

\begin{code}
  makeEntries ::  a -> Maybe Qtype->WriterT CompilerLogs SemStateMonad [(String,SymEntryLinear)]
  makeEntries a t = do
    (k,x) <- makeEntry a t
    return [(k,x)]
\end{code}
\incsubsubsec{\haskfuncnoref{makeEntriesList}}
\label{haskellfunction:makeEntriesList}
\index{Compiler Functions!symbol table!makeEntriesList}
Default processing for  a list of entries.

\begin{code}
  makeEntriesList ::  [(a, Maybe Qtype)]->WriterT CompilerLogs SemStateMonad [(String,SymEntryLinear)]
  makeEntriesList [] = return []
  makeEntriesList (x:xs) = do
    y <- uncurry makeEntries x
    ys<-makeEntriesList xs
    return (y++ys)
\end{code}


\incsubsec{Instances of the \haskclass{SymTabEntry}}
\label{incsec:symtabentry:instances}
We declare instances for each of the syntactic entries.

\begin{code}

addTypeEntry:: GlobalDefinition -> WriterT CompilerLogs SemStateMonad ()
addTypeEntry (DataDef td@(TypeDefinition tid vids) cons)
    = do modSymTabGlobal (Map.insert tid $
                               SeData tid vids (List.map consname cons))
         addConsEntries td  cons 0

addTypeEntry (ProcDef _)
    = return ()

addConsEntries ::TypeDefinition->[Constructor]->
                 Int->WriterT CompilerLogs SemStateMonad ()
addConsEntries _ [] _ = return ()
addConsEntries td (c:cs) offset
    = do addConsEntry td c offset
         addConsEntries td cs (offset+1)

addConsEntry ::TypeDefinition->Constructor->
               Int->WriterT CompilerLogs SemStateMonad ()
addConsEntry  (TypeDefinition tid vids) (Constructor cid argtypes) offset
    = modSymTabGlobal (  Map.insert cid $
                         SeCons cid offset argtypes
                         [DeclaredType tid (List.map  TypeVariable vids)])


addTypesToSt :: [GlobalDefinition] -> WriterT CompilerLogs SemStateMonad ()
addTypesToSt [] = return ()
addTypesToSt (gd:gds)
    = do addTypeToSt gd
         addTypesToSt gds

addTypeToSt :: GlobalDefinition -> WriterT CompilerLogs SemStateMonad ()
addTypeToSt = addTypeEntry

addProcsToSt :: [GlobalDefinition] -> WriterT CompilerLogs SemStateMonad ()
addProcsToSt [] = return ()
addProcsToSt (gd:gds)
    = do addProcToSt gd
         addProcsToSt gds
pnames :: (Procedure -> [ParameterDefinition]) ->
          Procedure -> [NodeName]
pnames   = (List.map parmId .)

addProcToSt :: GlobalDefinition -> WriterT CompilerLogs SemStateMonad ()
addProcToSt (DataDef _ _) = return ()
addProcToSt (ProcDef pd) = do
  cdl <- newfunclbl
  catypes <- gettypes $ inclassicalparms pd
  qatypes <- gettypes $ inquantumparms pd
  qrtypes <- gettypes $ outquantumparms pd
  crtypes <- gettypes $ outclassicalparms pd
  let   se  = SeFun  {cdlabel = cdl ,
                      gname       = procnm pd,
                      transform   = Nothing,
                      parmnames   = ((pnames inclassicalparms pd ,
                                      pnames outclassicalparms pd),
                                      (pnames inquantumparms pd,
                                      pnames outquantumparms pd)),
                      cargtypes   = catypes,
                      qargtypes   = qatypes,
                      qrettypes   = qrtypes,
                      crettypes   = crtypes}
  modSymTabGlobal (Map.insert (procnm pd) se)

instance SymTabEntry Procedure where
  makeEntry (Procedure{}) _    = fail addingProcToLinSt

  makeEntries (Procedure nm _ qfroms _ _ _) _  = do
    setargs 0
    makeEntriesList $ zip qfroms $ repeat Nothing

paramDefinitionSymEntry id typ = do
  lvl <- getSemLvl
  nargs <- gets numArgs
  incargs
  return (id, SeLArg (-nargs) id typ)

instance SymTabEntry ParameterDefinition where
  makeEntry (ParameterDefinition id typ) Nothing  = paramDefinitionSymEntry id typ
  makeEntry (ParameterDefinition id t) (Just typ) = paramDefinitionSymEntry id typ

updateClassicalParm :: ParameterDefinition -> WriterT CompilerLogs SemStateMonad ()
updateClassicalParm (ParameterDefinition id typ)
  = case typ of
      INT   -> addCparm id typ
      BOOL  -> addCparm id typ
      _     -> fail $ illegalClassicalType id (show typ)


addCparm :: Identifier -> Qtype -> WriterT CompilerLogs SemStateMonad()
addCparm id typ =  do
  off <- getOffset
  decOffset
  lvl <- getSemLvl
  addClassicalEntry (id, SeCArg off lvl id typ)

updateClassicalListP :: [ParameterDefinition] -> WriterT CompilerLogs SemStateMonad()
updateClassicalListP = mapM_ updateClassicalParm

instance SymTabEntry Statement where

\end{code}
Responsible for counting the number of bits as well.

|makeEntries| will add to the storage as needed and then prepare the actual
symbol table entries for addition to the symbol table. |makeEntry| will get
the information needed to build the entry and return those values.

These should actually only return something for bit and qubit allocations.
No other statements actually affect the symbol table of any statements
following them. \emph{Within} the statement, (e.g. and proc) there may
be changes to the symbol table, but that is to be taken care of by the
creation of the IR.
\begin{code}

  makeEntries _ _ = return []
  makeEntry (Assignment id exp) typ = do
    sentry <- maybeLookup id
    case sentry of
      Just se  -> fail $ alreadyDefined (show se)
      Nothing  -> do
        cstr <- getTypeConstraints
        let mt = Map.lookup id cstr
        case typ of
          Nothing -> do
            tv <- newTypeVar
            return (id,SeVar id (MalleableVariable  tv))
          Just t -> do
            containedInOrFailm t mt
            return (id, SeVar id t)


  makeEntry s _  = fail $ illegalMakeEntStatement $ show s


instance SymTabEntry Expression where
  makeEntry e _ = fail $ illegalMakeEntExp $ show e


\end{code}
\incsubsubsec{\haskfuncnoref{procDefCalls}}
\label{haskellfunction:procDefCalls}
\index{Compiler Functions!Symbol Table!procDefCalls}
NOTUSED

Was used to decide upon recursive calling - now we call everything
recursively

procDefCalls ::  [Statement] -> String-> Bool
procDefCalls  block name
       = foldl (\b s->b || (statementCalls name s)) False block

\incsubsubsec{\haskfuncnoref{statementCalls}}
\label{haskellfunction:statementCalls}
\index{Compiler Functions!Symbol Table!statementCalls}

statementCalls :: String->Statement -> Bool
statementCalls name (ProcResult _ nm _) = name == nm
statementCalls name (Call nm _ _) = name == nm
statementCalls name (BlockStatement blk) = procDefCalls blk name
statementCalls name (Measure _ s1 s2) =
  statementListCalls name s1 || (statementListCalls name s2)
statementCalls name (Proc (Procedure _ _ _ blk)) =
  procDefCalls blk name
statementCalls _ _ = False

statementListCalls :: String -> [Statement] -> Bool
statementListCalls nm
   = (foldl (||) False) . (List.map (statementCalls nm))


\incsubsubsec{\haskfuncnoref{newfunclbl}}
\label{haskellfunction:newfunclbl}
\index{Compiler Functions!Symbol Table helper functions!newfunclbl}

\begin{code}
newfunclbl :: WriterT CompilerLogs SemStateMonad String
newfunclbl = do
  s <- getlabel
  inclabel
  return ("fcdlbl"++show s)

\end{code}
\incsubsubsec{\haskfuncnoref{gettype}}
\label{haskellfunction:gettype}
\index{Compiler Functions!Symbol Table helper functions!gettype}

\begin{code}

gettypes :: [ParameterDefinition] -> WriterT CompilerLogs SemStateMonad [Qtype]
gettypes = mapM gettype

gettype :: ParameterDefinition ->WriterT CompilerLogs SemStateMonad Qtype
gettype (ParameterDefinition _ t) =  return t

callingLabel :: SymEntryGlobal -> String
callingLabel (SeFun gnm cdl _ _ _ _ _ _)
             = mkflabel gnm cdl
callingLabel _ = error callinglblOnCons

varMap :: [Identifier] -> WriterT CompilerLogs SemStateMonad (Map Identifier Identifier)
varMap [] = return Map.empty
varMap (a:aas) = do
  tvar <- newTypeVar
  tmp <- varMap aas
  return $ Map.insert a tvar tmp
{-
freshTvars :: [Qtype] -> WriterT CompilerLogs SemStateMonad ([Qtype], Map Identifier String )
freshTvars = freshTvars' Map.empty

freshTvars' :: Map Identifier String -> [Qtype] ->
                WriterT CompilerLogs SemStateMonad ([Qtype], Map Identifier String)
freshTvars' m [] = do return ([],m)
freshTvars' m (t:ts) =
    do (t',m') <- freshTvar m t
       (ts',m'') <- freshTvars' m' ts
       return (t':ts', m'')

freshTvar :: Map Identifier String -> Qtype ->
             WriterT CompilerLogs SemStateMonad (Qtype, Map Identifier String)
freshTvar m t
    | isBaseType t = return (t,m)
    | isTypeVar t = let Just tv = getTypeVar t
                        tventry = Map.lookup tv m
                    in case tventry of
                         Nothing -> do tvar <- newTypeVar
                                       return (TYPEVAR tvar,
                                                       Map.insert tv tvar m)
                         Just tvr -> return (TYPEVAR tvr, m)
    | otherwise = let dtypes = getDeclTypes t
                  in do (dtypes',m') <- freshTvars' m dtypes
                        let t' = setDeclTypes t dtypes'
                        return (t',m')



-}

\end{code}

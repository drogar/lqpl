\incsec{Types for Symbol Table}
\label{inscec:typessymboltable}
%if false
\begin{code}
module Compiler.SemTypes where
import Control.Monad.State
import Control.Monad.Writer

import Data.Map as Map
import Data.List as List( delete, map)
import Data.Set as Set

import Compiler.Qtypes
import Compiler.IrTypes
import Compiler.BaseTypes

import Data.Tuples
import Data.Stack as Stack

\end{code}
%endif

\incsubsubsec{\hasktypenoref{SymEntry}}
\label{haskelltype:SymEntry}\index{Compiler Data Types!symbol table!SymEntry}
This type is the basic entry in the symbol table, holding our information about
either arguments, variables or functions that are known at a particular point
in time.

For arguments and variables, we track the offset, which level it is at, its name and type.
Functions track their level, name, a generated code label, argument types and return types.

Currently types are limited to \bit, \qubit{} and a variable length integer.
\CodeContinueNumbers
\begin{code}


data SymEntryGlobal
    =  SeData {
               gname::String,
               steTypeVars ::[Identifier],
               steConstructors :: [ConsIdentifier] }
    |  SeCons {
               gname::String,
               goffset::Int,
               argtypes::[Qtype],
               rettypes::[Qtype] }-- Always just one..
    |  SeFun {
              gname::String,
              cdlabel::String,
              transform :: Maybe UnitaryTransform,
              parmnames :: (([NodeName],[NodeName]),
                            ([NodeName],[NodeName])),
              cargtypes::[Qtype],
              qargtypes::[Qtype],
              qrettypes::[Qtype],
              crettypes::[Qtype] }
     deriving (Eq,Show)

data SymEntryClassical
    = SeUse  {cname::NodeName,
              coffset::Int ,
              level::Level,
              ctype::Qtype }
    | SeCArg  {
              coffset::Int,
              level::Level,
              cname::NodeName,
              ctype::Qtype }
   | SeCVar  {level :: Level,
              coffset::Int,
              cname::NodeName,
              ctype::Qtype }
     deriving (Eq,Show)

data SymEntryLinear
    = SeCaseId  {
                 offset::Int ,
                 name::NodeName,
                 qtype::Qtype }
    | SeLArg  {
              offset::Int ,
              name::NodeName,
              qtype::Qtype }
    | SeVar {
             name::NodeName,
             qtype::Qtype }
     deriving (Eq,Show)


\end{code}
%endif
\incsubsubsec{\hasktypenoref{SymbolTable}}
\label{haskelltype:SymbolTable}\index{Compiler Data Types!symbol table!SymbolTable}
The symbol table is held a a map from strings (the identifier) to symbol
table entries. As we will be keeping a stack of symbol tables in the
semantic analysis phase, we need only keep the
current current definition of a particular variable.
\CodeContinueNumbers
\begin{code}
type SymbolTableLinear     = Map.Map String SymEntryLinear
type SymbolTableGlobal     = Map.Map String SymEntryGlobal
type SymbolTableClassical  = Map.Map String SymEntryClassical

\end{code}

\begin{code}
nameAndType :: SymEntryLinear -> (NodeName, Qtype)
nameAndType se = (name se, qtype se)
\end{code}

\CodeContinueNumbers
\begin{code}


data TypeRelation = ContainedIn Qtype Qtype
                  deriving (Eq, Show, Ord)

\end{code}

\incsubsubsec{\haskfuncnoref{offLvlType}}\label{haskellfunction:offLvlType}\index{Compiler Functions!Symbol Table!offLvlType}
This gets the offset, the level and the current types.
\CodeContinueNumbers
\begin{code}

makeUseEntries :: Int -> Level -> [SymEntryLinear] -> [SymEntryClassical]
makeUseEntries  = mkUseEntries'

mkUseEntries' :: Int-> Level -> [SymEntryLinear] -> [SymEntryClassical]
mkUseEntries' i l [] = []
mkUseEntries' i l (SeVar nn t : es)
    = SeUse nn i l t : mkUseEntries' (i-1) l es
mkUseEntries' i l (SeLArg o nn t : es)
    = SeUse nn i l t : mkUseEntries' (i-1) l es
mkUseEntries' i l (SeCaseId o nn t : es)
    = SeUse nn i l t : mkUseEntries' (i-1) l es

inUse :: SymEntryClassical -> Bool
inUse = insideUse . level


-- subInArgs :: [SymEntry]->[SymEntry]->[SymEntry]
-- subInArgs [] _ = []
-- subInArgs (q:qs) ss= (subInArgsList q ss) : (subInArgs qs ss)

subInArgsList :: [SymEntryLinear] ->[SymEntryLinear] -> [SymEntryLinear]
subInArgsList [] _ = []
subInArgsList ((SeCaseId offset name QUBIT):qs) ss
        = (ss !!(-(1 + offset))) : subInArgsList qs ss
subInArgsList (q:qs) ss
        = q : subInArgsList qs ss


argsmatch :: SymEntryGlobal->[SymEntryLinear]->Bool
argsmatch se  =
     foldl (&&) True . zipWith (==) (argtypes se) . List.map qtype



selectQBits :: [SymEntryLinear]->[SymEntryLinear]
selectQBits [] = []
selectQBits (s:ss)
             | qtype s == QUBIT = s:selectQBits ss
             | otherwise = selectQBits ss
\end{code}
\incsubsubsec{\hasktypenoref{SemanticState}}
\label{haskelltype:SemanticState}\index{Compiler Data Types!symbol table!SemanticState}
The monad used in the semantic analysis phase carries the current
level (function and block), the symbol table, the storage requirement, the
number of arguments and a counter for creating new code labels.
\incsubsec{Declaration Level}
\label{incsec:monadsupport}
The semantic analysis and code generation makes extensive use of
Exception-State-IO monads. We simplify the use of the monads by the definition
of various functions and a data type.
\CodeContinueNumbers
\begin{code}
data SemanticState
     = SemState {semLvl :: Level,
                 stateStabGlobal :: SymbolTableGlobal,
                 stateStabLinear :: SymbolTableLinear,
                 stateStabClassical :: SymbolTableClassical,
--               stor :: Storage,
                 numArgs :: Int,
                 idCounter :: Int,
                 typeVarCounter :: Int,
                 typeConstraints :: Map Identifier Qtype, --Return vars
                 typeEquations :: Map Identifier Qtype, -- malleables
                 warnings :: [String],
                 cStackOffset :: Stack Int,
                 logLevel ::Int
                }
\end{code}
\incsubsubsec{\hasktypenoref{SemStateMonad}}
\label{haskelltype:SemStateMonad}\index{Compiler Data Types!symbol table!SemStateMonad}
A monad based on a state transformer of the \hasktypenoref{SemanticState} and
\hasktypenoref{IO}.
\CodeContinueNumbers
\begin{code}
type SemStateMonad   =  StateT SemanticState IO
\end{code}
%if false
\CodeContinueNumbers
\begin{code}
semLog :: Int -> String -> WriterT CompilerLogs SemStateMonad ()
semLog ll output
    =  do loglev <-  gets logLevel
          when (ll <= loglev) $  tell [output]


semLLAlways, semLLTrace, semLLDebug3, semLLDebug2, semLLDebug1, semLLDebug, semLLInfo, semLLWarn, semLLError, semLLFail :: Int

semLLAlways = 0
semLLTrace = 9
semLLDebug3 = 8
semLLDebug2 = 7
semLLDebug1 = 6
semLLDebug = 5
semLLInfo = 4
semLLWarn = 3
semLLError = 2
semLLFail = 1

dumpst  :: Int-> WriterT CompilerLogs SemStateMonad ()
dumpst ll
     = getSymTabLinear >>= (semLog ll . show)

dumpg :: Int ->  WriterT CompilerLogs SemStateMonad ()
dumpg ll
    = do  semLog ll  "Type SymTab"
          stabt <- getSymTabGlobal
          semLog ll (show stabt)

dumpsts  ::  Int ->  WriterT CompilerLogs SemStateMonad ()
dumpsts ll
     = do  semLog ll "Reg SymTab"
           stab <- getSymTabLinear
           semLog ll "got stab"
           semLog ll $ show stab
           dumpg ll
           semLog ll "Classical SymTab"
           stabc <- getSymTabClassical
           semLog ll $ show stabc


\end{code}
%endif

\begin{code}

\end{code}
\incsubsec{|SemStateMonad| accessors}
\label{haskellfunction:getSemLvl}
\label{haskellfunction:setSemLvl}
\label{haskellfunction:getSymTab}
\label{haskellfunction:setSymTab}
\label{haskellfunction:getQubitUsage}
\label{haskellfunction:setQubitUsage}
\label{haskellfunction:addQubitUsage}
\index{Compiler Functions!Semantic Analysis!|SemStateMonad| accessors}
\begin{code}

incOffsetByN :: Int->  WriterT CompilerLogs SemStateMonad ()
incOffsetByN n =  modify (\st -> let  (top, offStack) = pop $ cStackOffset st
                                       in   st{cStackOffset = push (n+ top) offStack})

incOffset ::  WriterT CompilerLogs SemStateMonad ()
incOffset = incOffsetByN 1

decOffset ::  WriterT CompilerLogs SemStateMonad ()
decOffset = incOffsetByN (-1)

decOffsetByN :: Int ->  WriterT CompilerLogs SemStateMonad()
decOffsetByN = incOffsetByN . negate

setOffsetStack :: Stack Int ->  WriterT CompilerLogs SemStateMonad ()
setOffsetStack sis =  modify (\st -> st{cStackOffset = sis})

popOffset ::  WriterT CompilerLogs SemStateMonad Int
popOffset = do  (top, cstack) <-  gets (pop . cStackOffset)
                setOffsetStack cstack
                return top

getOffset ::  WriterT CompilerLogs SemStateMonad Int
getOffset =  gets (fst . pop . cStackOffset)

pushOffsetN :: Int ->  WriterT CompilerLogs SemStateMonad ()
pushOffsetN n = do  cso <- gets cStackOffset
                    modify (\st -> st{cStackOffset = push n cso})
pushOffset ::  WriterT CompilerLogs SemStateMonad ()
pushOffset = pushOffsetN 0

copyAndPushOffset :: WriterT CompilerLogs  SemStateMonad ()
copyAndPushOffset =  getOffset >>= pushOffsetN

getSemLvl  ::   WriterT CompilerLogs SemStateMonad Level
getSemLvl =  gets semLvl

setSemLvl  ::  Level ->  WriterT CompilerLogs SemStateMonad ()
setSemLvl l =  modify (\ state -> state {semLvl = l})

warnssm :: String ->  WriterT CompilerLogs SemStateMonad ()
warnssm warning =  modify (\ssm -> ssm {warnings = warning : warnings ssm})

modSemTopOfStack ::  (SemanticState -> Stack a)->
                     (Stack a -> SemanticState->SemanticState)->
                     (a -> a) ->  WriterT CompilerLogs SemStateMonad ()
modSemTopOfStack getit putit modit
     = do  st <-  gets getit
           let  (s, st') = pop st
                s' = modit s
           setSemStack putit $ push s' st'

getSemStack  :: (SemanticState -> Stack a)->  WriterT CompilerLogs SemStateMonad a
getSemStack  =  gets . (Stack.get .)

popSemStack  :: (SemanticState -> Stack a)->
                 (Stack a -> SemanticState->SemanticState)->
                      WriterT CompilerLogs SemStateMonad a
popSemStack getit putit
     =  do  st <- gets getit
            let (s, st') = pop st
            setSemStack putit st'
            return s

pushSemStack ::  (SemanticState -> Stack a)->
                  (Stack a -> SemanticState->SemanticState) ->
                  a ->  WriterT CompilerLogs SemStateMonad ()
pushSemStack getit putit st
     = do  stk <-  gets getit
           setSemStack putit $ push st stk

setSemStack  ::   (Stack a -> SemanticState->SemanticState) ->
                   Stack a ->  WriterT CompilerLogs SemStateMonad ()
setSemStack = (modify .)

{-
getSymTabClassical :: SemStateMonad (SymbolTableClassical)
getSymTabClassical = getSemStack stateStabClassical

updateSymTabClassical :: Stack SymbolTableClassical ->
                             SemanticState ->SemanticState
updateSymTabClassical s st = st{stateStabClassical = s}

pushSymTabClassical :: SymbolTableClassical -> SemStateMonad ()
pushSymTabClassical = pushSemStack stateStabClassical updateSymTabClassical
-}

updateSymTabLinear :: SymbolTableLinear ->SemanticState ->SemanticState
updateSymTabLinear s st = st{stateStabLinear = s}

updateSymTabGlobal :: SymbolTableGlobal ->SemanticState ->SemanticState
updateSymTabGlobal s st = st{stateStabGlobal = s}

updateSymTabClassical :: SymbolTableClassical ->SemanticState ->SemanticState
updateSymTabClassical s st = st{stateStabClassical = s}


modSymTabGlobal :: (SymbolTableGlobal -> SymbolTableGlobal) ->
                    WriterT CompilerLogs SemStateMonad ()
modSymTabGlobal f
    =  modify (\ state -> state{stateStabGlobal
                                   = f $ stateStabGlobal state})

modSymTabLinear :: (SymbolTableLinear -> SymbolTableLinear) ->
                    WriterT CompilerLogs SemStateMonad ()
modSymTabLinear f
    =  modify (\ state -> state{stateStabLinear
                                   = f $ stateStabLinear state})

modSymTabClassical :: (SymbolTableClassical -> SymbolTableClassical) ->
                       WriterT CompilerLogs SemStateMonad ()
modSymTabClassical f
    =  modify (\ state -> state{stateStabClassical
                                   = f $ stateStabClassical state})


getSymTabGlobal  ::   WriterT CompilerLogs SemStateMonad SymbolTableGlobal
getSymTabGlobal =   gets stateStabGlobal

getSymTabLinear  ::  WriterT CompilerLogs SemStateMonad SymbolTableLinear
getSymTabLinear =   gets stateStabLinear

getSymTabClassical  ::   WriterT CompilerLogs SemStateMonad SymbolTableClassical
getSymTabClassical =   gets stateStabClassical

setSymTabGlobal  ::  SymbolTableGlobal ->  WriterT CompilerLogs SemStateMonad ()
setSymTabGlobal  = modSymTabGlobal . const

setSymTabLinear  ::  SymbolTableLinear ->  WriterT CompilerLogs SemStateMonad ()
setSymTabLinear = modSymTabLinear . const

setSymTabClassical  ::  SymbolTableClassical ->  WriterT CompilerLogs SemStateMonad ()
setSymTabClassical  = modSymTabClassical . const


inclabel  ::   WriterT CompilerLogs SemStateMonad ()
inclabel =  modify (\ state ->
                    state {idCounter
                           = 1+idCounter state})

inctypevar  ::   WriterT CompilerLogs SemStateMonad ()
inctypevar =  modify (\ state ->
                          state {typeVarCounter
                                     = 1+typeVarCounter state})

incargs  ::   WriterT CompilerLogs SemStateMonad ()
incargs =  modify (\ state ->
                   state {numArgs = 1 + numArgs state})
decargs  ::   WriterT CompilerLogs SemStateMonad ()
decargs =  modify (\ state ->
                   state {numArgs = (-1) + numArgs state})

getlabel  ::   WriterT CompilerLogs SemStateMonad Int
getlabel =  gets idCounter

getTypeConstraints ::  WriterT CompilerLogs SemStateMonad (Map Identifier Qtype)
getTypeConstraints = gets typeConstraints

setTypeConstraints :: Map Identifier Qtype ->  WriterT CompilerLogs SemStateMonad ()
setTypeConstraints mp
    = modify (\ state -> state{typeConstraints = mp})

addTypeConstraints :: [(Identifier,Qtype)] ->  WriterT CompilerLogs  SemStateMonad ()
addTypeConstraints = mapM_ (uncurry addTypeConstraint)

addTypeConstraint :: Identifier -> Qtype -> WriterT CompilerLogs  SemStateMonad ()
addTypeConstraint iden typ
   = modify (\ state ->
                state {typeConstraints =
                           Map.insert iden typ $ typeConstraints state})

remTypeConstraint :: Identifier ->  WriterT CompilerLogs SemStateMonad ()
remTypeConstraint iden
   = modify (\ state ->
                state {typeConstraints =
                           Map.delete iden $ typeConstraints state})

getTypeRelations ::  WriterT CompilerLogs SemStateMonad (Map Identifier Qtype)
getTypeRelations = gets typeEquations

addTypeRelation :: Identifier -> Qtype -> WriterT CompilerLogs  SemStateMonad ()
addTypeRelation mlblvr typ
    = modify (\state -> state {typeEquations =
                                   Map.insert mlblvr typ (typeEquations state)})

newTypeVar ::  WriterT CompilerLogs SemStateMonad String
newTypeVar =
    do  tvc <- gets typeVarCounter
        modify (\s -> s{typeVarCounter = 1 + tvc})
        return $ "tv" ++ show tvc

setargs  ::  Int ->  WriterT CompilerLogs SemStateMonad ()
setargs n = modify (\state -> state{numArgs = n})

setLL  ::  Int ->  WriterT CompilerLogs SemStateMonad ()
setLL n = modify (\state -> state{logLevel = n})


\end{code}


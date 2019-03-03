\begin{code}
module Lqpl.Compiler.SemanticErrors where
import Lqpl.Compiler.SemTypes
import Control.Monad.Writer
import Lqpl.Compiler.BaseTypes

semanticError = "Semantic Error: "

qToCError :: String -> String -> String
qToCError ex id =
    semanticError ++ "Can not assign quantum data " ++ ex ++
                " to classical variable " ++ id

notfound :: String -> String
notfound i = semanticError ++ "Variable "++i ++ " not found."

glblNotFound :: String -> String
glblNotFound nm = semanticError ++ "Type, Cons or Proc " ++
                     nm ++ " not found."

measureNotQubit :: String -> String -> String
measureNotQubit nm typ
     = semanticError ++ " Measure requires a Qubit. The measure expression, '"++
         nm ++ "' is of type "++ typ

controlNotQubit :: String -> String
controlNotQubit clist
     = semanticError ++ " All controls must be a Qubit. The control list was " ++
         "found" ++ clist

duplicateQubitsInCall :: String ->String
duplicateQubitsInCall nm = semanticError ++
      " The same Qubit was used in more than one place in the call to "++nm

procCallTypeError :: String -> String ->String -> String
procCallTypeError nm prmtypes exptypes
   = semanticError ++ "Call to procedure "++nm++" with non-unifiable types." ++
       " Expected Types:" ++ prmtypes ++ "; Got types "++exptypes

semanticwarn = "Semantic Warning: "

convertClassToLin  :: String -> String -> String
convertClassToLin  iden expr
    = semanticwarn ++ "Converting classical var to quantum(linear), in "
      ++ iden ++ " = " ++ expr

unbalancedCreation :: String -> String -> String
unbalancedCreation id qtype
   = semanticwarn ++ "Unbalanced creation, discarding " ++ id ++
          " of type " ++ qtype
idsNotUsed :: String->String -> String
idsNotUsed kind id
    = semanticwarn ++ kind ++" id '"++id++"' not used in dependent code. Deleted at end of dependent code."

internalerror = "Internal Compiler Error: "

callinglblOnCons = internalerror ++
     "Tried to create a code label for a Constructor or Type entry"

addingProcToLinSt = internalerror ++
     "Tried to add a procedure to the linear symbol table."

illegalMakeEntStatement :: String -> String
illegalMakeEntStatement s = internalerror ++
     "Tried to call MakeEntry on statement : " ++ s

illegalMakeEntExp :: String -> String
illegalMakeEntExp s = internalerror ++
     "Tried to call MakeEntry on expression : " ++ s

notyetimp :: String
notyetimp = "Feature not yet implemented: "

checkQubitUsageInParms :: String
checkQubitUsageInParms = notyetimp ++ "Checking Qubit usage in parameters."

guardClauseType :: String -> String
guardClauseType = ("Guard clause must be a classical BOOL type, not " ++ )

dupInList :: [String] -> String
dupInList = ("The control list has a duplicate: " ++) . flip showList "."

ctrlScopeErr :: [String]  -> WriterT CompilerLogs SemStateMonad ()
ctrlScopeErr [] = return () ;
ctrlScopeErr cl  =
    do tell cl
       fail $ "The controlled code has created variables which are also in the current control list: "++ showList cl "."

callIllegalRets :: String -> String
callIllegalRets nm  =   semanticError ++ "Call of " ++ nm ++
                        " not allowed in expression as there is no return variable."

callIllegalArgs :: String -> String
callIllegalArgs nm =  semanticError ++ "Call of " ++ nm ++
                        " called with illegal arguements."

alreadyDefined :: String -> String
alreadyDefined nm = semanticError ++"You can not redefine the variable '"++nm ++ "'. Please 'discard' it first."

illegalClassicalType :: String -> String -> String
illegalClassicalType nm typ
    = semanticError ++
      "Classical variables must be of type INT or BOOL. You tried to make '"++
      nm++"' into classical of type '"++typ++"'."

illegalMinusType :: String -> String -> String
illegalMinusType exp typ
    = semanticError ++
      "Minus (-) is only defined on Int values. The expression '"++exp++
      "' is of type '"++typ++"'."

illegalNotType :: String -> String -> String
illegalNotType exp typ
    = semanticError ++
      "Not (~) is only defined on Boolean values. The expression '"++exp++
      "' is of type '"++typ++"'."

illegalTypeVar ::String -> String
illegalTypeVar t
    = semanticError ++ "The type '"++t++
      "' has a type variable not defined in this context."

constructorTypeError ::String -> String
constructorTypeError cexp
    = semanticError ++ "The constructor arguments in the expression '"++
      cexp++"' are of incorrect types."

parmInFunctionNotRemoved :: String -> String
parmInFunctionNotRemoved  str
    = semanticError ++ "The in parameter "++ str++
      " was not used (and destroyed) in the procedure where it was defined."

parmInFunctionNotCreated :: String -> String
parmInFunctionNotCreated  str
    = semanticError ++ "The out parameter "++ str++
      " was not created in the procedure where it was defined."

\end{code}

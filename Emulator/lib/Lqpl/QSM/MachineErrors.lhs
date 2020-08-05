%include polycode.fmt
\subsection{Error Definitions}\label{subsec:errordefinitions}

\begin{singlespace}
\begin{code}
module Lqpl.QSM.MachineErrors where
import Lqpl.QSM.QuantumStack.QSDefinition
import Lqpl.Data.Matrix
import Lqpl.QSM.BasicData

badDiscard :: String
badDiscard =  "MachineException: Can not discard when multiple branches exist"
backwardsJump :: String
backwardsJump = "MachineException: Jumping back - must iterate"

wrongCall :: String
wrongCall = "MachineException: Call must be processed by Stream functions"

useDataCheck :: String
useDataCheck =  "MachineException: Use must be done on a StackInt node"

splitDataCheck :: String
splitDataCheck = "MachineException: Can not use other than StackCons in a split"

measureDataCheck :: String
measureDataCheck = "MachineException: Measure must be done on a StackQubit node"

qcontrolBadEnd :: String
qcontrolBadEnd =  "MachineException: End of QControl with invalid Dump Element."

bindDataCheck :: String
bindDataCheck = "MachineException : Bind of non Cons stack"

bindMultiCons :: String
bindMultiCons = "MachineException : Bind with multiple cons"

unbindDataCheck :: String
unbindDataCheck = "MachineException : UnBind of non Cons stack"

unbindNothingBound :: String
unbindNothingBound = "MachineException: UnBinding with no bound variables"

unbindBadCons :: String
unbindBadCons = "MachineException : UnBind with multiple cons"

matByStackError :: String -> String -> String
matByStackError typ bt
  = "MachineException: Cannot transform from a "++typ ++ ". Data is "++bt

stackByMatError :: String -> String -> String
stackByMatError  typ bt
  = "MachineException: Cannot transform to a "++typ ++ ". Data is "++bt

stackToMatError :: String
stackToMatError = "MachineCheck: toMatrix: Operations on negative number of qubits illegal."

setValsError :: String
setValsError = "MachineCheck: setValsFromMat: Operations on negative number of qubits illegal."

setValsDataError :: String
setValsDataError = "MachineCheck: setValsFromMat: You may not set the values of non-Qubit elements in the stack"

setValsTypeError :: (Quantum b) => Int ->
                    Matrix (QuantumStack b) ->
                    QuantumStack b -> String
setValsTypeError n m q =
    "MachineCheck: SetValsFromMat error on n=" ++ show n ++
         ";   m="++ showMat m ++" ;;;;   q="++ show q

clsstackError :: String
clsstackError = "MachineCheck: CStack operation: Insufficient Stack elements for classical operation"

clsstackTypeError :: String
clsstackTypeError = "MachineCheck: CStack operation: Invalid types for classical operation"

badRotate :: String -> String -> String
badRotate nm nms
    = "MachineCheck: Rotate: "++nm++
      " not rotated up. The names found are:"
      ++ nms

badqsAdd :: String -> String ->String
badqsAdd s1 s2
   = "MachineCheck: Qstack add: Undefined add of stacks: "++
      s1 ++", and "++ s2++"."

badqsmul :: String
badqsmul = "MachineERROR: Trying to multiply quantum stacks."
\end{code}
\end{singlespace}

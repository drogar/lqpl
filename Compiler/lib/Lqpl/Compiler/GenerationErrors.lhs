\begin{code}
module Lqpl.Compiler.GenerationErrors where
import Lqpl.Compiler.IrTypes
import Lqpl.Compiler.Qtypes (Qtype, UnitaryTransform)

generationError = "Code Generation Error: "

noProcName =
    generationError ++"Looking for a proc name in the procstack and it is empty."

noControlStartName =
    generationError ++"Trying to start control but there is no name in the list passed in."

illegalTypeToLoad :: Qtype -> String
illegalTypeToLoad qt =
    generationError ++"Type " ++ show qt ++ " is not valid when using LOAD."

illegalTypeToAllocate :: Qtype -> String
illegalTypeToAllocate qt =
    generationError ++"Type " ++ show qt ++ " is not allowed in allocation."

illegalTransform :: UnitaryTransform -> String
illegalTransform ut =
    generationError ++"Trying to generate code for transform '"++ show ut ++"' but no qubits were supplied to instruction."

unableToDetermineQstackTop :: String
unableToDetermineQstackTop = generationError ++ "Unable to determine top of the stack after code generation of expression."

iceNoidsWithUse :: String
iceNoidsWithUse = generationError++"Use construct with no variables."

unsetTop :: IrExpression -> Istmt -> String
unsetTop e meas = generationError ++
                  "Unset the stack top when generating for the expression '"++
                  show e ++"' in the statment '"++ show meas ++"'."
\end{code}

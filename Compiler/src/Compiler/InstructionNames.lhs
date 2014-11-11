\begin{code}
module Compiler.InstructionNames where

import Data.List

inameNewint      = "QMove"
inameNewqubit    = "QLoad"
inameNewdata     = "QCons"
inameDiscard     = "QDiscard"
inameBind        = "QBind"
inameUnbind      = "QUnbind"
inameTransform   = "QApply"
inameMeasure     = "Measure"
inameSplit       = "Split"
inameUse         = "Use"
inameSwapd       = "SwapD"
inameJump        = "Jump"
inameCjump       = "CondJump"
inameCall        = "Call"
inameReturn      = "Return"
inamePullup      = "QPullup"
inameDelete      = "QDelete"
inameRename      = "Rename"
inameEnscope      = "EnScope"
inameDescope      = "DeScope"
inameLoadi       = "CLoad"
inameCget        = "CGet"
inameCput        = "CPut"
inameCoperation  = "CApply"
inameCpop        = "CPop"
inameNoop        = "NoOp"
inameZerostack   = "ZeroStack"
inamePopcontrol  = "UnCtrl"
inamePushcontrol  = "AddCtrl"
inameControlit    = "QCtrl"

\end{code}

\begin{code}

inamepartPut          = "put"
inamepartCombine      = "combine"
inamepartControlled   = "Controlled"
inamepartHad          = "Hadamard"
inamepartNot          = "NOT"
inamepartV            = "'V'"
inamepartW            = "'W'"
inamepartToff         = "Toffoli"
inamepartX            = "'X'"
inamepartRotate       = "Rotate"

glue :: [String] -> String
glue = ("    " ++) . unwords

glue2 :: String -> String -> String
glue2 first second = glue [first, second]

glue3 :: String -> String -> String -> String
glue3 first second third = glue [first,second,third]

--glue4 :: String -> String -> String -> String-> String
--glue4 first second third fourth
--    = "    "++ first ++ " " ++ second ++ " " ++ third++ " " ++ fourth


assemDirectiveStartproc :: String -> String
assemDirectiveStartproc  = (++ "   Start")

assemDirectiveEndproc :: String
assemDirectiveEndproc = "   EndProc"

assemDirectiveStarttrns :: String -> String
assemDirectiveStarttrns  = (++ "   Trans")

assemDirectiveEndtrns :: String
assemDirectiveEndtrns = "   EndTrans"

\end{code}

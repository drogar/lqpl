\begin{code}
module Compiler.InstructionNames where

import Data.List

iname_newint      = "QMove"
iname_newqubit    = "QLoad" 
iname_newdata     = "QCons"
iname_discard     = "QDiscard"
iname_bind        = "QBind"
iname_unbind      = "QUnbind"
iname_transform   = "QApply"
iname_measure     = "Measure"
iname_split       = "Split"
iname_use         = "Use" 
iname_swapd       = "SwapD" 
iname_jump        = "Jump"
iname_cjump       = "CondJump"
iname_call        = "Call"
iname_return      = "Return"
iname_pullup      = "QPullup"
iname_delete      = "QDelete"
iname_rename      = "Rename"
iname_enscope      = "EnScope"
iname_descope      = "DeScope"
iname_loadi       = "CLoad"
iname_cget        = "CGet"
iname_cput        = "CPut"
iname_coperation  = "CApply"
iname_cpop        = "CPop"
iname_noop        = "NoOp"
iname_zerostack   = "ZeroStack"
iname_popcontrol  = "UnCtrl"
iname_pushcontrol  = "AddCtrl"
iname_controlit    = "QCtrl"

\end{code}

\begin{code}

inamepart_put          = "put"
inamepart_combine      = "combine"
inamepart_controlled   = "Controlled"
inamepart_Had          = "Hadamard"
inamepart_Not          = "NOT"
inamepart_V            = "'V'"
inamepart_W            = "'W'"
inamepart_Toff         = "Toffoli"
inamepart_X            = "'X'"
inamepart_rotate       = "Rotate"

glue :: [String] -> String
glue = ("    " ++) . concat . (intersperse " ")

glue2 :: String -> String -> String
glue2 first second = glue [first, second]

glue3 :: String -> String -> String -> String
glue3 first second third = glue [first,second,third]

--glue4 :: String -> String -> String -> String-> String
--glue4 first second third fourth 
--    = "    "++ first ++ " " ++ second ++ " " ++ third++ " " ++ fourth


assem_directive_startproc :: String -> String
assem_directive_startproc  = (++ "   Start")

assem_directive_endproc :: String
assem_directive_endproc = "   EndProc"

assem_directive_starttrns :: String -> String
assem_directive_starttrns  = (++ "   Trans")

assem_directive_endtrns :: String
assem_directive_endtrns = "   EndTrans"

\end{code}

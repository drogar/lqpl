\begin{code}
module InstructionNames where

iname_newint = "loadi"
iname_newqubit = "loadq" 
iname_newdata = "loadd"
iname_discard = "discard"
iname_setint = "set"
iname_transform = "transform"
iname_cond = "cond" 
iname_swap = "swap" 
iname_pop = "pop" 
iname_measure = "measure"
iname_switch = "switch"
iname_case = "case" 
iname_startloop = "loop"
iname_endloop = "endloop"
iname_call = "call"
iname_return = "return"
iname_callrec = "callrec" 
iname_pullup = "pullup"
iname_createname = "createname"
iname_usename = "usename"
iname_rename = "rename"
iname_genrename = "genrename" 
iname_loadi = "cload"
\end{code}

--iname_putHad = inamepart_put ++"put Hadamard"
iname_putCHad = "put Controlled Hadadmard"
iname_putNot = "put NOT"
iname_putCNot = "put Controlled NOT"
iname_putV = "put 'V'"
iname_putCV = "put Controlled 'V'"
iname_putW = "put 'W'"
iname_putCW = "put Controlled 'W'"
iname_putToff = "put Toffoli"
iname_putX = "put 'X'"
iname_combineHad = "combine Hadamard"
iname_combineCHad = "combine Controlled Hadamard"
iname_combineNot = "combine NOT"
iname_combineCNot = "combine Controlled NOT"
iname_combineV = "combine 'V'"
iname_combineCV = "combine Controlled 'V'"
iname_combineW = "combine 'W'"
iname_combineCW = "combine Controlled 'W'"
iname_combineToff = "combine Toffoli"
iname_combineX = "combine 'X'"

\begin{code}

iname_use = "use" 
iname_enduse = "enduse"
--iname_cspushimm = "cload"
iname_csget = "cget"
iname_csadd = "csadd"
iname_cssub = "csaub"
iname_csmul = "csaul"
iname_csmod = "cssmod"
iname_csdiv = "csdiv"
iname_csneg = "csneg"
iname_cseq = "cseq"
iname_csneq = "csneq"
iname_cslt = "cslt"
iname_cslte = "cslte"
iname_csgt = "csgt"
iname_csgte = "csgte"
iname_csand = "csand"
iname_csor = "csor"
iname_csxor = "csxor"
iname_csnot = "csnot"
iname_csshl = "csshl"
iname_csshr = "csshr"
iname_cspop = "cspop"

inamepart_put = "put"
inamepart_combine = "combine"
inamepart_controlled = "Controlled"
inamepart_Had = "Hadamard"
inamepart_Not = "NOT"
inamepart_V = "'V'"
inamepart_W = "'W'"
inamepart_Toff = "Toffoli"
inamepart_X = "'X'"
inamepart_rotate = "Rotate"

glue2 :: String -> String -> String
glue2 first second = first ++ " " ++ second

glue3 :: String -> String -> String -> String
glue3 first second third = first ++ " " ++ second++ " " ++ third
\end{code}

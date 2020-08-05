%include polycode.fmt
%format ^* = "\ltimes"
%format *^ = "\rtimes"

\subsection{XML output of the quantum stack}\label{subsec:quantumstackxml}

%if false
\begin{code}

module Lqpl.QSM.QuantumStack.QSXML
    (toXML)
 where

import Lqpl.QSM.BasicData
import Lqpl.QSM.QuantumStack.QSDefinition

\end{code}
%endif

{\begin{figure}[htbp]
\begin{singlespace}
\begin{code}

xmlDiagVal :: Bool -> Bool -> String
xmlDiagVal False _ = ""
xmlDiagVal True vl = " onDiag='"++(show vl) ++ "'"

xmlCvalues :: Quantum b => Bool -> StackDescriptor b ->
              [QuantumStack b] -> String
xmlCvalues _ (StackClassical _) [] = ""
xmlCvalues showd (StackClassical (c1:crest)) (q1:qrest) =
                "<cval value='"++(show c1)++">\n"++
                               (toXML showd q1) ++ "\n</cval>\n" ++
                (xmlCvalues showd (StackClassical crest) qrest)


xmlDvalues :: Quantum b => Bool -> StackDescriptor b ->
              [QuantumStack b] -> String
xmlDvalues _ (StackData _) [] = ""
xmlDvalues showd (StackData ((dc1,das1):drest)) (q1:qrest) =
                "<dval constructor='"++(show dc1)++
               "addresses='"++(showList das1 "'>\n") ++
                                (toXML showd q1) ++ "\n</dval>\n" ++
                (xmlDvalues showd (StackData drest) qrest)

xmlQvalues :: Quantum b => Bool ->
              [QuantumStack b] -> String
xmlQvalues showd  [q00,q01,q10,q11]
               = "<qval value='00'>\n"++
                 (toXML showd q00) ++ "\n</qval>\n" ++
               "<qval value='01'>\n"++
                 (toXML showd q01) ++ "\n</qval>\n" ++
               "<qval value='10'>\n"++
                 (toXML showd q10) ++ "\n</qval>\n" ++
               "<qval value='11'>\n"++
                 (toXML showd q11) ++ "\n</qval>\n"


toXML :: Quantum b => Bool -> QuantumStack b -> String
toXML showDiag qs
      | isStackZero qs       = "<zero />\n"
      | isStackValue qs
          = case descriptor qs of
              StackValue b ->   "<value value='"++(show b) ++ "'/>\n"
      | isStackClassical qs
           = "<classical address='"++show (address qs) ++
             (xmlDiagVal showDiag (onDiagonal qs)) ++"'>\n" ++
             (xmlCvalues showDiag (descriptor qs) (subStacks qs)) ++
             " </classical>"
      | isStackQubit qs
           = "<quantum address='"++show (address qs) ++
             (xmlDiagVal showDiag (onDiagonal qs)) ++"'>\n" ++
             (xmlQvalues showDiag (qvalues qs)) ++
             " </quantum>"
      | otherwise  -- stackData
        = "<data address='"++show (address qs) ++
             (xmlDiagVal showDiag (onDiagonal qs)) ++"'>\n" ++
             (xmlDvalues showDiag (descriptor qs) (subStacks qs)) ++
              "</data>"

\end{code}
\end{singlespace}
\caption{XML out of Qstack}\label{fig:haskellDefinitionOfQstackXML}
\end{figure}}

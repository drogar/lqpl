%include polycode.fmt
%format ^* = "\ltimes"
%format *^ = "\rtimes"

\subsection{Description of the quantum stack}\label{subsec:quantumstackdescription}

Code related to producing the pieces required for viewing the
quantum stacks.

%if false
\begin{code}

module QSM.QuantumStack.QSViewQueries
    (lefttrees, righttrees,leftcount,namesAndNodes
    )
 where
import QSM.BasicData
import Data.ClassicalData

import QSM.QuantumStack.QSDefinition


import Data.List


\end{code}
%endif
{\begin{singlespace}
\begin{code}

leftcount :: [a] -> Int
leftcount = (`div` 2) . length


lefttrees :: (Quantum b) =>
             QuantumStack b ->
             [QuantumStack b]
lefttrees q
    | isStackQubit q      = take 2 $ qvalues q
    | isStackLeaf q       = []
    | otherwise           = let ss = subStacks q
                            in take (max 1 $ leftcount ss) ss


righttrees :: (Quantum b) =>
             QuantumStack b ->
             [QuantumStack b]
righttrees q
    | isStackQubit q    = drop 2 $ qvalues q
    | isStackLeaf q     = []
    | otherwise         = let ss = subStacks q
                          in drop (leftcount ss) ss




namesAndNodes :: (Quantum b) => QuantumStack b ->
                 [(String,QuantumStack b)]
namesAndNodes q
 = case (descriptor q) of
        StackQubit qbts        -> let (ss,qbvals) = dropzsQ (qvalues q) fullQbs
                                      in case qbvals of
                                            StackZero      -> []
                                            StackQubit qs  ->
                                                zip (map showQv qs) ss
        StackClassical cbts    -> zip (map showCv cbts) (subStacks q)
        StackData dbts         -> zip (map showSplitdv dbts) (subStacks q)
        _                      -> []
    


showSplitdv :: (Constructor,[StackAddress])
               -> String
showSplitdv (cons,sas)
            = (shows cons . showList sas) ""

\end{code}
\end{singlespace}
}


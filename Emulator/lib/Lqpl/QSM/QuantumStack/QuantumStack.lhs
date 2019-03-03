%include polycode.fmt
%format ^* = "\ltimes"
%format *^ = "\rtimes"

\subsection{Description of the quantum stack}\label{subsec:quantumstackdescription}

%if false
\begin{code}

module Lqpl.QSM.QuantumStack.QuantumStack
    (
     module Lqpl.QSM.QuantumStack.QSDefinition,
     module Lqpl.QSM.QuantumStack.QSManipulation,
     module Lqpl.QSM.QuantumStack.QSRotation
    )
 where
import Lqpl.QSM.QuantumStack.QSDefinition
import Lqpl.QSM.QuantumStack.QSManipulation
import Lqpl.QSM.QuantumStack.QSRotation

\end{code}

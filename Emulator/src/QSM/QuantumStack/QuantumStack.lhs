%include polycode.fmt
%format ^* = "\ltimes"
%format *^ = "\rtimes"

\subsection{Description of the quantum stack}\label{subsec:quantumstackdescription}

%if false
\begin{code}

module QSM.QuantumStack.QuantumStack 
    ( 
     module QSM.QuantumStack.QSDefinition,
     module QSM.QuantumStack.QSManipulation,
     module QSM.QuantumStack.QSRotation
    )
 where
import QSM.QuantumStack.QSDefinition
import QSM.QuantumStack.QSManipulation
import QSM.QuantumStack.QSRotation

\end{code}
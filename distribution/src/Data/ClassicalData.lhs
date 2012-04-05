%include polycode.fmt
\subsection{Classical data and the classical operations}
\label{subsec:classicaldata}
A datatype used as elements of the classical stack in the quantum stack
machine.
%if false
\begin{code}
module Data.ClassicalData(
    ClassicalData,
    ClassicalOp(..),
    showCv,
    module Data.EitherLocal) where
 
import Data.EitherLocal
\end{code}
%endif

{\begin{singlespace}
\begin{code}
type ClassicalData = Either Int Bool

data ClassicalOp = CAdd | CSub | CTimes | CDiv |
                   CMod | CRem | CNeg | COr | CAnd |CXor |
                   CNot | CEq | CNeq | CLt | CLte | CGt | CGte |
                   CShl | CShr 
  deriving (Eq, Read, Show)

showCv :: ClassicalData -> String
showCv (Left i) = show i
showCv (Right t) = show t
\end{code}
\end{singlespace}
}


%include polycode.fmt
\subsection{Class for complex numbers}\label{subsection:classcomp}
A simple class that extends |Data.Complex| with a predefined
value for $i$ and a conjugation operation.
%if false
\begin{code}
 module Lqpl.Data.ClassComp(Comp(..)) where
 import Data.Complex

\end{code}
%endif
\incsubsubsec{\haskclassnoref{Num}}
\label{haskellclass:Num}
\index{Interpretor Classes!support!Num}
{\begin{singlespace}
\begin{code}
 class (Floating a)=>Comp a where
     conjgt :: a->a
     sqrtMinusOne :: a
     approximate :: a -> Complex Double
     mag :: a -> Double
     pow :: Int -> a

-- instance (RealFloat a) => Comp (Complex a) where
--     conjgt = conjugate
--     sqrtMinusOne = 0 :+ 1
--     approximate = id
--     mag = magnitude
--     pow i = (10.0 ^^ i) :+ 0

\end{code}
\end{singlespace}
}

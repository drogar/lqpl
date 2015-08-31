%include polycode.fmt
\subsection{Basis definitions for quantum computing}
\label{subsec:Basis:basisdefinitions}
Quantum values are typically written as:
\[\sum_i \alpha_i b_i\]
where the $\alpha_i$ are complex values and the $b_i$ are
orthogonal basis vectors in the space of our interest.
A typical representation is to consider
the complex plane and use two unit vectors
\ket{0} and \ket{1}. In \cite{sabry03:qcinH}, a method
of representing a basis and quantum values over the basis
is discussed. This module implements  the essence of the idea and the code
for the |Basis| class from that paper.
There are  differences in the actual representation,
due to the use of a density matrix representation
for \qubits{} and the quantum stack being defined for
multiple node types.

%if false
\begin{code}
module Data.Basis(Basis(..),
	     OurBasis(..),
             showQv,
	     offdiag) where
import Data.List as List (elemIndex)
import Data.Map as Map

\end{code}
%endif

The |Basis| class contracts that the type will have a defined list
of basis elements, purported to be orthogonal. Due to the representations
used for actual elements of the stack, the implementation  requires that
any type a member of the |Basis| class is also a member of |Eq| and |Ord|.

\begin{figure}[htbp]
\begin{singlespace}
\begin{code}
class (Eq a, Ord a) => Basis a where
    basis            ::  [a]
    ei               ::  a->Int
    ei a             =   fromMaybe (error "Just shouldn't happen") (elemIndex a basis)
    ind              ::  a -> a -> Int
    ind a c          =   length (basis::[a]) * ei a + ei c
    toggle           ::  a -> a
    toggle a         =   basis !! ((1 + ei a) `mod` length (basis::[a]))
    isFirst          ::  a -> Bool
    isFirst          =   (==  head basis)
    assocBasis       ::  [b]-> [(a,b)]
    assocBasis       =   zip basis
    assocPairsBasis  ::  [b] -> [((a,a),b)]
    assocPairsBasis  =   zip [(a,b) | a<-basis, b<-basis ]
\end{code}
\end{singlespace}
\caption{Haskell definition of a Basis}\label{fig:haskellClassBasis}
\end{figure}

Once  a type is defined as being an instance  of |Basis|, the
instance of |Basis| for the pair type is straightforward.

{\begin{singlespace}
\begin{code}
instance (Basis a) => Basis (a,a) where
    basis = [(a,b) | a<-basis, b<-basis ]
    toggle (a,b) = (toggle a, toggle b)
\end{code}
\end{singlespace}
}

The implementation detailed in this thesis and in
\lqpl{} uses  one basis, \ket{0} and
\ket{1}.

{\begin{singlespace}
\begin{code}
data OurBasis = Zero | One
	      deriving (Eq, Show, Read, Ord)

instance Basis OurBasis where
    basis = [Zero, One]
    isFirst Zero = True
    isFirst One = False
    toggle Zero = One
    toggle One = Zero

offdiag :: (Basis a, Show a, Num b) => [a] -> [((a,a),b)]
offdiag basis = [((a,b), 0) |
                    a<- basis, b<-basis, a /= b]

showQv :: (OurBasis, OurBasis) -> String
showQv (Zero,Zero) = "00"
showQv (Zero,One) = "01"
showQv (One,Zero) = "10"
showQv (One,One) = "11"
\end{code}
\end{singlespace}
}
%if false
{-
instance Basis (OurBasis, OurBasis) where
    basis = [(Zero,Zero), (Zero,One), (One,Zero), (One,One)]
    toggle (Zero,Zero) = (One,One)
    toggle (Zero,One) = (One,Zero)
    toggle (One,Zero) = (Zero,One)
    toggle (One,One)  = (Zero,Zero)
  -}

%endif

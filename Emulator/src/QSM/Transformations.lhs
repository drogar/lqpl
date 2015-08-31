%include polycode.fmt
\subsection{Unitary transformations}\label{subsec:unitarytransformations}
Unitary transformations are the basic operations for \qubit{}s. In the
quantum stack machine,  a number of basic transformations are defined
and capacity for  the programmer to define their own is anticipated.

Theoretically, this is not required. All unitary transformations
effects can be approximated to within any $\epsilon > 0$ by
repeated applications of a few basic transformation.

Practically, however, the determination of what those transforms should
be is not straightforward.

%if false
\begin{code}

module QSM.Transformations (Trans,
                            UnitaryOp(..),
                            getTransform,
          i2by2,
          controlled,
                            notGate,
                            oneOvSqr2,
                            oneHalf,
          diag)
    where
import Data.Matrix
import Data.ClassComp
import Data.Permutation
\end{code}
%endif

The following data definition is
 a \emph{meta-type} of unitary operations, which
defines the operations for the quantum stack machine.
\begin{code}

data UnitaryOp =
\end{code}
The quantum stack machine has the following completely built-in
transformations:
\begin{code}
       Ident Int| NotGate | Tgate | Phase |  Hadamard | MinusI |
       RhoX |  RhoY | RhoZ | Swap | Toffoli | Rotate| UM |
\end{code}

Of these above transforms, note that |Rotate| and |UM| are
\emph{parameterized} transforms. |Rotate| is expressible as the matrix
\[\begin{bmatrix}
1&0\\
0&e^{2\pi i/2^n}
\end{bmatrix}\]
where the $n$ controls the amount of rotation applied. The
transform |UM| requires three parameters, $a,n$ and $t$. It
creates the unitary matrix that will apply the transform
\[ a \rightarrow x^{a 2^t} \mod n \]
which is required for Shor's factoring algorithm. Note this
is expected to be generalized in the future, see \vref{chap:futurework}.

Additionally, |Controlled| transformations are easily created from
the original transformation.
\begin{code}
       Controlled UnitaryOp |
       Inverse UnitaryOp |
\end{code}
Finally, the type includes
 \emph{named} transforms. These would be the transforms
defined in a quantum assembly or linear qpl program.  At this stage,
the type  simply tracks a name,
the number of \qubit{s} and the number of integer
parameters used by the transform. This is not currently used in the
machine or the compiler but is expected to be essential in the future.
\begin{code}
       DefinedOp String Int
\end{code}
%if false
\begin{code}
                deriving (Eq, Read, Show)
\end{code}
%endif
To get the actual matrix used for transformation, the |getTransform| function
translates from |UnitaryOp| to |Trans|. Below are a
 few of the interesting cases.
\begin{singlespace}
\begin{code}
getTransform :: (Comp b) => [Either Int Bool] -> UnitaryOp -> Trans b

getTransform ((Left k):_) Rotate    = rotateGate k
getTransform l (Controlled g)       = controlled $ getTransform l g
getTransform l (Inverse g)          = conjtrans $ getTransform l g
getTransform l (DefinedOp s k)      = error "To be defined yet"
getTransform _ NotGate              = notGate
getTransform _ (Ident n)            = i2by2
getTransform _ Tgate                = tGate
getTransform _ Phase                = phaseGate
getTransform _ Hadamard             = had
getTransform _ MinusI               = minusIdentity
getTransform _ RhoX                 = rhox
getTransform _ RhoY                 = rhoy
getTransform _ RhoZ                 = rhoz
getTransform _ Swap                 = swapGate
getTransform _ Toffoli              = toffoli

\end{code}
%endif

Transformations are simply matrices. See \vref{subsec:matrices} for
their definition. Some of the specific matrices are shown below.

The function |controlled| is used to create a controlled version of a
transformation, by creating a block matrix with the identity $2\times2$
matrix in the upper left and the matrix of the subject
transformation in the lower right.

\begin{singlespace}
\begin{code}
type Trans a = Matrix a

notGate :: (Comp b) =>Trans b
notGate =  [[0,1],[1,0]]

rotateGate :: (Comp b) => Int -> Trans b
rotateGate k
    =   [[1,0],
         [0, exp (pi * sqrtMinusOne /
                        (2 ** (fromInteger (toInteger k) -1)))]]
diag :: (Num a) =>Trans a -> Trans a -> Trans a
diag tl br
    = [a ++ take (dimx br) (map fromInteger zeros) |
          a <-  tl] ++
   [ take (dimx tl)  (map fromInteger zeros) ++ a |
           a <-  br]

controlled :: (Comp b) =>Trans b -> Trans b
controlled = diag i2by2
\end{code}
\end{singlespace}

%if false
\begin{code}
oneOvSqr2 :: (Comp b) => b
oneOvSqr2 =  fromInteger 1 / sqrt (fromInteger 2)

oneHalf :: (Comp b) => b
oneHalf =  fromInteger 1 / fromInteger 2

had :: (Comp b) =>Trans b
had =  [[oneOvSqr2, oneOvSqr2], [oneOvSqr2, (- oneOvSqr2)]]

minusIdentity :: (Comp b) =>Trans b
minusIdentity  =  [[-1,0],[0,-1]]

rhox  :: (Comp b) =>Trans b
rhox = notGate

tGate :: (Comp b) =>Trans b
tGate =  [[1,0],[0,sqrt sqrtMinusOne]]

phaseGate :: (Comp b) =>Trans b
phaseGate =  [[1,0],[0, sqrtMinusOne]]

i2by2 :: (Comp b) =>Trans b
i2by2 =  [[1,0],[0,1]]

rhoy:: (Comp b) =>Trans b
rhoy =  [[0,-sqrtMinusOne],[sqrtMinusOne,0]]

rhoz :: (Comp b) =>Trans b
rhoz =  [[1,0],[0,-1]]


swapGate :: (Comp b) =>Trans b
swapGate =  [[1,0,0,0],
       [0,0,1,0],
       [0,1,0,0],
       [0,0,0,1]]

toffoli :: (Comp b) =>Trans b
toffoli = diag (diag i2by2 i2by2) $ controlled notGate


zeros::[Integer]
zeros = 0:zeros

\end{code}
\end{singlspace}

\subsubsection{Computing a transform from a permuation of the basis vectors.}
As transforms are represented by matrices of size $2^n \times 2^n$, it is
possible to "lift" a permutation to this size. For example, a permutation
on $7$ elements would be lifted to one on $8$, a permuation on $11$ would be
lifted to one on $16$. This is used in creating the matrix for the
|UM| transformation.

\begin{singlespace}
\begin{code}
{- unused
permOrder :: Perm -> Int
permOrder p = qorder' 1 ((domain p) - 1)

lifted ::Perm -> Perm
lifted p = liftPerm (permOrder p) p

permuteCols :: Perm -> Matrix a -> Matrix a
permuteCols = permuteList

orderFindGate ::(Num a) => Int -> Int -> Int -> Matrix a
orderFindGate n k t
   = let  um::(Num a)=> Matrix a
          um = permuteCols (orderFindPerm n k) $ idMat $ (1+maxNum n)
     in mat2ToTheT um t
-}
\end{code}
\end{singlespace}
%endif

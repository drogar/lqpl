%include polycode.fmt
\subsection{Permutations}\label{subsec:permutations}
Our permutations are written as a pair between the size
of the permutation and a function from |Int -> Int| which
gives the actual permutation.
%if false
\begin{code}
module Data.Permutation(Perm(..),
                        liftPerm,
                        orderFindPerm,
                        permuteList,
                        maxNum
                       ) where
import Data.Bits
\end{code}
%endif

\begin{singlespace}
\begin{code}
data Perm = Perm {domain :: Int,
                  runPerm :: Int -> Int}
\end{code}
\end{singlespace}

%if false
\begin{code}
instance Eq Perm where
    a == b =
        not (domain a /= domain b) &&
                [runPerm a i | i <- [0..(domain a)]] ==
                [runPerm b i | i <- [0..(domain b)]]

instance Show Perm where
   showsPrec _ a =
       showString (show (domain a)) . showString ":" .
       showList [(i,runPerm a i) | i <- [0..(domain a)]]
\end{code}
%endif

We provide a way to extend the permutation to higher domains. This will
be extensively used in the creation of transformations from permutations.
The lifted permutation will be the identity on all
elements not in the original domain.

{\begin{singlespace}
\begin{code}
liftPerm :: Int -> Perm -> Perm
liftPerm newdomain p =
    if newdomain < domain p
     then error "Perms can not be restricted"
     else Perm newdomain f
         where f i | i <= domain p = runPerm p i
                   | otherwise = i

permuteList :: Perm -> [a] -> [a]
permuteList p aas = [aas !! runPerm p j |
                     j <- [0..(length aas - 1)]]



orderFindPerm :: Int -> Int -> Perm
orderFindPerm n
     = Perm (maxNum n) . aToTheKModN n


numBits :: Int -> Int
numBits = numBits' 0

numBits' :: Int -> Int -> Int
numBits' size 0 = size
numBits' size n = numBits' (1 + size) (n `shiftR` 1)


maxNum :: Int -> Int
maxNum n = 2 ^ numBits n -1

aToTheKModN :: Int -> Int -> Int -> Int
aToTheKModN n k a
   | a >= n = a
   | otherwise = (a^k) `mod` maxNum n

\end{code}
\end{singlespace}
}

%include polycode.fmt
\subsection{Definition of matrices and matrix functions}
\label{subsec:matrices}

A Haskell class for the handling of matrices. A simple representation of lists of lists are used.
%if false
\begin{code}

module Data.Matrix (
  Matrix,
  mmap,dimx,qorder,dimy,conjtrans,idMat,mat2ToTheT,qorder',
  showMat,genmatmul,indexM,grab,reduceM
)
 where
import Data.ClassComp
import Data.Bits
import Data.List
\end{code}
%endif
\begin{code}

type Matrix a  = [[a]]
	     
mmap :: (a ->b) -> Matrix a -> Matrix b
mmap   =  map . map 
\end{code}
Accessor functions |dimx, dimy| are defined to provide the matrix
 dimensions.

{\begin{singlespace}
\begin{code}

dimx::Matrix a -> Int
dimx  = length 

qorder :: Matrix a -> Int
qorder = qorder' 0 . dimx 

qorder'::Int->Int -> Int
qorder' acc val =
     if val < 2 then acc
	else qorder' (acc + 1) (shiftR val 1) 
 
dimy::Matrix a -> Int
dimy [] = 0
dimy (r:_) = length r
\end{code}
\end{singlespace}
}

%if false
\begin{code}

showMat :: (Show a) => Matrix a -> String
showMat [] = ""
showMat (r:rs) = showList r ('\n' : showMat  rs)

 
{- unused
printMat :: (Show a) => Matrix a -> IO ()
printMat m = putStr $ showMat m
-}
\end{code}
%endif

The idea for the function |gendotprod| was borrowed from APL. This
allows for a generalization of the general notion of vector product
and therefore matrix multiplication.

{\begin{singlespace}
\begin{code}
gendotprod :: (c->c->c) -> (a -> b -> c) -> [a] -> [b] -> c
gendotprod f g xs = foldr1 f . zipWith g xs

genmatmul  :: (c->c->c) -> (a -> b -> c) -> 
              Matrix a -> Matrix b -> Matrix c
genmatmul  f g m1 m2  
     = transpose [[gendotprod f g a b| a <- m1] | 
		       b <- transpose m2]

\end{code}
\end{singlespace}
}

|indexM| is a 
function to retrieve the $i,j^{\mathrm{th}}$ element of the matrix.

{\begin{singlespace}
\begin{code}

indexM :: Int -> Int -> Matrix a -> a
indexM i j  m = (m !! i) !! j
\end{code}
\end{singlespace}
}

In a quantum space, the application of a unitary transform requires 
taking the conjugate transpose of a matrix.

{\begin{singlespace}
\begin{code}

conjtrans :: (Comp a)=>[[a]] -> [[a]]
conjtrans = mmap conjgt . transpose

\end{code}
\end{singlespace}
}

The module also provides a
 variation of the standard Haskell |zipWith| that signals an error
whenever the lists are of different length. This is used in the definition
of matrix addition below.

{\begin{singlespace}
\begin{code}

xzipWith :: (a->b->c)->[a]->[b]->[c]
xzipWith _ [] [] = []
xzipWith _ [] ys 
     = error "xzipWith: 2nd list longer"
xzipWith _ xs [] 
     = error "xzipWith: 1st list longer"
xzipWith f (x:xs) (y:ys) 
     = f x y : xzipWith f xs ys

\end{code}
\end{singlespace}
}

This is a 
standard vector |dotprod| using addition and multiplication.

{\begin{singlespace}
\begin{code}

dotprod :: (Num a)=>[a]->[a] ->a
dotprod = gendotprod (+) (*)
\end{code}
\end{singlespace}
}

Creation of a zero matrix, used in the definition of the numeric instance.

{\begin{singlespace}
\begin{code}

zeromat :: (Num a)=>Int->Matrix a
zeromat 0 = error "Invalid dimension"
zeromat n =  [[fromInteger 0 |
	       inner<-[1..n]] |
	      outer<-[1..n]]

idMat :: (Num a) => Int -> Matrix a
idMat n = [[if inner == outer then fromInteger 1 else fromInteger 0 |
	       inner<-[1..n]] |
	      outer<-[1..n]]
                                                                  
\end{code}
\end{singlespace}
}

When the base elements of a matrix are of the Haskell |Num| class
the module defines  an instance of the matrix as part of the |Num| class.

{\begin{singlespace}
\begin{code}

instance (Num a)=> Num (Matrix a)  where
     (+)          = xzipWith (xzipWith (+))
     (-)          = xzipWith (xzipWith (-))
     (*) qm1 qm2  =  [[dotprod a b| b <- transpose qm2] | 
	                 a <-  qm1]
     negate       = mmap negate
     abs          = mmap abs
     signum       = mmap signum
     fromInteger  = zeromat . fromInteger 

mat2ToTheT :: (Num a) => Matrix a -> Int -> Matrix a
mat2ToTheT m 0 = m
mat2ToTheT m t 
    | t > 0 = m' * m' 
    | otherwise = error "Negative value for t in m^(2^t)"
    where m' = mat2ToTheT m (t-1)

\end{code}
\end{singlespace}
}



In certain cases, functions may create matrices whose elements 
are matrices.

Using our "list of lists" representation,  a function to
parametrically combine 
the rows of the |Matrix| creating a column vector is defined. 
The 
|paste| function which "pastes" matrices in a 
side-by-side fashion is defined as

{\begin{singlespace}
\[
\mathrm{paste\ }\begin{bmatrix}
a&b\\
c&d
\end{bmatrix}\ \begin{bmatrix}
e&f\\
g&h
\end{bmatrix}\ = \begin{bmatrix}
a&b&e&f\\
c&d&g&h
\end{bmatrix}.
\]
\end{singlespace}
}

Those functions are then used
 to take a matrix of matrices of type $\alpha$ to a 
matrix of type $\alpha$

{\begin{singlespace}
\begin{code}
tocolVector::(a->a->a)->Matrix a -> [a]
tocolVector f m = [ foldr1 f a | a<-  m ]

paste :: Matrix a -> Matrix a -> Matrix a
paste  = zipWith (++) 

reduceM :: Matrix (Matrix a) -> Matrix a
reduceM  = concat . tocolVector paste

\end{code}
\end{singlespace}
}

Similarly, the reverse operation is required. That is, to take a matrix to
a $2\times 2$ matrix of matrices.

{\begin{singlespace}
\begin{code}
{- unused
tenderize :: Matrix a -> Matrix (Matrix a)
tenderize matr =
      [[grab 0 0 matr, grab 0 1 matr],
       [grab 1 0 matr, grab 1 1 matr]]
-}
grab :: Int -> Int -> Matrix a -> Matrix a
grab 0 0 matr =  
    take d2 $ map (take d2)  matr
        where d2 = dimy matr `div` 2
grab 0 1 matr
     =  take d2  $ map (take d2 . drop d2)  matr
       where d2 = dimy matr `div` 2
grab 1 0 matr
     =  take d2 $ drop d2 $ map (take d2)  matr
       where d2 = dimy matr `div` 2

grab 1 1 matr
     =  (take d2 . drop d2) $ map (take d2 . drop d2)  matr
       where d2 = dimy matr `div` 2
\end{code}
\end{singlespace}
}

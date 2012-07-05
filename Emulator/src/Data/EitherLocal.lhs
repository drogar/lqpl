%include polycode.fmt
\subsection{Definition of extra functions on Either}
\label{subsec:eitherfunctions}
%if false
\begin{code}
module Data.EitherLocal 
  ( module Data.Either,
  fromLeft,  fl2,
  fromRight, fr2,
  eLeftfun,eRightfun,eLtoRightfun)
where
 
import Data.Either
\end{code}
%endif
{\begin{singlespace}
\begin{code}

fromLeft             :: Either a b -> a
fromLeft (Left a  )  = a
fromLeft _           = error "fromLeft: Needed left value"

fl2      ::  Either a b -> Either a b -> (a,a)
fl2 x y  =   (fromLeft x, fromLeft y)

fromRight             :: Either a b -> b
fromRight (Right b)   = b
fromRight _           = error "fromRight: Needed right value"

fr2      :: Either a b -> Either a b -> (b,b)
fr2 x y  = (fromRight x, fromRight y)


eLeftfun    :: (a->a->a) -> Either a b -> Either a b -> Either a b
eLeftfun f  = curry $ Left . uncurry f . uncurry fl2

eRightfun    :: (b->b->b) -> Either a b -> Either a b -> Either a b
eRightfun f  = curry $ Right . uncurry f . uncurry fr2

eLtoRightfun    :: (a->a->b) -> Either a b -> Either a b -> Either a b
eLtoRightfun f  = curry $ Right . uncurry f . uncurry fl2

\end{code}
\end{singlespace}
}


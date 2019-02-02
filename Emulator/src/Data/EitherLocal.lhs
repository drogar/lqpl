%include polycode.fmt
\subsection{Definition of extra functions on Either}
\label{subsec:eitherfunctions}
%if false
\begin{code}
module Data.EitherLocal
  ( module Data.Either,
  localFromLeft,  fl2,
  localFromRight, fr2,
  eLeftfun,eRightfun,eLtoRightfun)
where

import Data.Either
\end{code}
%endif
{\begin{singlespace}
\begin{code}

localFromLeft             :: Either a b -> a
localFromLeft (Left a  )  = a
localFromLeft _           = error "localFromLeft: Needed left value"

fl2      ::  Either a b -> Either a b -> (a,a)
fl2 x y  =   (localFromLeft x, localFromLeft y)

localFromRight             :: Either a b -> b
localFromRight (Right b)   = b
localFromRight _           = error "localFromRight: Needed right value"

fr2      :: Either a b -> Either a b -> (b,b)
fr2 x y  = (localFromRight x, localFromRight y)


eLeftfun    :: (a->a->a) -> Either a b -> Either a b -> Either a b
eLeftfun f  = curry $ Left . uncurry f . uncurry fl2

eRightfun    :: (b->b->b) -> Either a b -> Either a b -> Either a b
eRightfun f  = curry $ Right . uncurry f . uncurry fr2

eLtoRightfun    :: (a->a->b) -> Either a b -> Either a b -> Either a b
eLtoRightfun f  = curry $ Right . uncurry f . uncurry fl2

\end{code}
\end{singlespace}
}

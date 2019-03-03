%include polycode.fmt
\subsection{Definition of extra functions on Either}
\label{subsec:eitherfunctions}
%if false
\begin{code}
module Lqpl.Data.EitherLocal
  ( module Data.Either,
  fl2,
  fr2,
  eLeftfun,eRightfun,eLtoRightfun)
where

import Data.Either
\end{code}
%endif
{\begin{singlespace}
\begin{code}

fromLeftE             :: Either a b -> a
fromLeftE (Left a  )  = a
fromLeftE _           = error "fromLeftE: Needed left value"

fl2      ::  Either a b -> Either a b -> (a,a)
fl2 x y  =   (fromLeftE x, fromLeftE y)

fromRightE             :: Either a b -> b
fromRightE (Right b)   = b
fromRightE _           = error "fromRightE: Needed right value"

fr2      :: Either a b -> Either a b -> (b,b)
fr2 x y  = (fromRightE x, fromRightE y)


eLeftfun    :: (a->a->a) -> Either a b -> Either a b -> Either a b
eLeftfun f  = curry $ Left . uncurry f . uncurry fl2

eRightfun    :: (b->b->b) -> Either a b -> Either a b -> Either a b
eRightfun f  = curry $ Right . uncurry f . uncurry fr2

eLtoRightfun    :: (a->a->b) -> Either a b -> Either a b -> Either a b
eLtoRightfun f  = curry $ Right . uncurry f . uncurry fl2

\end{code}
\end{singlespace}
}

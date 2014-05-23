%include polycode.fmt
\subsection{Definition of the Stack datatype}
\label{subsec:stack}
The |Stack| is implemented by a |List|. Instances  of
|Functor|, |Monad| and |MonadPlus| are defined for the type.
%if false
\begin{code}
module Data.Stack
  ( Stack(..),
    emptyStack,push,pushM, popM, pop,fromList, toList,
    get,getM,
    stackElem,stackSplitAt,addn)
where
import Data.List as List
import Control.Monad
\end{code}
%endif

\begin{singlespace}
\begin{code}
newtype Stack a = Stack [a]
    deriving Show

instance Functor Stack where
    fmap f (Stack elts) = Stack (map f elts)

instance Monad Stack where
    return a   = Stack [a]
    m >>= k    = stkfoldr ( concStack . k) (Stack []) m
    m >> k     = stkfoldr ( concStack . const k) (Stack []) m
    fail _     = Stack []
instance MonadPlus Stack where
    mzero = Stack []
    mplus (Stack a) (Stack b) = Stack (a ++ b)
\end{code}
\end{singlespace}

A variety of manipulation functions are provided, giving the standard
push and pop functionality of a stack.

{\begin{singlespace}
\begin{code}
emptyStack :: Stack a
emptyStack =  mzero

push::a-> Stack a  -> Stack a
push a (Stack as) = Stack (a:as)

pushM :: Maybe a -> Stack a -> Stack a
pushM Nothing s = s
pushM (Just a) s = push a s

popM :: Stack a -> (Maybe a, Stack a)
popM (Stack [] )     = (Nothing, Stack [])
popM (Stack (a:as))  = (Just a, Stack as)

pop :: Stack a -> (a, Stack a)
pop s = let (v,s') = popM s
        in case v of
	   Nothing -> error "Stack is empty on pop"
	   Just v -> (v,s')
{-unused
popNum :: (Num a)=>Stack a -> (a, Stack a)
popNum s = let (v,s') = popM s
           in case v of
	      Nothing -> (fromInteger 0, s)
	      Just v -> (v,s')
-}
addn :: Int -> Stack a -> Stack a -> Stack a
addn n (Stack e1) (Stack e2)
    = Stack (take n e1 ++ e2)

{-unused
multiPop :: (Num a)=> Int -> Stack a -> (a, Stack a)
multiPop 1 stk = pop stk
multiPop n stk
  | n > 1 = pop $ snd $ multiPop (n-1) stk
  | otherwise = error "Can not pop 0 or less elements"
-}
getM ::Stack a -> Maybe a
getM (Stack []) = Nothing
getM (Stack (a:as)) = Just a

get :: Stack a -> a
get s = let v = getM s
        in fromMaybe (error "Stack is empty on get")  v



stackElem :: Stack a -> Int -> a
stackElem (Stack aelems) i
          | i >= 0 = aelems List.!! i
          | (-i) <= length aelems  =
              aelems List.!! (length aelems + i)
          | otherwise =
              aelems List.!!
                         ((length aelems + i) `mod` length aelems)
{- unused
isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty (Stack (a:as)) = False
-}

toList :: Stack a -> [a]
toList (Stack a) = a

fromList ::[a] -> Stack a
fromList  = Stack

stkfoldr :: (a->b->b) -> b -> Stack a -> b
stkfoldr f i (Stack a) = foldr f i a

concStack :: Stack a -> Stack a -> Stack a
concStack  = mplus

stackSplitAt :: Int -> Stack a -> (Stack a, Stack a)
stackSplitAt n (Stack elms) = (Stack l, Stack r)
                              where (l,r) = splitAt n elms
\end{code}
\end{singlespace}
}

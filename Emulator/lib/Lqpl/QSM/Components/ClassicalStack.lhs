%include polycode.fmt
\subsection{Description of the classical stack}\label{subsec:classicalstack}
%if false
\begin{code}
module Lqpl.QSM.Components.ClassicalStack (
         ClassicalStack,
         module Lqpl.Data.ClassicalData,
         module Lqpl.Data.Stack,
         getStackOp,
         showCstack,
         showCstackElem,
         stkget,
         stkput)
   where
import Data.Bits
import Lqpl.Data.Stack
import Lqpl.Data.ClassicalData
import Lqpl.QSM.MachineErrors
import Lqpl.Utility.Extras
\end{code}
%endif
The classical stack data type is a standard  stack of either integers or
booleans with random access.

\begin{singlespace}
\begin{code}
type ChoiceStack a b = Stack (Either a b)
type ClassicalStack = ChoiceStack Int Bool
\end{code}
\end{singlespace}
%if false
Code to display the cstack for our visualization.
\begin{code}
showCstackElem :: ClassicalData -> String
showCstackElem (Left i) = show i
showCstackElem (Right b) = show b

showCstack :: ClassicalStack -> String
showCstack = scs' 0

scs'::Int ->ClassicalStack -> String
scs' 0 (Stack [])       =  "Empty Stack"
scs' 0 (Stack (a:[]))   =  "[" ++ showCstackElem a ++ "]"
scs' 0 (Stack (a:aas))  =  "[" ++ showCstackElem a ++ "," ++ scs' 1 (Stack aas)
scs' 1 (Stack (a:[]))   =  showCstackElem a ++ "]"
scs' 1 (Stack (a:aas))  =  showCstackElem a ++ ","++ scs' 1 (Stack aas)
\end{code}
%endif

The classical operations done on the stack are created by
delegating to functions that will work with the |Either|
elements on the stack. See \vref{subsec:eitherfunctions} for the
details of these functions.

\begin{singlespace}
\begin{code}
sLtoLfun :: (a->a->a) ->
        ChoiceStack a b ->
        ChoiceStack a b
sLtoLfun f (Stack (a:b:rest)) = Stack (eLeftfun f a b : rest)
sLtoLfun f (Stack _)
    = error clsstackError

sRtoRfun :: (b->b->b) ->
        ChoiceStack a b ->
        ChoiceStack a b
sRtoRfun f (Stack (a:b:rest)) = Stack (eRightfun f a b : rest)
sRtoRfun f (Stack _)
    = error clsstackError

sLtoRfun :: (a->a->b) ->
        ChoiceStack a b ->
        ChoiceStack a b
sLtoRfun f (Stack (a:b:rest)) = Stack (eLtoRightfun f a b : rest)
sLtoRfun f (Stack _)
    = error clsstackError
\end{code}
\end{singlespace}

Individual functions for each operation are created here. Below is
definition of the add function. The definitions of all the others
are similar.

\begin{singlespace}
\begin{code}
stkadd :: ClassicalStack -> ClassicalStack
stkadd = sLtoLfun (+)
\end{code}
\end{singlespace}

%if false
\begin{code}

stkneg :: (Num a) => ChoiceStack a b -> ChoiceStack a b
stkneg (Stack ((Left a):rest)) = Stack (Left (-a) : rest)
stkneg (Stack []) = error clsstackError
stkneg _ = error clsstackTypeError

stkcmpl :: (Bits a) => ChoiceStack  a Bool -> ChoiceStack a Bool
stkcmpl (Stack ((Left a):rest))   = Stack (Left (complement a) : rest)
stkcmpl (Stack ((Right b):rest))  = Stack (Right (not b) : rest)
stkcmpl _ = error clsstackError

stksub :: ClassicalStack -> ClassicalStack
stksub = sLtoLfun (-)
stkmul :: ClassicalStack -> ClassicalStack
stkmul = sLtoLfun (*)
stkdiv :: ClassicalStack -> ClassicalStack
stkdiv = sLtoLfun div
stkmod :: ClassicalStack -> ClassicalStack
stkmod = sLtoLfun mod
stkeq :: ClassicalStack -> ClassicalStack
stkeq = sLtoRfun (==)
stkneq :: ClassicalStack -> ClassicalStack
stkneq  = sLtoRfun (/=)
stklt :: ClassicalStack -> ClassicalStack
stklt  = sLtoRfun (<)
stkgt :: ClassicalStack -> ClassicalStack
stkgt  = sLtoRfun (>)
stkgte :: ClassicalStack -> ClassicalStack
stkgte  = sLtoRfun (>=)
stklte :: ClassicalStack -> ClassicalStack
stklte  = sLtoRfun (<=)

stkand :: ClassicalStack -> ClassicalStack
stkand = sRtoRfun (&&)
stkor :: ClassicalStack -> ClassicalStack
stkor = sRtoRfun (||)


stkxor :: ClassicalStack -> ClassicalStack
stkxor = sRtoRfun (/=)

stkshl :: ClassicalStack -> ClassicalStack
stkshl = sLtoLfun (\ a -> shiftL a . fromInteger . toInteger)

stkshr :: ClassicalStack -> ClassicalStack
stkshr = sLtoLfun (\ a ->shiftR a . fromInteger . toInteger)


stkrem :: ClassicalStack -> ClassicalStack
stkrem = sLtoLfun rem

stkget :: Int -> Stack a -> Stack a
stkget i (Stack l) = Stack l'
    where len = length l
          index = len + i --Assume neg or zero
          ival = l !! (index -1)
          l' = ival : l

\end{code}
%endif
Similarly, a function to convert from the operation to
the appropriate stack function is required. Again, only the add
function is shown here.

\begin{singlespace}
\begin{code}
getStackOp :: ClassicalOp -> ClassicalStack -> ClassicalStack
getStackOp CAdd    = stkadd
\end{code}
\end{singlespace}

%if false
\begin{code}
getStackOp CSub    = stksub
getStackOp CTimes  = stkmul
getStackOp CDiv    = stkdiv
getStackOp CMod    = stkmod
getStackOp CRem    = stkrem
getStackOp CNeg    = stkneg
getStackOp COr     = stkor
getStackOp CAnd    = stkand
getStackOp CXor    = stkxor
getStackOp CNot    = stkcmpl
getStackOp CEq     = stkeq
getStackOp CNeq    = stkneq
getStackOp CLt     = stklt
getStackOp CLte    = stklte
getStackOp CGt     = stkgt
getStackOp CGte    = stkgte
getStackOp CShl    = stkshl
getStackOp CShr    = stkshr
\end{code}
%endif

Finally, |stkput| is defined, which allows values to be placed into the
 classical stack at any location. |stkput|
 takes the top element of the stack and puts
it at the designated index.

\begin{singlespace}
\begin{code}
stkput :: Int -> Stack a -> Stack a
stkput i (Stack l) = Stack l'
    where len = length l
          index = len + i --Assume neg or zero
          ival = qHead "stkput in ClassicalStack" l
          l' = take index l ++  (ival : drop (index + 1) l)
\end{code}
\end{singlespace}

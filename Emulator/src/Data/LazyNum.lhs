%include polycode.fmt
\subsection{Definition of the LazyNum type and operations}
\label{subsec:lazynum}
The semantics of the language QPL specify creation of
superoperators over a vector space of Complex numbers. Since computer
representation of real numbers is fraught with rounding, this module
provides a
symbolic type that will allow certain values to be represented exactly,
together with a function that will compute the value at the end.
%if false
\begin{code}
 module Data.LazyNum(LazyNum(..),
                QOperation(..),
    QFunction(..),
                qprob
         ) where
 import Data.Complex
 import Data.ClassComp
\end{code}
%endif

\incsubsec{\hasktypenoref{QOperation}}\label{haskelltype:QOperation}\index{QPL Interpretor Data Types!QOperation}

{\begin{singlespace}
\begin{code}
 data QOperation = Plus | Minus | Times | Div
    deriving (Enum, Eq)
\end{code}
\end{singlespace}}

%if false
\begin{code}
 instance Show QOperation where
     show Plus = "+"
     show Minus = "-"
     show Times = "*"
     show Div    = "/"

\end{code}
%endif

\incsubsec{\hasktypenoref{QFunction}}\label{haskelltype:QFunction}\index{QPL Interpretor Data Types!QFunction}
Functions used in Quantum values.

{\begin{singlespace}
\begin{code}
 data QFunction = SquareRoot | Exp | AbsoluteValue
    | Signum |Negate| Conjugate
    | Log | ExpToThe LazyNum | LogBase LazyNum
    | Sin | Cos | Tan | Asin | Acos | Atan
    | Sinh | Cosh | Tanh | Asinh | Acosh | Atanh

    deriving  Eq
\end{code}
\end{singlespace}
}

%if false
\begin{code}
 instance Show QFunction where
     show SquareRoot = "sqrt"
     show Exp = "exp"
     show AbsoluteValue = "abs"
     show Signum = "signum"
     show Negate = "-"
     show Conjugate  = "~"
     show Log = "log"
     show (ExpToThe a) = "(exp to " ++ show a ++ ")"
     show (LogBase a) = "(log base " ++ show a ++ ")"
     show Sin = "sin"
     show Cos = "cos"
     show Tan = "tan"
     show Sinh = "sinh"
     show Cosh = "cosh"
     show Tanh = "tanh"
     show Asin = "asin"
     show Acos = "acos"
     show Atan = "atan"
     show Asinh = "asinh"
     show Acosh = "acosh"
     show Atanh = "atanh"

\end{code}
%endif

\incsubsec{\hasktypenoref{LazyNum}}\label{haskelltype:LazyNum}\index{QPL Interpretor Data Types!LazyNum}
All possible symbolic values for a computation. A recursive type
allowing for complex numbers, real numbers, operations and function
application and unknowns.

{\begin{singlespace}
\begin{code}
 data LazyNum = Si
       | Snum Double
       | Sbop QOperation LazyNum LazyNum
       | Sfun QFunction LazyNum
       | Svar String
       | SZero
\end{code}
\end{singlespace}
}

%if false
\begin{code}
 instance Eq LazyNum where
     (==) (Snum a) (Snum b) = a == b
     (==) (Snum a) (SZero) = a == 0.0
     (==) (SZero) (Snum a)  = a == 0.0
     (==) (SZero) (SZero) = True
     (==) (Svar a) (Svar b) = a == b
     (==) (Sfun f a) (Sfun g b) = a == b && f == g
     (==) (Sbop op a a') (Sbop op2 b b') = a == b && a' == b' && op == op2
     (==) (Si) (Si) = True
     (==) _ _ = False

\end{code}
%endif

%if false
\begin{code}
 instance Show LazyNum where
     show (Snum x) = show x
     show (Si) = "i"
     show (Sbop op p1 p2) = "("++show p1 ++  show op ++ show p2 ++")"
     show (Sfun f p) = show f ++ "("++show p ++")"
     show (Svar s) = s
     show (SZero) = "0"
\end{code}
%endif

%if false
\begin{code}
 instance Num LazyNum where
     (+) (Snum 0.0) p = p
     (+) p (Snum 0.0) = p
     (+) (SZero) p = p
     (+) p (SZero) = p
     (+) (Snum x) (Snum y) = Snum (x+y)
     (+) p1 (Sfun Negate p2) = p1 - p2
     (+) (Sfun Negate p2) p1 = p1 - p2
     (+) (Sbop Div q1 d1) (Sbop Div q2 d2)
         | d1 == d2 = (q1+ q2) / d1
         | otherwise = ((q1*d2)+ (q2*d1)) / (d1*d2)
     (+) p1 p2 = Sbop Plus p1 p2


     (*) (Snum 0.0) _ = SZero
     (*) _ (Snum 0.0) = SZero

     (*) (SZero) _ = SZero
     (*) _ (SZero) = SZero
     (*) (Snum 1.0) p = p
     (*) p (Snum 1.0) = p
     (*) (Snum (-1.0)) p = negate p
     (*) p (Snum (-1.0)) = negate p
     (*) (Snum x) (Snum y) = Snum (x*y)
     (*) (Sfun Negate x) y = negate (x*y)
     (*) x (Sfun Negate y) = negate (x*y)
     (*) (Sfun SquareRoot x) (Sfun SquareRoot y)
         | x == y = x
         | otherwise = Sfun SquareRoot (x*y)

     (*) (Si)  (Si) = Snum (-1.0)

     (*) (Si) x = multbyi x

     (*) x (Si) = multbyi x


     (*) (Sbop Div x1 y1) (Sbop Div x2 y2) = (x1 *x2) / (y1*y2)

     (*) (Sbop Div x1 y1)  x2 = (x1 *x2) / y1

     (*)  x2 (Sbop Div x1 y1) = (x1 *x2) / y1

     (*) p1 p2 = Sbop Times p1 p2

     (-) (Snum x) (Snum y) = Snum (x-y)
     (-) p1 (Sfun Negate p2) = p1 + p2
     (-) (Sfun Negate p2) p1 = negate (p1 + p2)

     (-) (Sbop Div q1 d1) (Sbop Div q2 d2)
         | d1 == d2 = (q1- q2) / d1
         | otherwise = ((q1*d2)- (q2*d1)) / (d1*d2)

     (-) p1 p2 = Sbop Minus p1 p2

     negate (SZero) = SZero
     negate (Snum 0.0) = SZero
     negate (Snum x) = Snum (-x)

     negate (Sfun Negate x)  = x

     negate (Sbop Div q1 d1) =  Sbop Div (negate q1)  d1

     negate (Sbop Times q1 d1) =  Sbop Times (negate q1)  d1

     negate (Sbop Plus q1 d1) =  Sbop Plus (negate q1) (negate d1)

     negate (Sbop Minus q1 d1) =  d1 - q1

     negate p1 = Sfun Negate p1


     abs (SZero) = SZero
     abs (Snum 0.0) = SZero
     abs (Snum x)  = Snum (abs x)
     abs (Sfun Negate x)  = abs x

     abs p = Sfun AbsoluteValue p

     signum (SZero) = signum 0
     signum (Snum x) = Snum (signum x)
     signum p = Sfun Signum p

     fromInteger 0 = SZero
     fromInteger i = Snum (fromInteger i)

 multbyi :: LazyNum -> LazyNum
 multbyi  (Sbop Times x Si)   = - x
 multbyi  (Sbop Times Si x)   = - x
 multbyi  (Sbop Div Si x)     = Sfun Negate (Sbop Div (Snum 1.0) x)
 multbyi  (Sbop Div x Si)     = x
 multbyi  (Sbop Div x y)      = (Si * x) / y
 multbyi  x                   = Sbop Times Si x


 instance Fractional LazyNum where
     (/) (Snum x) (Snum y) = Snum (x/y)
     (/) (Sfun Negate x) y = negate (x/y)
     (/) y (Sfun Negate x) = negate (y/x)
     (/) (Sfun SquareRoot x) (Sfun SquareRoot y) = Sfun SquareRoot (x / y)
     (/) (Sbop Div x1 y1)  x2 =  x1 / (x2 * y1)
     (/)  x2 (Sbop Div x1 y1) =  (y1 * x2) / x1
     (/) (SZero) _ = SZero
     (/) _ (SZero) = error "Division by SZero"

     (/) q1 q2
         | q1 == q2 = Snum 1.0
         | otherwise =  Sbop Div q1 q2

     fromRational = Snum . fromRational

 instance Floating LazyNum where
     pi = Snum pi
     exp  = Sfun Exp
     log  = Sfun Log
     sqrt  = Sfun SquareRoot
     (**) a b = Sfun (ExpToThe b) a
     logBase a b = Sfun (LogBase b) a
     sin  = Sfun Sin
     cos  = Sfun Cos
     tan  = Sfun Tan
     asin   = Sfun Asin
     acos  = Sfun Acos
     atan  = Sfun Atan
     sinh  = Sfun Sinh
     cosh  = Sfun Cosh
     tanh  = Sfun Tanh
     asinh  = Sfun Asinh
     acosh  = Sfun Acosh
     atanh  = Sfun Atanh

 isPosReal :: LazyNum -> Bool
 isPosReal (Snum x)
     = x >= 0.0
 isPosReal (Si)
     = False
 isPosReal (Svar _)
     = False
 isPosReal (Sbop Plus _ _ )
     = False
 isPosReal (Sbop Minus _ _ )
     = False
 isPosReal (Sbop Times p q)
     = (isPosReal p && isPosReal q) ||
       not (isPosReal p && isPosReal q)
 isPosReal (Sbop Div p q )
     = (isPosReal p && isPosReal q) ||
       not (isPosReal p && isPosReal q)
 isPosReal (Sfun SquareRoot p )
     = isPosReal p
 isPosReal (Sfun Exp q)
     = isPosReal q
 isPosReal (Sfun AbsoluteValue q)
     = True
 isPosReal (Sfun Signum q)
     = isPosReal q
 isPosReal (Sfun Negate q)
     = not $ isPosReal q
 isPosReal (Sfun Conjugate q)
     = isPosReal q

 isPosReal (SZero) = False
 isPosReal (Sfun _ _) = False -- Just for completeness


 isReal :: LazyNum -> Bool
 isReal (SZero) = True
 isReal (Snum _)
     = True
 isReal (Si)
     = False
 isReal (Svar _)
     = False
 isReal (Sbop _ p q)
     = isReal p && isReal q
 isReal (Sfun SquareRoot (Snum x))
     = x >= 0.0
 isReal (Sfun SquareRoot p )
     = isReal p && isPosReal p
 isReal (Sfun Exp q)
     = isReal q
 isReal (Sfun AbsoluteValue q)
     = True
 isReal (Sfun Signum q)
     = isReal q
 isReal (Sfun Negate q)
     = isReal q
 isReal (Sfun Conjugate q)
     = isReal q
 isReal (Sfun _ q) = isReal q -- Just for completeness

 instance Comp LazyNum where
     conjgt (SZero)         = SZero
     conjgt (Snum 0.0)      = SZero
     conjgt  (Snum x)       = Snum x
     conjgt (Si)            = Sfun Negate Si
     conjgt p  | isReal p   =  p
               | otherwise  = Sfun Conjugate p
     sqrtMinusOne           = Si
     approximate            = approx
     mag                    = magnitude . approx
     pow                    = (Snum 10.0 ^^)

\end{code}
%endif

\incsubsec{The \haskfuncnoref{approx} function}\label{insec:approx}
When completed processing, the results may be approximated.
Note that  more processing on the symbolic values could first be done
 to come to a
closer result. For example, squaring a square root value or take the log
of an exponential could be cancelled out.

\index{QPL Interpretor Functions!approx}
This allows us to compute a numeric value for any
|LazyNum| that does not contain an unknown.

{\begin{singlespace}
\begin{code}
 approx :: LazyNum -> Complex Double
 approx (SZero)                 = 0:+0
 approx (Snum x)                = x:+0
 approx (Si)                    = 0:+1
 approx (Sbop Plus p1 p2)       = approx p1 + approx p2
 approx (Sfun (ExpToThe b) p)   = approx p ** approx b
 approx (Sfun (LogBase b) p)    = logBase (approx p) (approx b)
 approx (Sfun Log a)            = log (approx a)
 approx (Svar _)                = 1.0
 approx (Sfun SquareRoot p)     = sqrt(approx p)
\end{code}
\end{singlespace}
}

%if false
\begin{code}
 approx (Sbop Minus p1 p2)      = approx p1 - approx p2
 approx (Sbop Times p1 p2)      = approx p1 * approx p2
 approx (Sbop Div p1 p2)        = approx p1 / approx p2
 approx (Sfun Exp p)            = exp(approx p)
 approx (Sfun AbsoluteValue p)  = abs(approx p)
 approx (Sfun Signum p)         = signum(approx p)
 approx (Sfun Negate p)         = negate(approx p)
 approx (Sfun Conjugate p)      = conjugate(approx p)
 approx (Sfun Sin p)            = sin(approx p)
 approx (Sfun Cos p)            = cos(approx p)
 approx (Sfun Tan p)            = tan(approx p)
 approx (Sfun Asin p)           = asin(approx p)
 approx (Sfun Acos p)           = acos(approx p)
 approx (Sfun Atan p)           = atan(approx p)
 approx (Sfun Sinh p)           = sinh(approx p)
 approx (Sfun Cosh p)           = cosh(approx p)
 approx (Sfun Tanh p)           = tanh(approx p)
 approx (Sfun Asinh p)          = asinh(approx p)
 approx (Sfun Acosh p)          = acosh(approx p)
 approx (Sfun Atanh p)          = atanh(approx p)
\end{code}
%endif

\incsubsec{\haskfunctionnoref{qprob}}
\label{haskellfunction:qprob}
\index{QPL Interpretor Functions!qprob}
Convert a number into a |LazyNum|.

{
\begin{singlespace}
\begin{code}
 qprob :: Double -> LazyNum
 qprob  = Snum

\end{code}
\end{singlespace}
}

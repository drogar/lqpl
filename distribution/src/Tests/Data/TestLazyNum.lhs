\begin{code}
  module Tests.Data.TestLazyNum where
    import Test.HUnit
    
    import Data.LazyNum
    import Data.ClassComp
    
    
    tests =  ["eq1 " ~: "a=b if a is b" ~: Snum 1.0 @=? (Snum 1.0),
      "eq2 " ~: "szero = 0 " ~: Snum 0.0 @=? SZero,
      "cj" ~: "cj i = -i" ~: (Sfun Negate Si) @=? conjgt sqrtMinusOne]
      
\end{code}
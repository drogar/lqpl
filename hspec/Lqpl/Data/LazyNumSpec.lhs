\begin{code}
  module Lqpl.Data.LazyNumSpec where
    import Test.Hspec
    import Lqpl.Data.LazyNum
    import Lqpl.Data.ClassComp

    spec = describe "LazyNum" $ do
      describe "equality" $ do
        it "is true that the same things are equal" $ do
          Snum 1.0 `shouldBe` (Snum 1.0)
        it "is true the special zero equals the numeric zero" $ do
          Snum 0.0 `shouldBe` SZero
        it "is true the conjugate of i is -i" $ do
          Sfun Negate Si `shouldBe` conjgt sqrtMinusOne

\end{code}

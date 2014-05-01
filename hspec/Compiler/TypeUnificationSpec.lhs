\begin{code}
  module Main where
    import Test.Hspec.Core(Example(..),Result(..))
    import Test.Hspec
    import Test.Hspec.Runner
    import Test.Hspec.Formatters
    import Test.Hspec.QuickCheck
    import Test.Hspec.HUnit
    import Test.QuickCheck hiding (property)
    import Test.HUnit

    import Data.Map as Map
    import SpecHelper


    import Control.Concurrent
    import Control.Monad.State
    import Control.Monad.Writer

    import Compiler.TypeUnification
    import Compiler.Qtypes
    import Compiler.SemTypes
    import Data.Stack

    startsemstate =  SemState (Lvl 0 0 0 0)
            Map.empty Map.empty Map.empty
            0 0 0
            Map.empty
            Map.empty [] (push 0 emptyStack) 9

    main = hspecWith defaultConfig{configFormatter=progress} unificationSpecs

    runinstanceOf whichtv tv1 tv2 = do
      return $ evalStateT (liftM fst $ runWriterT $ instanceOf whichtv tv1 tv2 (Map.singleton "b" INT)) startsemstate


    unificationSpecs = describe "TypeUnification" $ do
      context "instanceOf" $ do
        it "accepts a typevariable to INT when called with rigid"    $ do
              let tva = TypeVariable "a"
                  inttype  =  INT
              iomp <- runinstanceOf isRigidTypeVar tva inttype
              do mp <- iomp
                 if Map.null mp
                   then return False
                   else return $ (mp ! "a") == INT
        it "accepts when both tvs are the same " $ do
          iomp <- runinstanceOf isRigidTypeVar INT INT
          do mp <- iomp
             return (1 == size mp)
\end{code}

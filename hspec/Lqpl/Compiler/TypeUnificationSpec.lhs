\begin{code}
  module Lqpl.Compiler.TypeUnificationSpec(spec) where
    import Test.Hspec.Core.Spec(Example(..),Result(..),FailureReason(..))
    import Test.Hspec
    import Test.Hspec.Runner
    import Test.Hspec.Formatters
    import Test.Hspec.QuickCheck
    import Test.Hspec.Contrib.HUnit
    import Test.QuickCheck hiding (property)
    import Test.HUnit

    import Data.Map as Map
    import SpecHelper


    import Control.Concurrent
    import Control.Monad.State
    import Control.Monad.Writer

    import Lqpl.Compiler.TypeUnification
    import Lqpl.Compiler.Qtypes
    import Lqpl.Compiler.SemTypes
    import Lqpl.Data.Stack
    import System.Exit

    startsemstate =  SemState (Lvl 0 0 0 0)
            Map.empty Map.empty Map.empty
            0 0 0
            Map.empty
            Map.empty [] (push 0 emptyStack) 9

    spec = unificationSpecs
    -- main = do
      -- summary <- hspecWithResult defaultConfig{configFormatter = Just progress} unificationSpecs
      -- if summaryFailures summary > 0 then exitWith (ExitFailure $ summaryFailures summary)
         --                            else exitWith ExitSuccess

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

\begin{code}
  module Main where
    import Test.Hspec
    import Test.Hspec.Core
    import Test.Hspec.QuickCheck
    import Test.Hspec.HUnit
    import Test.QuickCheck hiding (property)
    import Test.HUnit


    import Spec.SpecHelper

    import QSM.BasicData

    import Data.LazyNum
    import QSM.QuantumStack.QSDefinition
    import QServer.StackToXML

    xmlValues =  [(StackZero, "<Zero/>"),
                  (StackValue (SZero), "<Value><number>0</number></Value>" )]

    checkIt :: StackDescriptor LazyNum -> String -> [Spec]
    checkIt sd res = it ("returns "++show sd++" as '"++res++"'") $ res ~=? (toXML sd)

    tests =  describe "StackToXML" $ map (uncurry checkIt) xmlValues


    main = hspecX tests

\end{code}
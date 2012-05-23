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

    stackXML = [ (QuantumStack 1 True [QuantumStack (-1) True [] (StackValue (SZero))] (StackQubit [(Z,Z)]),
      "<Qstack><int>1</int><bool>True</bool><substacks><Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>0</number></Value></Qstack></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>")]


    stackBoundedXML = [ (QuantumStack 1 True [QuantumStack (-1) True [] (StackValue (SZero))] (StackQubit [(Z,Z)]), "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>")]


    checkIt :: StackDescriptor LazyNum -> String -> [Spec]
    checkIt sd res = it ("returns "++show sd++" as '"++res++"'") $ res ~=? (toXML sd)

    checkUnbounded :: QuantumStack LazyNum -> String -> [Spec]
    checkUnbounded qs res = it ("returns "++show qs++" as '"++res++"'") $ res ~=? (toXML qs)

    checkBounded :: QuantumStack LazyNum -> String -> [Spec]
    checkBounded qs res = it ("returns "++show qs++" as '"++res++"'") $ res ~=? (boundedToXML 1 qs)


    tests =  describe "StackToXML" $ map (uncurry checkIt) xmlValues ++
      [context "unbounded qstack" (map (uncurry checkUnbounded) stackXML)] ++
      [context "bounded qstack" (map (uncurry checkBounded) stackBoundedXML)]

    main = hspecX tests

\end{code}
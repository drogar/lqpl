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

    stackXML = [ (QuantumStack 1 True [QuantumStack (-1) True [] (StackValue (Snum 0.5))] (StackQubit [(Z,Z)]),
      "<Qstack><int>1</int><bool>True</bool><substacks><Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"),
       (QuantumStack 1 True [QuantumStack (-1) True [] (StackValue (Snum 0.5)),QuantumStack (-1) False [] (StackValue (Snum 0.5)),QuantumStack (-1) True [] (StackValue (Snum 0.5))] (StackQubit [(Z,Z),(Z,O),(O,O)]),
        "<Qstack><int>1</int><bool>True</bool><substacks>"++
        "<Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"++  -- OnDiag adds O,Z leg
        "<Qstack><int>-1</int><bool>False</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"++
        "<Qstack><int>-1</int><bool>False</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"++
        "<Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"++
        "</substacks>"++
        "<Qubits><pair><qz/><qz/></pair><pair><qz/><qo/></pair><pair><qo/><qz/></pair><pair><qo/><qo/></pair></Qubits></Qstack>"),
        (QuantumStack 1 False [QuantumStack (-1) False [] (StackValue (Snum 0.5)),QuantumStack (-1) False [] (StackValue (Snum 0.5)),QuantumStack (-1) False [] (StackValue (Snum 0.5))] (StackQubit [(Z,Z),(Z,O),(O,O)]),
          "<Qstack><int>1</int><bool>False</bool><substacks>"++
          "<Qstack><int>-1</int><bool>False</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"++
          "<Qstack><int>-1</int><bool>False</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"++ -- OnDiag False takes just what it sees.
          "<Qstack><int>-1</int><bool>False</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"++
          "</substacks>"++
          "<Qubits><pair><qz/><qz/></pair><pair><qz/><qo/></pair><pair><qo/><qo/></pair></Qubits></Qstack>")]


    stackBoundedXML = [ (QuantumStack 1 True [QuantumStack (-1) True [] (StackValue (Snum 0.5))] (StackQubit [(Z,Z)]), "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>")]


    checkIt :: StackDescriptor LazyNum -> String -> [Spec]
    checkIt sd res = it ("returns "++show sd++" as '"++res++"'") $ res ~=? (toXML sd)

    checkUnbounded :: QuantumStack LazyNum -> String -> [Spec]
    checkUnbounded qs res = it ("returns "++show (fixDiags qs)++" as '"++res++"'") $ res ~=? (toXML $ fixDiags qs)

    checkBounded :: QuantumStack LazyNum -> String -> [Spec]
    checkBounded qs res = it ("returns "++show (fixDiags qs)++" as '"++res++"'") $ res ~=? (boundedToXML 1  $ fixDiags qs)


    tests =  describe "StackToXML" $ map (uncurry checkIt) xmlValues ++
      [context "unbounded qstack" (map (uncurry checkUnbounded) stackXML)] ++
      [context "bounded qstack" (map (uncurry checkBounded) stackBoundedXML)]

    main = hspecX tests

\end{code}
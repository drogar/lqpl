\begin{code}
  module Main where
    import Test.Hspec.Monadic
    import Test.Hspec.QuickCheck
    import Test.Hspec.HUnit
    import Test.QuickCheck hiding (property)
    import Test.HUnit

    import Data.Stack

    import Spec.SpecHelper

    import QSM.BasicData

    import Data.LazyNum
    import QSM.Components.ClassicalStack
    import QSM.QuantumStack.QSDefinition
    import QServer.StackToXML


    instance Quantum LazyNum

    main = hspecX tests


    xmlValues =  [(StackZero, "<Zero/>"),
                  (StackValue (SZero), "<Value><number>0</number></Value>" )]
    cstacks :: [(ClassicalStack,String)]
    cstacks = [(Stack [Left 5, Right False, Left (-1)], "<Classical><cint>5</cint><cbool>False</cbool><cint>-1</cint></Classical>")]

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
          "<Qubits><pair><qz/><qz/></pair><pair><qz/><qo/></pair><pair><qo/><qo/></pair></Qubits></Qstack>"),
          (QuantumStack 1 True [QuantumStack (-1) True [] (StackValue (Snum 0.5)),QuantumStack (-1) True [] (StackValue (Snum 0.5))] (StackClassical [Left 4, Right True]),
            "<Qstack><int>1</int><bool>True</bool><substacks>"++
            "<Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"++
            "<Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack></substacks>"++
            "<Classical><cint>4</cint><cbool>True</cbool></Classical></Qstack>")]


    stackBoundedXML = [ (QuantumStack 1 True [QuantumStack (-1) True [] (StackValue (Snum 0.5))] (StackQubit [(Z,Z)]), "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>")]


    --checkIt :: StackDescriptor LazyNum -> String -> SpecM ()
    checkIt sd res = it ("returns "++show sd++" as '"++res++"'") $ res ~=? (toXML sd)

    --checkUnbounded :: QuantumStack LazyNum -> String -> SpecM ()
    checkUnbounded qs res = it ("returns "++show (fixDiags qs)++" as '"++res++"'") $ res ~=? (toXML $ fixDiags qs)

    --checkBounded :: QuantumStack LazyNum -> String -> SpecM ()
    checkBounded qs res = it ("returns "++show (fixDiags qs)++" as '"++res++"'") $ res ~=? (boundedToXML 1  $ fixDiags qs)


    tests =  describe "StackToXML" $ do
      mapM_ (uncurry checkIt) xmlValues
      mapM_ (uncurry checkIt) cstacks
      context "unbounded qstack" $ mapM_ (uncurry checkUnbounded) stackXML
      context "bounded qstack" $ mapM_ (uncurry checkBounded) stackBoundedXML


\end{code}
\begin{code}
  module Main where
    import Test.Hspec
    import Test.Hspec.Runner
    import Test.Hspec.Formatters
    import Test.Hspec.QuickCheck
    import Test.Hspec.HUnit
    import Test.QuickCheck hiding (property)
    import Test.HUnit

    import Data.Stack

    import SpecHelper

    import QSM.BasicData

    import Data.LazyNum
    import QSM.Components.ClassicalStack
    import QSM.QuantumStack.QSDefinition
    import QServer.StackToJSON

    import System.Exit


    instance Quantum LazyNum

    main = do
      summary <- hspecWith defaultConfig{configFormatter=progress} tests
      if summaryFailures summary > 0 then exitWith (ExitFailure $ summaryFailures summary)
                                     else exitWith ExitSuccess
{-      data  StackDescriptor b
    = StackZero  |
      StackValue ! b |
      StackClassical [ClassicalData] |
      StackQubit [(Basis,Basis)]|
      StackData [(Constructor,[StackAddress])]
                deriving Show
-}

    jsonValues =  [(StackZero, "{\"zero\":0}"),
                   (StackValue (SZero), "{\"value\" : 0}" ),
                   (StackClassical [Left 1, Right True], "{\"classical\" : [1,true]}"),
                   (StackQubit [(Z,Z), (Z,O), (O,Z), (O,O)], "{\"qubit\" : [\"ZZ\",\"ZO\",\"OZ\",\"OO\"]}"),
                   (StackData [("Cons", [1,4]), ("Nil",[])],
                                "{\"data\" : [{\"cons\" : \"Cons\", \"addresses\" : [1,4]},{\"cons\" : \"Nil\", \"addresses\" : []}]}")]
    cstacks :: [(ClassicalStack,String)]
    cstacks = [(Stack [Left 5, Right False, Left (-1)], "{\"cstack\":[5, false,-1]}")]

    stackJSON = [ (QuantumStack 1 True [QuantumStack (-1) True [] (StackValue (Snum 0.5))] (StackQubit [(Z,Z)]),
                  "{\"qstack\":{\"id\":1,\"diagonal\":true,\"substacks\":" ++
                  "[{\"qstack\":{\"id\":-1,\"diagonal\":true,\"substacks\":[],\"qnode\":{\"value\":0.5}}}]," ++
                  "\"qnode\":{\"qbit\":[\"ZZ\"]}}}" ),
                  (QuantumStack 1 True [QuantumStack (-1) True []
                       (StackValue (Snum 0.5)),
                        QuantumStack (-1) False [] (StackValue (Snum 0.5)),
                        QuantumStack (-1) True [] (StackValue (Snum 0.5))]
                   (StackQubit [(Z,Z),(Z,O),(O,O)]),
                  "{\"qstack\":{\"id\":1,\"diagonal\":true,\"substacks\":" ++
                  "[{\"qstack\":{\"id\":-1,\"diagonal\":true,\"substacks\":[],\"qnode\":{\"value\":0.5}}}," ++
                  "{\"qstack\":{\"id\":-1,\"diagonal\":false,\"substacks\":[],\"qnode\":{\"value\":0.5}}}," ++
                  "{\"qstack\":{\"id\":-1,\"diagonal\":false,\"substacks\":[],\"qnode\":{\"value\":0.5}}}," ++
                  "{\"qstack\":{\"id\":-1,\"diagonal\":true,\"substacks\":[],\"qnode\":{\"value\":0.5}}}]," ++
                  "\"qnode\":{\"qbit\":[\"ZZ\",\"ZO\",\"OZ\",\"OO\"]}}}" ),
                  (QuantumStack 1 False [QuantumStack (-1) False [] (StackValue (Snum 0.5)),
                      QuantumStack (-1) False []  (StackValue (Snum 0.5)),
                      QuantumStack (-1) False [] (StackValue (Snum 0.5))]
                    (StackQubit [(Z,Z),(Z,O),(O,O)]),
                  "{\"qstack\":{\"id\":1,\"diagonal\":false,\"substacks\":" ++
                  "[{\"qstack\":{\"id\":-1,\"diagonal\":false,\"substacks\":[],\"qnode\":{\"value\":0.5}}}," ++
                  "{\"qstack\":{\"id\":-1,\"diagonal\":false,\"substacks\":[],\"qnode\":{\"value\":0.5}}}," ++
                  "{\"qstack\":{\"id\":-1,\"diagonal\":false,\"substacks\":[],\"qnode\":{\"value\":0.5}}}]," ++
                  "\"qnode\":{\"qbit\":[\"ZZ\",\"ZO\",\"OO\"]}}}" ),
                  (QuantumStack 1 True [QuantumStack (-1) True [] (StackValue (Snum 0.5)),
                                            QuantumStack (-1) True [] (StackValue (Snum 0.5))]
                    (StackClassical [Left 4, Right True]),
                  "{\"qstack\":{\"id\":1,\"diagonal\":true,\"substacks\":" ++
                  "[{\"qstack\":{\"id\":-1,\"diagonal\":true,\"substacks\":[],\"qnode\":{\"value\":0.5}}}," ++
                  "{\"qstack\":{\"id\":-1,\"diagonal\":true,\"substacks\":[],\"qnode\":{\"value\":0.5}}}]," ++
                  "\"qnode\":{\"cstack\":[4,true]}}}" )]

    stackBoundedJSON = [ (QuantumStack 1 True [QuantumStack (-1) True [] (StackValue (Snum 0.5))] (StackQubit [(Z,Z)]),
                  "{\"qstack\":{\"id\":1,\"diagonal\":true,\"substacks\":\"bottom\"" ++
                  "\"qnode\":{\"qbit\":[\"ZZ\"]}}}" )]

    --checkIt :: StackDescriptor LazyNum -> String -> SpecM ()
    checkIt sd res = it ("returns "++show sd++" as '"++res++"'") $ res ~=? (toJSON sd)

    --checkUnbounded :: QuantumStack LazyNum -> String -> SpecM ()
    checkUnbounded qs res = it ("returns "++show (fixDiags qs)++" as '"++res++"'") $ res ~=? (toJSON $ fixDiags qs)

    --checkBounded :: QuantumStack LazyNum -> String -> SpecM ()
    checkBounded qs res = it ("returns "++show (fixDiags qs)++" as '"++res++"'") $ res ~=? (boundedToJSON 1  $ fixDiags qs)


    tests =  describe "StackToJSON" $ do
      mapM_ (uncurry checkIt) jsonValues
--      mapM_ (uncurry checkIt) cstacks
--      context "unbounded qstack" $ mapM_ (uncurry checkUnbounded) stackJSON
--      context "bounded qstack" $ mapM_ (uncurry checkBounded) stackBoundedJSON


\end{code}

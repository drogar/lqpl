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
    import qualified Data.List as List
        
    import SpecHelper

    import QSM.BasicData

    import Data.LazyNum
    import QSM.Components.ClassicalStack
    import QSM.QuantumStack.QSDefinition
    import QServer.StackToJSON


    import Utility.MakeJSON
        
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
                   (StackClassical [Left 1, Right True], "{\"classical\" : [1, true]}"),
                   (StackQubit [(Z,Z), (Z,O), (O,Z), (O,O)], "{\"qubit\" : [\"ZZ\", \"ZO\", \"OZ\", \"OO\"]}"),
                   (StackData [("Cons", [1,4]), ("Nil",[])],
                                "{\"data\" : [{\"cons\" : \"Cons\", \"addresses\" : [1, 4]}, {\"cons\" : \"Nil\", \"addresses\" : []}]}")]
    cstacks :: [(ClassicalStack,String)]
    cstacks = [(Stack [Left 5, Right False, Left (-1)], "{\"cstack\":[5, false,-1]}")]



    sub5 = QuantumStack (-1) True [] (StackValue (Snum 0.5))
    sub5f = QuantumStack (-1) False [] (StackValue (Snum 0.5))

    sqbzz :: StackDescriptor LazyNum
    sqbzz = StackQubit [(Z,Z)]

    sqb3 :: StackDescriptor LazyNum
    sqb3  = StackQubit [(Z,Z),(Z,O),(O,O)]

    sqb4 :: StackDescriptor LazyNum
    sqb4  = StackQubit [(Z,Z),(Z,O),(O,Z),(O,O)]

    sclass :: StackDescriptor LazyNum
    sclass = StackClassical [Left 4, Right True]

    stackJSON = [ (QuantumStack 1 True [sub5] sqbzz,
                  "{\"qstack\" : {\"id\" : 1, \"diagonal\" : true, \"substacks\" : [" ++(toJSON sub5) ++ 
                  "], \"qnode\" : "++(toJSON sqbzz) ++ "}}"),
                  (QuantumStack 1 True [sub5, sub5f, sub5]  sqb3,
                   "{\"qstack\" : {\"id\" : 1, \"diagonal\" : true, \"substacks\" : [" ++
                   (toCommaSepString [toJSON sub5, toJSON sub5f, toJSON sub5f, toJSON sub5]) ++
                   "], \"qnode\" : " ++ (toJSON sqb4) ++ "}}"),
                  (QuantumStack 1 False [sub5f,sub5f,sub5f] sqb3,
                   "{\"qstack\" : {\"id\" : 1, \"diagonal\" : false, \"substacks\" : [" ++
                   (toCommaSepString [toJSON sub5f, toJSON sub5f, toJSON sub5f]) ++
                   "], \"qnode\" : "++ (toJSON sqb3) ++ "}}"),
                  (QuantumStack 1 True [sub5, sub5]  sclass,
                   "{\"qstack\" : {\"id\" : 1, \"diagonal\" : true, \"substacks\" : [" ++
                   (toCommaSepString [toJSON sub5, toJSON sub5]) ++
                   "], \"qnode\" : " ++ (toJSON sclass) ++ "}}")]

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
      context "unbounded qstack" $ mapM_ (uncurry checkUnbounded) stackJSON
--      context "bounded qstack" $ mapM_ (uncurry checkBounded) stackBoundedJSON


\end{code}

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
    import qualified Data.Map as Map
    import Data.Tuples

    import SpecHelper

    import QSM.BasicData

    import Data.LazyNum
    import QSM.Components.ClassicalStack
    import QSM.Components.MemoryMap
    import QSM.Components.Dump
    import QSM.QuantumStack.QSDefinition
    import QSM.Components.Instructions
    import QServer.StackToJSON


    import Utility.MakeJSON

    import System.Exit


    instance Quantum LazyNum

    main = do
      summary <- hspecWith defaultConfig{configFormatter=progress} tests
      if summaryFailures summary > 0 then exitWith (ExitFailure $ summaryFailures summary)
                                     else exitWith ExitSuccess

    jsonValues =  [(StackZero, "{\"zero\":0}"),
                   (StackValue (SZero), "{\"value\" : 0}" ),
                   (StackClassical [Left 1, Right True], "{\"classical\" : [1, true]}"),
                   (StackQubit [(Z,Z), (Z,O), (O,Z), (O,O)], "{\"qubit\" : [\"ZZ\", \"ZO\", \"OZ\", \"OO\"]}"),
                   (StackData [("Cons", [1,4]), ("Nil",[])],
                                "{\"data\" : [{\"cons\" : \"Cons\", \"addresses\" : [1, 4]}, {\"cons\" : \"Nil\", \"addresses\" : []}]}")]

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

    boundedQstack :: QuantumStack LazyNum
    boundedQstack = QuantumStack 1 True [QuantumStack (-1) True [] (StackValue (Snum 0.5))] (StackQubit [(Z,Z)])


    stackBoundedJSON = [ (boundedQstack,
                  "{\"qstack\" : {\"id\" : 1, \"diagonal\" : true, \"substacks\" : [{\"bottom\" : true}], " ++
                  "\"qnode\" : " ++ (toJSON sqbzz) ++"}}" ),
                  (QuantumStack 1 True [sub5, sub5f, sub5]  sqb3,
                   "{\"qstack\" : {\"id\" : 1, \"diagonal\" : true, \"substacks\" : [" ++
                   "{\"bottom\" : true}, {\"bottom\" : true}, {\"bottom\" : true}, {\"bottom\" : true}" ++
                   "], \"qnode\" : " ++ (toJSON sqb4) ++ "}}")
                       ]

    baseCstack :: ClassicalStack
    baseCstack = Stack [Left 5, Right False, Left (-1)]

    cstacks :: [(ClassicalStack,String)]
    cstacks = [(baseCstack, "{\"cstack\" : [5, false, -1]}")]

    cstacksbnd :: [(ClassicalStack,String)]
    cstacksbnd = [(baseCstack, "{\"cstack\" : [5]}")]

    codepointer :: [(CodePointer, String)]
    codepointer = [(("ent",5), "{\"codepointer\" : [\"ent\", 5]}")]


    memory :: [(Memory LazyNum, String)]
    memory = [(Map.fromList [("ep1", [EnScope, QLoad "@q" 0, DeScope]), ("ep2",[AddCtrl, UnCtrl])],
               "{\"ep1\" : [\"EnScope\",\"QLoad \\\"@q\\\" 0\",\"DeScope\"], \"ep2\" : [\"AddCtrl\",\"UnCtrl\"]}")]

    baseMmap :: MemoryMap
    baseMmap = [Map.fromList [("x", 1), ("x2",3)]]

    mmap :: [(MemoryMap, String)]
    mmap =  [(baseMmap, "{\"memory_map\" : [{\"x\" : 1, \"x2\" : 3}]}"),
             ([Map.fromList [("x", 1), ("x2",3)], Map.fromList [("x", 2), ("a",3)]],
              "{\"memory_map\" : [{\"x\" : 1, \"x2\" : 3}, {\"a\" : 3, \"x\" : 2}]}")]


            {-
          DumpStackSplit {
                     returnLabel :: Label,
                     branchesToDo:: [(QuantumStack b, Label)],
                     resultQStack :: QuantumStack b,
                     saveClsStack :: ClassicalStack,
                     saveNS :: NameSupply,
                     resultNS :: NameSupply,
                     saveStrans :: MemoryMap,
                     resultStrans :: MemoryMap}|
-}

    baseNS :: NameSupply
    baseNS = ([1,2,3,4], 15)

    nameSupplies :: [(NameSupply, String)]
    nameSupplies =[(baseNS, "{\"int_list\" : [1, 2, 3, 4], \"address\" : 15}")]

    dumps :: [(Dump LazyNum, String)]
    dumps = [([DumpCall 3 "ep" baseCstack], "{\"dump\" : [{\"dump_call\" : {\"return_label\" : 3, \"return_ep\" : \"ep\", " ++
                                              (toJSON baseCstack) ++ "}}]}"),
            ([DumpStackSplit 3 [(sub5,1), (sub5, 2)] sub5 baseCstack baseNS baseNS baseMmap baseMmap],
             "{\"dump\" : [{\"dump_split\" : {\"return_label\" : 3, \"branches\" : [{\"qsbranch\" : " ++ (toJSON sub5) ++
             ", \"branch_label\" : 1}, {\"qsbranch\" : " ++ (toJSON sub5) ++ ", \"branch_label\" : 2}], \"qsresult\" : " ++
             (toJSON sub5) ++ ", \"save_cstack\" : " ++ (toJSON baseCstack) ++ ", \"save_ns\" : " ++ (toJSON baseNS) ++
             ", \"result_ns\" : " ++ (toJSON baseNS) ++ ", \"save_stacktrans\" : " ++ (toJSON baseMmap) ++
             ", \"result_stacktrans\" : " ++ (toJSON baseMmap) ++ "}}]}"),
             ([DumpCall 3 "ep" baseCstack, DumpStackSplit 3 [(sub5,1), (sub5, 2)] sub5 baseCstack baseNS baseNS baseMmap baseMmap],
             "{\"dump\" : [{\"dump_call\" : {\"return_label\" : 3, \"return_ep\" : \"ep\", " ++ (toJSON baseCstack) ++
             "}}, {\"dump_split\" : {\"return_label\" : 3, \"branches\" : [{\"qsbranch\" : " ++ (toJSON sub5) ++
             ", \"branch_label\" : 1}, {\"qsbranch\" : " ++ (toJSON sub5) ++ ", \"branch_label\" : 2}], \"qsresult\" : " ++
             (toJSON sub5) ++ ", \"save_cstack\" : " ++ (toJSON baseCstack) ++ ", \"save_ns\" : " ++ (toJSON baseNS) ++
             ", \"result_ns\" : " ++ (toJSON baseNS) ++ ", \"save_stacktrans\" : " ++ (toJSON baseMmap) ++
             ", \"result_stacktrans\" : " ++ (toJSON baseMmap) ++ "}}]}")]


    dumpsBounded :: [(Int, Dump LazyNum, String)]
    dumpsBounded = [(1, [DumpCall 3 "ep" baseCstack], "{\"dump\" : [{\"dump_call\" : {\"return_label\" : 3, \"return_ep\" : \"ep\", " ++
                                                     (boundedToJSON 1 baseCstack) ++ "}}]}"),
                   (0, [DumpCall 3 "ep" baseCstack], "{\"dump\" : []}"),
                   (1, [DumpStackSplit 3 [(sub5,1), (sub5, 2)] boundedQstack baseCstack baseNS baseNS baseMmap baseMmap],
                    "{\"dump\" : [{\"dump_split\" : {\"return_label\" : 3, \"branches\" : [{\"qsbranch\" : " ++ (boundedToJSON 1 sub5) ++
                    ", \"branch_label\" : 1}, {\"qsbranch\" : " ++ (boundedToJSON 1 sub5) ++ ", \"branch_label\" : 2}], \"qsresult\" : " ++
                    (boundedToJSON 1 boundedQstack) ++ ", \"save_cstack\" : " ++ (boundedToJSON 1 baseCstack) ++ ", \"save_ns\" : " ++ (toJSON baseNS) ++
                    ", \"result_ns\" : " ++ (toJSON baseNS) ++ ", \"save_stacktrans\" : " ++ (toJSON baseMmap) ++
                    ", \"result_stacktrans\" : " ++ (toJSON baseMmap) ++ "}}]}")]



    --Checkit :: a -> String -> SpecM ()
    checkIt sd res = it ("returns "++show sd++" as '"++res++"'") $ res ~=? (toJSON sd)
    checkItBounded sd res = it ("returns "++show sd++" as '"++res++"'") $ res ~=? (boundedToJSON 1 sd)
    checkItBoundedWithBound bound sd res  = it ("returns "++show sd++" as '"++res++"'") $ res ~=? (boundedToJSON bound sd)

    --checkUnbounded :: QuantumStack LazyNum -> String -> SpecM ()
    checkUnbounded qs res = it ("returns "++show (fixDiags qs)++" as '"++res++"'") $ res ~=? (toJSON $ fixDiags qs)

    --checkBounded :: QuantumStack LazyNum -> String -> SpecM ()
    checkBounded qs res = it ("returns "++show (fixDiags qs)++" as '"++res++"'") $ res ~=? (boundedToJSON 1  $ fixDiags qs)


    tests =  describe "StackToJSON" $ do
      mapM_ (uncurry checkIt) jsonValues
      context "unbounded qstack" $ mapM_ (uncurry checkUnbounded) stackJSON
      context "bounded qstack" $ mapM_ (uncurry checkBounded) stackBoundedJSON
      mapM_ (uncurry checkIt) cstacks
      mapM_ (uncurry checkItBounded) cstacksbnd
      context "code pointers" $ mapM_ (uncurry checkIt) codepointer
      context "memory" $ mapM_ (uncurry checkIt) memory
      context "memorymap" $ mapM_ (uncurry checkIt) mmap
      context "name supply" $ mapM_ (uncurry checkIt) nameSupplies
      context "dump" $ mapM_ (uncurry checkIt) dumps
      context "bounded dump" $ mapM_ (uncurry3 checkItBoundedWithBound) dumpsBounded


\end{code}

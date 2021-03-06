\begin{code}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
  module Lqpl.Server.StackToJSON where

  import Lqpl.QSM.BasicData
  import Lqpl.Data.ClassicalData
  import Data.Complex (realPart)
  import Lqpl.QSM.QuantumStack.QSDefinition
  import Lqpl.QSM.QuantumStack.QSManipulation
  import Lqpl.Data.Stack as Stack
  import qualified Data.Map as Map
  import qualified Data.List as List
  import Lqpl.Data.Tuples
  import Data.Tuple
  import Lqpl.Data.Computation.BaseType
  import Lqpl.Data.LazyNum
  import Lqpl.QSM.Components.ClassicalStack
  import Lqpl.QSM.Components.Dump
  import Lqpl.QSM.Components.Instructions
  import Lqpl.QSM.Components.MemoryMap
  import Lqpl.Utility.MakeJSON

  fixDiags:: Quantum b => QuantumStack b -> QuantumStack b
  fixDiags stk =
       case descriptor stk of
          StackQubit _  ->
            if onDiagonal stk
              then let (ss,qs) = discardZeros [zz stk, zo stk, oz stk, oo stk] [(Z,Z),(Z,O),(O,Z),(O,O)]
              in stk{subStacks = Prelude.map fixDiags ss,
                  descriptor = StackQubit qs }
              else stk
          _            -> stk

  discardZeros :: Quantum b => [QuantumStack b] -> [(Basis,Basis)] -> ([QuantumStack b], [(Basis,Basis)])
  discardZeros [] [] = ([], [])
  discardZeros (q:qs) (b:bs) =
    let (qs',bs') = discardZeros qs bs
    in if isStackZero q then (qs',bs') else ((q:qs'),(b:bs'))

  class JSON a where
    toJSON :: a -> String
    listToJSON :: String -> [a] -> String
    listToJSON s items = jsonObject [jsonArrayElement s $ List.map toJSON items]

    bounder :: (a->String)->a -> String
    bounder _ = toJSON

    boundedToJSON :: Int -> a -> String
    boundedToJSON _ = toJSON

    boundedListToJSON :: Int -> String -> [a] -> String
    boundedListToJSON n label items = jsonObject [jsonArrayElement label $ List.map (boundedToJSON n) $ items]


  instance JSON ClassicalStack where
    toJSON a = jsonObject [jsonArrayElement "cstack"  $ List.map stripClassical $ Stack.toList a]
    boundedToJSON n a = jsonObject [jsonArrayElement "cstack"  $ List.map stripClassical $ take n $ Stack.toList a]

  boolForJSON :: Bool -> String
  boolForJSON b = if b then "true" else "false"
  stripClassical :: Either Int Bool -> String
  stripClassical (Left i) = show i
  stripClassical (Right b) = boolForJSON b

  stripBasis :: (Basis,Basis) -> String
  stripBasis (Z,Z) = "ZZ"
  stripBasis (Z,O) = "ZO"
  stripBasis (O,Z) = "OZ"
  stripBasis (O,O) = "OO"


  instance JSON (StackDescriptor BaseType) where
    toJSON StackZero = jsonObject ["\"zero\":0"]
    toJSON (StackValue b) = jsonObject [jsonValueElement "value" $ realPart $ approximate b]
    toJSON (StackClassical cs) = jsonObject [jsonArrayElement "classical" (List.map stripClassical cs)]
    toJSON (StackQubit basis) = jsonObject [jsonValueArrayElement "qubit" (List.map stripBasis basis)]
    toJSON (StackData constructors) =
      jsonObject $ [jsonArrayElement "data" $ sdJSON constructors]
      where
        sdJSON [] = []
        sdJSON ((c,stackAddresses):constructors) =
          (jsonObject [jsonValueElement "cons" c, jsonValueArrayElement "addresses" stackAddresses]) : (sdJSON constructors)

  instance JSON CodePointer where
      toJSON (ep,lab) = jsonObject [jsonArrayElement "codepointer" $ [show ep, show lab]]

  instance JSON MemoryMap where
      toJSON mm = jsonObject [jsonArrayElement "memory_map" $ List.map jsonBareObjectFromMap mm]

  instance JSON (QuantumStack BaseType) where
    toJSON fqs = jsonObject  [ jsonElement "qstack" $
                               jsonObject [jsonValueElement "id" (address fqs),
                                           jsonElement "diagonal" (boolForJSON $ onDiagonal fqs),
                                           jsonArrayElement "substacks" (List.map toJSON (subStacks fqs)),
                                           jsonElement "qnode" (toJSON (descriptor fqs))
                                          ]
                             ]

    boundedToJSON 1 fqs =
        jsonObject  [ jsonElement "qstack" $
                      jsonObject [jsonValueElement "id" (address fqs),
                                  jsonElement "diagonal" (boolForJSON $ onDiagonal fqs),
                                  jsonArrayElement "substacks" $
                                     List.map (\_ ->   jsonObject [jsonElement "bottom" "true"]) (subStacks fqs),
                                  jsonElement "qnode" (toJSON (descriptor fqs))
                                 ]
                    ]
    boundedToJSON n fqs =
        jsonObject  [ jsonElement "qstack" $
                      jsonObject [jsonValueElement "id" (address fqs),
                                  jsonElement "diagonal" (boolForJSON $ onDiagonal fqs),
                                  jsonArrayElement "substacks" (List.map (\x -> boundedToJSON (n-1) x) (subStacks fqs)),
                                  jsonElement "qnode" (toJSON (descriptor fqs))
                                 ]
                    ]


  instance (Show a) => JSON (Memory a) where
        toJSON mem = let show_ins = Map.map (List.map show) mem
--                         strascs = List.map (app2of2 (List.map show)) ascs
--                         jvelts =  List.map (:[]) $ List.map (uncurry jsonValueElement) strascs
                     in jsonObject [jsonValueElementFromMap show_ins]

  instance JSON NameSupply where
      toJSON (ints, i) = jsonObject [jsonValueArrayElement "int_list" ints, jsonValueElement "address" i]

  instance JSON (Dump BaseType) where
    toJSON = listToJSON "dump"
    boundedToJSON n = boundedListToJSON n "dump"

  instance JSON (DumpElement BaseType) where
    toJSON (DumpStackSplit ret branches resultQ saveC saveNS resultNS saveMM resultMM) =
        let jsonBranch (a,b) = jsonObject [jsonElement "qsbranch" $ toJSON a, jsonValueElement "branch_label" b]
        in  jsonObject [jsonElement  "dump_split" $ jsonObject [
                                     jsonValueElement "return_label" ret,
                                     jsonArrayElement "branches" $ List.map jsonBranch branches,
                                     jsonElement "qsresult" $ toJSON resultQ,
                                     jsonElement "save_cstack" $ toJSON saveC,
                                     jsonElement "save_ns" $ toJSON saveNS,
                                     jsonElement "result_ns" $ toJSON resultNS,
                                     jsonElement "save_stacktrans" $ toJSON saveMM,
                                     jsonElement "result_stacktrans" $ toJSON resultMM
                                        ]
                        ]

    toJSON (DumpCall ret ep saveC) = jsonObject [jsonElement "dump_call" $ jsonObject [
                                                                  jsonValueElement "return_label" ret,
                                                                  jsonValueElement "return_ep" ep,
                                                                  jsonElement "classical" $ toJSON saveC]
                                                ]
    boundedToJSON 0 _ = ""
    boundedToJSON n (DumpStackSplit ret branches resultQ saveC saveNS resultNS saveMM resultMM) =
        let jsonBranch (a,b) = jsonObject [jsonElement "qsbranch" $ boundedToJSON n a,
                                           jsonValueElement "branch_label" b]
        in  jsonObject [jsonElement  "dump_split" $ jsonObject [
                                     jsonValueElement "return_label" ret,
                                     jsonArrayElement "branches" $ List.map jsonBranch branches,
                                     jsonElement "qsresult" $ boundedToJSON n resultQ,
                                     jsonElement "save_cstack" $ boundedToJSON n saveC,
                                     jsonElement "save_ns" $ toJSON saveNS,
                                     jsonElement "result_ns" $ toJSON resultNS,
                                     jsonElement "save_stacktrans" $ toJSON saveMM,
                                     jsonElement "result_stacktrans" $ toJSON resultMM
                                        ]
                        ]
    boundedToJSON n  (DumpCall ret ep saveC) = jsonObject [jsonElement "dump_call" $ jsonObject [
                                                                  jsonValueElement "return_label" ret,
                                                                  jsonValueElement "return_ep" ep,
                                                                  jsonElement "classical" $ boundedToJSON n saveC]
                                                          ]
\end{code}

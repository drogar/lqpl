\begin{code}

  {-# LANGUAGE OverlappingInstances #-}
  module QServer.StackToJSON where

  import QSM.BasicData
  import Data.ClassicalData
  import QSM.QuantumStack.QSDefinition
  import QSM.QuantumStack.QSManipulation
  import Data.Stack as Stack
  import qualified Data.Map as Map
  import qualified Data.List as List
  import Data.Tuples
  import Data.Tuple
  import Data.LazyNum
  import QSM.Components.ClassicalStack
  import QSM.Components.Dump
  import QSM.Components.Instructions
  import QSM.Components.MemoryMap
  import Utility.MakeJSON

  --instance Quantum LazyNum

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

  surroundWith :: String->String -> String
  surroundWith tag item = '<':tag++">"++item++"</"++tag++">"

  class JSON a where
    toJSON :: a -> String
    listToJSON :: String -> [a] -> String
    listToJSON s items = jsonArrayElement s $ List.map toJSON items

    bounder :: (a->String)->a -> String
    bounder _ = toJSON

    boundedToJSON :: Int -> a -> String
    boundedToJSON _ = toJSON

    boundedListToJSON :: Int -> String -> [a] -> String
    boundedListToJSON n label items = jsonArrayElement label $ List.map (boundedToJSON n) $ items


--  instance JSON ClassicalStack where
--    toJSON a = listToJSON "cstack"  $ Stack.toList a
--    boundedToJSON n  a = listToJSON "cstack"  $ take n $ Stack.toList a

  stripClassical :: Either Int Bool -> String
  stripClassical (Left i) = show i
  stripClassical (Right b) = if b then "true" else "false"

  stripBasis :: (Basis,Basis) -> String
  stripBasis (Z,Z) = "ZZ"
  stripBasis (Z,O) = "ZO"
  stripBasis (O,Z) = "OZ"
  stripBasis (O,O) = "OO"

  instance (Show b) => JSON (StackDescriptor b) where
    toJSON StackZero = jsonObject ["\"zero\":0"]
    toJSON (StackValue b) = jsonObject [jsonValueElement "value" b]
    toJSON (StackClassical cs) = jsonObject [jsonArrayElement "classical" (List.map stripClassical cs)]
    toJSON (StackQubit basis) = jsonObject [jsonValueArrayElement "qubit" (List.map stripBasis basis)]
    toJSON (StackData constructors) =
      jsonObject $ [jsonArrayElement "data" $ sdJSON constructors]
      where
        sdJSON [] = []
        sdJSON ((c,stackAddresses):constructors) =
          (jsonObject [jsonValueElement "cons" c, jsonValueArrayElement "addresses" stackAddresses]) : (sdJSON constructors)

{-
  instance (Show a)=> JSON (Instruction a) where
    toJSON = show

  instance (Show a) => JSON [Instruction a] where
    toJSON ins = jsonObject $ jsonArrayElement "mmap" $ List.map toJSON ins

  instance (JSON b)=> JSON (QuantumStack b) where
    toJSON fqs = jsonObject  [ jsonElement "qstack" $
      jsonObject [jsonElement "id" (show $ address fqs), jsonElement "diagonal" (show $ onDiagonal fqs), jsonArrayElement "substacks" (List.map toJSON (substacks fqs)), toJson (descriptor fqs)]]
      surroundWith "sstack" $ toJSON (address fqs) ++
                  toJSON (onDiagonal fqs) ++
                  (listToJSON "substacks" (subStacks fqs)) ++
                  toJSON (descriptor fqs)

    boundedToJSON 0 _ = "\"bottom\""
    boundedToJSON n fqs =
      surroundWith "Qstack" $ toJSON (address fqs) ++
                  toJSON (onDiagonal fqs) ++
                  (boundedListToJSON (n-1) "substacks" (subStacks fqs)) ++
                  toJSON (descriptor fqs)

  instance (JSON b) => JSON (Dump b) where
    toJSON = listToJSON "Dump"
    boundedToJSON n = boundedListToJSON n "Dump"

  instance (JSON b)=> JSON (DumpElement b) where
    toJSON (DumpStackSplit ret branches resultQ saveC saveNS resultNS saveMM resultMM) =
        surroundWith "DumpSplit" $ (toJSON ret) ++ (listToJSON "Branches" branches) ++ toJSON resultQ ++ toJSON saveC ++
            (surroundWith "SaveNameSpace" $ listToJSON "ints" saveilist ++ toJSON savestackaddress) ++
            (surroundWith "ResultNameSpace" $ listToJSON "ints" resultislist ++ toJSON resultstackaddress) ++
            listToJSON "SavedMemoryMap" saveMM ++ listToJSON "ResultMemoryMap" resultMM
            where
              saveilist = fst saveNS
              savestackaddress = snd saveNS
              resultislist = fst resultNS
              resultstackaddress = snd resultNS

    toJSON (DumpCall ret ep saveC) = surroundWith "DumpCall" $ toJSON ret ++ toJSON ep ++ toJSON saveC

    boundedToJSON 0 _ = "<dumpbottom/>"
    boundedToJSON n (DumpStackSplit ret branches resultQ saveC saveNS resultNS saveMM resultMM) =
         surroundWith "DumpSplit" $ (toJSON ret) ++ (boundedListToJSON n "Branches" branches) ++ boundedToJSON n resultQ ++ toJSON saveC ++
            (surroundWith "SaveNameSpace" $ listToJSON "ints" saveilist ++ toJSON savestackaddress) ++
            (surroundWith "ResultNameSpace" $ listToJSON "ints" resultislist ++ toJSON resultstackaddress) ++
            listToJSON "SavedMemoryMap"  saveMM ++ listToJSON "ResultMemoryMap"  resultMM
            where
              saveilist = fst saveNS
              savestackaddress = snd saveNS
              resultislist = fst resultNS
              resultstackaddress = snd resultNS
    boundedToJSON n c@(DumpCall _ _ _) = toJSON c

-}
\end{code}

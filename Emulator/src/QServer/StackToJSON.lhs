\begin{code}

  {-# LANGUAGE OverlappingInstances #-}
  module QServer.StackToJSON where

  import QSM.BasicData
  import Data.ClassicalData
  import QSM.QuantumStack.QSDefinition
  import QSM.QuantumStack.QSManipulation
  import Data.Stack as Stack
  import Data.Map as Map
  import Data.List as List
  import Data.Tuples
  import Data.Tuple
  import Data.LazyNum
  import QSM.Components.ClassicalStack
  import QSM.Components.Dump
  import QSM.Components.Instructions
  import QSM.Components.MemoryMap


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
    listToJSON label = (surroundWith label) . listToJSON'
      where
        listToJSON' [] = ""
        listToJSON' (x:xs) = (toJSON x) ++ listToJSON' xs

    bounder :: (a->String)->a -> String
    bounder _ = toJSON

    boundedToJSON :: Int -> a -> String
    boundedToJSON _ = toJSON

    boundedListToJSON :: Int -> String -> [a] -> String
    boundedListToJSON n label = (surroundWith label) . boundedListToJSON' n
      where
        boundedListToJSON' n [] = ""
        boundedListToJSON' n (x:xs) = (boundedToJSON n x) ++ boundedListToJSON' n xs

  instance JSON Bool where
    toJSON = (surroundWith "bool") . show

  instance JSON Char where
    toJSON = (surroundWith "char") . show

  instance JSON String where
    toJSON = (surroundWith "string")

  instance JSON LazyNum where
    toJSON = (surroundWith "number") . show

  instance (JSON k,JSON v)=>JSON (Map k v) where
    toJSON a = surroundWith "map"  $ List.foldl (++) "" $ fmap (surroundWith "kvpair") $
      uncurry (zipWith (++)) $ unzip $
      fmap (pair (surroundWith "key" . toJSON) (surroundWith "value" . toJSON)) $ Map.toList a

  instance (JSON a, JSON b) => JSON (a,b) where
    toJSON (a,b) = surroundWith "pair" $ toJSON a ++ toJSON b
    boundedToJSON 0 (a,b) = surroundWith "pair" "<bottom/>"
    boundedToJSON n (a,b) = surroundWith "pair" $ boundedToJSON n a ++ boundedToJSON n b

  instance (JSON a, JSON b,JSON c) => JSON (a,b,c) where
    toJSON (a,b,c) = surroundWith "triple" $ toJSON a ++ toJSON b ++ toJSON c
    boundedToJSON 0 (_,_,_) = surroundWith "triple" "<bottom/>"
    boundedToJSON n (a,b,c) = surroundWith "triple" $ boundedToJSON n a ++ boundedToJSON n b ++ boundedToJSON n c

  instance JSON Basis where
    toJSON Z = "<qz/>"
    toJSON O = "<qo/>"


  instance JSON Int where
    toJSON i = surroundWith "int" $ show i

  instance JSON Double where
    toJSON d = surroundWith "double" $ show d


  instance JSON ClassicalData where
    toJSON (Left i) = surroundWith "cint" $ show i
    toJSON (Right b) = surroundWith "cbool" $ show b

  instance JSON ClassicalStack where
    toJSON a = listToJSON "Classical"  $ Stack.toList a
    boundedToJSON n  a = listToJSON "Classical"  $ take n $ Stack.toList a

  instance (JSON b) => JSON (StackDescriptor b) where
    toJSON StackZero = "<Zero/>"
    toJSON (StackValue b) = surroundWith "Value" $ toJSON b
    toJSON (StackClassical cs) = listToJSON "Classical" cs
    toJSON (StackQubit b) = listToJSON "Qubits" b
    toJSON (StackData constructors) =
      surroundWith "AlgebraicData" $ sdJSON constructors
      where
        sdJSON [] = ""
        sdJSON ((c,stackAddresses):constructors) =
          toJSON c ++ listToJSON "StackAddresses" stackAddresses ++ sdJSON constructors


  instance (Show a)=> JSON (Instruction a) where
    toJSON = surroundWith "i" . show

  instance (Show a) => JSON [Instruction a] where
    toJSON = surroundWith "instructions" . concat . (List.map toJSON)

  instance (JSON b)=> JSON (QuantumStack b) where
    toJSON fqs =
      surroundWith "Qstack" $ toJSON (address fqs) ++
                  toJSON (onDiagonal fqs) ++
                  (listToJSON "substacks" (subStacks fqs)) ++
                  toJSON (descriptor fqs)

    boundedToJSON 0 _ = "<bottom/>"
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


\end{code}

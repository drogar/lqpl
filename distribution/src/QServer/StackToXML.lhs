\begin{code}
  
  {-# LANGUAGE OverlappingInstances #-}
  module QServer.StackToXML where

  import QSM.BasicData
  import Data.ClassicalData
  import QSM.QuantumStack.QSDefinition
  import Data.Stack as Stack
  import Data.Map as Map
  import Data.Tuples
  import Data.Tuple
  import Data.LazyNum
  import QSM.Components.ClassicalStack
  import QSM.Components.Dump
  import QSM.Components.Instructions
  import QSM.Components.MemoryMap
  
  surroundWith :: String->String -> String
  surroundWith tag item = '<':tag++">"++item++"</"++tag++">"
  
  class XML a where
    toXML :: a -> String
    listToXML :: String -> [a] -> String
    listToXML label = (surroundWith label) . listToXML' 
      where 
        listToXML' [] = ""
        listToXML' (x:xs) = (toXML x) ++ listToXML' xs
          
    bounder :: (a->String)->a -> String
    bounder _ = toXML
    
    boundedToXML :: Int -> a -> String
    boundedToXML _ = toXML
    
    boundedListToXML :: Int -> String -> [a] -> String
    boundedListToXML n label = (surroundWith label) . boundedListToXML' n
      where 
        boundedListToXML' n [] = ""
        boundedListToXML' n (x:xs) = (boundedToXML n x) ++ boundedListToXML' n xs
    
  instance XML Bool where
    toXML = (surroundWith "bool") . show
    
  instance XML Char where
    toXML = (surroundWith "char") . show
      
  instance XML String where
    toXML = (surroundWith "string") . show
    
  instance XML LazyNum where
    toXML = (surroundWith "number") . show
     
  instance (XML k,XML v)=>XML (Map k v) where
    toXML a = surroundWith "map"  $ foldl (++) "" $ fmap (surroundWith "kvpair") $ 
      uncurry (zipWith (++)) $ unzip $
      fmap (pair (surroundWith "key" . toXML) (surroundWith "value" . toXML)) $ Map.toList a

  instance (XML a, XML b) => XML (a,b) where
    toXML (a,b) = surroundWith "pair" $ toXML a ++ toXML b
    boundedToXML 0 (a,b) = surroundWith "pair" "<bottom/>"
    boundedToXML n (a,b) = surroundWith "pair" $ boundedToXML n a ++ boundedToXML n b
    
  instance XML Basis where
    toXML Z = "<qz/>"
    toXML O = "<qo/>"
    
  instance XML Label where
    toXML = surroundWith "label" . show
    
 -- instance XML NameSupply where
--    toXML (a,b) = surroundWith "nameSupply" $ toXML (a,b)
    
 -- instance XML Constructor where
--    toXML  = surroundWith "constructor"
    
--  instance XML StackAddress where
--    toXML  = (surroundWith "address") . show 
    
--  instance XML EntryPoint where
--    toXML = surroundWith "entrypoint" . show
    
  instance XML ClassicalData where
    toXML (Left i) = surroundWith "cint" $ show i
    toXML (Right b) = surroundWith "cbool" $ show b
    
  instance XML ClassicalStack where
    toXML a = listToXML "cstack"  $ Stack.toList a
    boundedToXML n  a = listToXML "cstack"  $ take n $ Stack.toList a
    
  instance (XML b) => XML (StackDescriptor b) where
    toXML StackZero = "<Zero/>"
    toXML (StackValue b) = surroundWith "Value" $ toXML b
    toXML (StackClassical cs) = listToXML "ClassicalStack" cs
    toXML (StackQubit b) = listToXML "Qubits" b
    toXML (StackData constructors) = 
      surroundWith "AlgebraicData" $ sdXML constructors
      where 
        sdXML [] = ""
        sdXML ((c,stackAddresses):constructors) =
          toXML c ++ listToXML "StackAddresses" stackAddresses ++ sdXML constructors
    
    
  instance (Show a)=> XML (Instruction a) where
    toXML = surroundWith "qinstruction" . show
  
  instance (XML b)=> XML (QuantumStack b) where
    toXML qs = surroundWith "Qstack" $ toXML (address qs) ++
                  toXML (onDiagonal qs) ++
                  (listToXML "substacks" (subStacks qs)) ++
                  toXML (descriptor qs)  
    
    boundedToXML 0 _ = "<bottom/>"
    boundedToXML n qs = surroundWith "Qstack" $ toXML (address qs) ++
                  toXML (onDiagonal qs) ++
                  (listToXML "substacks" $ take (n-1) (subStacks qs)) ++ 
                  toXML (descriptor qs)
    
  instance (XML b)=> XML (DumpElement b) where
    toXML (DumpStackSplit ret branches resultQ saveC saveNS resultNS saveMM resultMM) =
        surroundWith "DumpSplit" $ (toXML ret) ++ (listToXML "Branches" branches) ++ toXML resultQ ++ toXML saveC ++
            (surroundWith "SaveNameSpace" $ listToXML "ints" saveilist ++ toXML savestackaddress) ++ 
            (surroundWith "ResultNameSpace" $ listToXML "ints" resultislist ++ toXML resultstackaddress) ++
            listToXML "SavedMemoryMap" saveMM ++ listToXML "ResultMemoryMap" resultMM
            where 
              saveilist = fst saveNS
              savestackaddress = snd saveNS
              resultislist = fst resultNS
              resultstackaddress = snd resultNS
          
    toXML (DumpCall ret ep saveC) = surroundWith "DumpCall" $ toXML ret ++ toXML ep ++ toXML saveC
      
    boundedToXML 0 _ = "<dumpbottom/>"
    boundedToXML n (DumpStackSplit ret branches resultQ saveC saveNS resultNS saveMM resultMM) =
         surroundWith "DumpSplit" $ (toXML ret) ++ (boundedListToXML n "Branches" branches) ++ boundedToXML n resultQ ++ toXML saveC ++
            (surroundWith "SaveNameSpace" $ listToXML "ints" saveilist ++ toXML savestackaddress) ++ 
            (surroundWith "ResultNameSpace" $ listToXML "ints" resultislist ++ toXML resultstackaddress) ++ 
            listToXML "SavedMemoryMap"  saveMM ++ listToXML "ResultMemoryMap"  resultMM
            where 
              saveilist = fst saveNS
              savestackaddress = snd saveNS
              resultislist = fst resultNS
              resultstackaddress = snd resultNS
    boundedToXML n c@(DumpCall _ _ _) = toXML c
    
    
\end{code}
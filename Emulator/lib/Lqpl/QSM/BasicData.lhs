%include polycode.fmt
\subsection{Description of basic data and types}\label{subsec:basicdata}
%if false
\begin{code}
module Lqpl.QSM.BasicData (Label,
                      NameSupply,
                      Constructor,
                      StackAddress,
                      StackPointer,
                      Quantum,
                      Basis(..),
                      noAddress,
                      foldToAddress,
                      freshPtr,
                      freshAddress,
                      combineNS,
                      emptyNameSupply,
                      fullQbs,
                      diagQbs,
                      isDiag,
                      toggle,
                      compBasisPairs,
                      showQv
                     )
   where
import Lqpl.Data.ClassComp
import Data.List as List (intersect)
import Lqpl.Data.Tuples (app1of2)
import Data.Map as Map (Map,elems)

\end{code}
%endif
These are the basic data types and definitions used throughout
the quantum stack machine and emulator.

\subsubsection{Labels, Name Supply etc.}\label{subsec:instructionlabels}
\begin{singlespace}
\begin{code}
type Label = Int


type NameSupply = ([Int],StackAddress)

emptyNameSupply :: NameSupply
emptyNameSupply = ([],0)

freshPtr :: NameSupply -> (NameSupply, StackPointer)
freshPtr  = fresh' 1
fresh' :: Int-> NameSupply -> (NameSupply, StackPointer)
fresh' start ([],i) = (([start],i),mkName  start)
fresh' start ~((j:ns),i)
    | j > start = ((start:j:ns,i),mkName start)
    | otherwise = app1of2 (app1of2 (j:)) $ fresh' (start+1) (ns,i)
mkName :: Int -> StackPointer
mkName = ('$':) . show


freshAddress :: NameSupply -> (NameSupply, StackAddress)
freshAddress (ns,nxt) = ((ns,nxt+1),nxt)

combineNS :: NameSupply -> NameSupply -> NameSupply
combineNS (nms1,sa1) (nms2,sa2)
  = (List.intersect nms1 nms2, max sa1 sa2)

type StackAddress = Int

type StackPointer = String

type Constructor = String

class (Show b, Eq b, Comp b) => Quantum  b

noAddress :: StackAddress
noAddress = -1

foldToAddress :: [StackAddress] -> StackAddress
foldToAddress [] = noAddress
foldToAddress (nm:rest) | nm == noAddress = foldToAddress rest
                    | otherwise = nm

--foldMapToName :: (Ord k) => Map k StackAddress -> StackAddress
--foldMapToName = foldToName . (Map.elems)

data Basis = Z | O
           deriving (Eq,Ord,Enum)

instance Show Basis where
   show Z = "0"
   show O = "1"


showQv :: (Basis,Basis) -> String
showQv (Z,Z) = "00"
showQv (Z,O) = "01"
showQv (O,Z) = "10"
showQv (O,O) = "11"

fullQbs :: [(Basis,Basis)]
fullQbs = [(Z,Z), (Z,O),(O,Z), (O,O)]

diagQbs  :: [(Basis,Basis)]
diagQbs = [(Z,Z), (Z,O),(O,O)]

isDiag :: Bool -> [(Basis,Basis)]
isDiag True = diagQbs
isDiag False = fullQbs

toggle :: Basis -> Basis
toggle Z = O
toggle O = Z


compBasisPairs :: (Basis,Basis) -> (Basis,Basis) -> Ordering
compBasisPairs  = compare

\end{code}
\end{singlespace}

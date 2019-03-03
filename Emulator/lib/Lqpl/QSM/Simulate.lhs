%include polycode.fmt
%format ^* = "\ltimes"
%format *^ = "\rtimes"
\subsection{Determination of simulation}\label{subsec:QSM:simulation}

%if false
\begin{code}
{-# LANGUAGE ExistentialQuantification #-}
module Lqpl.QSM.Simulate where
import Lqpl.QSM.QSM
import Lqpl.QSM.BasicData
import Lqpl.QSM.QuantumStack.QSDefinition(trace)
import Lqpl.QSM.QuantumStack.QSRotation(rotateInOrder)
import Lqpl.Data.Computation.BaseType
import Data.List
import Lqpl.Data.Tuples
import System.Random
import Data.Complex

\end{code}
%endif
\subsubsection{Canonicalizing the quantum stack}\label{subsubsec:QSM:canonicalization}

When calculating a simulation of the quantum stack,
the display of variables
needs to be in a predictable order. The rule for this
is to group
all dependant stacks of a data type with that datatype.

{
\begin{singlespace}
\begin{code}

canonicalize :: (Quantum b ) => QuantumStack b ->
                QuantumStack b
canonicalize q
    = case  (descriptor q) of
            StackData  dvals   -> q{subStacks = map canonRot
                                               (zip (map snd dvals) $ subStacks q)}
            _                  -> q

canonRot :: forall b.
            (Quantum b) =>
            ([StackAddress],QuantumStack b) -> QuantumStack b
canonRot = canonicalize . uncurry rotateInOrder

-- regroup -- = (pair (fst . fst) (pair (fst . snd)  id))
regroup :: ((a,b),c) -> (a,c)
regroup ((a,b),c) = (a,c)

\end{code}
\end{singlespace}
}

\subsubsection{Picking sub-trees}
To "run" the quantum machine, a random number
between 0 and 1 is chosen.
This is used to create a choice throughout the tree.
The function
|chooseIt| does this, returning a list of pairs
of strings. Each
pair is composed of the node name and type paired
with its value.


{\begin{singlespace}
\begin{code}

chooseIt :: QuantumStack BaseType ->
            IO (Double, [(String,String,String)]) --nm,typ,val
chooseIt q = do rval <- randomIO
                return  (rval, chooseAndDescend rval q [])

chooseAndDescend ::  Double -> QuantumStack BaseType ->
                    [(String,String,String)]-> [(String,String,String)]
chooseAndDescend f s lis
    | isStackLeaf s              = lis
    | f > (realPart . approximate . trace) s
        = (show (address s), "","DIVERGE "):lis
    | otherwise
      = case (descriptor s) of
          StackQubit qvls  ->
              let possibleChoices = filter (uncurry (==) . fst) $
                                    zip qvls (subStacks s)
                  (f',sval,choice) = chooseq f possibleChoices
              in chooseAndDescend f' choice  ((show $ address s,"Qubit",sval):lis)
          StackClassical cvls  ->
              let (f',sval,choice) = choosei f $ zip cvls (subStacks s)
              in chooseAndDescend f' choice ((show $ address s,"Int",sval):lis)
          StackData dvls  ->
              let (f',sval,choice) = choosed f $ zip dvls (subStacks s)
              in chooseAndDescend f' choice ((show $ address s,"Alg Dtype",sval):lis)

chooseq  :: Double ->
            [((Basis,Basis),QuantumStack BaseType)] ->
            (Double, String, QuantumStack BaseType)
chooseq  f lis
    = (f',show $ fst $ fst chosen, snd chosen)
         where (f',chosen) = choose f lis

choosei  :: Double -> [(ClassicalData,QuantumStack BaseType)] ->
            (Double,String, QuantumStack BaseType)
choosei  f lis
    = (f',show $ fst  chosen, snd chosen)
         where (f',chosen) = choose f lis

choosed  :: Double ->
            [((Constructor, [StackAddress]),QuantumStack BaseType)] ->
            (Double,String, QuantumStack BaseType)
choosed  f lis
    = (f', show $ fst  chosen, snd chosen)
        where (f',chosen) = choose f lis


choose :: Double -> [(a,QuantumStack BaseType)] ->
          (Double,(a,QuantumStack BaseType))
choose f lis = (f',lis !! ind)
               where  traced =  map (realPart . approximate) $
                                scanl1 (+) $ map (trace . snd) lis
                      ind =     length $ takeWhile (< f) traced
                      f' =      if 0 == ind then f
                                else f - (traced !! (ind-1))

\end{code}
\end{singlespace}
}

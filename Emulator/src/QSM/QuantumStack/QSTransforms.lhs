%include polycode.fmt
%format ^* = "\ltimes"
%format *^ = "\rtimes"

\subsection{Description of the quantum stack}\label{subsec:quantumstackdescription}

Code related to producing the various Unitary Transforms
of quantum stacks.

%if false
\begin{code}

module QSM.QuantumStack.QSTransforms 
   (uopToSpecial, setValsFromMat, cTransform
    )
 where
import QSM.BasicData
import QSM.Transformations
import QSM.MachineErrors
import QSM.QuantumStack.QSDefinition
import QSM.QuantumStack.QSManipulation
import QSM.QuantumStack.QSRotation
import QSM.Components.MemoryMap
import QSM.Components.ControlStack

import Data.Matrix
import Data.ClassComp
import Data.Tuples



import Data.List

import Utility.Extras

\end{code}
%endif
{\begin{singlespace}
\begin{code}


uopToSpecial :: (Quantum b) => UnitaryOp ->  
               Maybe (ControlType -> QuantumStack b ->QuantumStack b)
uopToSpecial NotGate    = Just notSpecial
uopToSpecial Hadamard   = Just hadSpecial
uopToSpecial RhoX       = Just notSpecial
uopToSpecial RhoY       = Just rhoYSpecial
uopToSpecial RhoZ       = Just rhoZSpecial
uopToSpecial MinusI     = Just minusIdentSpecial
uopToSpecial Phase      = Just phaseSpecial
uopToSpecial _          = Nothing

\end{code}
\end{singlespace}
}

The \nottr{} transform is accomplished by remapping the keys 
of the dependent substacks of the \qubit.

{\begin{singlespace}
\begin{code}

applySpecial :: (Quantum b)=> 
                (ControlType ->[QuantumStack b] ->
                               [QuantumStack b]) ->
                ControlType ->
                QuantumStack b ->  
                QuantumStack  b
applySpecial f ctrl q
           | isStackLeaf q     = q
           | isStackQubit q   
             = let  qvls =  qvaluesDiag q
                    qvls' =  f ctrl qvls
               in  fixDiagonal (onDiagonal q) $ setQvalues qvls' q
           | otherwise  = error "notspecial"

notSpecial :: (Quantum b)=> ControlType ->
              QuantumStack b ->  QuantumStack  b

notSpecial = applySpecial controlNotTransform


controlNotTransform :: (Quantum b) => ControlType -> 
                       [QuantumStack b] ->
                      [QuantumStack b]
controlNotTransform  IdentityOnly lis     = lis
controlNotTransform RightOnly lis
    = case lis of 
        [a,b,d]      -> [b,a, cjgtTranspose b]
        [a,b,c,d]    -> [b,a,d,c]
controlNotTransform   LeftOnly  lis    
    = case lis of 
        [a,b,d]      -> [cjgtTranspose b,d,b]
        [a,b,c,d]    -> [c,d,a,b]
controlNotTransform Full lis
    = case lis of 
        [a,b,d]      -> [d,cjgtTranspose b,a]
        [a,b,c,d]    -> [d,c,b,a]


\end{code}
\end{singlespace}}

The \Had{} transform is accomplished by 
arithmetic on the  dependent substacks of the \qubit.

{\begin{singlespace}
\begin{code}

hadSpecial :: (Quantum b)=> ControlType ->
              QuantumStack b -> QuantumStack b
hadSpecial  = applySpecial hadTransform


hadTransform :: (Quantum b) => ControlType -> 
                [QuantumStack b] ->
                    [QuantumStack b]
hadTransform IdentityOnly lis    = lis

hadTransform RightOnly lis
    = case lis of
        [a,b,d]    -> let  a' = (a +/+ b)
                           b' = (a -^- b)
                           d' = (cjgtTranspose b -/- d)
                      in   map (oneOvSqr2 ^*) [a',b',d']
        [a,b,c,d]  -> let  a' = (a +^+ b)
                           b' = (a -^- b)
                           c' = (c +^+ d)
                           d' = (c -^- d)
                      in  map (oneOvSqr2 ^*) [a',b',c',d']
hadTransform LeftOnly lis
    = case lis of
        [a,b,d]    -> let  a' = (a +/+ cjgtTranspose b)
                           b' = (b +^+ d)
                           d' = (b -/- d)
                      in  map (oneOvSqr2 ^*) [a',b',d']
        [a,b,c,d]  -> let  a' = (a +^+ c)
                           b' = (b +^+ d)
                           c' = (a -^- c)
                           d' = (b -^- d)
                      in  map (oneOvSqr2 ^*) [a',b',c',d']
hadTransform Full lis
    = case lis of
        [a,b,d]     -> let  c = cjgtTranspose b
                            abm = a -^- b
                            cdm = c -^- d
                            a' = (a +/+ b) +/+ (c +/+ d)
                            b' = (abm +^+ cdm)
                            d' = (a +/+ d) -/- (b +/+ c)
                       in map (oneHalf ^*)  [a',b',d']
        [a,b,c,d]   -> let  ab = a +^+ b
                            cd = c +^+ d
                            abm = a -^- b
                            cdm = c -^- d
                            a' = (ab +^+ cd)
                            b' = (abm +^+ cdm)
                            c' = (ab -^- cd)
                            d' = (abm -^- cdm)
                       in map (oneHalf ^*)  [a',b',c',d']
--hadTransform _ lis = error $ "Had Transforming " ++ (show lis)

\end{code}
\end{singlespace}}



The MinusIdent is the simplest transform in that
a Left or Right version just negates all the sub-trees, 
while the full version does exactly nothing to
the density matrix.

{\begin{singlespace}
\begin{code}

minusIdentSpecial :: (Quantum b)=> ControlType -> 
                     QuantumStack b -> QuantumStack b
minusIdentSpecial  = applySpecial minusIdentTransform

minusIdentTransform :: (Quantum b) => ControlType -> 
                       [QuantumStack b] ->
                           [QuantumStack b]
minusIdentTransform  IdentityOnly lis    = lis
minusIdentTransform  Full lis            = lis
minusIdentTransform  _ lis    = map qsNegate lis

\end{code}
\end{singlespace}}


The RhoY transform is accomplished by remapping the keys 
in the same way as the not transform and then
 some constant multiplications applied to
 the dependent substacks of the \qubit.

{\begin{singlespace}
\begin{code}

rhoYSpecial :: (Quantum b)=> ControlType ->
               QuantumStack b -> QuantumStack b
rhoYSpecial  = applySpecial rhoYTransform

rhoYTransform :: (Quantum b) => ControlType -> 
                 [QuantumStack b] ->
                     [QuantumStack b]
rhoYTransform  IdentityOnly lis    = lis
rhoYTransform  ctrl lis
    = let mi :: (Quantum b)=>b
          mi = - sqrtMinusOne
          i :: (Quantum b)=> b
          i = sqrtMinusOne
      in case lis of
           [a,b,d] -> 
               let  c = cjgtTranspose b
               in   case  ctrl of 
                     RightOnly  -> [i ^* b, mi ^* a,  mi^* c]
                     LeftOnly   -> [mi ^* c, mi ^* d, i^* b]
                     Full       -> [d, qsNegate c,  a]
           [a,b,c,d] ->
               case  ctrl of 
                     RightOnly  -> [i ^* b, mi ^* a, i ^* d, mi^* c]
                     LeftOnly   -> [mi ^* c, mi ^* d, i ^* a, i^* b]
                     Full       -> [d, qsNegate c, qsNegate b, a]


\end{code}
\end{singlespace}}

The RhoZ (or Z) transform is accomplished by negations
of specific  substacks of the \qubit.

{\begin{singlespace}
\begin{code}

rhoZSpecial ::  (Quantum b)=> ControlType -> 
                     QuantumStack b -> QuantumStack b
rhoZSpecial  = applySpecial rhoZTransform

rhoZTransform :: (Quantum b) => ControlType -> 
                [QuantumStack b] ->
                    [QuantumStack b]
rhoZTransform IdentityOnly lis   = lis
rhoZTransform  ctrl lis
    = case lis of
        [a,b,d] ->
           case  ctrl of 
                 RightOnly  ->  [a, qsNegate b, qsNegate d]
                 LeftOnly   ->  [a, b, qsNegate d]
                 Full       ->  [a, qsNegate b, d]
        [a,b,c,d] ->
           case  ctrl of 
                 RightOnly  ->  [a, qsNegate b, c, qsNegate d]
                 LeftOnly   ->  [a,b,qsNegate c, qsNegate d]
                 Full       ->  [a, qsNegate b, qsNegate c, d]



\end{code}
\end{singlespace}}


The Phase transform rotates the phase of the  \qubit.

{\begin{singlespace}
\begin{code}

phaseSpecial :: (Quantum b)=> ControlType ->
                QuantumStack b -> QuantumStack b
phaseSpecial  = applySpecial phaseTransform

phaseTransform :: (Quantum b) => ControlType -> 
                  [QuantumStack b] ->
                  [QuantumStack b]
phaseTransform IdentityOnly lis    = lis
phaseTransform   ctrl lis
    = let mi  :: (Quantum b)=>b
          mi = - sqrtMinusOne
          i  :: (Quantum b)=>b
          i = sqrtMinusOne
      in case lis of
           [a,b,d]  ->
              case  ctrl of 
                    RightOnly  ->  [a, mi ^* b,  mi^* d]
                    LeftOnly   ->  [a, b,  i ^* d]
                    Full       ->  [a, mi ^* b,  d]
           [a,b,c,d]  ->
              case  ctrl of 
                    RightOnly  ->  [a, mi ^* b, c, mi^* d]
                    LeftOnly   ->  [a, b, i ^* c, i ^* d]
                    Full       ->  [a, mi ^* b, i ^* c, d]

\end{code}
\end{singlespace}}


The function |matByStack| performs a matrix multiplication of
the subject matrix with a quantum stack. The quantum stack is first
transformed to a matrix of the appropriate size. The result is a 
matrix of quantum stacks.

{
\begin{singlespace}
\begin{code}
matByStack :: (Quantum b) => Matrix b -> QuantumStack b -> 
              Matrix (QuantumStack b)
matByStack m q
           | isStackZero q       = zeroMat $ qorder m
           | isStackValue q      = error $ matByStackError "data" (show q)
           | isStackData q       = error $ matByStackError "Constructor" (show q)
           | isStackClassical q  = error $ matByStackError "Int" $ show m ++ show q
           | otherwise           = newVals 
           where  newVals  = genmatmul (+^+) (^*) m (stackToMat order q)
	          order    = qorder m
\end{code}
\end{singlespace}
}



The function |stackByMat| is complementary to matByStack, multiplying 
a quantum stack on the right by a matrix and is
used in |cTransform| for |RightOnly| controlled transforms.

{
\begin{singlespace}
\begin{code}
stackByMat :: (Quantum b) => QuantumStack b -> Matrix b -> 
              Matrix (QuantumStack b)
stackByMat q m 
    | isStackZero q       = zeroMat $ qorder m
    | isStackValue q      = error $ stackByMatError "data" (show q)
    | isStackData q       = error $ stackByMatError "Constructor" (show q)
    | isStackClassical q  = error $ stackByMatError "Int" $ show m ++ show q
    | otherwise           =  newVals 
      where newVals  = genmatmul (+^+) (*^) (stackToMat order q) m
	    order     = qorder m

\end{code}
\end{singlespace}
}

The function |setValsFromMat| and dependent function |svfm|
extract the values from a matrix of quantum stacks and
assigns them as new sub-stacks of the argument quantum stack.

{
\begin{singlespace}
\begin{code}
setValsFromMat ::(Quantum b) => Int->
                 Matrix (QuantumStack  b) -> 
                 QuantumStack b -> 
                 QuantumStack b
setValsFromMat n m qb
   | isStackQubit qb = svfm (addresses n qb) n m qb 
   | otherwise = error $ setValsTypeError n m qb
     
\end{code}
\end{singlespace}
}


{
\begin{singlespace}
\begin{code}


svfm :: (Quantum b) => [StackAddress]-> Int-> 
        Matrix (QuantumStack  b) -> 
        QuantumStack b -> QuantumStack b

svfm addrs n m sq
    | n == 1 && isStackQubit sq
        = setQvalues [indexM a b m | a<-[0,1], b<-[0,1]] sq{address = qHead "svfm" addrs}
    | n > 1 && isStackQubit sq
      = setQvalues [svfm (tail addrs) (n-1) (grab a b m) (val (toEnum a, toEnum b) sq)|
                    a<- [0,1], b <- [0,1]] sq

    | otherwise =  error setValsError

{-
svfm _ _ _ _
    = error setValsDataError

svfm nms 1 m qb@(StackZero)
    = StackQubit (head nms) (qv [((a,b), indexM (ei a) (ei b) m) | 
                                a<- basis, b<- basis ])
svfm nms n m sq@(StackZero) 
    = StackQubit (head nms) (qv [((a,b), svfm (tail nms) 
                                 (n-1) (grab  (ei a) (ei b) m) (StackZero))
                                | a<- basis, b<- basis ])
-}
\end{code}
\end{singlespace}
}

Converting a stack to a matrix is done by a recursive descent down
the stack. For example, if 
converting to a $4\times 4$ matrix (two \qubit{}s), 
|stackToMat| converts the four sub matrices of the second level 
\qubit. These four $2\times 2$ matrices 
are then amalgamated via "pasting" to create  a $4\times 4$ matrix.

{
\begin{singlespace}
\begin{code}
stackToMat :: (Quantum b) => Int->QuantumStack  b -> 
              Matrix (QuantumStack b)
stackToMat 1 qs 
    =  let [a,b,c,d] = qvalues qs
       in [[a,b],[c,d]]

stackToMat n qs 
    | n > 1 = reduceM theMat
    | otherwise = error stackToMatError
    where  theMat =  [[a,b],[c,d]]
	   [a,b,c,d] = map (stackToMat (n-1)) (qvalues qs)


zeroMat :: (Num b) => Int -> Matrix (QuantumStack b)
zeroMat  n =  replicate n $ replicate n zerostack
\end{code}
\end{singlespace}
}

\subsubsection{Support for unitary transforms}
\label{subsubsec:supportforunitary}
Applying unitary transforms is done via  matrix multiplication, with the
exception of the \nottr{} transform, which is done via the 
|notSpecial| function.
To do the multiplication requires transforming the stack to a matrix,
then using the new values of the matrix to reset the stack. 

The first defined function, |cTransform|, will transform a |Controlled|
quantum stack. This will result in one of four possibilities:
\begin{equation}
\mathrm{cTransform}\ T\ Q =
\begin{cases}
Q & \mathrm{IdentityOnly\ control}\\
T Q & \mathrm{LeftOnly\ control}\\
Q T^{*} & \mathrm{RightOnly\ control}\\
T Q T^{*} & \mathrm{Full\ control}
\end{cases}
\end{equation}


{
\begin{singlespace}
\begin{code}
cTransform :: (Quantum b)=>ControlType -> Trans b -> 
              QuantumStack b -> QuantumStack b
cTransform IdentityOnly mtrans q   = q
cTransform LeftOnly mtrans  q
    = ctrans' (qorder mtrans) tr q
          where tr = matByStack mtrans 

cTransform RightOnly mtrans q 
    = ctrans' (qorder mtrans) tr q
          where tr qs = stackByMat qs $ conjtrans mtrans

cTransform Full mtrans q
    =  ctrans' (qorder mtrans) tr q
          where tr  qs = genmatmul (+^+) (*^) 
                           (matByStack mtrans qs) (conjtrans mtrans)


ctrans' :: (Quantum b) => Int ->
           (QuantumStack b -> Matrix (QuantumStack b))->
           QuantumStack b -> QuantumStack b
ctrans' order f q = setValsFromMat order (f q) q

\end{code}
\end{singlespace}
}


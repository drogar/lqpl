%include polycode.fmt
%format ^* = "\ltimes"
%format *^ = "\rtimes"

\subsection{Description of the quantum stack}\label{subsec:quantumstackdescription}

%if false
\begin{code}

module QSM.QuantumStack.QSManipulation
    ((^*), (*^), trimStack, descendMap,readdressAll,
     firstReaddressInStack, applyToFirstAddressed, byAddress,
     addressedVal, doBreak, breakQuantum,
     insertQubitOnTop, insertDataOnTop, insertClassicalOnTop,
     bind, unbind, associateCons, associateQbs,
     deepReaddress
    )
 where
import QSM.QuantumStack.QSDefinition
import Data.Tuples
import Data.ClassComp
import Data.ClassicalData
import QSM.BasicData
import Data.Maybe(listToMaybe)
import Data.List
import QSM.Components.MemoryMap
import QSM.MachineErrors
import Data.Map(Map,findWithDefault)

import Utility.Extras

\end{code}
%endif


The public function |breakQuantum| and its
private delegates |bqn| and |doBreak| move the splitting
 down the quantum stack. So, rather
that having to rotate the stack first and then
split, it is split in place.

This does still require traversing down the quantum
stack to the particular |StackAddress| which will be
used in the split or measure. After it is broken, the
stack is repasted on top of each of the pieces by
|bqn|.

This is used when processing \emph{quantum control}
instructions (see \vref{para:measurmentdeconstruction}).
 Each of those
types of instructions requires accessing the
sub-branches of the tree
individually.
{
\begin{singlespace}
\begin{code}

breakQuantum :: (Quantum b) =>
                (QuantumStack b -> Bool) ->
               (QuantumStack b -> Bool) ->
               ([QuantumStack b] -> [(QuantumStack b, Label)]) ->
               String ->
               QuantumStack b ->
               [(QuantumStack b, Label)]
breakQuantum checkf filt lblize errstring qs
   | isStackZero qs = lblize [zerostack]
   | otherwise
     = if checkf qs then lblize $ filter filt $ unfilteredDoBreak qs
        else  error $ errstring ++ " checking " ++ show qs


\end{code}
\end{singlespace}
}


The functions |unfilteredDoBreak| and |doBreak| does the actual split of sub stacks
and returns a list of stacks holding the data of the passed
in stack.


{
\begin{singlespace}
\begin{code}

unfilteredDoBreak :: (Quantum b)=> QuantumStack b ->
                     [QuantumStack b]
unfilteredDoBreak  qs@(QuantumStack _ _ [] _) = [qs]
unfilteredDoBreak   qs@(QuantumStack _ _ stks desc)
    | isStackQubit qs
        = let sstks = qvalues qs
          in [qs{subStacks=[(sstks !! i)],
                     descriptor = elemIn (StackQubit fullQbs) i }
                      | i <- [0..3]]
    | otherwise
        = [qs{subStacks=[(stks !! i)], descriptor = elemIn desc i }
               | i <- [0..(length stks - 1)]]



doBreak :: (Quantum b)=> QuantumStack b ->
               [QuantumStack b]
doBreak qs@(QuantumStack _ _ [] _) = [qs]
doBreak qs@(QuantumStack _ _ stks desc)
    | isStackQubit qs
        = let sstks = qvalues qs
          in filter (\ x -> [zerostack] /= subStacks x)
                 [qs{subStacks=[(sstks !! i)],
                     descriptor = elemIn (StackQubit fullQbs) i }
                      | i <- [0..3]]
    | otherwise
        = [qs{subStacks=[(stks !! i)], descriptor = elemIn desc i }
               | i <- [0..(length stks - 1)]]


elemIn :: StackDescriptor b -> Int -> StackDescriptor b
elemIn StackZero _            = StackZero
elemIn (StackValue b) _       = StackValue b
elemIn (StackClassical ds) i  = StackClassical [(ds !! i)]
elemIn (StackQubit ds) i      = StackQubit [(ds !! i)]
elemIn (StackData ds) i       = StackData [(ds !! i)]

\end{code}
\end{singlespace}
}


{
\begin{singlespace}
\begin{code}

deepReaddress :: StackAddress -> StackAddress -> QuantumStack b ->
                 QuantumStack b
deepReaddress old new qs =
   let  desc = if (isStackData qs) then reAddressDescriptor old new (descriptor qs)
               else (descriptor qs)
        sstacks = map (deepReaddress old new) (subStacks qs)
        addr = if (old == address qs) then new else (address qs)
   in qs{address = addr, subStacks = sstacks, descriptor = desc}


readdressAll :: [(StackAddress,StackAddress)] ->
             QuantumStack b -> QuantumStack b
readdressAll [] qs = qs
readdressAll ((l,r):rest) qs
          = if l == r then readdressAll rest qs
            else readdressAll rest $ firstReaddressInStack r l qs

\end{code}
\end{singlespace}
}

All the value data is stored at the leaves of the quantum stack,
while operations are defined on the actual stacks.
The operations, such as setting a value,
unitary transformations
etc., create linear combinations of the current sub-stacks as
new sub-stacks.
A left scalar multiple function |^*| and
a corresponding right scalar multiple function |*^| are defined to support
this. Note that
scalar multiplication
creates a |StackZero| if the
scalar is $0$.

{
\begin{singlespace}
\begin{code}

(^*) ::(Eq b, Num b)=>  b-> QuantumStack b ->
       QuantumStack b
b ^* _ | b == fromInteger 0 = zerostack
b ^* qs@(QuantumStack _ _ _ StackZero) = qs
b ^* qs@(QuantumStack _ _ _ (StackValue c)) = qs{descriptor= StackValue (b*c)}
b ^* s = descendMap (b ^*) s


(*^) ::(Eq b, Num b)=> QuantumStack b ->    b->
       QuantumStack b
(*^) x = (^* x)

\end{code}
\end{singlespace}
}

The auxiliary function |trimStack| is used to
reduce full stacks of
zero values to |StackZero|, which is then used by the |Num.+| function.
%if codeOnly || showSupportFunctionDetail
\begin{code}

trimStack ::(Quantum b)=>
            Maybe Int ->
            QuantumStack b->
            QuantumStack b
trimStack eps q = case eps of
                           Nothing  -> trimStack' q
                           Just i   -> trimStackWithEpsilon (pow i) q

trimStack' ::(Quantum b)=>
            QuantumStack b->
            QuantumStack b
trimStack' qs@(QuantumStack _ _ _ StackZero) = qs
trimStack' qs@(QuantumStack _ _ _ (StackValue b))
    | b == fromInteger 0 = qs{descriptor=StackZero}
    | otherwise = qs
trimStack' qs
    = let cv' = map trimStack' $ subStacks qs
          cond = foldr (\ stk  -> (&& isStackZero stk))  True cv'
      in if cond then zerostack else setSubStacks cv' qs


trimStackWithEpsilon ::(Quantum b)=>
                       b ->
                       QuantumStack b->
                       QuantumStack b
trimStackWithEpsilon  _ qs@(QuantumStack _ _ _ StackZero) = qs
trimStackWithEpsilon   v qs@(QuantumStack _ _ _ (StackValue b))
    | mag b < mag v = qs{descriptor=StackZero, subStacks = []}
    | otherwise = qs
trimStackWithEpsilon v qs@(QuantumStack _ od ss (StackQubit qvls))
    = let cv' = map (trimStackWithEpsilon v) $ qvaluesDiag qs
          cond = foldr (\ stk  -> (&& isStackZero stk))  True cv'
      in if cond then zerostack
         else setQvalues cv' qs
trimStackWithEpsilon v qs
    = let cv' = map (trimStackWithEpsilon v) $ subStacks qs
          cond = foldr (\ stk  -> (&& isStackZero stk))  True cv'
      in if cond then zerostack
         else setSubStacks cv' qs

\end{code}
%endif

The |descendMap| function works like a functor,
applying a |QuantumStack|
endomorphism to each of the sub-stacks, but
not the container elements themselves.

\begin{code}

descendMap :: (QuantumStack b -> QuantumStack b) ->
              QuantumStack b ->
              QuantumStack b
descendMap f qs
    | isStackLeaf qs = f qs
    | otherwise  = qApply f qs

\end{code}

The function |applyToFirstAddressed| searches for the
first node matching the supplied address and then applies
the given quantum stack endomorphism to that node.

\begin{code}

applyToFirstAddressed ::  StackAddress ->
              ( QuantumStack b -> QuantumStack b) ->
              QuantumStack b ->
              QuantumStack b

applyToFirstAddressed addr f qs
    | isStackLeaf qs = f qs
    | addr == address qs = f qs
    | otherwise = descendMap (applyToFirstAddressed addr f) qs

byAddress ::  StackAddress ->
              ( QuantumStack b -> QuantumStack b) ->
              QuantumStack b ->
              QuantumStack b
byAddress   = applyToFirstAddressed

firstReaddressInStack ::   StackAddress-> StackAddress ->
                           QuantumStack b ->   QuantumStack b
firstReaddressInStack  oldAddress  = applyToFirstAddressed oldAddress . setAddress
{-
descendFold :: (Quantum b) => (a -> QuantumStack b -> a)
            -> a -> QuantumStack b -> a
descendFold f val qs
            | isStackLeaf qs = f val qs
            | otherwise = foldl' f val $ subStacks qs
-}
addressedVal ::  StackAddress ->
          QuantumStack b ->
          Maybe (ClassicalData)
addressedVal _ (QuantumStack _ _ _ (StackValue _))  = Nothing
addressedVal _ (QuantumStack _ _ _ StackZero)       = Nothing
addressedVal nm q
    | nm == address q  = topVal q
    | otherwise        = firstJust $ goDown (addressedVal  nm)  q
    where goDown f  qs
              | isStackLeaf qs = [f qs]
             | otherwise = map f $ subStacks qs

firstJust :: [Maybe a] -> Maybe a
firstJust []          = Nothing
firstJust ((Just a):_)  = Just a
firstJust (x:xs)      = firstJust xs




topVal ::  QuantumStack b ->
          Maybe (ClassicalData)
topVal qs@(QuantumStack _ _ _ (StackClassical cvals)) = listToMaybe cvals
topVal _ = Nothing



insertQubitOnTop :: StackAddress ->Basis
                    -> QuantumStack b -> QuantumStack b
insertQubitOnTop addr base qs  =
    QuantumStack addr True [qs] (StackQubit [(base,base)])

insertDataOnTop ::  StackAddress ->Constructor
                    -> QuantumStack b -> QuantumStack b
insertDataOnTop addr cons qs =
    QuantumStack addr True [qs] (StackData [(cons,[])])


insertClassicalOnTop :: StackAddress ->ClassicalData
                    -> QuantumStack b -> QuantumStack b
insertClassicalOnTop addr clscl qs =
    QuantumStack addr True [qs] (StackClassical [clscl])

\end{code}
\end{singlespace}
}

\subsubsection{Support for node construction and deletion}
\label{subsubsec:supportfornode}
The act of binding nodes to data nodes requires
traversing the quantum stack for the desired name and creating a new
name for it. That new name is then attached to the data node.

{
\begin{singlespace}
\begin{code}
bind :: StackAddress ->  QuantumStack b ->
        QuantumStack b
bind addr theQstack@(QuantumStack _ _ _ (StackData  [(c,daddrs)]))
     = theQstack{descriptor = StackData [(c,addr:daddrs)]}

bind addr theQstack@(QuantumStack _ _ _ (StackData  _))
    = error  bindMultiCons

bind _ _ = error bindDataCheck

\end{code}
\end{singlespace}
}

Unbinding node from data nodes requires
renaming the bound node and removing from the list of those
 attached to the data node. Without rotates, we must supply
the target name and apply that as a "first" in stack algorithm.
% Had the line in the let   ns' = reuse oldnm ns
% and in the result         else (ns', StackCons dname $ dv [(c,(tail bvs,
% before in the code below. However, can't reuse names due to
% name capture issues in different sides of measure etc.
{
\begin{singlespace}
\begin{code}
unbind :: (Quantum b) => StackPointer ->
          StackPointer ->  MemoryMap -> QuantumStack b ->
          (QuantumStack b, MemoryMap)

unbind target nm mm qs =
    let targetAddr = getAddress target mm
    in applyAndUnzip targetAddr (unbind' nm mm) qs

unbind' :: (Show b)=> StackPointer -> MemoryMap ->
           QuantumStack b-> (QuantumStack  b, MemoryMap)

unbind' nm mm qstk@(QuantumStack _ _ [qs] (StackData [(c,bvs)]))
       = let numBound = length bvs
             --qs' = rotateup oldnm qs
         in if numBound == 0
            then error unbindNothingBound
            else  let  addr = qHead "unbind' in QSManip" bvs
                       mm' = addTranslation nm addr mm
                  in  (qstk{descriptor = StackData [(c,tail bvs)]},mm')

unbind' nm mm qstk@(QuantumStack _ _ _ (StackData _))
    = error unbindBadCons

unbind' a b c= error $ unbindDataCheck ++ ", "  ++ show a ++ ", " ++ show b ++ ", " ++ show c

applyAndUnzip :: (Quantum b) => StackAddress ->
              ( QuantumStack b -> (QuantumStack b,MemoryMap)) ->
              QuantumStack b -> (QuantumStack b ,MemoryMap)
applyAndUnzip sa f  qs
    | isStackLeaf qs  || sa == address qs       = f qs
    | otherwise
      = let (newSubStacks, mms) = unzip $ map (applyAndUnzip sa f) (subStacks qs)
            mm' = qHead "applyandunzip qsmanip" mms
        in (setSubStacks newSubStacks qs , mm')

\end{code}
\end{singlespace}
}

Some of the details of associating various sub-stacks
with the appropriate labels in the executing code is
broken out as separate functions below.
***** Potentially sort, potentially pass down during break...
{\begin{singlespace}
\begin{code}
associateQbs :: Label -> Label ->
            [QuantumStack b] ->
            [(QuantumStack b, Label)]
associateQbs  _ _ [] = []
associateQbs   lbl0 lbl1 (qs:qss)
    = (case descriptor qs of
        StackQubit [(Z,_)]     ->  (qs,lbl0)
        StackQubit [(O,_)]     ->  (qs,lbl1)
        _      -> error measureDataCheck) : associateQbs lbl0 lbl1 qss

associateCons :: Map Constructor Label->
                 [QuantumStack b] -> [(QuantumStack b, Label)]
associateCons jumpMap [] = []
associateCons jumpMap (qs@(QuantumStack _ _ _ (StackData dvals)):qss)
    = let cons = fst $ qHead "associateCons" dvals
          lbl = findWithDefault (-1) cons jumpMap
      in (qs, lbl):associateCons jumpMap qss
associateCons _ _ = error splitDataCheck
\end{code}
\end{singlespace}
}

\begin{code}
module StackFunctions where

import QSM.QSM

import QSM.BasicData
import QSM.MachineErrors
import QSM.Transformations

import QSM.Components.ClassicalStack
import QSM.Components.ControlStack
import QSM.Components.Dump
import QSM.Components.Instructions
import QSM.Components.MemoryMap

import QSM.QuantumStack.QSDefinition
import QSM.QuantumStack.QSManipulation
import QSM.QuantumStack.QSRotation
import QSM.QuantumStack.QSTransforms

import Data.LazyNum
import Data.ClassComp
import Data.Matrix
import Data.Map as Map
import Data.List as List
import Data.Stream
import Data.InfList

instance Quantum LazyNum

mat1::Matrix LazyNum

mat1 = [ [(Svar "a"), (Svar "b")], [(Svar "c"), (Svar "d")]]

p251 :: QuantumStack  LazyNum
p252 :: QuantumStack  LazyNum
p253 :: QuantumStack  LazyNum
p254 :: QuantumStack  LazyNum

p251 = QuantumStack  1 True [] (StackValue (Snum 0.25)) 
p252 = QuantumStack  2 True [] (StackValue (Snum 0.25)) 
p253 = QuantumStack  3 True [] (StackValue (Snum 0.25)) 
p254 = QuantumStack  4 True [] (StackValue (Snum 0.25)) 


pgirl :: QuantumStack  LazyNum
pboys  :: QuantumStack  LazyNum

pgirl =  QuantumStack 6 True [p251] (StackData [("Girl", [])]) 
pboys =  QuantumStack 7 True [p252,p253] (StackData [("Boy1", []),("Boy2",[])])

mmgirl :: MemoryMap
mmboys :: MemoryMap

mmgirl = [singleton "@result" 6, Map.empty,Map.empty,Map.empty]
mmboys = [singleton "@result" 7, Map.empty,Map.empty]

nsgirl :: NameSupply
nsgirl = ([],7)

ldleft  :: QuantumStack  LazyNum
ldright :: QuantumStack  LazyNum


l2  :: QuantumStack  LazyNum


l12  :: QuantumStack  LazyNum

r1  :: QuantumStack  LazyNum
r2  :: QuantumStack  LazyNum
r12  :: QuantumStack  LazyNum
r13  :: QuantumStack  LazyNum

l2 = QuantumStack 2 True [p251] (StackClassical [Left 1])

l12 = QuantumStack 12 True [l2] (StackData [("Nil", [])])

ldleft = QuantumStack 13 True [l12] (StackData [("Cons", [2,12])])

r1 = QuantumStack 1 True [p252] (StackClassical [Left 2])
r2 = QuantumStack 2 True [r1] (StackClassical [Left 1])
r12 = QuantumStack 12 True [r2] (StackData [("Nil", [])])
r13 = QuantumStack 13 True [r12] (StackData [("Cons", [2,12])])
ldright = QuantumStack 14 True [r13] (StackData [("Cons", [1,13])])

nsright ::NameSupply
nsright = ([],15)

mmleft :: MemoryMap
mmleft = [Map.empty, Map.singleton "@outslis" 13, Map.empty, Map.empty]

mmright :: MemoryMap
mmright  = [Map.singleton "@outslis" 14, Map.empty, Map.empty]

olft ::  QuantumStack  LazyNum -- 12,nil -> 11,Tails  -> 1(2) -> .25
orgt ::  QuantumStack  LazyNum -- 13,cons(2,12) -> 12,Nil -> 11,Heads -> 2(1) -> 1(2) -> .25

ol1  ::  QuantumStack  LazyNum
ol11  ::  QuantumStack  LazyNum

or1  ::  QuantumStack  LazyNum
or2  ::  QuantumStack  LazyNum

or11  ::  QuantumStack  LazyNum
or12  ::  QuantumStack  LazyNum

ol1 = QuantumStack 1 True [p251] (StackClassical [Left 2])

ol11 = QuantumStack 11 True [ol1] (StackData [("Tails",[])])

olft = QuantumStack 12 True [ol11]  (StackData [("Nil", [])])

or1 =  QuantumStack 1 True [p252] (StackClassical [Left 2])

or2 = QuantumStack 2 True [or1] (StackClassical [Left 1])
or11 = QuantumStack 11 True [or2] (StackData [("Heads",[])])

or12 = QuantumStack 12 True [or11]  (StackData [("Nil", [])])

orgt = QuantumStack 13 True [or12] (StackData [("Cons", [2,12])])

olmm :: MemoryMap
olmm = [Map.fromList [("@bt",11), ("@otl",12)], Map.empty, Map.empty, Map.singleton "@hd" 1, Map.empty, Map.empty, Map.empty, Map.empty]


ormm :: MemoryMap
ormm = [Map.fromList [("@bt",11), ("@otl",13)], Map.empty, Map.empty, Map.singleton "@hd" 1, Map.empty, Map.empty, Map.empty, Map.empty]

ons :: NameSupply
ons = ([],14)




plft ::  QuantumStack  LazyNum -- 15->14,12,2,1
prgt ::  QuantumStack  LazyNum -- 15->17,12,2,16 

pl1a  ::  QuantumStack  LazyNum
pl1b  ::  QuantumStack  LazyNum
pl2  ::  QuantumStack  LazyNum
pl12  ::  QuantumStack  LazyNum
pl14  ::  QuantumStack  LazyNum

pr16a  ::  QuantumStack  LazyNum
pr16b  ::  QuantumStack  LazyNum
pr2  ::  QuantumStack  LazyNum

pr12  ::  QuantumStack  LazyNum
pr17  ::  QuantumStack  LazyNum

pl1a = QuantumStack 1 True [p251] (StackClassical [Left 2])
pl1b = QuantumStack 1 True [p252] (StackClassical [Left 2])

pl2 = QuantumStack 2 True [pl1a] (StackClassical [Left 2])
pl12 = QuantumStack 12 True [pl2]  (StackData [("Nil", [])])
pl14 = QuantumStack 14 True [pl12,pl1b]  (StackData [("Cons",[2,12]),("Nil", [])])

plft = QuantumStack 15 True [pl14]  (StackData [("Cons", [1,14])])

pr16a =  QuantumStack 16 True [p253] (StackClassical [Left 2])
pr16b =  QuantumStack 16 True [p254] (StackClassical [Left 1])

pr2 = QuantumStack 2 True [pr16a] (StackClassical [Left 1])
pr12 = QuantumStack 12 True [pr2]  (StackData [("Nil", [])])
pr17 = QuantumStack 17 True [pr12,pr16b]  (StackData [("Cons",[2,12]),("Nil", [])])

prgt = QuantumStack 15 True [pr17] (StackData [("Cons", [16,17])])

prmm :: MemoryMap
prmm = [Map.singleton "@otl" 15, Map.singleton "@otl" 15, Map.empty, Map.empty, Map.empty, Map.empty]


plmm :: MemoryMap
plmm = [Map.singleton "@otl" 15, Map.empty, Map.empty, Map.empty]

pns :: NameSupply
pns = ([],18)




\end{code}

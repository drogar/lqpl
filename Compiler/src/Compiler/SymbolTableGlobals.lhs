\begin{code}
module Compiler.SymbolTableGlobals (
                          addGlobalTransforms
                          ) where
import Data.Map as Map
import Data.List as List

import Control.Monad.Writer

import Compiler.BaseTypes
import Compiler.Qtypes
import Compiler.SemTypes


seTransforms = [ SeFun "Not" "" (Just NotGate) (([],[]),([],[]))
                           [] [QUBIT] [QUBIT] [],
                 SeFun "T" "" (Just Tgate) (([],[]),([],[]))
                            [] [QUBIT] [QUBIT] [],
                 SeFun "Phase" "" (Just Phase) (([],[]),([],[]))
                            [] [QUBIT] [QUBIT] [],
                 SeFun "Had" "" (Just Hadamard) (([],[]),([],[]))
                            [] [QUBIT] [QUBIT] [],
                 SeFun "MinusI" "" (Just MinusI) (([],[]),([],[]))
                            [] [QUBIT] [QUBIT] [],
                 SeFun "RhoX" "" (Just RhoX) (([],[]),([],[]))
                            [] [QUBIT] [QUBIT] [],
                 SeFun "RhoY" "" (Just RhoY) (([],[]),([],[]))
                            [] [QUBIT] [QUBIT] [],
                 SeFun "RhoZ" "" (Just RhoZ) (([],[]),([],[]))
                            [] [QUBIT] [QUBIT] [],
                 SeFun "Swap" "" (Just Swap) (([],[]),([],[]))
                            []  [DeclaredType "List" [QUBIT]] 
                           [DeclaredType "List" [QUBIT]] [],
--[QUBIT,QUBIT] [QUBIT,QUBIT] [],
                 SeFun "Toffoli3" "" (Just Toffoli) (([],[]),([],[]))
                            [] 
                           [QUBIT,QUBIT,QUBIT] [QUBIT,QUBIT,QUBIT] [],
                 SeFun "Rot" "" (Just Rotate) (([],[]),([],[]))
                             [INT] [QUBIT] 
                           [QUBIT] [],
                 SeFun "UM" "" (Just UM) (([],[]),([],[]))
                             [INT,INT,INT] [DeclaredType "List" [QUBIT]] 
                           [DeclaredType "List" [QUBIT]] [],

                 SeFun "CNot" "" (Just (Controlled NotGate)) (([],[]),([],[]))
                       []
                           [QUBIT,QUBIT] [QUBIT,QUBIT] [] ]
 {- ,                 SeFun "CT" "" (Just (Controlled Tgate)) [] 
                           [QUBIT,QUBIT] [QUBIT,QUBIT],
                 SeFun "CPhase" "" (Just (Controlled Phase)) [] 
                           [QUBIT,QUBIT] [QUBIT,QUBIT],
                 SeFun "CHad" "" (Just (Controlled Hadamard)) [] 
                           [QUBIT,QUBIT] [QUBIT,QUBIT],
                 SeFun "CRhoX" "" (Just (Controlled RhoX)) [] 
                           [QUBIT,QUBIT] [QUBIT,QUBIT],
                 SeFun "CRhoY" "" (Just (Controlled RhoY)) [] 
                           [QUBIT,QUBIT] [QUBIT,QUBIT],
                 SeFun "CRhoZ" "" (Just (Controlled RhoZ)) [] 
                           [QUBIT,QUBIT] [QUBIT,QUBIT],
                 SeFun "CSwap" "" (Just (Controlled Swap)) [] 
                           [QUBIT,QUBIT,QUBIT] [QUBIT,QUBIT,QUBIT],
                 SeFun "CToffoli3" "" (Just (Controlled Toffoli)) [] 
                           [QUBIT,QUBIT,QUBIT,QUBIT] [QUBIT,QUBIT,QUBIT,QUBIT],
                 SeFun "CRot" "" (Just (Controlled Rotate)) [] 
                           [INT,QUBIT,QUBIT,QUBIT] [QUBIT,QUBIT,QUBIT]]

   Need way to generate the Controlled and Inverse version on the fly from
   the base ones.
-}

globalST = Map.fromList $ List.map (\ s -> (gname s, s)) seTransforms

addGlobalTransforms :: WriterT CompilerLogs SemStateMonad()
addGlobalTransforms = setSymTabGlobal globalST

\end{code}

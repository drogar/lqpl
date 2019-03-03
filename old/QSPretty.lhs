\begin{code}

module Lqpl.QSM.QSPretty where
import Lqpl.Data.ClassComp
import Lqpl.Data.ClassicalData
import Lqpl.Data.LazyNum
import Data.Map as Map
import Data.List as List
import Lqpl.QSM.QuantumStack.QuantumStack
import Lqpl.Data.Basis

import Simulator.SimBase

ltxPretty :: QuantumStack  LazyNum -> String
ltxPretty StackZero = "0"
ltxPretty (StackData d ) = show d
ltxPretty (StackInt nm st) = "INT("++nm++")"
ltxPretty (StackCons nm st) = "CONS("++nm++")"
ltxPretty (StackQubit nm sb)
          = "\\qsqubit{"++nm++"}{"
               ++ ltxPretty (val (Zero,Zero) sb) ++ "}{"
               ++ ltxPretty (val (Zero,One) sb) ++ "}{"
               ++ ltxPretty (val (One,Zero) sb) ++ "}{"
               ++ ltxPretty (val (One,One) sb) ++ "}{}"

\end{code}

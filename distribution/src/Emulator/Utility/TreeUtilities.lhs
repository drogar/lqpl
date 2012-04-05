\begin{code}
module Emulator.Utility.TreeUtilities (
                                       approxWidthTree,
                                       txtMove,
                                       trMove
)
         where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

import QSM.BasicData

import Emulator.Data.Preferences




approxWidthTree ::(Quantum b)=>
                  QsWindowPrefs ->
                  Int-> 
                  Maybe(QuantumStack b) -> Int
approxWidthTree qsprefs 0 _ = nodeSeparation qsprefs
approxWidthTree _ _ Nothing = 0
approxWidthTree qsprefs mdp (Just q) = awt qsprefs mdp q

awt::(Quantum b)=>
      QsWindowPrefs ->
      Int-> 
      QuantumStack b -> Int
awt qsprefs mdp q
    | isStackZero q        =  nodeSize qsprefs
    | isStackValue q       =  nodeSeparation qsprefs + 50
    | otherwise            =  nodeSeparation qsprefs * 2 + 
                              sum [awt qsprefs (mdp - 1) q' | q' <- subStacks q]



\end{code}

\begin{code}
module Emulator.Dialogs.SimulateResults (showSimulateResultsDialog) where

import Graphics.UI.Gtk


import Data.Computation.BaseType
import QSM.Simulate

import Data.Map as Map
import Data.IORef
import Data.Maybe (isJust, fromJust)
import Control.Monad (when)

import Emulator.Data.GUI





showSimulateResultsDialog :: PrimeGUI ->
                  QuantumStack  BaseType ->
                  IO ()
showSimulateResultsDialog primeGui  qstack
    = do (f,vals) <- chooseIt (canonicalize qstack)
         let simDialog = simulateDlg primeGui
         windowSetTransientFor simDialog (mainWindow primeGui)
         randomValText primeGui `labelSetText` show f
         subtab <- setUpTable (resultsTable primeGui) vals
         widgetShowAll simDialog
         resp <- dialogRun simDialog
         widgetHideAll simDialog
         case resp of
           ResponseClose -> do widgetDestroy subtab
                               return ()
           _ -> do widgetDestroy subtab
                   widgetQueueDraw simDialog
                   showSimulateResultsDialog primeGui qstack
         

setUpTable ::Table -> [(String,String,String)]->IO Table
setUpTable tab vals
    = do subtab <- tableNew (1 + length vals) 3 False
         tableAttach tab subtab 0 2 2 3 [Expand, Fill] [Expand,Fill] 0 0
         attachAll 0  subtab (("Name", "Type", "Value"):vals)
         return subtab

attachAll :: Int-> Table -> [(String,String,String)]->IO()
attachAll _ tab [] = return ()
attachAll n tab ((nm,typ,val):vals)
    = do nml <- labelNew (Just nm)
         typl <- labelNew (Just typ)
         vall <- labelNew (Just val)
         let aops = [Expand, Fill]
         tableAttach tab nml 0 1 n (n+1) aops aops 2 0
         tableAttach tab typl 1 2 n (n+1) aops aops 2 0
         tableAttach tab vall 2 3 n (n+1) aops aops 2 0
         attachAll (n+1) tab vals
\end{code}

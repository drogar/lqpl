\begin{code}
module Emulator.Dialogs.VersionMismatch where

import Control.Monad (when)

import Graphics.UI.Gtk


import Data.Map as Map
import Data.IORef
import Data.Maybe (isJust, fromJust)
import Data.Version

import Text.ParserCombinators.ReadP


import Assembler.AssemParser

import QSM.QSM

import Emulator.Data.GUI

import Utility.Extras 

import Paths_lqpl



condshowVersionMismatchDialog ::  PrimeGUI ->
                                  [String] ->
                                  IO (Bool)
condshowVersionMismatchDialog primeGui []
    = showVmDialog primeGui (Version [0, 0, 0] ["Unknown"])
condshowVersionMismatchDialog primeGui (versstring:_)
     = do print versstring
          print $ words versstring
          csvmdOnWords primeGui (words versstring)

csvmdOnWords ::  PrimeGUI -> [String] -> IO (Bool)
csvmdOnWords primeGui []
    = showVmDialog primeGui (Version [0, 0, 0] ["Unknown"])
csvmdOnWords primeGui (cv:cvrest)
    = do print cv
         case splitAt 8 cv of 
            ("Version=",ver)   -> let  cversion = (fst . last) (readP_to_S parseVersion ver)
                                   in   if cversion == version then return True
                                        else showVmDialog primeGui cversion
            _                   -> csvmdOnWords primeGui cvrest




showVmDialog :: PrimeGUI -> Version ->
                IO (Bool)
showVmDialog primeGui cversion
    = do  let  vmDialog = versionWarnDlg primeGui
          windowSetTransientFor vmDialog (mainWindow primeGui)
          let  cvText = compilerVersion primeGui
               svText = emulatorVersion primeGui
          cvText `labelSetText` showVersion cversion
          svText `labelSetText` showVersion version
          widgetShowAll vmDialog
          resp <- dialogRun vmDialog
          widgetHideAll vmDialog
          case  resp of
                ResponseOk -> return True           
                _ -> return False
         

\end{code}

\begin{code}
module Emulator.Dialogs.FileDialogs 
    ( openQPOFileDialog,
      openQPLFileDialog,
      readAssemblyCode,
      readSourceCode,
      addToRecent,
      updateDefaultFolder,
      updateFileMenuWithMRUs)
  where

import System.FilePath 

import Graphics.UI.Gtk

import Control.Monad

import Data.IORef
import Data.Maybe


import Assembler.AssemParser

import Emulator.Configuration.Configuration
import Emulator.Data.GUI
import Emulator.Data.Preferences
import Emulator.Dialogs.VersionMismatch

import Emulator.Utility.Logging



import QSM.BasicData
import QSM.QSM

readSourceCode :: (FilePath -> Bool -> IO()) ->  Maybe String -> IO()
readSourceCode  _ Nothing = return ()
readSourceCode  updateSourceWindow (Just fn) = updateSourceWindow fn False



readAssemblyCode :: (Int -> Memory Basis -> IO()) -> PrimeGUI-> Maybe String -> IO()
readAssemblyCode _ _ Nothing = return ()
readAssemblyCode startMeUp primeGui (Just fn) = 
    do  inss <- readFile fn
        parseassem <- parseQPA "" "" inss
       
        case  parseassem of
              Left error -> 
                  do  md <- messageDialogNew Nothing [] MessageOther 
                            ButtonsClose error
                      dialogRun md
                      widgetDestroy md
              Right ((cnotes,trs),mb) -> 
                  do   cont <- condshowVersionMismatchDialog primeGui cnotes 
                       mainWindow primeGui `windowSetTitle` baseTitle
                       when cont $
                            do  mainWindow primeGui `windowSetTitle` (baseTitle ++ " - " ++ 
                                                           (snd . splitFileName) fn)
                                qsp <- qsPreferences
                                prefs <- readIORef qsp
	                        startMeUp (callDepth prefs) mb
                                elognoRef prefs logLevelTrace "Loading - did noStep"




assemblyCodeFilter :: IO FileFilter
assemblyCodeFilter =
    do  qpoFilter <- fileFilterNew
        qpoFilter `fileFilterAddPattern` "*.qpo"
        qpoFilter `fileFilterSetName` "LQPL assembly files (*.qpo)"
        return qpoFilter


sourceCodeFilter :: IO FileFilter
sourceCodeFilter =
    do  qplFilter <- fileFilterNew
        qplFilter `fileFilterAddPattern` "*.qpl"
        qplFilter `fileFilterSetName` "LQPL source files (*.qpl)"
        return qplFilter


openQPOFileDialog :: Window -> IORef (Maybe FilePath) ->
                     IO (Maybe String)
openQPOFileDialog = openFileDialog "Open QPO (Assembled) file..." assemblyCodeFilter

openQPLFileDialog :: Window -> IORef (Maybe FilePath) ->
                     IO (Maybe String)
openQPLFileDialog = openFileDialog "Open LQPL (Source) file..." sourceCodeFilter



openFileDialog :: String -> IO FileFilter -> Window -> 
                  IORef (Maybe FilePath) ->
                  IO (Maybe String)
openFileDialog title whichType parentWindow ioRefdefaultFolder= do
  dialog <- fileChooserDialogNew
              (Just title)
              (Just parentWindow)
	      FileChooserActionOpen
	      [("gtk-cancel", ResponseCancel)
	      ,("gtk-open", ResponseAccept)]
  fldr <- readIORef ioRefdefaultFolder
  when (isJust fldr) (do dialog `fileChooserSetCurrentFolder` fromJust fldr
                         return ())
  filter <- whichType
  dialog `fileChooserAddFilter` filter
  widgetShow dialog
  response <- dialogRun dialog
  deffld <- fileChooserGetCurrentFolder dialog
  writeIORef  ioRefdefaultFolder deffld
  widgetHide dialog
  case response of
      ResponseAccept -> fileChooserGetFilename dialog
      _ -> return Nothing


updateDefaultFolder :: Maybe FilePath -> 
                       IORef (Maybe FilePath) ->
                       IO()
updateDefaultFolder Nothing _ = return ()
updateDefaultFolder a ior  = writeIORef ior a

updateFileMenuWithMRUs :: [String] -> 
                          (Int -> Memory Basis -> IO ()) -> 
                          (String->Bool->IO()) ->
                          PrimeGUI -> IO()
updateFileMenuWithMRUs [] _ _ _ = return ()
updateFileMenuWithMRUs (a:rest) startup scdread primeGui 
    = do  case takeExtension a of
              ".qpl"  -> addToRecent False (rsfMI primeGui) (Just a) (readSourceCode scdread)
              ".qpo"  -> addToRecent False (rofMI primeGui) (Just a) (readAssemblyCode startup primeGui)
          updateFileMenuWithMRUs rest startup scdread primeGui 

addToRecent :: Bool -> MenuItem -> Maybe String-> (Maybe String -> IO()) -> IO()
addToRecent _ _ Nothing _  = return ()
addToRecent updMRU recentFilesMenu (Just fn) readIt 
    = do item1 <- menuItemNewWithLabel fn
         smenu <- menuItemGetSubmenu recentFilesMenu
         item1  `afterActivateLeaf` readIt (Just fn)
         when updMRU $ updateMRUList fn
         case smenu of 
            Nothing  -> return ()
            Just sm   ->
                do let submenu = castToMenuShell sm
                   submenu `menuShellAppend` item1
                   widgetShowAll item1


\end{code}

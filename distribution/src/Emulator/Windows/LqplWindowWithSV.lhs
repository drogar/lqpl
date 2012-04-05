\begin{code}
module Emulator.Windows.LqplWindow(
                                   LqplWindowUpdate,
                                   lqplWindowSetup) where

import Graphics.UI.Gtk

--import Graphics.UI.Gtk.SourceView

import Graphics.UI.Gtk.Gdk.Events

import Control.Monad(when)
import Control.Monad.Writer


import Data.Map as Map
import Data.List as List
import Data.IORef

import System.FilePath(splitFileName)

import Emulator.Utility.GraphicUtilities(removePages)
import Emulator.Utility.WindowOperations(windowShowHide,windowForceShow)
import Data.Computation.BaseType(BaseType)
import Emulator.Data.GUI
import Emulator.Dialogs.CompilerPreferences
import QSM.QSM

import Utility.Extras(splitFilePath)

import Compiler.Compiler


type LqplWindowUpdate = FilePath -> Bool -> IO ()


lqplWindowSetup :: CompilerPrefDialogGUI -> LQPLWindowGUI -> IO (LqplWindowUpdate )
lqplWindowSetup  cpGui (LQPLWindowGUI subGui compBtn) =
     do   let  lqplWindow = tdWindow subGui
               Right lqplTabs = dataElts subGui
               showHideLqplMI = showHideMI subGui
          winPos <- newIORef (0,0)
          
          showHideLqplMI `afterActivateLeaf` do
                     lqplTabs `notebookSetCurrentPage` 0
                     windowShowHide winPos showHideLqplMI lqplWindow
                     lqplTabs `notebookSetCurrentPage` 0
          lqplWindow `onDelete` \_ -> do
                     lqplTabs `notebookSetCurrentPage` 0
                     windowShowHide winPos showHideLqplMI lqplWindow
                     lqplTabs `notebookSetCurrentPage` 0
                     return True
          lqplWindow `windowSetSkipPagerHint` True
          lqplWindow `windowSetSkipTaskbarHint` True
          windowSetDefaultSize lqplWindow 400 500
          widgetHideAll lqplWindow
          return (updateTabs (windowForceShow winPos showHideLqplMI lqplWindow) cpGui compBtn lqplTabs)




updateTabs :: IO() -> CompilerPrefDialogGUI -> Button -> Notebook -> FilePath -> 
              Bool -> IO()
updateTabs  showwin cpGui cbtn nb file reset =
    do showwin
       setTabData nb file reset
       nb `notebookSetCurrentPage` 0
       cbtn `afterClicked`  compileTheFile cpGui file
       return ()

compileTheFile :: CompilerPrefDialogGUI -> FilePath -> IO ()
compileTheFile cpGui fp = do 
  (flgs, ldcd) <- cpDialogToOptions cpGui
  res <- execWriterT (doCompile False flgs (splitFilePath fp))
  md <- dialogNew 
  md `windowSetTitle` (last res)
  windowSetDefaultSize md 800 600
  dialogAddButton md "gtk-close" ResponseClose
  vbox <- dialogGetUpper md
  swin <- scrolledWindowNew Nothing Nothing
  boxPackStart vbox swin PackGrow 5
  buff <- textBufferNew Nothing
  buff `textBufferSetText` (concat $ intersperse "\n" res)
  tv <- textViewNewWithBuffer buff
  tv `textViewSetEditable` False
  tv `textViewSetCursorVisible` False
  tv `textViewSetWrapMode` WrapWord
  swin `scrolledWindowAddWithViewport` tv
  widgetShow tv
  widgetShow swin
  dialogRun md
  widgetDestroy md
       


setTabData :: Notebook -> FilePath -> Bool -> IO()
setTabData  nb file  reset = 
    do  when reset $  removePages nb
        fdata <- readFile file
        let (_,fil) = splitFileName file
        addPage nb fil fdata
        widgetShowAll nb



addPage :: Notebook -> FilePath -> String -> IO ()
addPage nb file lqpl
    = do  {-lm <- sourceLanguageManagerNew
          langM <- sourceLanguageManagerGuessLanguage lm Nothing (Just "text/x-haskell")
          lang <- case langM of
               (Just lang) -> return lang
               Nothing -> do
                  langDirs <- sourceLanguageManagerGetSearchPath lm
                  error ("please copy lqpl.lang to one of the following directories:\n" 
                  	     ++unlines langDirs)

          -- create a new SourceBuffer object
          tb <- sourceBufferNewWithLanguage lang -}
          tb <- textBufferNew Nothing
          tb `textBufferSetText` lqpl
          textBufferSetModified tb False

          --sourceBufferSetHighlightSyntax tb True
          --sourceBufferSetHighlightMatchingBrackets tb True

          -- create a new SourceView Widget
          --sv <- sourceViewNewWithBuffer tb
          tv <- textViewNewWithBuffer tb
          sw <- scrolledWindowNew Nothing Nothing
          scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
          sw `containerAdd` tv --sv
          notebookAppendPage nb sw file
          return ()




\end{code}

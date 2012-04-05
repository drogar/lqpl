\begin{code}
module Emulator.Windows.InstructionWindow(
                                           InstructionWindowUpdate,
                                           instructionWindowSetup) where

import Graphics.UI.Gtk

import Graphics.UI.Gtk.Gdk.Events

import Control.Monad(when)

import Data.Map as Map
import Data.List as List
import Data.IORef
import Text.Printf
import Emulator.Utility.GraphicUtilities(removePages)
import Emulator.Utility.WindowOperations(windowShowHide,windowForceShow)
import Data.Computation.BaseType(BaseType)
import Emulator.Data.GUI
import QSM.QSM


type InstructionWindowUpdate = (IORef (MachineState  BaseType)) ->
                                           SpinButton -> Bool ->
                                           IO ()


instructionWindowSetup :: SubDisplayGUI -> IO (InstructionWindowUpdate )
instructionWindowSetup  subGui
    = do  let  instructionWindow = tdWindow subGui
               Right instructionTabs = dataElts subGui
               showHideInstructionMI = showHideMI subGui
          winPos <- newIORef (0,0)
          showHideInstructionMI `onActivateLeaf` do
                     instructionTabs `notebookSetCurrentPage` 0
                     windowShowHide winPos showHideInstructionMI instructionWindow
                     instructionTabs `notebookSetCurrentPage` 0
          instructionWindow `onDelete` \_ -> do
                     instructionTabs `notebookSetCurrentPage` 0
                     windowShowHide winPos showHideInstructionMI instructionWindow
                     instructionTabs `notebookSetCurrentPage` 0
                     return True
          instructionWindow `windowSetSkipPagerHint` True
          instructionWindow `windowSetSkipTaskbarHint` True
          widgetHideAll instructionWindow
          return (updateInsTabs (windowForceShow winPos showHideInstructionMI instructionWindow)instructionTabs)




updateInsTabs :: IO() -> Notebook -> 
                 (IORef (MachineState BaseType)) ->
                 SpinButton ->
                 Bool ->
                 IO ()
updateInsTabs showWin nb ms sb reset =
    do cmstate <- readIORef ms
       sdepth <- spinButtonGetValueAsInt sb
       let current = snd $ hd $ dropI sdepth cmstate
           cm = cmsCodeMem current
           rcode = cmsRunningCode current
           ip = snd $ cmsInstructionPointer current
           epname = fst $ cmsInstructionPointer current
       -- print $ "About to set tabs data, reset=" ++ (show reset)
       setTabData nb cm rcode reset ip epname
       when (length rcode > 0) $ showWin
       nb `notebookSetCurrentPage` 0

setTabData :: Notebook -> 
              Memory Basis -> 
              [Instruction Basis] -> 
              Bool ->
              Int->
              String ->
              IO()
setTabData  nb cm current reset ip epname= 
    if reset
    then do -- print "Removing pages"
            removePages nb
            -- print $ "Adding pages "++(show $ Map.size cm) ++" Pages."
            addPages nb $ ("(C)"++epname,current): Map.toList cm
            -- print "showing all widgets in the nb" 
            widgetShowAll nb
    else resetCurrentPage nb ip current epname


resetCurrentPage :: Notebook -> 
                    Int ->
                    [Instruction Basis] -> 
                    String ->
                    IO ()
resetCurrentPage  nb ip ins epname
    = do tv <- setPageData ins ip
         -- print "rcp, Creating a new scrolled window"
         sw <- scrolledWindowNew Nothing Nothing
         -- print "rcp, setting SW policy to automatic"
         scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
         -- print "rcp, Adding tree view (page data) to sw"
         sw `containerAdd` tv
         notebookRemovePage nb 0
         notebookInsertPage nb sw ("(C)"++epname) 0
         widgetShow tv
         widgetShow sw
         widgetShow nb
         return ()


addPages :: Notebook -> 
            [(String, [Instruction Basis])] -> 
            IO ()
addPages nb [] = return ()
addPages nb ((label,ins):code) =
    do -- print $ " Adding the page " ++ label
       addSinglePage nb label ins
       -- print "looping add pages"
       addPages nb code


addSinglePage :: Notebook -> 
                 String -> 
                 [Instruction Basis] -> 
                 IO ()
addSinglePage nb label ins 
    = do -- print "ASP, setting page data"
         tv <- setPageData  ins 0
         -- print "ASP, Creating a new scrolled window"
         sw <- scrolledWindowNew Nothing Nothing
         -- print "ASP, setting SW policy to automatic"
         scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
         -- print "ASP, Adding tree view (page data) to sw"
         sw `containerAdd` tv
         -- print $ "ASP, Appending page " ++ label
         notebookAppendPage nb sw label
         -- print "ASP, done, returnin"
         return ()


setPageData :: [Instruction Basis] -> Int-> IO TextView
setPageData ins ip
    = do txtv <- textViewNew
         tb <- textViewGetBuffer txtv
         tb `textBufferSetText` concat ( zipWith (printf " %3d  %s\n") [ip..] (List.map show ins))
         return txtv






\end{code}

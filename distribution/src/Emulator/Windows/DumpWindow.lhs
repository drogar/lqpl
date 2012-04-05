\begin{code}
module Emulator.Windows.DumpWindow(dumpWindowSetup)  where

import Graphics.UI.Gtk

import Graphics.UI.Gtk.Gdk.Events

import Data.Computation.BaseType
import Emulator.Data.GUI
import Emulator.Utility.WindowOperations

import Data.Map as Map
import Data.List as List
import Data.IORef

import Text.Printf
import QSM.Components.MemoryMap
import QSM.QSM


dumpWindowSetup :: SubDisplayGUI -> IO ( (IORef (MachineState  BaseType)) ->
                                 SpinButton ->
                                 IO ())
dumpWindowSetup  dumpGui
    = do let  dumpWindow = tdWindow dumpGui
              Left dumpTextView = dataElts dumpGui
              showHideDumpMI = showHideMI dumpGui
         wpos <- newIORef (0,0)
         showHideDumpMI `onActivateLeaf` windowShowHide wpos showHideDumpMI dumpWindow
         textDisplayWindowSetup dumpWindow wpos dumpTextView showHideDumpMI
         widgetHideAll dumpWindow
         return (dumpWindowUpdate dumpTextView)



dumpWindowUpdate :: TextView ->
                    (IORef (MachineState  BaseType)) ->
                    SpinButton ->
                    IO ()
dumpWindowUpdate   tv ms sdSpin =
    do mstate <- readIORef ms
       depth <- spinButtonGetValueAsInt sdSpin 
       tvbuffer <- textViewGetBuffer tv
       tvbuffer `textBufferSetText`  showDumpTop ( dump $  pickIthMS depth mstate)
       tv `textViewSetBuffer` tvbuffer






\end{code}

\begin{code}
module Emulator.Windows.MemoryMapWindow(mmapWindowSetup)  where

import Graphics.UI.Gtk

import Graphics.UI.Gtk.Gdk.Events


import Data.Map as Map
import Data.List as List
import Data.IORef

import Text.Printf
import QSM.Components.MemoryMap
import QSM.QSM

import Data.Computation.BaseType
import Emulator.Utility.WindowOperations
import Emulator.Data.GUI

mmapWindowSetup :: SubDisplayGUI -> IO ( (IORef (MachineState  BaseType)) ->
                                 IO ())
mmapWindowSetup  mmg
    = do let  mmapW = tdWindow mmg
              Left dataEltTV = dataElts mmg
              showHideMMap = showHideMI mmg
         wpos <- newIORef (0,0)
         showHideMMap  `onActivateLeaf` windowShowHide wpos showHideMMap  mmapW
         textDisplayWindowSetup  mmapW wpos dataEltTV showHideMMap
         widgetHideAll mmapW
         return (mmapWindowUpdate dataEltTV )

mmapWindowUpdate :: TextView ->
                    (IORef (MachineState  BaseType)) ->
                    IO ()
mmapWindowUpdate  tv  ms =
    do mstate <- readIORef ms
       tvbuffer <- textViewGetBuffer tv
       tvbuffer `textBufferSetText`  mmshow ( stackTranslation $  pickIthMS  0 mstate)
       tv `textViewSetBuffer` tvbuffer


\end{code}

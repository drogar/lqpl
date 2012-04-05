\subsubsection{Window and drawing to show the |Qstack|}\label{subsubsec:qswindow}
\begin{code}
module Emulator.Drawing.QsWindow where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC

import System.Glib.Types

import Control.Monad

import Data.Map as Map
import Data.List as List
import Data.IORef


import Data.ClassComp

import Data.Computation.BaseType

import Emulator.Data.Preferences
import Emulator.Data.GUI

import Emulator.Drawing.QstackToNodes

import Emulator.Utility.Logging
import Emulator.Utility.GraphicUtilities(intersectRect)
import Emulator.Utility.Timings
import Emulator.Utility.NodesAndLines


import QSM.QSM



     
\end{code}
                           
dotsize=7
trXoffset = 10
trYoffset = -10
ndX = 20 
ndY = 20
ndSep = 15
mdoffset=15
txty=(-2)
txtx=(-2)
clientsize = 500
lblFontsize = 8
stdFontsize = 10
trcFontsize = 10
dddFontsize = 8

\begin{code}


exposeQstack ::  IORef (MachineState BaseType) ->
              PrimeGUI ->
              IORef(QsWindowPrefs) ->
              Rectangle ->
              IO () --Bool
exposeQstack msRef primeGui  iorPref rect -- (Expose {eventArea=rect})  
    = do let (Rectangle xdr ydr wdr hdr) = rect
         elog iorPref logLevelTrace  $ "Expose qswindow " ++ show (xdr, ydr, wdr, hdr)
         dw <- widgetGetDrawWindow (stackDrawArea primeGui)
         gc <- gcNew dw 

         printClock  "Exposing display at: "
               
         timeCheckWithMessage "This expose used " $
            do (w,h) <- drawQstack  iorPref msRef dw (streamDepthSpin primeGui) (treeDepthSpin primeGui) rect
               widgetSetSizeRequest (stackDrawArea primeGui) (w + 2) (h + 2)
         printClock "Finished expose of display at: "
         return () --True

drawQstack :: (DrawableClass d,  DrawWindowClass d) =>
              IORef(QsWindowPrefs) ->
              IORef (MachineState BaseType) ->
              d ->
              SpinButton ->
              SpinButton ->
              Rectangle ->
              IO (Int,Int)
drawQstack  iorPrefs msRef dw sdSpin tdSpin rect
    = do print "Drawing qstack"
         qsPref <- readIORef iorPrefs
         elognoRef qsPref  logLevelTrace "Getting new gc"
         gc <- gcNew dw
         elognoRef qsPref  logLevelTrace "Setting values in  gc"
         gcSetValues gc $ newGCValues {
                                     foreground = Color 0 0 0,
                                     background = Color 0 0 65535,
                                     capStyle = CapRound,
                                     lineWidth = 1,
                                     joinStyle = JoinRound}
         elognoRef qsPref  logLevelTrace "Getting machine state reference"
         mstate <- readIORef msRef
         elognoRef qsPref  logLevelTrace "Getting tree depth and stack depth spins"
         td <- spinButtonGetValueAsInt tdSpin
         sd <- spinButtonGetValueAsInt sdSpin
         elognoRef qsPref  logLevelTrace $ "td is " ++ show td ++ " sd is " ++ show sd
         elognoRef qsPref  logLevelTrace "Getting sd quantum stack - no trim."
         let bms =  pickIthMS  sd mstate
             qs = quantumStack bms
             mmap = stackTranslation bms
             showTr = showTrace qsPref             
         elognoRef qsPref  logLevelDebug $ "The graph:: " ++ show qs ++ "\n\n"
         elognoRef qsPref  logLevelTrace "Making the nodes and lines for the graph"
         graph <- makeNodesAndLines 1 td Nothing qsPref qs mmap
         elognoRef qsPref  logLevelTrace "Computing the size of the graph"
         let ((_,startx), graph') = setX (showTr,0) graph
             (w,h) = getFullSize showTr graph'
         elognoRef qsPref  logLevelTrace  $ "Starting left size is " ++ show startx
         elognoRef qsPref  logLevelTrace  $ "Top conn pt is "++ show (connectionPt $ node graph')
         elognoRef qsPref  logLevelTrace  $ "Tree is " ++ show qs
         elognoRef qsPref  logLevelTrace  $ "Tree depth spin is " ++ show td
         elognoRef qsPref  logLevelTrace  $ "Size returned is " ++ show w ++ ", "++show h
         let ns = nodeSeparation qsPref
             w' = w+ns
             h' = h + ns
             fullrect = Rectangle 0 0 w' h'
             dregion = intersectRect fullrect rect
             (Rectangle xdr ydr wdr hdr) = dregion
         elognoRef qsPref  logLevelTrace  $ "Region for dwindow is" ++ show (xdr, ydr, wdr, hdr)
         dw `drawWindowBeginPaintRect` dregion

         drawWindowClear dw
         drawIt dw gc showTr  graph' 
         --blackRect dw gc ((0,0),(w',h'))
         drawWindowEndPaint dw
         
         return (w',h')


\end{code}

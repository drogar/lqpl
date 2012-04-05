\begin{code}
module Emulator.Utility.WindowOperations (
                          showAboutDialog,
                          textDisplayWindowSetup,
                          windowShowHide,
                          windowForceShow,
                          windowForceHide
)
         where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

import Control.Monad(unless)

import Data.IORef
import Data.Version

import Emulator.Data.GUI

import Utility.Extras

import Paths_lqpl

showAboutDialog :: PrimeGUI -> IO ()

showAboutDialog primeGui
    = do  let  parent   = mainWindow primeGui
               aboutDialog = aboutDlg primeGui
          windowSetTransientFor aboutDialog parent
          aboutDialog `aboutDialogSetVersion` showVersion version
-- make the dialog non-modal. When the user closes the dialog hide it.
          aboutDialog `afterResponse` (\_ -> widgetHideAll aboutDialog)
          widgetShowAll aboutDialog


textDisplayWindowSetup ::   Window -> 
                            IORef (Int,Int) ->
                            TextView ->
                            MenuItem ->
                            IO ()
textDisplayWindowSetup  w iorPos tv mi =
    do widgetShowAll w
       tvbuffer <- textViewGetBuffer tv
       tvbuffer `textBufferSetText` ""
       tv `textViewSetBuffer` tvbuffer
       w `onDelete` \_ -> do
             windowShowHide iorPos mi w 
             return True
       return ()


windowShowHide :: IORef (Int,Int) -> MenuItem -> Window -> IO()
windowShowHide iorPos mi w =
    do  mlbl <- binGetChild mi
        case mlbl of
          Nothing  -> widgetShowAll w
          Just lblo  -> 
              do  let lbl = castToLabel lblo
                  txt <- labelGetText lbl
                  if qHead "windowShowHide" txt == 'S' 
                    then do widgetShowAll w
                            (top,left) <- readIORef iorPos
                            windowMove w top left
                            lbl `labelSetText` ("Hide"++drop 4 txt)
                    else do (top,left) <- windowGetPosition w
                            writeIORef iorPos (top,left)
                            widgetHideAll w
                            lbl `labelSetText` ("Show"++drop 4 txt)


windowForceShow :: IORef (Int,Int) -> MenuItem -> Window -> IO()
windowForceShow iorPos mi w =
    do  mlbl <- binGetChild mi
        case mlbl of
          Nothing  -> widgetShowAll w
          Just lblo  -> do
                  let lbl = castToLabel lblo
                  txt <- labelGetText lbl
                  unless ('H' == qHead "windowShowHide" txt) $ do
                                  widgetShowAll w
                                  (top,left) <- readIORef iorPos
                                  windowMove w top left
                                  lbl `labelSetText` ("Hide"++drop 4 txt)




windowForceHide :: IORef (Int,Int) -> MenuItem -> Window -> IO()
windowForceHide iorPos mi w =
    do  mlbl <- binGetChild mi
        case mlbl of
          Nothing  -> widgetShowAll w
          Just lblo  -> do
                  let lbl = castToLabel lblo
                  txt <- labelGetText lbl
                  unless ('S' == qHead "windowShowHide" txt) $ do
                                  (top,left) <- windowGetPosition w
                                  writeIORef iorPos (top,left)
                                  widgetHideAll w
                                  lbl `labelSetText` ("Show"++drop 4 txt)

\end{code}

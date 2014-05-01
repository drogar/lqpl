\begin{code}
module Emulator.Dialogs.CompilerPreferences(
                                    setUpCompilerPreferences,
                                    showCompilerPrefDialog,
                                    cpDialogToOptions
                                   ) 
where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM

import Compiler.Opts


import Emulator.Configuration.Configuration

import Emulator.Data.GUI

firstCol :: ColumnId String String
firstCol = makeColumnIdString 1

setUpCompilerPreferences :: PrimeGUI  ->  CompilerPrefDialogGUI -> IO()
setUpCompilerPreferences   primeGui prefGui
    = do  compPreferencesMI primeGui `onActivateLeaf` showCompilerPrefDialog  primeGui prefGui
          let fchooser = cpIncludeDirectoryChooser prefGui
          let lview = cpIncludePath prefGui

          incfolders <- listStoreNew ["."]
          treeModelSetColumn incfolders firstCol id
          setUpIncludeListView lview incfolders fchooser
          setUpRightMenu lview incfolders
                          
          widgetHideAll (compileOptionsDialog prefGui)

setUpRightMenu :: TreeView -> ListStore String -> IO (ConnectId TreeView)
setUpRightMenu lview str = do
    lview `on` buttonReleaseEvent $ tryEvent $ do
       btn <- eventButton
       t <- eventTime
       (x,y) <- eventCoordinates
       case btn of 
         RightButton -> liftIO $ menuDetails lview (round x, round y) str btn t
         _   -> stopEvent

menuDetails :: TreeView -> Point-> ListStore String -> MouseButton -> TimeStamp -> IO ()
menuDetails lview pt str btn t = do
             maybePCpt <- treeViewGetPathAtPos lview pt
             case maybePCpt of 
                Just (ind:_,_,_) -> do
                  pmenu <- menuNew
                  delrow <- menuItemNewWithLabel "Remove from list"
                  delrow `afterActivateLeaf` do
                      str `listStoreRemove` ind
                      menuPopdown pmenu
                  pmenu `menuShellAppend` delrow
                  sep <- separatorMenuItemNew
                  pmenu `menuShellAppend` sep
                  item2 <- menuItemNewWithLabel "Close Menu"
                  item2 `afterActivateLeaf` (menuPopdown pmenu)
                  pmenu `menuShellAppend` item2
                  widgetShowAll pmenu
                  pmenu `menuPopup` Just (btn,t)
                  return ()
                _ -> return ()

setUpIncludeListView :: TreeView -> 
                        ListStore String ->
                        FileChooserButton -> 
                        IO (ConnectId FileChooserButton)
setUpIncludeListView lview incfolders fchooser = do
          lview `treeViewSetModel` incfolders
          tvcol <- treeViewColumnNew
          tvcol `treeViewColumnSetTitle` "Include Folder"
          tvcol `treeViewColumnSetVisible` True
          rend <- cellRendererTextNew
          treeViewColumnPackStart tvcol rend True
          cellLayoutSetAttributes tvcol rend incfolders $ \row -> [cellText := row ]
          treeViewAppendColumn lview tvcol
          treeViewSetHeadersVisible lview True
          treeViewColumnsAutosize lview
          fchooser `afterCurrentFolderChanged`  do
                 folder <- fileChooserGetCurrentFolder fchooser
                 case folder of
                     Just aFolder  -> do 
                         print aFolder
                         folders <- listStoreToList incfolders
                         unless (aFolder `elem` folders)  $ do
                              incfolders `listStoreAppend` aFolder 
                              set fchooser [widgetHeightRequest := 30 ]
                              return ()
                     Nothing       -> return ()

showCompilerPrefDialog ::  PrimeGUI -> CompilerPrefDialogGUI -> IO()
showCompilerPrefDialog  primeGui prefGui
    = do  let prefDialog = compileOptionsDialog prefGui
          windowSetTransientFor prefDialog (mainWindow primeGui)
          widgetShowAll prefDialog
          widgetHide (cpLoadCompiledCode prefGui) --todo implement this
          resp <- dialogRun prefDialog
          print resp
          case resp of
           ResponseOk ->
               do print "ok"
                  widgetHideAll prefDialog
           _ ->  widgetHideAll prefDialog

cpDialogToOptions :: CompilerPrefDialogGUI -> IO ([Flag], Bool)
cpDialogToOptions cpdGui = do
  [showSyntax, showIR, loadCode] <-
        mapM toggleButtonGetActive 
                 [(cpShowSyntax cpdGui), (cpShowIR cpdGui), (cpLoadCompiledCode cpdGui)]
  [logAll, logErr, logWrn, logInf, logDbg,  logTrc] <- 
        mapM toggleButtonGetActive 
                 [(cpLogEverything cpdGui),(cpLogErrors cpdGui),(cpLogWarnings cpdGui),
                  (cpLogInformation cpdGui),(cpLogDebug cpdGui),(cpLogFullTrace cpdGui)]
  searchStrings <- getSearchStrings (cpIncludePath cpdGui)
  let flgsdir   = (SearchDirs searchStrings) 
      flgss     = if showSyntax then [Syntactic] else []
      flgir     = if showIR then [IRoptPrint] else []
      flgll     = if logAll then [LogLevel 0] 
                  else if logErr then [LogLevel 2]
                       else if logWrn then [LogLevel 3]
                            else if logInf then [LogLevel 4]
                                 else if logDbg then [LogLevel 6]
                                      else if logTrc then [LogLevel 9]
                                           else []
      flags     = flgsdir : flgss ++ flgir ++ flgll
  return (flags,loadCode)

getSearchStrings :: TreeView -> IO [String]
getSearchStrings tview = do
  inPathsModel  <- treeViewGetModel tview
  case inPathsModel of
      Nothing    -> return []
      Just mdl   -> do 
         mbtiter  <- treeModelGetIterFirst mdl
         getStrings mdl mbtiter

getStrings :: TreeModel -> Maybe TreeIter -> IO [String]
getStrings = getStrings' []

getStrings' :: [String] -> TreeModel -> Maybe TreeIter -> IO [String]
getStrings' accum mdl Nothing = return accum
getStrings' accum mdl (Just iter) = do
  val <- treeModelGetValue mdl iter firstCol
  nxtIter <- treeModelIterNext mdl iter
  getStrings' (val:accum) mdl nxtIter

 

\end{code}

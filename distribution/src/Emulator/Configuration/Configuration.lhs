\subsubsection{Configuration of the GUI}\label{subsubsec:configuration}
Functions and constants used to find and load the glade file and manage the 
most recently used lists.
\begin{code}
module Emulator.Configuration.Configuration(
                                             getFromBuilder,
                                             updateMRUList,
                                             baseTitle,
                                             getMRUs) where

import Graphics.UI.Gtk
--import Graphics.UI.Gtk.Builder


import Data.IORef
import Data.List as List
import Data.Maybe

import System.Directory (doesFileExist,getHomeDirectory,createDirectoryIfMissing)
import System.FilePath
import System.Glib
import System.IO.Error
import System.IO

import Emulator.Data.GUI

import Paths_lqpl

baseTitle :: String 
baseTitle = "Quantum Stack Machine"


gladeFileName :: String 
gladeFileName = "qstack.glade"
subdir :: String 
subdir = "glade/"
homeconfdir :: String 
homeconfdir = "/.lqpl/"

mruconffile :: String
mruconffile = "MRU"


updateMRUList :: String -> IO()
updateMRUList newfile
    = do (_,files) <- getMRUs
         let fls = nub $ take 10 $ newfile:files
             (cdir',_) = splitFileName newfile 
         setMRUs cdir' fls

mru :: IO String
mru = do  hdir <- getHomeDirectory
          let  cnfdir = hdir ++ homeconfdir
               mruFile = cnfdir ++ mruconffile
          mruExists <- doesFileExist mruFile
          if mruExists then return mruFile else 
               do  createDirectoryIfMissing True cnfdir
                   hMRU <- openFile mruFile WriteMode
                   hPutStr hMRU ""
                   hClose hMRU
                   return mruFile



getMRUs :: IO (Maybe String, [String])
getMRUs =  do  fnm <- mru
               hMRU <- openFile fnm ReadMode
               contents <- hGetLines hMRU
               case  contents of
                     hd:rst  -> return (Just hd,rst)
                     []      -> return (Nothing,[])

setMRUs :: String -> [String] -> IO()
setMRUs cdir fls = do  fnm <- mru
                       hMRU <- openFile fnm WriteMode
                       hPutStr hMRU $ unlines $ cdir:fls
                       hClose hMRU


fileLoc :: IO FilePath
fileLoc = 
    catch (getDataFileName (subdir++gladeFileName))
              (\ _ -> return  gladeFileName)

                                  
getBuilder :: IO (Builder)
getBuilder =  do fpth <- fileLoc
                 bldr <- builderNew
                 print "Created Builder"
                 catchGError (builderAddFromFile bldr fpth) (\(GError dom cd msg) -> print msg)
                 return bldr



getFromBuilder :: IO (PrimeGUI, SubDisplayGUI,SubDisplayGUI,SubDisplayGUI,LQPLWindowGUI,PrefDialogGUI,CompilerPrefDialogGUI)
getFromBuilder =
    do builder <- getBuilder
       print "Got the builder"
       mainW <- builderGetObject builder castToWindow "qsMainWindow"
       print "Got main window"
       scroller <- builderGetObject builder castToScrolledWindow "qswScrolling"
       qsArea <- builderGetObject builder castToDrawingArea "qswArea"
       progBar <-   builderGetObject builder castToProgressBar "executionProgress"  
       showTraceCB <-builderGetObject builder castToCheckButton "showTrace"
       
       [stepSpin, sdSpin, tdSpin] <- 
           mapM (builderGetObject builder castToSpinButton)
                    ["stepCountSpinButton","streamDepthSpin","treeDepthSpin"]

       cstackLabel <- builderGetObject builder castToLabel "classicalStackLabel"
       [trimButton, stepButton, goButton] <- 
            mapM (builderGetObject builder castToButton) 
                     ["trimStackButton","stepButton","goButton"]
       [simMenuItem, fileMenu, openMenuItem, openLQPLMI, rofMI,
        rsfMI, snapshotMenuItem, prefMenuItem, compilerPrefMenuItem,
        quitMenuItem, aboutMenuItem] <- 
              mapM (builderGetObject builder castToMenuItem)  
                       ["simulate1","fileMenu","open","openLQPL",
                        "recentObjectFiles","recentSourceFiles","snapshotMI",
                        "preferences_menu_item","compiler_pref_menu_item",
                        "quit","about"]
       
       vmDialog <- builderGetObject builder castToDialog "versionWarningDialog"
       cvText<- builderGetObject builder castToLabel "compilerVersionLabel"
       svText <- builderGetObject builder castToLabel "emulatorVersionLabel"
       widgetHideAll vmDialog
       simDialog <- builderGetObject builder castToDialog "simulateDialog"
       qsAbout  <- builderGetObject builder  castToAboutDialog "aboutdialog"
       widgetHideAll qsAbout
       rvText <- builderGetObject builder castToLabel "randomValText"
       resTable <- builderGetObject builder castToTable "resultsTable"
       widgetHideAll simDialog

       let mainGui = PrimeGUI mainW scroller qsArea progBar showTraceCB stepSpin sdSpin tdSpin 
                     cstackLabel trimButton stepButton goButton simMenuItem fileMenu openMenuItem
                     openLQPLMI rofMI rsfMI snapshotMenuItem prefMenuItem compilerPrefMenuItem aboutMenuItem 
                     quitMenuItem  qsAbout 
                     vmDialog cvText svText
                     simDialog rvText resTable

       mmw <- builderGetObject builder  castToWindow "MMapWindow"
       mmTextView <- builderGetObject builder castToTextView "mapElementsTextView"
       widgetHideAll mmw
       showHideMM <- builderGetObject builder castToMenuItem "hide_or_show_mmap"
       let mmGui = SubDisplayGUI mmw (Left mmTextView) showHideMM

       dumpWindow <- builderGetObject builder  castToWindow "DumpWindow"
       dumpTextView <- builderGetObject builder castToTextView "dumpTextView"
       widgetHideAll dumpWindow
       showHideDumpMI <-builderGetObject builder castToMenuItem "hide_or_show_dump"
       let dumpGui = SubDisplayGUI dumpWindow (Left dumpTextView) showHideDumpMI 

       codeWindow <- builderGetObject builder  castToWindow "CodeWindow"
       instructionTabs <- builderGetObject builder castToNotebook "instructionLists"
       showHideCodeMI <-builderGetObject builder castToMenuItem "hide_or_show_code"
       let qpoGui = SubDisplayGUI codeWindow (Right instructionTabs) showHideCodeMI 

       lqplWindow <- builderGetObject builder  castToWindow "LqplWindow"
       lqplTabs <- builderGetObject builder castToNotebook "lqplNotebook"
       showHideLqplMI <-builderGetObject builder castToMenuItem "hide_or_show_LQPL"
       compileBtn  <-builderGetObject builder castToButton "compileLQPL"
       let lqplGui = LQPLWindowGUI (SubDisplayGUI lqplWindow (Right lqplTabs) showHideLqplMI) compileBtn 
       
       prefDialog <- builderGetObject builder castToDialog "configQSWindowDialog"
       fontButtonFB <-  builderGetObject builder castToFontButton "fontbutton"
       [pdshowTraceCB,useEpsilonCB] <- 
             mapM (builderGetObject builder castToCheckButton) 
                      ["showTraceCheckButton","useEpsilonTrimming"]
       [nsSB, msSB, nmXoffsetSB, nmYoffsetSB,  trXoffsetSB, trYoffsetSB, 
            nameSizeSB, brLblSizeSB, traceSizeSB, elisionSizeSB, 
            leafSizeSB, tracePrecSB, leafPrecSB, cdSB, debugLevelSB, 
            epsilonAmountSB, trimRateSB] <- 
              mapM (builderGetObject builder castToSpinButton)  
                ["dotsizeSpinButton", "minSeparationSpinButton", "nameXoffsetSpinButton",
                 "nameYoffsetSpinButton", "traceXoffsetSpinButton", "traceYoffsetSpinButton",
                 "nameFontSizeSpinButton", "branchLabelFontSizeSpinButton",
                 "traceFontSizeSpinButton", "elisionFontSizeSpinButton", "leafFontSizeSpinButton",
                 "traceShowDigitsSpinButton", "leafShowDigitsSpinButton", "callDepthSpinButton",
                 "debugLevelSpinButton", "epsilonAmountSpinButton", "trimRateSpinButton"]
       let prefDGui = PrefDialogGUI prefDialog fontButtonFB pdshowTraceCB useEpsilonCB 
                      nsSB msSB nmXoffsetSB nmYoffsetSB trXoffsetSB trYoffsetSB 
                      nameSizeSB brLblSizeSB traceSizeSB elisionSizeSB 
                      leafSizeSB tracePrecSB leafPrecSB cdSB debugLevelSB 
                      epsilonAmountSB trimRateSB

       compPrefDialog <- builderGetObject builder castToDialog "cpOptDialog"
       cpFileChooserBtn <-  builderGetObject builder castToFileChooserButton "includeDirectoryChooser"
       cpincpathtv <- builderGetObject builder castToTreeView "includeDirectories"
       [showSynCB,showIRCB,showLoadCodeCB] <- 
             mapM (builderGetObject builder castToCheckButton) 
                      ["showSyntax","showIR", "loadCode"]
       [cplogEverything, cplogErrors, cplogWarnings, cplogInfo, cplogDebug, cplogFull] <-
              mapM (builderGetObject builder castToRadioButton)  
                ["logEverything", "logError", "logWarning", "logInfo",
                 "logDebug", "logTrace"]
       let compPrefDGui = CompilerPrefDialogGUI compPrefDialog showSynCB showIRCB showLoadCodeCB
                          cplogEverything cplogErrors cplogWarnings cplogInfo
                          cplogDebug cplogFull cpFileChooserBtn cpincpathtv
       return (mainGui,mmGui, dumpGui, qpoGui, lqplGui, prefDGui, compPrefDGui)
        

hGetLines :: Handle -> IO [String]
hGetLines h
    = do  isEOF <-  hIsEOF h
          isClosed <- hIsClosed h
          if isEOF || isClosed 
            then do  hClose h
                     return []
            else do  ln <- hGetLine h
                     lines <- hGetLines h
                     return $ ln : lines
\end{code}

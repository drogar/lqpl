\section{The quantum stack emulator user interface}\label{section:qstakgtk}
\begin{code}
module Main where


import Graphics.UI.Gtk


import Graphics.UI.Gtk.Gdk.EventM

import Control.Monad (when)
import Control.Monad.Trans



import Data.IORef
import Data.List as List
import Data.Map as Map
import Data.Maybe (isJust, fromJust)



import Data.Computation.BaseType

import Emulator.Configuration.Configuration

import Emulator.Data.Preferences
import Emulator.Data.GUI

import Emulator.Dialogs.Preferences
import Emulator.Dialogs.CompilerPreferences
import Emulator.Dialogs.SimulateResults
import Emulator.Dialogs.FileDialogs

import Emulator.Drawing.QsWindow


import Emulator.Utility.GraphicUtilities(spinButtonSetValueAsInt)
import Emulator.Utility.Logging
import Emulator.Utility.Timings
import Emulator.Utility.WindowOperations

import Emulator.Windows.DumpWindow
import Emulator.Windows.InstructionWindow
import Emulator.Windows.LqplWindow
import Emulator.Windows.MemoryMapWindow


import QSM.BasicData
import QSM.QSM

\end{code}
\paragraph{StepGUI} shortens the signature for the various functions which
run the emulator in slightly different ways.
\begin{code}

type StepGUI = (QsWindowPrefs -> PrimeGUI -> 
                IORef (MachineState BaseType) ->  
                InstructionWindowUpdate -> IO ())

\end{code}
\paragraph{Constants} |maxX| and |maxY| are hacks used to ensure we are drawing in
the correct region, rather than figuring out the actual size of the window.
\begin{code}

maxX :: Int
maxX = 3200

maxY :: Int
maxY = 1600

streamDepthSpinDefault :: Int
streamDepthSpinDefault = 1
treeDepthSpinDefault :: Int
treeDepthSpinDefault = 5
stepCountSpinDefault :: Int
stepCountSpinDefault = 1

main :: IO ()
main
  = do initGUI
       qsprefref <- qsPreferences
       ms <- newIORef (startMachine defaultCallDepth initialMachine  noCode) 
       (primeGui,memWinGui,dumpWinGui,qpoCodeGui,lqplWinGui, prefGui, compPrefGui) <- getFromBuilder
       setUpCompilerPreferences  primeGui compPrefGui                
       mmapWindowUpdate <- mmapWindowSetup memWinGui
       dumpWindowUpdate <- dumpWindowSetup dumpWinGui
       instructionTabsUpdate <- instructionWindowSetup qpoCodeGui
       sourceCodeWindowUpdate <- lqplWindowSetup compPrefGui lqplWinGui
       
       let  activityUpDate :: StepGUI -> Bool -> IO()
            activityUpDate f  doMMap =
               do  qspref <- readIORef qsprefref
                   f qspref primeGui ms instructionTabsUpdate
                   when doMMap $ mmapWindowUpdate ms
                   dumpWindowUpdate ms (streamDepthSpin primeGui)

       let  startMeUp :: Int -> Memory Basis -> IO ()
            startMeUp cd mb =  do  writeIORef ms $ startMachine cd initialMachine mb
                                   instructionTabsUpdate ms (streamDepthSpin primeGui) True
                                   activityUpDate nostep  False

       qsp <- readIORef qsprefref
       showTraceCB primeGui `toggleButtonSetActive` showTrace qsp
       let qs = mainWindow primeGui
       qs `windowSetTitle` baseTitle
       qs `onDestroy` mainQuit
       instructionTabsUpdate ms (streamDepthSpin primeGui) True
       connectUItoModel primeGui qsprefref ms activityUpDate
       ioRefDefaultFolder <- newIORef Nothing   
       
       connectUItoModelQpoWindow qpoCodeGui primeGui startMeUp ioRefDefaultFolder
       connectUItoModelLqplWindow lqplWinGui primeGui sourceCodeWindowUpdate ioRefDefaultFolder
                                      
       (df,mrus) <- getMRUs
       updateDefaultFolder df ioRefDefaultFolder
       updateFileMenuWithMRUs mrus  startMeUp sourceCodeWindowUpdate primeGui
 
       setUpPreferences qsprefref primeGui prefGui  ms
       streamDepthSpin  primeGui `spinButtonSetValueAsInt` streamDepthSpinDefault
       treeDepthSpin  primeGui `spinButtonSetValueAsInt` treeDepthSpinDefault
       stepCountSpin  primeGui `spinButtonSetValueAsInt` stepCountSpinDefault
       widgetShowAll qs
       widgetHide (progBar primeGui)
       widgetHide (snapshotMI primeGui)
       mainGUI


connectUItoModel ::  PrimeGUI -> 
                     IORef(QsWindowPrefs) ->
                     IORef(MachineState BaseType) -> 
                     (StepGUI -> Bool -> IO()) ->
                     IO (ConnectId MenuItem)
connectUItoModel primeGui qsprefref ms activityUpDate = 
    do  trimButton primeGui `onClicked` activityUpDate trimit  True
        stepButton primeGui `onClicked` activityUpDate step True
        goButton primeGui `onClicked` activityUpDate execute  True
        streamDepthSpin primeGui `onValueSpinned` activityUpDate nostep  False
        treeDepthSpin primeGui `onValueSpinned`  activityUpDate nostep False
        showTraceCB primeGui `onToggled` do
                  val <- toggleButtonGetActive (showTraceCB primeGui)
                  modifyIORef  qsprefref (\ x -> x{showTrace = val})
                  activityUpDate nostep False
        stackDrawArea primeGui `on` exposeEvent $ tryEvent $ do
                  rect <- eventArea
                  liftIO $ exposeQstack ms primeGui qsprefref rect
        simulateMI primeGui `afterActivateLeaf` do
                  mstate <- readIORef ms
                  sdepth <- spinButtonGetValueAsInt (streamDepthSpin primeGui)
                  let qstk = quantumStack $ pickIthMS sdepth mstate
                  showSimulateResultsDialog primeGui qstk
        
        quitMI primeGui `afterActivateLeaf` mainQuit
        aboutMI primeGui `afterActivateLeaf` showAboutDialog primeGui

connectUItoModelQpoWindow ::  SubDisplayGUI -> PrimeGUI -> 
                              (Int -> Memory Basis -> IO ()) ->
                              IORef (Maybe FilePath) ->
                              IO (ConnectId MenuItem)
connectUItoModelQpoWindow  qpoGui primeGui startMeUp ioRefDefaultFolder = 
    openMI primeGui `afterActivateLeaf` do
                  filename <- openQPOFileDialog (mainWindow primeGui) ioRefDefaultFolder
                  addToRecent True (rofMI primeGui) filename (readAssemblyCode startMeUp primeGui)
                  readAssemblyCode startMeUp primeGui filename
                  widgetShowAll (tdWindow qpoGui) 

        

connectUItoModelLqplWindow ::  LQPLWindowGUI -> PrimeGUI -> 
                               LqplWindowUpdate ->              
                               IORef (Maybe FilePath) ->
                               IO (ConnectId MenuItem)
connectUItoModelLqplWindow  lqplGUI primeGui   scdRead ioRefDefaultFolder = 
   openLQPLMI primeGui `afterActivateLeaf` do
                  filename <- openQPLFileDialog (mainWindow primeGui) ioRefDefaultFolder
                  addToRecent True (rsfMI primeGui) filename (readSourceCode scdRead)
                  readSourceCode scdRead filename
                  widgetShowAll (tdWindow $ subGUI lqplGUI)

        



execute ::  StepGUI

execute  qspref primeGui  ms instructionTabsUpdate
  = do depth <- spinButtonGetValueAsInt (streamDepthSpin primeGui) 
       --modifyIORef ms (go depth)
       printClock "Starting exec at: "
       timeCheck $ do  execToDepthAndPulse qspref ms depth (progBar primeGui)
                       printClock "Ending exec at: "

       refreshSubs  qspref primeGui ms instructionTabsUpdate 


execToDepthAndPulse ::  QsWindowPrefs -> IORef (MachineState BaseType) ->
                       Int -> ProgressBar -> IO()
execToDepthAndPulse prs ms depth pbar
    = do mstate <- readIORef ms
         widgetShow pbar
         mstate' <- pulse (trimRate prs) (epsilonForTrimming prs) depth pbar mstate
         widgetHide pbar
         writeIORef ms mstate'

pulse :: Int -> (Maybe Int) ->Int -> 
         ProgressBar ->MachineState BaseType ->
         IO(MachineState BaseType) 
pulse m = pulse' m m 

pulse' :: Int -> Int -> (Maybe Int) ->Int -> 
         ProgressBar ->MachineState BaseType ->
         IO(MachineState BaseType) 
pulse' trimrate 0 epsilon depth pbar mstate
    = let mstatetrim = trimMachine epsilon depth mstate
          ci = cmscurrIns $ snd $ hd $ dropI depth mstatetrim
      in case ci of
           Just i -> do progressBarPulse pbar
                        mainIterationDo False
--                        putStrLn $ (show i) ++ " trimmed"
                        pulse trimrate epsilon depth pbar $ runMachine mstatetrim
           Nothing ->  return mstatetrim
         
pulse' trimrate n epsilon depth pbar mstate
    = let ci = cmscurrIns $ snd $ hd $ dropI depth mstate
      in case ci of
           Just i ->   pulse' trimrate (n-1) epsilon depth pbar $ runMachine mstate
           Nothing ->  return $ trimMachine epsilon depth mstate
         


step :: StepGUI

step qspref primeGui  ms  instructionTabsUpdate
  = do count <- spinButtonGetValueAsInt (stepCountSpin primeGui)
       runIt count ms
       ms' <- readIORef ms
       --depth <- spinButtonGetValueAsInt (streamDepthSpin primeGui)
       --let cms = snd $ hd $ dropI depth ms'
           --bms = collapse cms
       {- assert (checkNames $ quantumStack bms) $ "Error - QStack name issue." ++ 
                "ip = " ++ (show $ instructionPointer bms) ++
                "next 5 ins = " ++ ( showList (take 5 $ runningCode bms) "") ++
                "qsts =" ++ (showList (List.map (fst . snd) $ ctrldMS cms) "") ++
                "qstRebuilt = " ++ (show $ quantumStack bms) ++
                " dump = " ++ (show $ dump bms) -}
       --print $ "Ran "++(show count)++" instructions"
       --print $ "dump = " ++ (show $ dump bms)
       --print $ "qst = " ++ (show $ quantumStack bms)
       --print $ "ip = " ++ (show $ instructionPointer bms)
       --print $ "next 5 ins = " ++ ( showList (take 5 $ runningCode bms) "")
       --print "Ran, look again"
       --ms'' <- readIORef ms
       --print $ hd ms''
       refreshSubs qspref primeGui ms instructionTabsUpdate 
{-
checkForCall ::  Int -> Int -> MachineState BaseType -> IO()
checkForCall  i n ms
  = if i > n 
    then return ()
    else let bms = pickIthMS i ms
         in if List.null $ runningCode bms then return()
            else
              case (head $ runningCode bms) of
                 Call j ep -> elog logLevelTrace  $ 
                              "At level " ++ show i ++ " the instructions are :" ++ 
                                          showList (take 5 $ runningCode bms) ""
                 _ -> return ()

-}

runIt :: Int -> 
         IORef (MachineState BaseType) ->
         IO()
runIt 0 ms = return ()
runIt n ms 
      = do modifyIORef ms runMachine
--           mstate <- readIORef ms
--           let mstate' = runMachine mstate
--               (_,cms) = hd mstate'
--               isOK = liftAssert cms
--           print isOK
--           writeIORef ms mstate'
           runIt (n-1) ms


trimit :: StepGUI

trimit qspref primeGui ms cwUpdate =
    do mstate <- readIORef ms
       depth <-  spinButtonGetValueAsInt (streamDepthSpin primeGui)
       let mstate' = trimMachine (epsilonForTrimming qspref) depth mstate
       writeIORef ms mstate'
       refreshSubs  qspref primeGui ms cwUpdate

nostep ::  StepGUI

nostep = refreshSubs

refreshSubs :: StepGUI

refreshSubs  _ primeGui cms instructionTabsUpdate 
    = do  --dw <- drawingAreaGetDrawWindow da
          --(w,h) <- windowGetSize qscroller
          print "Updating qscroller draw area"
          widgetQueueDrawArea (scroller primeGui)  0 0 maxX maxY
          print "Updating instruction tabs"
          let sdspin = (streamDepthSpin primeGui)
          instructionTabsUpdate cms  sdspin False
          print "Classical stack update"
          setCstackOut cms sdspin (classicalStackLbl primeGui)
          logCallDepth cms sdspin

logCallDepth :: forall self a b (t :: * -> *).
                (IL t, Show a, SpinButtonClass self) =>
                IORef (t  (a,b)) -> self -> IO ()

logCallDepth cms sdSpin
  = do mstate <- readIORef cms
       sdepth <- spinButtonGetValueAsInt sdSpin
       putStrLn $ "Call depth at spin level "++ show sdepth ++
                       " is " ++ show (fst $ hd $ dropI sdepth mstate)




setCstackOut :: forall self self1 b.
                (LabelClass self1, Quantum b, SpinButtonClass self) =>
                IORef (MachineState b) -> self -> self1 -> IO ()

setCstackOut ms sdSpin cslbl
   = do mstate <- readIORef ms
        sdepth <- spinButtonGetValueAsInt sdSpin
        cslbl `labelSetText` showCstack (classicalStack $ pickIthMS sdepth mstate)

       
\end{code}

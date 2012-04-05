\begin{code}
module Emulator.Dialogs.Preferences(
                                    prefToFont,
                                    setUpPreferences,
                                    showPrefDialog,
                                    qsPreferences
                                   ) 
where

import Graphics.UI.Gtk

import Control.Monad (when)

import Data.Char (isDigit,isSpace)
import Data.Map as Map
import Data.List as List
import Data.IORef
import Data.Maybe (isJust, fromJust)

import Data.Computation.BaseType

import Assembler.AssemParser

import Emulator.Configuration.Configuration

import Emulator.Data.Preferences
import Emulator.Data.GUI


import Emulator.Utility.GraphicUtilities(spinButtonSetValueAsInt)
import Emulator.Utility.Logging



setPreferenceLayouts ::  IORef(QsWindowPrefs) -> DrawingArea -> IO()
setPreferenceLayouts ioRqsPref da
    = do qsPref <- readIORef ioRqsPref
         pcbl <- widgetCreatePangoContext da
         fdbl <- prefToFont qsPref branchLabelSize 
         eng <- languageFromString "en"
         pcbl `contextSetLanguage` eng
         pcbl `contextSetFontDescription` fdbl
         pfm <- contextGetMetrics pcbl fdbl eng
         --putStrLn $ "Metrics for branch label " ++ show pfm
         layoutbl <- widgetCreateLayout da ""
         layoutbl `layoutSetFontDescription` Just fdbl
         fdel <- prefToFont qsPref elisionSize
         layoutel <- widgetCreateLayout da ""
         layoutel `layoutSetFontDescription`  Just fdel
         pfmel <- contextGetMetrics pcbl fdel eng
         fdtr <- prefToFont qsPref  traceSize
         layouttr <- widgetCreateLayout da ""
         layouttr `layoutSetFontDescription`  Just fdtr
         pfmtr <- contextGetMetrics pcbl fdtr eng
         fdnm <- prefToFont qsPref   nameSize 
         layoutnm <- widgetCreateLayout da ""
         layoutnm `layoutSetFontDescription`  Just fdnm
         pfmnm <- contextGetMetrics pcbl fdnm eng
         fdlf <-  prefToFont qsPref  leafSize 
         layoutlf <- widgetCreateLayout da ""
         layoutlf `layoutSetFontDescription`  Just fdlf
         pfmlf <- contextGetMetrics pcbl fdlf eng
         writeIORef ioRqsPref (qsPref{branchLabelLayout = Just layoutbl,
                                      elisionLayout = Just layoutel,
                                      traceLayout = Just layouttr,
                                      nameLayout = Just layoutnm,
                                      leafLayout = Just layoutlf})

copyTheFont :: forall self.  (WidgetClass self) =>
               self -> FontDescription -> Double -> IO FontDescription

copyTheFont da fd psize 
  = do pc <- widgetCreatePangoContext da
       eng <- languageFromString "en"
       fd' <- fontDescriptionCopy fd
       fd' `fontDescriptionSetSize` psize
       pc  `contextSetLanguage` eng
       pc `contextSetFontDescription` fd'
       return fd'

setUpPreferences :: IORef(QsWindowPrefs) ->
                    PrimeGUI  -> 
                    PrefDialogGUI ->
                    IORef (MachineState BaseType) ->
                    IO ()

setUpPreferences iorPref primeGui prefGui ms
    = do  preferencesMI primeGui `onActivateLeaf` showPrefDialog iorPref primeGui prefGui ms
          setPreferenceLayouts iorPref (stackDrawArea primeGui)
          widgetHideAll (configureDialog prefGui)

showPrefDialog ::  IORef(QsWindowPrefs) ->
                   PrimeGUI -> 
                   PrefDialogGUI -> 
                   IORef (MachineState  BaseType) ->
                   IO ()

showPrefDialog iorqsPref primeGui prefGui mstate
    = do prefs <- readIORef iorqsPref
         windowSetTransientFor (configureDialog prefGui) (mainWindow primeGui)
         let  nsSB = pddotSizeSB prefGui
              msSB = pdminSeparationSB  prefGui
              nmXoffsetSB = pdnameXoffsetSB prefGui
              nmYoffsetSB =  pdnameYoffsetSB prefGui
              showTraceCB =  pdshowTraceCB prefGui
              trXoffsetSB =  pdtraceXoffsetSB  prefGui
              trYoffsetSB =  pdtraceYoffsetSB  prefGui
              fontButtonFB =  pdfontButtonFB prefGui
              nameSizeSB =  pdnameFontSizeSB prefGui
              brLblSizeSB =  pdbranchLabelFontSizeSB prefGui
              traceSizeSB =  pdtraceFontSizeSB prefGui
              elisionSizeSB =  pdelisionFontSizeSB prefGui
              leafSizeSB =  pdleafFontSizeSB prefGui
              tracePrecSB =  pdtraceShowDigitsSB prefGui
              leafPrecSB =  pdleafShowDigitsSB prefGui
              cdSB = pdcallDepthSB prefGui
              debugLevelSB = pddebugLevelSB prefGui
              useEpsilonCB = pduseEpsilonCB  prefGui
              epsilonAmountSB = pdepsilonAmountSB prefGui
              trimRateSB  = pdtrimRateSB prefGui
         useEpsilonCB `onToggled` (do
                             onOrOff <- toggleButtonGetActive useEpsilonCB
                             epsilonAmountSB `widgetSetCanFocus` onOrOff
                             return () )

         nsSB `spinButtonSetValueAsInt` nodeSize prefs
         msSB `spinButtonSetValueAsInt`  nodeSeparation prefs
         nmXoffsetSB  `spinButtonSetValueAsInt` nodeNameXOffset prefs
         nmYoffsetSB  `spinButtonSetValueAsInt` nodeNameYOffset prefs
         showTraceCB  `toggleButtonSetActive` showTrace prefs
         trXoffsetSB  `spinButtonSetValueAsInt` traceXOffset prefs
         trYoffsetSB  `spinButtonSetValueAsInt` traceYOffset prefs
         fontButtonFB  `fontButtonSetFontName` fontName prefs
         fontButtonFB `fontButtonSetShowStyle` False
         nameSizeSB  `spinButtonSetValueAsInt` nameSize prefs
         brLblSizeSB  `spinButtonSetValueAsInt` branchLabelSize prefs
         traceSizeSB  `spinButtonSetValueAsInt` traceSize prefs
         elisionSizeSB  `spinButtonSetValueAsInt` elisionSize prefs
         leafSizeSB  `spinButtonSetValueAsInt` leafSize prefs
         tracePrecSB  `spinButtonSetValueAsInt` tracePrecision prefs
         leafPrecSB  `spinButtonSetValueAsInt` leafPrecision prefs
         cdSB `spinButtonSetValueAsInt` callDepth prefs
         debugLevelSB `spinButtonSetValueAsInt` logLevel prefs
         let (uepsActive, eps)  = case (epsilonForTrimming prefs) of
                                      Just e   -> (True, e)
                                      Nothing  -> (False, -3)
         useEpsilonCB `toggleButtonSetActive` uepsActive
         epsilonAmountSB `widgetSetCanFocus` uepsActive
         epsilonAmountSB `spinButtonSetValueAsInt` eps
         trimRateSB  `spinButtonSetValueAsInt` trimRate prefs
         let prefDialog = configureDialog prefGui
         widgetShowAll prefDialog
         resp <- dialogRun prefDialog
         case resp of
           ResponseOk ->
               do ns <- spinButtonGetValueAsInt nsSB
                  ms <- spinButtonGetValueAsInt msSB
                  nmX <- spinButtonGetValueAsInt nmXoffsetSB
                  nmYo <- spinButtonGetValueAsInt nmYoffsetSB
                  showtr <-  toggleButtonGetActive showTraceCB
                  trXo  <- spinButtonGetValueAsInt trXoffsetSB
                  trYo  <- spinButtonGetValueAsInt trYoffsetSB
                  font  <- fontButtonGetFontName fontButtonFB
                  name  <- spinButtonGetValueAsInt nameSizeSB
                  brLb  <- spinButtonGetValueAsInt brLblSizeSB
                  tracsz <-  spinButtonGetValueAsInt traceSizeSB
                  elis  <- spinButtonGetValueAsInt elisionSizeSB
                  leafsz <-  spinButtonGetValueAsInt leafSizeSB
                  tracpr <-  spinButtonGetValueAsInt tracePrecSB
                  leafpr <-  spinButtonGetValueAsInt leafPrecSB
                  cd <- spinButtonGetValueAsInt cdSB
                  ll <-  spinButtonGetValueAsInt debugLevelSB
                  ue <- toggleButtonGetActive useEpsilonCB
                  epsVal <- spinButtonGetValueAsInt epsilonAmountSB
                  trimRateVal  <- spinButtonGetValueAsInt trimRateSB
                  let epsilon = if ue then Just epsVal else Nothing
                      newprefs = QsWindowPrefs  ns  ms  nmX  nmYo  showtr  
                                 trXo  trYo  font  name  brLb  elis tracsz
                                 leafsz  tracpr  leafpr Nothing Nothing Nothing
                                 Nothing Nothing ll cd epsilon trimRateVal
                  writeIORef iorqsPref newprefs
                  setPreferenceLayouts iorqsPref (stackDrawArea primeGui)
                  modifyIORef mstate (resetCallDepth cd)
                  --print "Wrote the qsprefs"
                  widgetHideAll prefDialog
                  --print "hid dialog"
           _ ->  widgetHideAll prefDialog
         

\end{code}

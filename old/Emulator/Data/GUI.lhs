\subsubsection{GUI type definitions}\label{subsubsec:GUItypes}
The data types for the GUIs are defined below as records containing the 
Gtk2Hs\ref{gtk2hs} items used in the interface.
\begin{code}
module Emulator.Data.GUI(
                         PrimeGUI(..),
                         SubDisplayGUI(..),
                         LQPLWindowGUI(..),
                         PrefDialogGUI(..),
                         CompilerPrefDialogGUI(..)
                        ) 
where

import Graphics.UI.Gtk

\end{code}
\paragraph{|PrimeGUI|} contains the elements used on the main screen (see
\vref{subsubsec:qswindow} and \vref{section:qstakgtk}).

\begin{code}


data PrimeGUI =
    PrimeGUI {
          mainWindow          :: Window,
          scroller            :: ScrolledWindow,
          stackDrawArea       :: DrawingArea,
          progBar             :: ProgressBar,
          showTraceCB         :: CheckButton,
          stepCountSpin       :: SpinButton,
          streamDepthSpin     :: SpinButton,
          treeDepthSpin       :: SpinButton,
          classicalStackLbl   :: Label,
          trimButton          :: Button,
          stepButton          :: Button,
          goButton            :: Button,
          simulateMI          :: MenuItem,
          fileMI              :: MenuItem,
          openMI              :: MenuItem,
          openLQPLMI          :: MenuItem,
          rofMI               :: MenuItem,
          rsfMI               :: MenuItem,
          snapshotMI          :: MenuItem,
          preferencesMI       :: MenuItem,
          compPreferencesMI   :: MenuItem,
          aboutMI             :: MenuItem,
          quitMI              :: MenuItem,
          aboutDlg            :: AboutDialog,
          versionWarnDlg      :: Dialog,
          compilerVersion     :: Label,
          emulatorVersion     :: Label,
          simulateDlg         :: Dialog,
          randomValText       :: Label,
          resultsTable        :: Table
}

\end{code}
\paragraph{|SubDisplayGUI|} contains the elements used in most of the "pop-up" windows that
display auxilliary data. These include the compiled code window which has a notebook and the 
memory and windows for displaying the dump and memory map.

Additionally, this is contained in |LQPLWindowGUI| which has these elements in addition to 
GUI elements related to compiling.

\begin{code}

data SubDisplayGUI =
  SubDisplayGUI {
                tdWindow      :: Window,
                dataElts      :: Either TextView Notebook,
                showHideMI    :: MenuItem
}

\end{code}
\paragraph{|LQPLWindowGUI|} contains a button for compiling in addition to
a |SubDisplayGUI|.

\begin{code}

data LQPLWindowGUI =
  LQPLWindowGUI {
                 subGUI         :: SubDisplayGUI,
                 compileButton  :: Button
}

\end{code}
\paragraph{|PrefDialogGUI|} has the elements of the view and processing preferences
for the emulator.

\begin{code}


data PrefDialogGUI =
  PrefDialogGUI {
      configureDialog             :: Dialog,
      pdfontButtonFB              :: FontButton,
      pdshowTraceCB               :: CheckButton,
      pduseEpsilonCB              :: CheckButton,
      pddotSizeSB                 :: SpinButton,
      pdminSeparationSB           :: SpinButton,
      pdnameXoffsetSB             :: SpinButton,
      pdnameYoffsetSB             :: SpinButton,
      pdtraceXoffsetSB            :: SpinButton,
      pdtraceYoffsetSB            :: SpinButton,
      pdnameFontSizeSB            :: SpinButton,
      pdbranchLabelFontSizeSB     :: SpinButton,
      pdtraceFontSizeSB           :: SpinButton,
      pdelisionFontSizeSB         :: SpinButton,
      pdleafFontSizeSB            :: SpinButton,
      pdtraceShowDigitsSB         :: SpinButton,
      pdleafShowDigitsSB          :: SpinButton,
      pdcallDepthSB               :: SpinButton,
      pddebugLevelSB              :: SpinButton,
      pdepsilonAmountSB           :: SpinButton,
      pdtrimRateSB                :: SpinButton
                }
      

\end{code}


\paragraph{|CompilePrefDialogGUI|} has the possible options for the compiler.

\begin{code}

                  
data CompilerPrefDialogGUI =
  CompilerPrefDialogGUI {
      compileOptionsDialog        :: Dialog,
      cpShowSyntax                :: CheckButton,
      cpShowIR                    :: CheckButton,
      cpLoadCompiledCode          :: CheckButton,
      cpLogEverything             :: RadioButton,
      cpLogErrors                 :: RadioButton,
      cpLogWarnings               :: RadioButton,
      cpLogInformation            :: RadioButton,
      cpLogDebug                  :: RadioButton,
      cpLogFullTrace              :: RadioButton,
      cpIncludeDirectoryChooser   :: FileChooserButton,
      cpIncludePath               :: TreeView
                }
      

\end{code}

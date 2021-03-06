\subsection{Controlling the Quantum Machine}\label{section:quantummachineserver.machinecontrol}
\begin{code}
 module Lqpl.Server.Types (
     module Lqpl.Utility.ServerTypes,
     QSData(..),
     QCommand(..)
     )
 where

 import Data.IORef
 import Lqpl.Utility.ServerTypes

 import Network.Socket

 import System.IO

 data QSData = QDQuantumStack | QDClassicalStack | QDDump | QDMemoryMap | QDExecutableCode | QDCodePointer
  deriving (Eq,Show,Read)

 data QCommand = QCLoad Int FilePath |
    QCStep Int Int |
    QCRun Int |
    QCGet QSData Int Int |
    QCSimulate Int |
    QCDepthMultiple Int |
    QCTrim
  deriving (Eq,Show,Read)

\end{code}

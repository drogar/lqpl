\subsection{Controlling the Quantum Machine}\label{section:quantummachineserver.machinecontrol}
\begin{code}
 module QServer.Types (
     module Utility.ServerTypes,
     QSData(..),
     QCommand(..)
     )
 where

 import Data.IORef
 import Utility.ServerTypes

 import Network.Socket
 import Network.BSD

 import System.IO

 data QSData = QDQuantumStack | QDClassicalStack | QDDump | QDMemoryMap
  deriving (Eq,Show,Read)

 data QCommand = QCLoad FilePath |
    QCStep Int |
    QCRun Int |
    QCGet QSData Int Int |
    QCSimulate Int
  deriving (Eq,Show,Read)

\end{code}
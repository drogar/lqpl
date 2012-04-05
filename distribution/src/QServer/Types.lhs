\subsection{Controlling the Quantum Machine}\label{section:quantummachineserver.machinecontrol}
\begin{code}
 module QServer.Types (
     HandlerFunc(..),
     Logger(..),
     QSData(..),
     QCommand(..)
     )
 where
 
 import Data.IORef  
 
 import Network.Socket
 import Network.BSD
 
 import System.IO
 
 type HandlerFunc a = IORef a -> Handle -> SockAddr -> String -> IO ()

 type Logger = SockAddr -> String -> IO ()
 
 data QSData = QDQuantumStack | QDClassicalStack | QDDump 
  deriving (Eq,Show,Read)
 
 data QCommand = QCLoad FilePath |
    QCStep Int |
    QCRun Int |
    QCGet QSData Int Int |
    QCSimulate Int
  deriving (Eq,Show,Read)

\end{code}
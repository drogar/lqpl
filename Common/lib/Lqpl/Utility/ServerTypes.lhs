\subsection{Types for network server}\label{section:netservertypes}
\begin{code}
 module Lqpl.Utility.ServerTypes (
     HandlerFunc
     )
 where

 import Data.IORef

 import Network.Socket

 import System.IO

 type HandlerFunc a = IORef a -> Handle -> SockAddr -> String -> IO ()

\end{code}

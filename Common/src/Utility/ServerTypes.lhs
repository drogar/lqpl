\subsection{Types for network server}\label{section:netservertypes}
\begin{code}
 module Utility.ServerTypes (
     HandlerFunc(..),
     Logger(..)
     )
 where

 import Data.IORef

 import Network.Socket

 import System.IO

 type HandlerFunc a = IORef a -> Handle -> SockAddr -> String -> IO ()

 type Logger = SockAddr -> String -> IO ()


\end{code}

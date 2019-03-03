\section{The quantum machine server main function}\label{section:quantummachineservermainfunction}
\begin{code}
module Main where

import Lqpl.Server.Server


defaultPortToUse :: String
defaultPortToUse = "9502"

main:: IO()

main = do serveLog defaultPortToUse commandHandler defaultLogger

\end{code}

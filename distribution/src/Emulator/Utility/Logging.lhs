\begin{code}
module Emulator.Utility.Logging (
                                 elognoRef ,
                                 elog ,
                                 logLevelAlways, logLevelTrace, logLevelDebug3, 
                                 logLevelDebug2, logLevelDebug1, logLevelDebug, 
                                 logLevelInfo, logLevelWarn, logLevelError, 
                                 logLevelFail, logLevelNoLog
)
         where

import Control.Monad(when)

import Data.IORef
import Emulator.Data.Preferences

elognoRef ::  QsWindowPrefs ->Int -> String  -> IO()
elognoRef prs ll 
    =  when (ll <= logLevel prs) . putStrLn 


elog :: IORef(QsWindowPrefs) -> Int -> String  -> IO()
elog prefs ll output
    =  do prs <- readIORef prefs
          elognoRef prs ll output

logLevelAlways, logLevelTrace, logLevelDebug3, logLevelDebug2, logLevelDebug1, logLevelDebug, logLevelInfo, logLevelWarn, logLevelError, logLevelFail, logLevelNoLog :: Int

logLevelAlways = 10
logLevelTrace = 9
logLevelDebug3 = 8
logLevelDebug2 = 7
logLevelDebug1 = 6
logLevelDebug = 5
logLevelInfo = 4
logLevelWarn = 3
logLevelError = 2
logLevelFail = 1
logLevelNoLog = 0



\end{code}

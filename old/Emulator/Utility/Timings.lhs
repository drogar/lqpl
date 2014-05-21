\begin{code}
module Emulator.Utility.Timings where

import System.CPUTime
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale
import Text.Printf


printClock :: String -> IO ()
printClock  prefix = 
    do  cnow <- getZonedTime 
        putStrLn $ prefix++formatTime defaultTimeLocale 
                            (iso8601DateFormat (Just " %H:%M:%S" )) cnow
       

timeCheck :: IO() -> IO()
timeCheck  = timeCheckWithMessage "This execute used "

timeCheckWithMessage :: String -> IO() -> IO()
timeCheckWithMessage msg prog = 
    do  start <- getCPUTime
        prog
        end <- getCPUTime
        let psecs = end - start
            secs = psecs `div` 1000000000000
            msecs = (psecs `mod` 1000000000000) `div` 1000000000
        printf "%s%d.%03d %s\n" msg secs msecs "seconds."
 
\end{code}

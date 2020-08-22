\subsection{Logger}\label{section:netservertypes}
\begin{code}
  module Lqpl.Utility.Logger (
     Logger,
     LogLevel(..),
     logWithMVarLock,
     defaultLogger
     )
  where

  import Control.Concurrent.MVar

  import Data.Time

  import Network.Socket


  data LogLevel = LogDebug | LogInfo | LogWarn | LogError
    deriving (Show, Eq)

  type Logger = LogLevel -> Maybe (SockAddr) -> String -> IO ()

  defaultLogger :: Logger
  defaultLogger level address message = do
     now <- getCurrentTime
     putStrLn $ "{ \"date\": \"" ++ (timeFormatter now) ++ "\", " ++
       " \"level\": \"" ++ show level ++ "\", " ++
       " \"address\": \"" ++ show address ++ "\", " ++
       " \"message\": \"" ++ message ++"\"}"

     where timeformat = iso8601DateFormat (Just "%H:%M:%Q")
           timeFormatter = formatTime defaultTimeLocale timeformat

  logWithMVarLock :: (Logger) -> MVar() -> Logger
  logWithMVarLock logger lock level clientAddress message =
     withMVar lock (\a -> logger level clientAddress message >> return a)


\end{code}

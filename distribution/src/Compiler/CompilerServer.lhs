\incsec{Compiler server driver}\label{incsec:compiler server main}

\begin{code}

  module Compiler.CompilerServer where




  import Control.Concurrent
  import Control.Concurrent.MVar

  import Data.IORef

  import Network.Socket
  import Network.BSD

  import System.IO

  import Utility.ServerTypes

  import Utility.Extras(filterNonPrintable)

  default_port = "7683"

  serveLog :: String              -- ^ Port number or name;
           -> HandlerFunc  (String)       -- ^ Function to handle incoming messages
           -> Logger               -- ^ Function handle logging
           -> IO ()
  serveLog port handlerfunc logger = withSocketsDo $
      do -- Look up the port.  Either raises an exception or returns
         -- a nonempty list.
         addrinfos <- getAddrInfo
                      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                      Nothing (Just port)
         let serveraddr = head addrinfos

         -- Create a socket
         sock <- socket (addrFamily serveraddr) Network.Socket.Stream defaultProtocol

         -- Bind it to the address we're listening to
         bindSocket sock (addrAddress serveraddr)

         -- Start listening for connection requests.  Maximum queue size
         -- of 2 connection requests waiting to be accepted.
         listen sock 2

         -- Create a lock to use for synchronizing access to the handler
         lock <- newMVar ()

         -- Loop forever waiting for connections.  Ctrl-C to abort.
         procRequests lock sock

      where
            -- | Process incoming connection requests
            procRequests :: MVar () -> Socket -> IO ()
            procRequests lock mastersock =
                do (connsock, clientaddr) <- accept mastersock
                   logit lock clientaddr
                      "lqpl-compiler-serv: client connnected"
                   forkIO $ procMessages lock connsock clientaddr
                   procRequests lock mastersock

            -- | Process incoming messages
            procMessages :: MVar () -> Socket -> SockAddr -> IO ()
            procMessages lock connsock clientaddr =
                do connhdl <- socketToHandle connsock ReadWriteMode
                   hSetBuffering connhdl LineBuffering
                   ref <- newIORef ""
                   messages <- hGetContents connhdl
                   mapM_ (handle lock  ref connhdl clientaddr) (lines messages)
                   hClose connhdl
                   logit lock clientaddr
                      "lqpl-compiler-serv: client disconnected"

            -- Lock the handler before passing data to it.
            handle :: MVar () -> HandlerFunc (String)
            handle lock ref shandle clientaddr msg =
                    withMVar lock  (\a -> handlerfunc ref shandle clientaddr (filterNonPrintable msg) >> return a)
            -- Lock the logger before passing data to it.
            logit :: MVar () -> Logger
            logit lock clientaddr msg =
                    withMVar lock (\a -> logger clientaddr msg >> return a)

  -- A simple logger that prints incoming packets
  defaultLogger :: Logger
  defaultLogger addr msg =
       putStrLn $ "LOGGED: " ++ show addr ++ ": " ++ msg


  -- A simple handler that prints incoming packets
  commandHandler :: HandlerFunc (String)
  commandHandler machineStateRef shandle addr msg = do
    putStrLn $ "From " ++ show addr ++ ": Message: " ++ msg

--    do
--      case (getCommand msg) o
--        Right (QCSimulate depth) ->
--          simulate depth machineStateRef shandle
--        Left x -> putStrLn $ "From " ++ show addr ++ ": unrecognized: " ++ msg ++ " -- " ++ x



\end{code}
do args <- getArgs
    (o, spltfps) <- compilerOpts args
          putStrLn (showVersion version)
    let tellsM = map (doCompile False o) spltfps
    tells <- mapM  execWriterT tellsM
          mapM_ putStrLn (concat tells)


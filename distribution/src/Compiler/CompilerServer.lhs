\incsec{Compiler server driver}\label{incsec:compiler server main}

\begin{code}

  module Compiler.CompilerServer where


  import Compiler.QPLParser
  import Compiler.Qtypes
  import Compiler.Semantic
  import Compiler.GenCode

  import Control.Concurrent
  import Control.Concurrent.MVar

  import Control.Monad.Writer as W

  import Data.IORef
  import Data.List

  import Network.Socket
  import Network.Socket as NS
  import Network.BSD

  import System.IO
  import System.IO.Error

  import Utility.ServerTypes
  import Utility.FileProvider

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
                      (Just (defaultHints {addrFlags = []}))
                      (Just "localhost") (Just port)
         let serveraddr = head addrinfos
         putStrLn $ show serveraddr

         -- Create a socket
         sock <- socket (addrFamily serveraddr) Network.Socket.Stream defaultProtocol

         -- Bind it to the address we're listening to
         bindSocket sock (addrAddress serveraddr)

         -- Start listening for connection requests.  Maximum queue size
         -- of 2 connection requests waiting to be accepted.
         NS.listen sock 2

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
  commandHandler prog shandle addr msg = do
    putStrLn $ "From " ++ show addr ++ ": Message: " ++ msg
    css <- compilerService (fp shandle) prog msg
    case css of
      CS_COMPILED_SUCCESS l   -> do
        hPutStrLn shandle "<qpo>"
        hPutStrLn shandle l
        hPutStrLn shandle "</qpo>"
      CS_COMPILED_FAIL l      -> do
        hPutStrLn shandle "<compilefail>"
        hPutStrLn shandle l
        hPutStrLn shandle "</compilefail>"
      _                       -> hPutStrLn shandle $ show css

  fp :: Handle -> FileProvider
  fp h = FileProvider {
    fpDoesFileExist = \ f -> do
        hPutStrLn h $ "<exists>"++f++"</exists>"
        hFlush h
        res <- hGetLine h
        case res of
          "<True>"  -> return True
          _  -> return False,
      fpReadFile = \f -> do
        hPutStrLn h $ "<read>"++f++"</read>"
        hFlush h
        hGetLine h,
      emptyProvider = "",
      currentFPDir = "",
      fpcombine = (++),
      getFirstFileInSearchPath = \p f -> do
        hPutStrLn h $ "<getFirst>"++f++"</getFirst>"
        hFlush h
        fname <- hGetLine h
        fdata <- hGetLinesDelimitedBy h "<file>" "</file>"
        return $ Just (fname,fdata)
      }

  hGetLinesDelimitedBy :: Handle -> String -> String -> IO String
  hGetLinesDelimitedBy h start end = do
    l <- hGetLine h
    if l == start
      then do
        ls <- hAccumLinesEndedBy h end []
        return $ concat $ intersperse "\n" ls
      else hGetLinesDelimitedBy h start end

  hAccumLinesEndedBy :: Handle -> String -> [String] -> IO [String]
  hAccumLinesEndedBy h end accum = do
    l <- hGetLine h
    if l == end
      then return $ reverse accum
      else hAccumLinesEndedBy h end (l:accum)


  compilerService :: FileProvider -> IORef (String) -> String -> IO CompilerServiceStatus
  compilerService _ ior "<qplprogram>" = do
    writeIORef ior ""
    return CS_READY
  compilerService _ ior "</qplprogram>" = do
    return CS_GOT_PROGRAM
  compilerService fp ior "<sendresult />" = do
    p <- readIORef ior
    errOrTxt <- try $ doCompile fp p
    case errOrTxt of
      Left e    -> return $ CS_COMPILED_FAIL $ ioeGetErrorString e
      Right txt -> return $ CS_COMPILED_SUCCESS $ concat $ intersperse "\n" txt
  compilerService _ ior a = do
    modifyIORef ior (++(a++"\n"))
    return CS_READY

  doCompile :: FileProvider -> String -> IO [String]
  doCompile fp p = do
    asyn <- parseQPL fp "" "" p []
    (ir,_) <- W.runWriterT $ W.mapWriterT (removeState 0) (makeIr asyn)
    ioGenCode ir 0

  data CompilerServiceStatus =  CS_READY | CS_GOT_PROGRAM | CS_COMPILED_SUCCESS String|
                                CS_COMPILED_FAIL String | CS_MESSAGE String
    deriving (Show,Eq)


\end{code}

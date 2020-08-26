\incsec{Compiler server driver}\label{incsec:compiler server main}

\begin{code}

  module Lqpl.Compiler.CompilerServer where


  import Lqpl.Compiler.CompilerCommand
  import Lqpl.Compiler.CompilerServiceStatus
  import Lqpl.Compiler.GenCode
  import Lqpl.Compiler.QPLParser
  import Lqpl.Compiler.Qtypes
  import Lqpl.Compiler.Semantic
  import Lqpl.Compiler.ServiceQPLFile


  import Lqpl.Utility.Extras(filterNonPrintable)
  import Lqpl.Utility.FileProvider
  import Lqpl.Utility.Logger
  import Lqpl.Utility.MakeJSON
  import Lqpl.Utility.ServerTypes

  import Control.Applicative
  import Control.Concurrent
  import Control.Concurrent.MVar

  import Control.Exception as CException

  import Control.Monad.Writer as W

  import Data.Aeson
  import qualified Data.ByteString.Char8 as B
  import Data.IORef
  import Data.List as List
  import Data.Map as Map
  import Data.Maybe
  import qualified Data.Text
  import Data.Time

  import Network.Socket as NS

  import System.IO
  import System.IO.Error


  import Paths_lqpl_compiler_server

  defaultPort = "7683"
  {-
  instance ToJSON AddrInfoFlag where
    toJSON f = object [ (Data.Text.pack "addressflag") .= (show f) ]

  instance ToJSON AddrInfo where
    toJSON addrinfo =
      object [ (Data.Text.pack "flags") .= toJSON (addrFlags addrinfo)]
-}
  serveLog :: String              -- ^ Port number or name;
           -> HandlerFunc  (Map (Maybe String) String)       -- ^ Function to handle incoming messages
           -> Logger               -- ^ Function handle logging (SockAddr -> String -> IO() )
           -> IO ()
  serveLog port handlerfunc logger = withSocketsDo $
      do -- Set buffering to line level so Docker outputs the info
        hSetBuffering stdout LineBuffering
        hSetBuffering stderr LineBuffering
         -- Look up the port.  Either raises an exception or returns
         -- a nonempty list.
        addrinfos <- getAddrInfo
                     (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                     Nothing (Just port)
        let serveraddr = head addrinfos
        logger LogDebug Nothing $ show serveraddr
        -- Create a socket
        sock <- socket (addrFamily serveraddr) NS.Stream defaultProtocol
        -- Bind it to the address we're listening to
        bind sock (addrAddress serveraddr)
        -- Start listening for connection requests.  Maximum queue size
        -- of 2 connection requests waiting to be accepted.
        NS.listen sock 2
        -- Create a lock to use for synchronizing access to the handler
        lock <- newMVar ()
        -- Loop forever waiting for connections.  Ctrl-C to abort.
        processIncomingRequests lock sock
      where
            -- | Process incoming connection requests
            processIncomingRequests :: MVar () -> Socket -> IO ()
            processIncomingRequests lock mastersock =
                do (connsock, clientaddr) <- accept mastersock
                   logWithMVarLock logger lock LogInfo (Just clientaddr)
                      "lqpl-compiler-serv: client connnected"
                   forkIO $ processIncomingMessages lock connsock clientaddr
                   processIncomingRequests lock mastersock

            -- | Process incoming messages
            processIncomingMessages :: MVar () -> Socket -> SockAddr -> IO ()
            processIncomingMessages lock connsock clientaddr =
                do connhdl <- socketToHandle connsock ReadWriteMode
                   hSetBuffering connhdl LineBuffering
                   ref <- newIORef Map.empty
                   messages <- hGetContents connhdl
                   mapM_ (handle lock ref connhdl clientaddr) (lines messages)
                   hClose connhdl
                   logWithMVarLock logger lock LogInfo (Just clientaddr)
                      "lqpl-compiler-serv: client disconnected"

            -- Lock the handler before passing data to it.
            handle :: MVar () -> HandlerFunc (Map (Maybe String) String)
            handle lock ref shandle clientaddr msg =
                    withMVar lock  (\a -> handlerfunc ref shandle clientaddr
                                                      (filterNonPrintable msg) >> return a)


  -- A simple handler that prints incoming packets
  commandHandler :: HandlerFunc (Map (Maybe String) String)
  commandHandler prog shandle addr msg = do
    defaultLogger LogDebug Nothing $ "From " ++ show addr ++ ": Message: " ++ msg
    css <- compilerService prog msg
    defaultLogger LogDebug Nothing $ show css
    hPutStrLn shandle $ resultToJSON css

  fp :: Map (Maybe String) String -> FileProvider
  fp imps = FileProvider {
    fpDoesFileExist = \ f -> return (Just f `elem` keys imps),
      fpReadFile = \f -> return "",
      emptyProvider = "",
      currentFPDir = "",
      fpcombine = (++),
      getFirstFileInSearchPath = \p f ->
        if imps `haskey` Just f
          then return $ Just (f, imps ! Just f)
          else ioError $ userError $  "Need file "++f
      }

  compileMe :: Maybe String
  compileMe = Nothing

  addProgramToIOREF :: IORef (Map (Maybe String) String) ->
                      String ->
                      [String] ->
                      IO ()
  addProgramToIOREF ior filename statements = do
    current_files <- readIORef ior
    let files = if Map.null current_files
                  then Map.singleton compileMe (toMultiLineString statements)
                  else current_files
        newFileMap = Map.singleton (Just filename) (toMultiLineString statements)
    writeIORef ior (Map.union newFileMap files)

  compilerService ::  IORef (Map (Maybe String) String) ->
                      String ->
                      IO CompilerServiceStatus
  compilerService ior input = do
    let qplfile = decodeStrict $ B.pack input :: Maybe ServiceQPLFile
    case qplfile of
      Just q  -> do
        addProgramToIOREF ior (fileName q) (qplProgram q)
        tryCompiling ior
      Nothing -> do
        let command = decodeStrict $ B.pack input :: Maybe CompilerCommand
        return $ commandToServiceStatus command


  tryCompiling :: IORef (Map (Maybe String) String) ->
                  IO CompilerServiceStatus
  tryCompiling ior = do
    imps <- readIORef ior
    let compile_file = fromJust $ Map.lookup compileMe imps
    errOrTxt <- CException.try $ doCompile (fp imps) compile_file
    case errOrTxt of
      Left e    -> do
        let errString = ioeGetErrorString e
        if "Need file " == List.take 10 errString
          then return $ CS_NEED_FILE $ List.drop 10 errString
          else do
            writeIORef ior Map.empty
            return $ CS_COMPILED_FAIL $ ioeGetErrorString e
      Right (txt,logs) -> do
            writeIORef ior Map.empty
            return $ CS_COMPILED_SUCCESS (toMultiLineString txt) (toMultiLineString logs)

  doCompile :: FileProvider -> String -> IO ([String],[String])
  doCompile fileProvider program = do
    abstractSyntax <- parseQPL fileProvider "" "" program []
    (intermediateRepresentation,cls) <- W.runWriterT $ W.mapWriterT (removeState 0) (makeIr abstractSyntax)
    code <- ioGenCode intermediateRepresentation 0
    return (code,cls)

  haskey map k = k `elem` keys map

\end{code}

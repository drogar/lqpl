\incsec{Compiler server driver}\label{incsec:compiler server main}

\begin{code}

  module Compiler.CompilerServer where


  import Compiler.QPLParser
  import Compiler.Qtypes
  import Compiler.Semantic
  import Compiler.GenCode

  import Control.Applicative
  import Control.Concurrent
  import Control.Concurrent.MVar

  import Control.Exception as CE

  import Control.Monad.Writer as W

  import Data.IORef
  import Data.List as List
  import Data.Map as Map

  import Network.Socket
  import Network.Socket as NS
  import Network.BSD

  import System.IO
  import System.IO.Error

  import Utility.ServerTypes
  import Utility.FileProvider

  import Utility.Extras(filterNonPrintable)

  import Data.Version

  import qualified Data.ByteString.Char8 as B
  import qualified Data.Text as DT
  import Data.Aeson
  import Data.Attoparsec
  import Data.Attoparsec.Number


  import Paths_lqpl

  default_port = "7683"

  data CompilerServiceStatus =  CS_COMPILED_SUCCESS String String |
                                CS_COMPILED_FAIL String |
                                CS_VERSION [Int] [String] |
                                CS_NEED_FILE String | CS_READY
    deriving (Show,Eq)


  data QPLFile = QPLFile {
    file_name :: String,
    qpl_program :: [String]
  }
    deriving(Eq, Show)

  instance FromJSON QPLFile where
    parseJSON (Object v) =
        QPLFile <$> v .: (DT.pack "file_name")
                <*> v .: (DT.pack "qpl_program")
    parseJSON _          = mzero

  serveLog :: String              -- ^ Port number or name;
           -> HandlerFunc  (CompilerServiceStatus,String, Map String (Maybe String))       -- ^ Function to handle incoming messages
           -> Logger               -- ^ Function handle logging
           -> IO ()
  serveLog port handlerfunc logger = withSocketsDo $
      do -- Look up the port.  Either raises an exception or returns
         -- a nonempty list.
         addrinfos <- getAddrInfo
                      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                      Nothing (Just port)
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
                   ref <- newIORef (CS_READY, "", Map.empty)
                   messages <- hGetContents connhdl
                   mapM_ (handle lock  ref connhdl clientaddr) (lines messages)
                   hClose connhdl
                   logit lock clientaddr
                      "lqpl-compiler-serv: client disconnected"

            -- Lock the handler before passing data to it.
            handle :: MVar () -> HandlerFunc (CompilerServiceStatus, String, Map String (Maybe String))
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
  commandHandler :: HandlerFunc (CompilerServiceStatus, String, Map String (Maybe String))
  commandHandler progAndImps shandle addr msg = do
    --putStrLn $ "From " ++ show addr ++ ": Message: " ++ msg
    css <- compilerService progAndImps msg
    --putStrLn $ show css
    hPutStrLn shandle $ resultToJSON css
--      _                       -> hPutStrLn shandle $ show css

  resultToJSON :: CompilerServiceStatus -> String
  resultToJSON (CS_COMPILED_SUCCESS l "") =
    jsonObject $ [jsonArrayElement "qpo" (lines l)]

  resultToJSON (CS_COMPILED_SUCCESS l w) =
    jsonObject $ [jsonArrayElement "qpo" (lines l),
                  jsonElement "warning" w]

  resultToJSON (CS_COMPILED_FAIL message) =
    jsonObject $ [jsonElement "compile_fail" message]

  resultToJSON (CS_NEED_FILE fileName) =
    jsonObject $ [jsonElement "get_file" fileName]

  surroundWithQuotes :: String -> String
  surroundWithQuotes = surroundWith '"' '"'

  surroundWithBraces :: String -> String
  surroundWithBraces = surroundWith '{' '}'

  surroundWithBrackets :: String -> String
  surroundWithBrackets = surroundWith '[' ']'

  surroundWith :: Char -> Char -> String -> String
  surroundWith c1 c2 s = c1:s ++ [c2]

  jsonObject :: [String] -> String
  jsonObject elements = surroundWithBraces $ concat $ intersperse ", " elements

  jsonElement :: String -> String -> String
  jsonElement key val = surroundWithQuotes key ++ " : " ++ surroundWithQuotes val

  jsonArrayElement :: String -> [String] -> String
  jsonArrayElement key val = surroundWithQuotes key ++ " : " ++
    surroundWithBrackets ( concat $ intersperse "," $ List.map surroundWithQuotes val )

  fp :: Map String (Maybe String) -> FileProvider
  fp imps = FileProvider {
    fpDoesFileExist = \ f -> do
          if f `elem` (keys imps)
            then return True
            else return False,
      fpReadFile = \f -> return "",
      emptyProvider = "",
      currentFPDir = "",
      fpcombine = (++),
      getFirstFileInSearchPath = \p f -> do
        if imps `haskey` f
          then do
            let fdata = imps ! f
            case fdata of
              Nothing   -> return Nothing
              Just fd   -> return $ Just (f,fd)
          else ioError $ userError $  "Need file "++f
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
    hPutStr stderr l
    if l == end
      then return $ reverse accum
      else hAccumLinesEndedBy h end (l:accum)


  compilerService ::  IORef (CompilerServiceStatus, String, Map String (Maybe String)) ->
                      String ->
                      IO CompilerServiceStatus
  compilerService ior "<qplprogram>" = do
    writeIORef ior (CS_READY, "", Map.empty)
    return CS_READY
  compilerService ior "</qplprogram>" = do
    tryCompiling ior
  compilerService ior "<sendversion />" = do
    setAndReturn ior $ CS_VERSION (versionBranch version) (versionTags version)
  compilerService ior a
    | "<file name='" == List.take 12 a = do
          let fname = List.takeWhile (/= '\'') $ drop 12 a
          (_,s,imps)<-readIORef ior
          if imps `haskey` fname
            then setAndReturn ior $ CS_READY
            else do
              writeIORef ior (CS_READY, s, Map.insert fname (Just "") imps)
              return $ CS_READY
    | "</file>" == a = do
          tryCompiling ior
    | otherwise = do
        (instatus,_,_) <- readIORef ior
        case instatus of
          CS_READY                    -> do
            modifyIORef ior (\(_, s,ims) -> (CS_READY, s++(a++"\n"), ims))
            return CS_READY
          _                           -> setAndReturn ior CS_READY

  setAndReturn :: IORef (CompilerServiceStatus, String, Map String (Maybe String)) ->
                  CompilerServiceStatus ->
                  IO CompilerServiceStatus
  setAndReturn ior cs = do
    (_,s,m) <- readIORef ior
    writeIORef ior (cs,s,m)
    return cs

  tryCompiling :: IORef (CompilerServiceStatus, String, Map String (Maybe String)) ->
                  IO CompilerServiceStatus
  tryCompiling ior = do
    (_,p,imps) <- readIORef ior
    errOrTxt <- CE.try $ doCompile (fp imps) p
    case errOrTxt of
      Left e    -> do
        let errString = ioeGetErrorString e
        if "Need file " == List.take 10 errString
          then setAndReturn ior $ CS_NEED_FILE $ drop 10 errString
          else setAndReturn ior $ CS_COMPILED_FAIL $ ioeGetErrorString e
      Right (txt,logs) -> setAndReturn ior $
        CS_COMPILED_SUCCESS (concat $ intersperse "\n" txt) (concat $ intersperse "\n" logs)

  appendJustWith :: [a] -> Maybe [a] -> Maybe [a]
  appendJustWith aas mas = do
    theas <- mas
    return $ theas ++ aas

  doCompile :: FileProvider -> String -> IO ([String],[String])
  doCompile fp p = do
    asyn <- parseQPL fp "" "" p []
    (ir,cls) <- W.runWriterT $ W.mapWriterT (removeState 0) (makeIr asyn)
    cd <- ioGenCode ir 0
    return (cd,cls)

  haskey mp k = k `elem` (keys mp)


\end{code}

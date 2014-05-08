\begin{code}
  module Main where
    import Test.Hspec.Core(Example(..),Result(..))
    import Test.Hspec
    import Test.Hspec.Runner
    import Test.Hspec.Formatters
    import Test.Hspec.QuickCheck
    import Test.Hspec.HUnit
    import Test.QuickCheck hiding (property)
    import Test.HUnit

    import Network.Socket
    import System.IO
    import System.IO.Error
    import System.Cmd
    import System.Exit

    import SpecHelper

    import Compiler.CompilerServer

    import Control.Concurrent

    import Control.Exception

    import Fixture.CompilerData

    main = do
      summary <- hspecWith defaultConfig{configFormatter=progress} compilerSpecs
      if summaryFailures summary > 0 then exitWith (ExitFailure $ summaryFailures summary)
                                     else exitWith ExitSuccess

    compilerSpecs = describe "compiler" $ do
      context "startup" $ do
        it ("runs on port "++default_port++" by default") $ do
            running <- checkOpenPort default_port
            if running
              then return True
              else do
                putStrLn "Not found - trying to start"
                rc <- system "lqpl-compiler-server &"
                putStrLn $ "Started - rc = "++ show rc
                threadDelay 2000
                putStrLn $ "Waited 2s for server to start"
                checkOpenPort default_port
      context "compiler server" $ do
        it "sends back a valid assembler code when sent a qpl program" $ do
              hndl <- connectToServer default_port
              hPutStrLn hndl program_one
              hFlush hndl
              res <- hGetLine hndl
              return $ if ((take 24 res) ==  (take 24 assembled_one))
                         then Test.Hspec.Core.Success
                         else Test.Hspec.Core.Fail $ "invalid program: " ++ res
        it "sends back a 'getFirst' request when sent a program with import" $ do
              hndl <- connectToServer default_port
              hPutStrLn hndl program_two
              hFlush hndl
              res <- hGetLine hndl
              return $ if (res == getfile_command)
                           then Test.Hspec.Core.Success
                           else Test.Hspec.Core.Fail $ "invalid import: " ++ res
        it "successfully compiles after a 'getFirst' request when sent a valid program" $ do
              hndl <- connectToServer default_port
              hPutStrLn hndl program_two
              hFlush hndl
              res <- hGetLine hndl
              if (res == getfile_command)
                 then do
                    hPutStrLn hndl program_one
                    hFlush hndl
                    fres <-hGetLine hndl
                    return $ if ((take 24 fres) ==  (take 24 assembled_one))
                               then Test.Hspec.Core.Success
                               else Test.Hspec.Core.Fail $ "invalid program after import: " ++ fres
                 else return $ Test.Hspec.Core.Fail $ "invalid import: " ++ res


    connectToServer :: String -> IO Handle
    connectToServer port = do
      addrinfo <- getAddrInfo
                  (Just (defaultHints {addrFlags = [AI_NUMERICSERV]})) Nothing (Just port)
      mhandle <- getHandle addrinfo
      case mhandle of
        Nothing   -> fail "Unable to connect to the compiler server"
        Just h    -> return h


    getHandle :: [AddrInfo] -> IO (Maybe Handle)
    getHandle [] = return (Nothing)
    getHandle (sa:rest) = do
      sock <- socket (addrFamily sa) Stream defaultProtocol
      setSocketOption sock KeepAlive 1
      catch (connect sock (addrAddress sa)) ignoreIOError --putStrLn ("gethandle" ++ (ioeGetErrorString err)))
      writable <- sIsWritable sock
      if (writable)
        then do
          h <- socketToHandle sock ReadWriteMode
          hSetBuffering h LineBuffering
          return (Just h)
        else getHandle rest

    ignoreIOError err =
      do let a = ioeGetErrorString err
         return ()


    checkOpenPort :: String -> IO Bool
    checkOpenPort port =
      do
        addrinfo <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_NUMERICSERV]})) Nothing (Just port)
        msock <- checkAddresses addrinfo
        case msock of
          Nothing   -> return False
          Just _    -> return True

    checkAddresses :: [AddrInfo] -> IO (Maybe Socket)
    checkAddresses [] = return (Nothing)
    checkAddresses (sa:rest) = do
      sock <- socket (addrFamily sa) Stream defaultProtocol
      setSocketOption sock KeepAlive 1
      catch (connect sock (addrAddress sa)) ignoreIOError --putStrLn ("checkaddresses" ++ (ioeGetErrorString err)))
      --bnd <- sIsBound sock
      --putStrLn $ "Socket bound? " ++ show bnd
      writable <- sIsWritable sock
      --putStrLn $ "Socket writable? " ++ show writable
      if (writable)
        then return (Just sock)
        else checkAddresses rest
\end{code}

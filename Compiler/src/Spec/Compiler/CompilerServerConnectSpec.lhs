\begin{code}
  module Main where
    import Test.Hspec.Core(Example(..),Result(..))
    import Test.Hspec.Monadic
    import Test.Hspec.QuickCheck
    import Test.Hspec.HUnit
    import Test.QuickCheck hiding (property)
    import Test.HUnit

    import Network.Socket
    import System.IO
    import System.IO.Error
    import System.Cmd

    import Spec.SpecHelper

    import Compiler.CompilerServer

    import Control.Concurrent
    
    import Control.Exception



    main = do
      hspecX compilerSpecs

    compilerSpecs = describe "compiler" $ do
      context "startup" $ do
        it ("runs on port "++default_port++" by default") $ do
            putStrLn "Checking open port"
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
        it "accepts the XML tag 'qplprogram'" $ do
              hndl <- connectToServer default_port
              hPutStrLn hndl "<qplprogram>"
              hFlush hndl
              res <- hGetLine hndl
              case res of
                "CS_READY"  -> return Test.Hspec.Core.Success
                _     -> return $ Test.Hspec.Core.Fail $ "invalid back status: " ++ res
        it "sends back a valid assembler code when sent a qpl program" $ do
              hndl <- connectToServer default_port
              hPutStrLn hndl "<qplprogram>"
              hFlush hndl
              hGetLine hndl
              hPutStrLn hndl "qdata C = {H|T}"
              hFlush hndl
              hGetLine hndl
              hPutStrLn hndl "app::(| ; )= {skip}"
              hFlush hndl
              hGetLine hndl
              hPutStrLn hndl "</qplprogram>"
              hFlush hndl
              fres <- hGetLine hndl
              res <- hGetLine hndl
              case res of
                "app_fcdlbl0   Start"   -> return Test.Hspec.Core.Success
                _                       -> return $ Test.Hspec.Core.Fail $ "invalid program: " ++ res
        it "sends back a 'getFirst' request when sent a program with import" $ do
              hndl <- connectToServer default_port
              hPutStrLn hndl "<qplprogram>"
              hFlush hndl
              hGetLine hndl
              hPutStrLn hndl "#Import f"
              hFlush hndl
              hGetLine hndl
              hPutStrLn hndl "</qplprogram>"
              hFlush hndl
              res <- hGetLine hndl
              case res of
                "<getFirst>f</getFirst>"    -> return Test.Hspec.Core.Success
                _                           -> return $ Test.Hspec.Core.Fail $ "invalid import: " ++ res
        it "successfully compiles after a 'getFirst' request when sent a valid program" $ do
              hndl <- connectToServer default_port
              hPutStrLn hndl "<qplprogram>"
              hFlush hndl
              hGetLine hndl
              hPutStrLn hndl "#Import f"
              hFlush hndl
              hGetLine hndl
              hPutStrLn hndl "</qplprogram>"
              hFlush hndl
              res <- hGetLine hndl
              case res of
                "<getFirst>f</getFirst>"    -> do
                    hPutStrLn hndl "<file name='f'>"
                    hFlush hndl
                    hGetLine hndl
                    hPutStrLn hndl "qdata C = {H|T}"
                    hFlush hndl
                    hGetLine hndl
                    hPutStrLn hndl "app::(| ; )= {skip}"
                    hFlush hndl
                    hGetLine hndl
                    hPutStrLn hndl "</file>"
                    hFlush hndl
                    fres <- hGetLine hndl
                    fres2 <-hGetLine hndl
                    case fres2 of
                      "app_fcdlbl0   Start"   -> return Test.Hspec.Core.Success
                      _                       -> return $ Test.Hspec.Core.Fail $ "invalid program after import: " ++ fres2
                _                           -> return $ Test.Hspec.Core.Fail $ "invalid import: " ++ res


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
      catch (connect sock (addrAddress sa)) (\ err -> putStrLn (ioeGetErrorString err))
      writable <- sIsWritable sock
      if (writable)
        then do
          h <- socketToHandle sock ReadWriteMode
          hSetBuffering h LineBuffering
          return (Just h)
        else getHandle rest


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
      putStrLn $ "addressinfo="++show sa
      sock <- socket (addrFamily sa) Stream defaultProtocol
      setSocketOption sock KeepAlive 1
      catch (connect sock (addrAddress sa)) (\ err -> putStrLn (ioeGetErrorString err))
      --bnd <- sIsBound sock
      --putStrLn $ "Socket bound? " ++ show bnd
      writable <- sIsWritable sock
      --putStrLn $ "Socket writable? " ++ show writable
      if (writable)
        then return (Just sock)
        else checkAddresses rest
\end{code}

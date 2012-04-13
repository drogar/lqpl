\begin{code}
  module Main where
    import Test.Hspec
    import Test.Hspec.Core
    import Test.Hspec.QuickCheck
    import Test.Hspec.HUnit
    import Test.QuickCheck hiding (property)
    import Test.HUnit

    import Network.Socket
    import System.IO
    import System.Cmd

    import Spec.SpecHelper

    import Compiler.CompilerServer

    import Control.Concurrent



    main = do
      hspecX compilerSpecs

    compilerSpecs = describe "compiler" [
      context "startup" [
        it ("runs on port "++default_port++" by default") $
          do
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
        ],
      context "compiler server" [
        it ("opens a port by default at port" ++ default_port)  $ checkOpenPort default_port,
        it "accepts the XML tag 'qplprogram' containing valid LQPL programs"    $
           (do
              hndl <- connectToServer default_port
              hPutStrLn hndl "<qplprogram>"
              hPutStrLn hndl "qdata C = {H|T}"
              hPutStrLn hndl "</qplprogram>"
              hFlush hndl
              res <- hGetLine hndl
              case res of
                "CS_READY"  -> return Test.Hspec.Core.Success
                _     -> return $ Test.Hspec.Core.Fail $ "invalid back status: " ++ res),
        it "sends back a valid assembler code when sent a qpl program" $
          (do
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
              hGetLine hndl
              hPutStrLn hndl "<sendresult />"
              hFlush hndl
              hGetLine hndl
              res <- hGetLine hndl
              case res of
                "app_fcdlbl0   Start"   -> return Test.Hspec.Core.Success
                _                       -> return $ Test.Hspec.Core.Fail $ "invalid program: " ++ res)
        ]
      ]


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
      catch (connect sock (addrAddress sa)) (\ err -> putStrLn (show err))
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
      catch (connect sock (addrAddress sa)) (\ err -> putStrLn (show err))
      --bnd <- sIsBound sock
      --putStrLn $ "Socket bound? " ++ show bnd
      writable <- sIsWritable sock
      --putStrLn $ "Socket writable? " ++ show writable
      if (writable)
        then return (Just sock)
        else checkAddresses rest
\end{code}

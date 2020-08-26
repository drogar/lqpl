\begin{code}
  module Lqpl.Compiler.CompilerServerConnectSpec(spec) where
    import Test.Hspec

    import Network.Socket hiding (defaultPort)
    import System.IO
    import System.IO.Error
    import System.Process

    import Lqpl.Compiler.CompilerServer

    import Control.Concurrent

    import Control.Exception

    import Fixture.CompilerData

    spec:: SpecWith()
    spec = compilerSpecs
    -- main = hspec compilerSpecs

    compilerSpecs:: SpecWith()
    compilerSpecs = describe "compiler server connection" $ do
      context "startup" $ do
        it ("runs on port "++defaultPort++" by default") $ do
            running <- checkOpenPort defaultPort
            if running
              then running `shouldBe` True
              else do
                putStrLn "Not found - trying to start"
                rc <- spawnCommand "lqpl-compiler-server &"
                putStrLn $ "Started - lqpl compiler server = "
                threadDelay 2000
                putStrLn $ "Waited 2s for server to start"
                checkOpenPort defaultPort `shouldReturn` True
      context "compiler server" $ do
        it "sends back a valid assembler code when sent a qpl program" $ do
              hndl <- connectToServer defaultPort
              hPutStrLn hndl program_one
              hFlush hndl
              res <- hGetLine hndl
              (take 24 res) `shouldBe` (take 24 assembled_one)
        it "sends back a 'getFirst' request when sent a program with import" $ do
              hndl <- connectToServer defaultPort
              hPutStrLn hndl program_two
              hFlush hndl
              res <- hGetLine hndl
              res `shouldBe` getfile_command
        it "successfully compiles after a 'getFirst' request when sent a valid program" $ do
              hndl <- connectToServer defaultPort
              hPutStrLn hndl program_two
              hFlush hndl
              res <- hGetLine hndl
              res `shouldBe` getfile_command
              hPutStrLn hndl program_one
              hFlush hndl
              fres <-hGetLine hndl
              (take 24 fres) `shouldBe`  (take 24 assembled_one)


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
      thePort <- socketPortSafe sock
      case thePort of
        Just _ -> do
          h <- socketToHandle sock ReadWriteMode
          hSetBuffering h LineBuffering
          return (Just h)
        Nothing -> getHandle rest

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
      thePort <- socketPortSafe sock
      case thePort of
        Just _  -> return (Just sock)
        Nothing -> checkAddresses rest
\end{code}

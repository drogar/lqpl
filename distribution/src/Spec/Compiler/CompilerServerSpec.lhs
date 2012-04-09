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
                checkOpenPort default_port
        ],
      context "compiler server" [
        it ("opens a port by default at port" ++ default_port)  $ checkOpenPort default_port,
        it "accepts the XML tag 'qplprogram' containing text"    $
          pending "This is input",
        it "sends back <sendmore>f</sendmore> when sent <qplprogram>#Import f</qplprogram>" $
          pending "requests data for imports",
        it "accepts the XML tag 'sendresult' with no content"   $
          pending "Command to send qpo code back",
        it "writes the qpo code to the socket after getting the 'sendresult' tag"    $
          pending "May test writing separate from socket",
        it "compiles a qpl program enclosed in the xml tag <qplprogram>"   $
          pending "Compile done and saved in memory"
        ]
      ]


    checkOpenPort :: String -> IO Bool
    checkOpenPort port =
      do
        addrinfo <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_NUMERICSERV]})) (Just "localhost") (Just port)
        let serveraddr = head addrinfo
        putStrLn $ "addressinfo="++show serveraddr
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        bnd <- sIsBound sock
        putStrLn $ "Socket bound? " ++ show bnd
        writable <- sIsWritable sock
        putStrLn $ "Socket writable? " ++ show writable
        return $ bnd && writable

\end{code}

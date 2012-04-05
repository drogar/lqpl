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

    import Spec.SpecHelper

    instance Example (IO Bool) where
      evaluateExample f =
        do
          r <- f
          return $ if r then Test.Hspec.Core.Success else (Fail "Action was false")

    main = do
      hspecX compilerSpecs

    compilerSpecs = describe "compiler" [
      context "compiler server" [
        it "opens a port by default at port 7863"   $ checkOpenPort "7863",
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
        addrinfo <- getAddrInfo Nothing (Just "localhost") (Just port)
        let serveraddr = head addrinfo
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        bnd <- sIsBound sock
        writable <- sIsWritable sock
        return $ bnd && writable

\end{code}

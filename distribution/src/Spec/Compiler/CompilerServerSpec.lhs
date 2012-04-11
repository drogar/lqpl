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
    import Data.IORef
    import System.Cmd

    import Spec.SpecHelper

    import Compiler.CompilerServer

    import Control.Concurrent



    main = do
      ior <- newIORef ""
      hspecX (compilerSpecs ior)

    compilerSpecs ior = describe "compiler server" [
      context "input commands" [
       it "accepts the XML tag 'qplprogram'"    $
           (do
              rc <- compilerService ior "<qplprogram>"
              case rc of
                CS_READY  -> return True
                _         -> return False),
       it "empties the held data when receiving the XML tag 'qplprogram'"    $
           (do
              writeIORef ior "test data"
              compilerService ior "<qplprogram>"
              tdata <- readIORef ior
              case tdata of
                ""  -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Ioref not emptied: '"++a++"'"),
       it "sets a ready status for each line after receiving the XML tag 'qplprogram'"    $
           (do
              compilerService ior "<qplprogram>"
              rc0 <- compilerService ior "something"
              rc1 <- compilerService ior "something"
              rc2 <- compilerService ior "something"
              case [rc0,rc1,rc2] of
                [CS_READY,CS_READY,CS_READY]  -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Status mismatch '"++(show a)++"'"),
       it "sets a complete status after receiving the closing XML tag 'qplprogram'"    $
           (do
              compilerService ior "<qplprogram>"
              compilerService ior "something"
              compilerService ior "something"
              rc2 <- compilerService ior "</qplprogram>"
              case rc2 of
                CS_GOT_PROGRAM  -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Status mismatch '"++(show a)++"'"),
       it "accumulates the characters between a 'qplprogram' tag pair"    $
           (do
              compilerService ior "<qplprogram>"
              compilerService ior "something"
              compilerService ior "</qplprogram>"
              tdata <- readIORef ior
              case tdata of
                "something\n"  -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Ioref not filled with 'something\\n': '"++a++"'"),
       it "accumulates multiple lines between a 'qplprogram' tag pair"    $
           (do
              compilerService ior "<qplprogram>"
              compilerService ior "something"
              compilerService ior "else"
              compilerService ior "</qplprogram>"
              tdata <- readIORef ior
              case tdata of
                "something\nelse\n"  -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Ioref not filled with 'something\\nelse\\n': '"++a++"'"),
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



\end{code}

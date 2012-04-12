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
    import Spec.Compiler.CompilerSpecHelper

    cstester = compilerService fpForTest

    main = do
      ior <- newIORef ""
      hspecX (compilerSpecs ior)

    compilerSpecs ior = describe "compiler server" [
      context "input commands" [
       it "accepts the XML tag 'qplprogram'"    $
           (do
              rc <- cstester ior "<qplprogram>"
              case rc of
                CS_READY  -> return True
                _         -> return False),
       it "empties the held data when receiving the XML tag 'qplprogram'"    $
           (do
              writeIORef ior "test data"
              cstester ior "<qplprogram>"
              tdata <- readIORef ior
              case tdata of
                ""  -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Ioref not emptied: '"++a++"'"),
       it "sets a ready status for each line after receiving the XML tag 'qplprogram'"    $
           (do
              cstester ior "<qplprogram>"
              rc0 <- cstester ior "something"
              rc1 <- cstester ior "something"
              rc2 <- cstester ior "something"
              case [rc0,rc1,rc2] of
                [CS_READY,CS_READY,CS_READY]  -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Status mismatch '"++(show a)++"'"),
       it "sets a complete status after receiving the closing XML tag 'qplprogram'"    $
           (do
              cstester ior "<qplprogram>"
              cstester ior "something"
              cstester ior "something"
              rc2 <- cstester ior "</qplprogram>"
              case rc2 of
                CS_GOT_PROGRAM  -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Status mismatch '"++(show a)++"'"),
       it "accumulates the characters between a 'qplprogram' tag pair"    $
           (do
              cstester ior "<qplprogram>"
              cstester ior "something"
              cstester ior "</qplprogram>"
              tdata <- readIORef ior
              case tdata of
                "something\n"  -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Ioref not filled with 'something\\n': '"++a++"'"),
       it "accumulates multiple lines between a 'qplprogram' tag pair"    $
           (do
              cstester ior "<qplprogram>"
              cstester ior "something"
              cstester ior "else"
              cstester ior "</qplprogram>"
              tdata <- readIORef ior
              case tdata of
                "something\nelse\n"  -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Ioref not filled with 'something\\nelse\\n': '"++a++"'"),
        it "accepts the XML tag 'sendresult' with no content"   $
           (do
              rc <- cstester ior "<sendresult />"
              case rc of
                CS_COMPILED_SUCCESS _ -> return True
                CS_COMPILED_FAIL  _ -> return True
                _         -> return False),
        it "sends back a compiled program when sent <qplprogram>#Import f</qplprogram> and then sent <sendresult />" $
          (do
              cstester ior "<qplprogram>"
              cstester ior "#Import f"
              cstester ior "</qplprogram>"
              rc <- cstester ior "<sendresult />"
              case rc of
                CS_COMPILED_SUCCESS  "app_fcdlbl0   Start\nEnScope\nDeScope\n    Return 0\n   EndProc" -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Got "++ show a)
        ]
      ]



\end{code}

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
    import Data.IORef
    import System.Cmd

    import Spec.SpecHelper

    import Compiler.CompilerServer

    import Control.Concurrent
    import Spec.Compiler.CompilerSpecHelper

    import Data.Version
    import Data.Map
    import Paths_lqpl

    cstester = compilerService


    main = do
      ior <- newIORef (CS_READY, "", empty)
      hspec (compilerSpecs ior)

    compilerSpecs ior = describe "compiler server" $ do
      context "input commands" $ do
        it "accepts the XML tag 'qplprogram'"    $ do
              rc <- cstester ior "<qplprogram>"
              case rc of
                CS_READY  -> return True
                _         -> return False
        it "empties the held data when receiving the XML tag 'qplprogram'"    $ do
              writeIORef ior (CS_READY, "test data",singleton "x" Nothing)
              cstester ior "<qplprogram>"
              tdata <- readIORef ior
              case tdata of
                (CS_READY, "",empty)  -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Ioref not emptied: '"++show a++"'"
        it "sets a ready status for each line after receiving the XML tag 'qplprogram'"    $ do
              rc<- cstester  ior "<qplprogram>"
              rc0 <- cstester ior "something"
              rc1 <- cstester ior "something"
              rc2 <- cstester ior "something"
              case [rc0,rc1,rc2] of
                [CS_READY,CS_READY,CS_READY]  -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Status mismatch '"++(show a)++"'"
        it "sets a complete status after receiving the closing XML tag 'qplprogram'"    $ do
              cstester ior "<qplprogram>"
              cstester ior "qdata C = {H|T}"
              cstester ior "app::(| ; )= {skip}"
              rc2 <- cstester ior "</qplprogram>"
              case rc2 of
                CS_COMPILED_SUCCESS  _ _-> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Status mismatch '"++(show a)++"'"
        it "accumulates the characters between a 'qplprogram' tag pair"    $ do
              writeIORef ior (CS_READY, "", empty)
              cstester ior "<qplprogram>"
              cstester ior "something"
              (_,tdata,_) <- readIORef ior
              case tdata of
                "something\n"  -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Ioref not filled with 'something\\n': '"++a++"'"
        it "accumulates multiple lines between a 'qplprogram' tag pair"    $ do
              writeIORef ior (CS_READY, "", empty)
              cstester ior "<qplprogram>"
              cstester ior "something"
              cstester ior "else"
              (_,tdata,_) <- readIORef ior
              case tdata of
                "something\nelse\n"  -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Ioref not filled with 'something\\nelse\\n': '"++a++"'"
        it "accepts the XML tag 'sendversion' with no content"   $ do
              rc <- cstester ior "<sendversion />"
              case rc of
                CS_VERSION _ _-> return True
                _         -> return False
        it "sends back '[d,d,d] []' when sent <sendversion />" $ do
              rc <- cstester ior "<sendversion />"
              case rc of
                CS_VERSION  vs tgs -> do
                  if vs == (versionBranch version) && tgs == (versionTags version)
                    then return Test.Hspec.Core.Success
                    else return $ Test.Hspec.Core.Fail $ "incorrect version, should be"++ showVersion version ++ "but was" ++ (showList vs $ showList tgs "")
                a         -> return $ Test.Hspec.Core.Fail $ "Got "++ show a
        it "adds the key 'f' when sent <file name='f'>" $ do
              writeIORef ior ((CS_NEED_FILE "f"), "", empty)
              cstester  ior "<file name='f'>"
              (_,_,imps) <- readIORef ior
              return $ if imps `haskey` "f"
                          then Test.Hspec.Core.Success
                          else Test.Hspec.Core.Fail $ "key 'f' not added"
        it "sends status CS_READING_FILE 'f' when sent <file name='f'>" $ do
              writeIORef ior ((CS_NEED_FILE "f"), "", empty)
              rc <- cstester ior "<file name='f'>"
              case rc of
                CS_READING_FILE  (Just "f")   -> return Test.Hspec.Core.Success
                a                             -> return $ Test.Hspec.Core.Fail $ "Got "++ show a
        it "sends status CS_READING_FILE 'f' when sent <file name='f'> followed by other lines" $ do
              writeIORef ior ((CS_NEED_FILE "f"), "", empty)
              cstester  ior "<file name='f'>"
              cstester ior "some line "
              rc <- cstester ior "of data"
              case rc of
                CS_READING_FILE (Just "f") -> return Test.Hspec.Core.Success
                a                   -> return $ Test.Hspec.Core.Fail $ "Got "++ show a
        it "adds the data between <file name='f'> and </file> to the map" $ do
              writeIORef ior ((CS_NEED_FILE "f"), "", empty)
              cstester ior "<file name='f'>"
              cstester  ior "line 1"
              cstester  ior "line 2"
              cstester ior "</file>"
              (_,_,imps) <- readIORef ior
              if imps ! "f" == Just "line 1\nline 2\n"
                then return Test.Hspec.Core.Success
                else return $ Test.Hspec.Core.Fail $ "'f' points to "++show (imps ! "f")
        it "leaves the data for 'f' if sent another request" $ do
              writeIORef ior ((CS_NEED_FILE "f"), "", empty)
              cstester  ior "<file name='f'>"
              cstester ior "line 1"
              cstester ior "line 2"
              cstester ior "</file>"
              cstester ior "<file name='f'>"
              (_,_,imps) <- readIORef ior
              if imps ! "f" == Just "line 1\nline 2\n"
                then return Test.Hspec.Core.Success
                else return $ Test.Hspec.Core.Fail $ "'f' points to "++show (imps ! "f")
        it "returns a status of reading Nothing if sent another request for the same file" $ do
              writeIORef ior ((CS_NEED_FILE "f"), "", empty)
              cstester ior "<file name='f'>"
              cstester ior "line 1"
              cstester ior "line 2"
              cstester ior "</file>"
              rc <- cstester ior "<file name='f'>"
              case rc of
                CS_READING_FILE (Nothing) -> return Test.Hspec.Core.Success
                a                   -> return $ Test.Hspec.Core.Fail $ "Got "++ show a
        it "sends back a Need File when sent <qplprogram>#Import f</qplprogram>" $ do
              cstester ior "<qplprogram>"
              cstester ior "#Import f"
              rc <- cstester ior "</qplprogram>"
              case rc of
                CS_NEED_FILE "f" -> return Test.Hspec.Core.Success
                a                -> return $ Test.Hspec.Core.Fail $ "Got "++ show a
        it "sends back a compiled ok when sent <qplprogram>#Import f</qplprogram> and then the file f" $ do
              cstester ior "<qplprogram>"
              cstester ior "#Import f"
              cstester ior "</qplprogram>"
              cstester ior "<file name='f'>"
              cstester ior "qdata C = {H|T}"
              cstester ior "app::(| ; )= {skip}"
              rc2 <- cstester ior "</file>"
              case rc2 of
                CS_COMPILED_SUCCESS _ _-> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Status mismatch '"++(show a)++"'"
        it "sends back an assembled file when sent compilable info with imports" $ do
              cstester ior "<qplprogram>"
              cstester ior "#Import f"
              cstester ior "</qplprogram>"
              cstester ior "<file name='f'>"
              cstester ior "qdata C = {H|T}"
              cstester ior "app::(| ; )= {skip}"
              rc2 <- cstester ior "</file>"
              case rc2 of
                CS_COMPILED_SUCCESS ('a':'p':'p':'_':_) _ -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "compile mismatch '"++(show a)++"'"
        it "sends back compile failed status with the error when the code has a syntax error" $ do
              cstester ior "<qplprogram>"
              cstester ior "qdata Coin = {head}"
              rc2 <- cstester ior "</qplprogram>"
              case rc2 of
                CS_COMPILED_FAIL ('(':'l':'i':_) -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "compile mismatch '"++(show a)++"'"
        it "sends back compile failed status with the error when the code has a semantic error" $ do
              cstester ior "<qplprogram>"
              cstester ior "main::() = { q = |0>; purejunk q}"
              rc2 <- cstester ior "</qplprogram>"
              case rc2 of
                CS_COMPILED_FAIL ('S':'e':'m':'a':_) -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "compile mismatch '"++(show a)++"'"
        it "sends back compile passed status with the error when the code has a creation balance error" $ do
              cstester ior "<qplprogram>"
              cstester ior "main::() = { q = |0>; measure q of |0> => {c=1} |1> => {d=2}}"
              rc2 <- cstester ior "</qplprogram>"
              case rc2 of
                CS_COMPILED_SUCCESS _ ('S':'e':'m':'a':_) -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "compile mismatch '"++(show a)++"'"

\end{code}

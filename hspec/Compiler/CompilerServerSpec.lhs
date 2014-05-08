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
    import Data.IORef
    import System.Cmd
    import System.Exit

    import SpecHelper

    import Compiler.CompilerServer

    import Control.Concurrent
    import Compiler.CompilerSpecHelper

    import Data.Version
    import Data.Map
    import Data.Aeson
    import qualified Data.ByteString.Char8 as B

    import Paths_lqpl

    cstester = compilerService
    program_one = "{ \"file_name\" : \"f\", \"qpl_program\" : [ \"qdata C = {H|T}\", \"app::(| ; )= {skip}\"] }"
    program_two = "{ \"file_name\" : \"g\", \"qpl_program\" : [ \"#Import f\"] }"
    program_bad = "{ \"file_name\" : \"g\", \"qpl_program\" : [ \"qdata Coin = {head}\"] }"
    program_bad2 = "{ \"file_name\" : \"g\", \"qpl_program\" : [ \"main::() = { q = |0>; purejunk q}\"] }"
    program_bad3 = "{ \"file_name\" : \"g\", \"qpl_program\" : [ \"main::() = { q = |0>; measure q of |0> => {c=1} |1> => {d=2}}\"] }"

    assembled_one = "{ \"qpo\" : [ \"app_fcdlbl0   Start\"] }"

    jsonSendVersion = "{\"command\" : \"send_version\"}"


    main = do
      ior <- newIORef (CS_READY, "", empty)
      summary <- hspecWith defaultConfig{configFormatter=progress} (compilerSpecs ior)
      if summaryFailures summary > 0 then exitWith (ExitFailure $ summaryFailures summary)
                                     else exitWith ExitSuccess

    compilerSpecs ior = describe "compiler server" $ do
      context "resultToJSON" $ do
        it "creates a single object with the code on success" $ do
          resultToJSON (CS_COMPILED_SUCCESS "t" "") `shouldBe` "{\"qpo\" : [\"t\"]}"
        it "creates a single object with the the code split into lines on success" $ do
          resultToJSON (CS_COMPILED_SUCCESS "t\nu\nv" "") `shouldBe` "{\"qpo\" : [\"t\",\"u\",\"v\"]}"
        it "creates a single object plus a warning if success with warning" $ do
          resultToJSON (CS_COMPILED_SUCCESS "t" "w") `shouldBe` "{\"qpo\" : [\"t\"], \"warning\" : \"w\"}"
        it "creates a single object with a single warning even if multiple lines in the warning" $ do
          resultToJSON (CS_COMPILED_SUCCESS "t" "w\nu") `shouldBe` "{\"qpo\" : [\"t\"], \"warning\" : \"w\nu\"}"
        it "creates a single object if the compile fails" $ do
          resultToJSON (CS_COMPILED_FAIL "t\nu") `shouldBe` "{\"compile_fail\" : \"t\nu\"}"
        it "creates a request object for a file if given" $ do
          resultToJSON (CS_NEED_FILE "t") `shouldBe` "{\"get_file\" : \"t\"}"
      context "qpl file" $ do
        it "should parse a single line file" $ do
          ((decodeStrict $ B.pack program_bad):: Maybe QPLFile) `shouldBe`
                      (Just $  QPLFile "g" ["qdata Coin = {head}"])
        it "should parse a multi line file" $ do
          ((decodeStrict $ B.pack program_one):: Maybe QPLFile) `shouldBe`
                                    (Just $ QPLFile "f" ["qdata C = {H|T}", "app::(| ; )= {skip}"])
        it "should fail on non file inputs" $ do
          ((decodeStrict $ B.pack jsonSendVersion):: Maybe QPLFile) `shouldBe` Nothing
      context "input commands" $ do
        it "accepts a complete file and responds with the QPO"    $ do
              rc <- cstester ior program_one
              case rc of
                CS_COMPILED_SUCCESS _ _  -> return True
                _                        -> return False
        it "accepts the json 'sendversion'"   $ do
              rc <- cstester ior jsonSendVersion
              case rc of
                CS_VERSION _ _-> return True
                _             -> return False
        it "sends back '[d,d,d] []' when sent json 'sendversion'" $ do
              rc <- cstester ior jsonSendVersion
              case rc of
                CS_VERSION  vs tgs -> do
                  if vs == (versionBranch version) && tgs == (versionTags version)
                    then return Test.Hspec.Core.Success
                    else return $ Test.Hspec.Core.Fail $ "incorrect version, should be" ++
                      showVersion version ++ "but was" ++ (showList vs $ showList tgs "")
                a         -> return $ Test.Hspec.Core.Fail $ "Got "++ show a
        it "adds the key 'f' when sent <file name='f'>" $ do
              writeIORef ior ((CS_NEED_FILE "f"), "", empty)
              cstester  ior program_one
              (_,_,imps) <- readIORef ior
              return $ if imps `haskey` "f"
                          then Test.Hspec.Core.Success
                          else Test.Hspec.Core.Fail $ "key 'f' not added"
        it "sends back a Need File when sent a program with an #Import" $ do
              rc <- cstester ior program_two
              case rc of
                CS_NEED_FILE "f" -> return Test.Hspec.Core.Success
                a                -> return $ Test.Hspec.Core.Fail $ "Got "++ show a
        it "sends back a compiled ok when sent #Import f and then the file f" $ do
              cstester ior program_two
              rc2 <- cstester ior program_one
              case rc2 of
                CS_COMPILED_SUCCESS _ _-> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "Status mismatch '"++(show a)++"'"
        it "sends back an assembled file when sent compilable info with imports" $ do
              cstester ior program_two
              rc2 <- cstester ior program_one
              case rc2 of
                CS_COMPILED_SUCCESS ('a':'p':'p':'_':_) _ -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "compile mismatch '"++(show a)++"'"
        it "sends back compile failed status with the error when the code has a syntax error" $ do
              rc2 <- cstester ior program_bad
              case rc2 of
                CS_COMPILED_FAIL ('(':'l':'i':_) -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "compile mismatch '"++(show a)++"'"
        it "sends back compile failed status with the error when the code has a semantic error" $ do
              rc2 <- cstester ior program_bad2
              case rc2 of
                CS_COMPILED_FAIL ('S':'e':'m':'a':_) -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "compile mismatch '"++(show a)++"'"
        it "sends back compile passed status with the error when the code has a creation balance error" $ do
              rc2 <- cstester ior program_bad3
              case rc2 of
                CS_COMPILED_SUCCESS _ ('S':'e':'m':'a':_) -> return Test.Hspec.Core.Success
                a         -> return $ Test.Hspec.Core.Fail $ "compile mismatch '"++(show a)++"'"

\end{code}

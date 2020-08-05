\begin{code}
  module Lqpl.Compiler.CompilerServerSpec(spec) where
    import Test.Hspec.Core.Spec(Example(..),Result(..), FailureReason(..))
    import Test.Hspec
    import Test.Hspec.Runner
    import Test.Hspec.Formatters
    import Test.Hspec.QuickCheck
    import Test.QuickCheck hiding (property)

    import Network.Socket
    import System.IO
    import Data.IORef

    import System.Exit

    import SpecHelper

    import Lqpl.Compiler.CompilerServer

    import Control.Concurrent
    import Lqpl.Compiler.CompilerSpecHelper

    import Data.Version
    import Data.Map
    import Data.Aeson
    import qualified Data.ByteString.Char8 as B

    import Fixture.CompilerData
    import Paths_lqpl_spec

    checkCompileStatus rc = do
      case rc of
        CS_COMPILED_SUCCESS _ _  -> 1 `shouldBe` 1
        CS_COMPILED_FAIL m       -> expectationFailure m
        a                        -> expectationFailure $ show a

    cstester = compilerService

--    main = do
  --    ior <- newIORef (empty)
    --  hspec (compilerSpecs ior)

    withIORef = newIORef (empty)

    spec = compilerSpecs
    compilerSpecs = do
        describe "compiler server" $ do
          context "resultToJSON" $ do
            it "creates a single object with the code on success" $ do
              resultToJSON (CS_COMPILED_SUCCESS "t" "") `shouldBe` "{\"qpo\" : [\"t\"]}"
            it "creates a single object with the the code split into lines on success" $ do
              resultToJSON (CS_COMPILED_SUCCESS "t\nu\nv" "") `shouldBe` "{\"qpo\" : [\"t\", \"u\", \"v\"]}"
            it "creates a single object plus a warning if success with warning" $ do
              resultToJSON (CS_COMPILED_SUCCESS "t" "w") `shouldBe` "{\"qpo\" : [\"t\"], \"warning\" : \"w\"}"
            it "creates a single object with a single warning even if multiple lines in the warning" $ do
              resultToJSON (CS_COMPILED_SUCCESS "t" "w\nu") `shouldBe` "{\"qpo\" : [\"t\"], \"warning\" : \"w\\nu\"}"
            it "creates a single object if the compile fails" $ do
              resultToJSON (CS_COMPILED_FAIL "t\nu") `shouldBe` "{\"compile_fail\" : \"t\\nu\"}"
            it "creates a request object for a file if given" $ do
              resultToJSON (CS_NEED_FILE "t") `shouldBe` "{\"send_file\" : \"t\"}"
          context "qpl file" $ do
            it "should parse a single line file" $ do
              ((decodeStrict $ B.pack program_bad):: Maybe QPLFile) `shouldBe`
                          (Just $  QPLFile "g" ["qdata Coin = {head}"])
            it "should parse a multi line file" $ do
              ((decodeStrict $ B.pack program_one):: Maybe QPLFile) `shouldBe`
                                        (Just $ QPLFile "f" ["qdata C = {H|T}", "app::(| ; )= {skip}"])
            it "should fail on non file inputs" $ do
              ((decodeStrict $ B.pack jsonSendVersion):: Maybe QPLFile) `shouldBe` Nothing
          context "command" $ do
            it "should parse a command object" $ do
              ((decodeStrict $ B.pack jsonSendVersion) :: Maybe CompilerCommand) `shouldBe`
                          (Just $ CompilerCommand "send_version")
            it "should return Nothing on a program input " $ do
              ((decodeStrict $ B.pack program_one):: Maybe CompilerCommand) `shouldBe` Nothing
          context "compiling from ior" $ do
            before withIORef $ do
              it "should return success when the map has a complete program" $ \ior -> do
                writeIORef ior (singleton compileMe "qdata C = {H|T}\napp::(| ; )= {skip}")
                rc <- tryCompiling ior
                checkCompileStatus rc
          context "input commands" $ do
            before withIORef $ do
              it "responds to bad input with a CS_ILLEGAL_INPUT" $ \ior -> do
                rc <- cstester ior "junk"
                case rc of
                  CS_ILLEGAL_INPUT "junk" -> 1 `shouldBe` 1
                  a                       -> expectationFailure $ show a
              it "accepts a complete file and responds with the QPO" $ \ior ->  do
                    rc <- cstester ior program_one
                    checkCompileStatus rc
              it "accepts the json 'sendversion'" $ \ior -> do
                    rc <- cstester ior jsonSendVersion
                    case rc of
                      CS_VERSION _ -> 1 `shouldBe` 1
                      _            -> expectationFailure "No version"
              it "sends back '[d,d,d] []' when sent json 'sendversion'" $ \ior -> do
                    rc <- cstester ior jsonSendVersion
                    case rc of
                      CS_VERSION  vs -> vs `shouldBe` (versionBranch version)
                      a         -> expectationFailure $ "Got "++ show a
              it "adds the key 'g' when sent a second file with name 'g' and still needing more" $ \ior -> do
                    writeIORef ior (empty)
                    cstester ior program_three
                    cstester ior program_two
                    imps <- readIORef ior
                    imps `shouldSatisfy` (\imps -> imps `haskey` (Just "g"))
              it "sends back a Need File when sent a program with an #Import" $ \ior -> do
                    rc <- cstester ior program_two
                    case rc of
                      CS_NEED_FILE "f" -> 1 `shouldBe` 1
                      a                -> expectationFailure $ "Got "++ show a
              it "sends back a compiled ok when sent #Import f and then the file f" $ \ior -> do
                    cstester ior program_two
                    rc2 <- cstester ior program_one
                    checkCompileStatus rc2
              it "sends back an assembled file when sent compilable info with imports" $ \ior -> do
                    cstester ior program_two
                    rc2 <- cstester ior program_one
                    case rc2 of
                      CS_COMPILED_SUCCESS ('a':'p':'p':'_':_) _ -> 1 `shouldBe` 1
                      a         -> expectationFailure $ "compile mismatch '"++(show a)++"'"
              it "sends back compile failed status with the error when the code has a syntax error" $ \ior -> do
                    rc2 <- cstester ior program_bad
                    case rc2 of
                      CS_COMPILED_FAIL ('(':'l':'i':_) -> 1 `shouldBe` 1
                      a         -> expectationFailure $ "compile mismatch '"++(show a)++"'"
              it "sends back compile failed status with the error when the code has a semantic error" $ \ior -> do
                    rc2 <- cstester ior program_bad2
                    case rc2 of
                      CS_COMPILED_FAIL ('S':'e':'m':'a':_) -> 1 `shouldBe` 1
                      a         -> expectationFailure $ "compile mismatch '"++(show a)++"'"
              it "sends back compile passed status with the error when the code has a creation balance error" $ \ior -> do
                    rc2 <- cstester ior program_bad3
                    case rc2 of
                      CS_COMPILED_SUCCESS _ ('S':'e':'m':'a':_) -> 1 `shouldBe` 1
                      a         -> expectationFailure $ "compile mismatch '"++(show a)++"'"

\end{code}

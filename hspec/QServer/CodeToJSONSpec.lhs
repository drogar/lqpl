\begin{code}
  module QServer.CodeToJSONSpec(spec) where
    import Test.Hspec
    import Test.Hspec.Runner
    import Test.Hspec.Formatters
    import Test.Hspec.QuickCheck

    import SpecHelper

    import QSM.BasicData

    import Data.LazyNum
    import QSM.Components.Instructions
    import QServer.StackToJSON
    import Data.Map as Map
    import Data.List as List
    import System.Exit

    jsonValues :: [(Memory Basis, String)]
    jsonValues =  [(Map.singleton "main" [QDelete "q", QPullup "r"],
                  "{\"main\" : [\"QDelete\" : \"q\",\"QPullup\" : \"r\"]}")]


    --checkIt :: Memory Basis -> String -> SpecM ()
    checkIt cd res = it ("returns "++show cd++" as '"++res++"'") $ do
                         toJSON cd `shouldBe` res

    tests =  describe "Stack - CodeToJSON" $ mapM_ (uncurry checkIt) jsonValues


    spec = tests
    -- main = hspec tests

\end{code}

\begin{code}
  module Main where
    import Test.Hspec
    import Test.Hspec.Runner
    import Test.Hspec.Formatters
    import Test.Hspec.QuickCheck
    import Test.Hspec.HUnit
    import Test.QuickCheck hiding (property)
    import Test.HUnit


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
                  "{\"code\":[\"main\":[\"QDelete\":\"q\",\"QPullup\":\"r\"]]}")]


    --checkIt :: Memory Basis -> String -> SpecM ()
    checkIt cd res = it ("returns "++show cd++" as '"++res++"'") $ res ~=? (surroundWith "Code" $ toJSON cd)

    tests =  describe "StackToJSON" $ mapM_ (uncurry checkIt) jsonValues


    main = do
      summary <- hspecWith defaultConfig{configFormatter=progress} tests
      if summaryFailures summary > 0 then exitWith (ExitFailure $ summaryFailures summary)
                                     else exitWith ExitSuccess

\end{code}

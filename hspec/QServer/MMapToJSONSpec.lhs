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
    import QSM.Components.MemoryMap
    import QServer.StackToJSON
    import Data.Map as Map
    import Data.List as List
    import System.Exit

    jsonValues :: [([Map String Int], String)]
    jsonValues =  [([Map.singleton "p" 2, Map.singleton "r" 3],
                    "{\"mmap\":[{\"p\":2},{\"r\":3}]}"),
                   ([Map.singleton "p" 2], "{\"mmap\":[{\"p\":2}]}"),
                   ([Map.insert "r" 3 $ Map.singleton "p" 7, Map.singleton "p" 2],
                    "{\"mmap\":[{\"p\":7, \"r\":3},{\"p\":2}]}")]

    --checkIt :: MemoryMap -> String -> SpecM ()
    checkIt mm res = it ("returns "++show mm++" as '"++res++"'") $ res ~=? (listToJSON "MMap" mm)

    tests =  describe "StackToJSON" $ mapM_ (uncurry checkIt) jsonValues


    main = do
      summary <- hspecWith defaultConfig{configFormatter=progress} tests
      if summaryFailures summary > 0 then exitWith (ExitFailure $ summaryFailures summary)
                                     else exitWith ExitSuccess

\end{code}

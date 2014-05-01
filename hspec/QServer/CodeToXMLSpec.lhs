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
    import QServer.StackToXML
    import Data.Map as Map
    import Data.List as List

    xmlValues :: [(Memory Basis, String)]
    xmlValues =  [(Map.singleton "main" [QDelete "q", QPullup "r"],
        "<Code><map><kvpair><key><string>main</string></key><value><instructions><i>QDelete \"q\"</i><i>QPullup \"r\"</i></instructions></value></kvpair></map></Code>")]
                  -- May need to revise as order of maps is undefined....

    --checkIt :: Memory Basis -> String -> SpecM ()
    checkIt cd res = it ("returns "++show cd++" as '"++res++"'") $ res ~=? (surroundWith "Code" $ toXML cd)

    tests =  describe "StackToXML" $ mapM_ (uncurry checkIt) xmlValues


    main = hspecWith defaultConfig{configFormatter=progress} tests

\end{code}
\begin{code}
  module Main where
    import Test.Hspec
    import Test.Hspec.Core
    import Test.Hspec.QuickCheck
    import Test.Hspec.HUnit
    import Test.QuickCheck hiding (property)
    import Test.HUnit


    import Spec.SpecHelper

    import QSM.BasicData

    import Data.LazyNum
    import QSM.Components.MemoryMap
    import QServer.StackToXML
    import Data.Map as Map
    import Data.List as List

    xmlValues =  [([Map.singleton "p" 2], "<MMap><map><kvpair><key><string>p</string></key><value><int>2</int></value></kvpair></map></MMap>"),
                  ([Map.insert "r" 3 $ Map.singleton "p" 7, Map.singleton "p" 2], "<MMap><map><kvpair><key><string>p</string></key><value><int>7</int></value></kvpair><kvpair><key><string>r</string></key><value><int>3</int></value></kvpair></map><map><kvpair><key><string>p</string></key><value><int>2</int></value></kvpair></map></MMap>")]
                  -- May need to revise as order of maps is undefined....

    checkIt :: MemoryMap -> String -> [Spec]
    checkIt mm res = it ("returns "++show mm++" as '"++res++"'") $ res ~=? (listToXML "MMap" mm)

    tests =  describe "StackToXML" $ List.map (uncurry checkIt) xmlValues


    main = hspecX tests

\end{code}
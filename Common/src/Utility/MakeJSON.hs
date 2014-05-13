module Utility.MakeJSON where

    import qualified Data.List as List

    surroundWithQuotes :: String -> String
    surroundWithQuotes = surroundWith '"' '"'

    surroundWithBraces :: String -> String
    surroundWithBraces = surroundWith '{' '}'

    surroundWithBrackets :: String -> String
    surroundWithBrackets = surroundWith '[' ']'

    surroundWith :: Char -> Char -> String -> String
    surroundWith c1 c2 s = c1:s ++ [c2]

    jsonObject :: [String] -> String
    jsonObject elements = surroundWithBraces $ concat $ List.intersperse ", " elements

    jsonElement :: String -> String -> String
    jsonElement key val = surroundWithQuotes key ++ " : " ++ val

    jsonValueElement :: (Show a) => String -> a -> String
    jsonValueElement key val = surroundWithQuotes key ++ " : " ++ show val

    jsonArrayElement :: String -> [String] -> String
    jsonArrayElement key val = surroundWithQuotes key ++ " : " ++
       surroundWithBrackets ( concat $ List.intersperse "," $  val )

    jsonValueArrayElement :: (Show a) => String -> [a] -> String
    jsonValueArrayElement key val = surroundWithQuotes key ++ " : " ++
       surroundWithBrackets ( concat $ List.intersperse "," $ List.map show val )

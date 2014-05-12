module Utility.MakeJSON where

  surroundWithQuotes :: String -> String
  surroundWithQuotes = surroundWith '"' '"'

  surroundWithBraces :: String -> String
  surroundWithBraces = surroundWith '{' '}'

  surroundWithBrackets :: String -> String
  surroundWithBrackets = surroundWith '[' ']'

  surroundWith :: Char -> Char -> String -> String
  surroundWith c1 c2 s = c1:s ++ [c2]

  jsonObject :: [String] -> String
  jsonObject elements = surroundWithBraces $ concat $ intersperse ", " elements

  jsonElement :: String -> String -> String
  jsonElement key val = surroundWithQuotes key ++ " : " ++ surroundWithQuotes val

  jsonArrayElement :: String -> [String] -> String
  jsonArrayElement key val = surroundWithQuotes key ++ " : " ++
    surroundWithBrackets ( concat $ intersperse "," $ List.map surroundWithQuotes val )

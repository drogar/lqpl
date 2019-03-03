module Lqpl.Utility.MakeJSON where

    import qualified Data.List as List
    import qualified Data.Map as Map

    surroundWithQuotes :: String -> String
    surroundWithQuotes = surroundWith '"' '"'

    surroundWithBraces :: String -> String
    surroundWithBraces = surroundWith '{' '}'

    surroundWithBrackets :: String -> String
    surroundWithBrackets = surroundWith '[' ']'

    surroundWith :: Char -> Char -> String -> String
    surroundWith c1 c2 s = c1:s ++ [c2]

    jsonObject :: [String] -> String
    jsonObject elements = surroundWithBraces $ toCommaSepString elements

    jsonValueElementFromMap :: (Show a, Show b) => Map.Map a b -> String
    jsonValueElementFromMap  mapab = toCommaSepString $
                                  Map.foldrWithKey (\k x js -> (show k ++ " : " ++ show x) : js) [] mapab

    jsonBareObjectFromMap :: (Show a, Show b) => Map.Map a b -> String
    jsonBareObjectFromMap mapab = surroundWithBraces $ toCommaSepString $
                                  Map.foldrWithKey (\k x js -> (show k ++ " : " ++ show x) : js) [] mapab

    jsonElement :: String -> String -> String
    jsonElement key val = surroundWithQuotes key ++ " : " ++ val

    jsonValueElement :: (Show a) => String -> a -> String
    jsonValueElement key val = surroundWithQuotes key ++ " : " ++ show val

    jsonArrayElement :: String -> [String] -> String
    jsonArrayElement key val = surroundWithQuotes key ++ " : " ++
       surroundWithBrackets ( toCommaSepString  val )

    jsonValueArrayElement :: (Show a) => String -> [a] -> String
    jsonValueArrayElement key val = surroundWithQuotes key ++ " : " ++
       surroundWithBrackets (toCommaSepString $ List.map show val )


    toMultiLineString :: [String] -> String
    toMultiLineString s = List.intercalate "\n" s


    toCommaSepString :: [String] -> String
    toCommaSepString s = List.intercalate ", " s

\begin{code}
module Emulator.Data.Preferences(
                                    QsWindowPrefs(..),
                                    defaultCallDepth,
                                    qsPreferences,
                                    prefToFont
                                   ) 
where

import Graphics.UI.Gtk

import Data.IORef

import Data.List as List
import Data.Char


data QsWindowPrefs =
    QsWindowPrefs {
          nodeSize :: Int,
          nodeSeparation :: Int,
          nodeNameXOffset :: Int,
          nodeNameYOffset :: Int,
          showTrace :: Bool,
          traceXOffset :: Int,
          traceYOffset :: Int,
          fontName :: String,
          nameSize :: Int,
          branchLabelSize :: Int,
          elisionSize :: Int,
          traceSize :: Int,
          leafSize :: Int,
          tracePrecision :: Int,
          leafPrecision :: Int,
          branchLabelLayout :: Maybe PangoLayout,
          elisionLayout :: Maybe PangoLayout,
          traceLayout :: Maybe PangoLayout,
          nameLayout :: Maybe PangoLayout,
          leafLayout :: Maybe PangoLayout,
          logLevel :: Int,
          callDepth :: Int,
          epsilonForTrimming :: Maybe Int,
          trimRate :: Int
}




defaultCallDepth :: Int
defaultCallDepth = 1000

qsPreferences :: IO (IORef QsWindowPrefs)
qsPreferences = defaultControls defaultCallDepth >>= newIORef
                      

defaultControls :: Int -> IO QsWindowPrefs
defaultControls callDepth =
    do fmap <- cairoFontMapGetDefault
       families <- pangoFontMapListFamilies fmap
       let fixedFams = List.filter pangoFontFamilyIsMonospace families
       ffaces <- mapM pangoFontFamilyListFaces fixedFams
       fdescs <- mapM pangoFontFaceDescribe $ concat ffaces
       fstrings <- mapM fontDescriptionToString fdescs
       let pureFontStrings = List.filter notModifier fstrings
       --putStrLn "Resulting output font strings ..."
       --mapM_ putStrLn pureFontStrings
       return  $ QsWindowPrefs 10 45 (-9) (-9) True 10 (-6) (head pureFontStrings)
                  10 8 8 8 10 2 5 Nothing Nothing Nothing Nothing Nothing 0 callDepth Nothing 100


notModifier :: String -> Bool
notModifier = List.null . intersect ["Bold","Oblique","Italic"] . words


prefToFont :: QsWindowPrefs -> (QsWindowPrefs -> Int) -> IO FontDescription
prefToFont pr fld
    = let fdesc = words $ fontName pr
          lastWordIsDigits = and $ 
                             List.map (\ x -> isDigit x || isSpace x) $ 
                             last fdesc          
          fdescr = if lastWordIsDigits then unwords $ init fdesc
                   else fontName pr
      in fontDescriptionFromString (fdescr ++ " " ++ show (fld pr))

\end{code}

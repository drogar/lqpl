\begin{code}
  module Utility.Extras (
       qHead,
       splitFilePath,
       filterNonPrintable
  )where

  import System.FilePath
  import Data.Char

  qHead :: String -> [a] -> a
  qHead s [] = error s
  qHead _ (a:_) = a
                 

  splitFilePath :: FilePath -> (String,String,String)
  splitFilePath fp = let (path,name) = splitFileName fp
                         (bname,_:ext) = splitExtension name
                     in  (path,bname,ext)
                     
  filterNonPrintable :: String -> String
  filterNonPrintable []           =  []
  filterNonPrintable (c:rest)   
    | isPrint(c)  =  c:filterNonPrintable rest
    | otherwise   =  filterNonPrintable rest
                   
                   
\end{code}
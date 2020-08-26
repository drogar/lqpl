\begin{code}
  module Lqpl.Utility.FileProvider (
       FileProvider(..)
  )where

    data FileProvider = FileProvider {
      fpDoesFileExist :: String -> IO Bool,
      fpReadFile :: String -> IO String,
      currentFPDir :: String,
      emptyProvider :: String,
      getFirstFileInSearchPath :: [String] -> String -> IO (Maybe(String,String)),
      fpcombine :: String -> String -> String
    }

\end{code}

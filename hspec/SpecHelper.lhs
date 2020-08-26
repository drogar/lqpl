\begin{code}

  {-#LANGUAGE FlexibleInstances#-}
  {-# OPTIONS_GHC -Wno-orphans #-}

  module SpecHelper (
    getTempFile,
    getTempFileWithContent,
    removeTempFile
    )
  where

  import System.Posix.Process
  import System.Directory
  import System.FilePath

  --import Test.Hspec
  import Test.Hspec.Core.Spec(Example(..),Result(..), FailureReason(..), ResultStatus(..))

  getTempFileName :: String -> IO(String)
  getTempFileName name = do
      pid <- getProcessID
      tmp <- getTemporaryDirectory
      return $ tmp ++ pathSeparator:(show pid) ++ '-': name

  getTempFileWithContent :: String -> String -> IO(String)
  getTempFileWithContent name content = do
    fname <- getTempFileName name
    writeFile fname content
    return fname

  getTempFile :: String ->IO(String)
  getTempFile name  = getTempFileWithContent name ""

  removeTempFile :: String -> IO(String)
  removeTempFile name = do
    fname <- getTempFileName name
    removeFile fname
    return fname

  instance Example (IO Bool) where
    evaluateExample f _ _ _=
      do
        r <- f
        return $ Result "" $ if r then Success else (Failure Nothing $ Reason "Action was false")

  instance Example (IO Result) where
    evaluateExample  f _ _ _ =
      do
        r <- f
        return r

\end{code}

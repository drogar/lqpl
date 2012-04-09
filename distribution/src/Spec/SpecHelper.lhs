\begin{code}

  {-#LANGUAGE FlexibleInstances#-}
  module Spec.SpecHelper (
    getTempFile,
    getTempFileWithContent,
    removeTempFile,
    context
    )
  where


  import System.Process
  import System.Posix.Process
  import System.Directory
  import System.FilePath
  import System.IO
  import Test.Hspec
  import Test.Hspec.Core

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
    evaluateExample f =
      do
        r <- f
        return $ if r then Test.Hspec.Core.Success else (Fail "Action was false")

  context = describe

\end{code}
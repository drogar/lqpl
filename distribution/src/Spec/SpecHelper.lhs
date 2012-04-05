\begin{code}
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



  context = describe

\end{code}
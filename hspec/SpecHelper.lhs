\begin{code}

  {-#LANGUAGE FlexibleInstances#-}
  module SpecHelper (
    getTempFile,
    getTempFileWithContent,
    removeTempFile
#if MIN_VERSION_hspec(1,1,2)

#else
    , context
#endif
    )
  where


  import System.Process
  import System.Posix.Process
  import System.Directory
  import System.FilePath
  import System.IO
  --import Test.Hspec
  import Test.Hspec.Core(Example(..),Result(..))

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
        return $ if r then Test.Hspec.Core.Success else (Fail "Action was false")

  instance Example (IO Test.Hspec.Core.Result) where
    evaluateExample  f _ _ _ =
      do
        r <- f
        return r

#if MIN_VERSION_hspec(1,1,2)

#else
  context = describe
#endif

\end{code}

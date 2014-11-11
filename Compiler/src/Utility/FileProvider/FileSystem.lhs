\begin{code}
module Utility.FileProvider.FileSystem (
     fpFileSystem
) where

  import System.Directory
  import System.FilePath
  import Utility.FileProvider

  fpFileSystem :: FileProvider
  fpFileSystem = FileProvider {
    fpDoesFileExist = doesFileExist,
    fpReadFile      = readFile,
    emptyProvider   =  "",
    currentFPDir    = ".",
    fpcombine       =  combine,
    getFirstFileInSearchPath  = \ searchPath impFile -> do
      let (path, fn) = splitFileName  impFile
          ddirs = filter ( /= ".") ( path:searchPath)
      filenm <- getFirstFile ddirs fn
      case filenm of
        Nothing -> return Nothing
        Just filename -> do
          fcontents <- readFile filename
          return (Just (filename, fcontents))
    }


  getFirstFile :: [String] -> String -> IO (Maybe String)
  getFirstFile [] fname = do
    exists <- doesFileExist fname
    return (if exists then (Just  fname) else Nothing)
  getFirstFile (d:ds) fname = do
    let fn = combine d fname
    exists <- doesFileExist fn
    if exists then return (Just fn)
              else getFirstFile ds fname


\end{code}

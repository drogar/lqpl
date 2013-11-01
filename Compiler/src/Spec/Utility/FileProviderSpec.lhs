\begin{code}
  module Main where
    import Test.Hspec
    import Test.Hspec.Runner
    import Test.Hspec.Formatters
    import Test.Hspec.QuickCheck
    import Test.Hspec.HUnit
    import Test.QuickCheck hiding (property)
    import Test.HUnit

    import Spec.SpecHelper

    import Network.Socket
    import System.IO
    import Utility.FileProvider
    import Utility.FileProvider.FileSystem


    main = hspecWith defaultConfig{configFormatter=progress} fileProviderSpecs

    fileProviderSpecs = describe "FileProvider" $ do
      context "filesystem provider class implementation" $ do
         it "returns 'True' for a file that exists"
                (do  fname <- getTempFileWithContent "fileprovider" "test"
                     putStrLn fname
                     (fpDoesFileExist fpFileSystem) fname
                  )
         it "returns 'False' for a non-existent file"
                (do  fname <- removeTempFile "fileprovider"
                     x <- (fpDoesFileExist fpFileSystem)  fname
                     return $ not x
                  )
         it "returns the contents from a file"
                (do   let content = "test content\nwith two lines"
                      fname <- getTempFileWithContent "fileprovider" content
                      readcontent <- (fpReadFile fpFileSystem) fname
                      return $ readcontent == content
                      )
      context "Network connection file provider" $ do
              it "sends the message '<doesfileexist name=xyz />' to the socket "
                $ pending 
                
    dotOnFirst = [".", "abc"]
    dotOnSecond = ["first", ".", "abc"]
    dotOnLast =  ["first", "."]
    onlyDot =  ["."]
    emptyList = []
    noDotOneElem = ["one"]
    noDotTwoElems = ["one", "two"]

    checkOpenPort :: String -> IO Bool
    checkOpenPort port =
      do
        addrinfo <- getAddrInfo Nothing (Just "localhost") (Just port)
        let serveraddr = head addrinfo
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        bnd <- sIsBound sock
        writable <- sIsWritable sock
        return $ bnd && writable

\end{code}


\incsec{Compiler main functions}\label{incsec:compiler calling}

\begin{code}

module Compiler.Compiler (
                          doCompile
                         ) where

import System.Exit (exitWith, ExitCode(..))
import System.IO
import System.FilePath

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer

import Compiler.GenCode
import Compiler.Opts
import Compiler.QPLParser
import Compiler.Qtypes
import Compiler.Semantic
import Compiler.SymbolTable
import Compiler.BaseTypes

import Utility.FileProvider.FileSystem

import Data.Version
import Paths_lqpl

doCompile :: Bool -> [Flag]->(String, String, String)->
             WriterT CompilerLogs IO String
doCompile quietMode flgs (dir, fname, fext) =
    do let infile = addExtension fname fext
           inpath = combine dir infile
           outfile = addExtension fname "qpo"
           outpath = combine dir outfile
           ll = getLogLevel flgs
       pr <- liftIO $ readFile inpath
       printCond EchoCode flgs $ "Input file\n"++ pr
       asyn <- liftIO $ parseQPL fpFileSystem dir infile pr (getIncludePath flgs)
       printCond  Syntactic flgs $ show asyn
       ip <- mapWriterT (removeState ll) (makeIr asyn)
       printCond IRoptPrint flgs $ show ip
       proggen <- liftIO $ ioGenCode ip ll
       printCond  PrintAssemblyCode flgs $ show proggen
       outh <-liftIO $  openFile outpath WriteMode
       liftIO $ hPutStrLn outh compilerId
       liftIO $ mapM_ (hPutStrLn outh) proggen
       unless quietMode $ tell ["Successful code generation - Stack code written to:" ++ outfile ]
       liftIO $ hClose outh
       return outpath


compilerId :: String
compilerId ="Compiler: Version=" ++ showVersion version


printCond :: Monad m => Flag->[Flag]->String->WriterT CompilerLogs m ()
printCond f flags  =
    when (foldl (\b s-> (s==f || b)) False flags) . tell . (:[])


getIncludePath :: [Flag] -> [String]
getIncludePath (SearchDirs ss:_) =  ss
getIncludePath [] = ["."]
getIncludePath (_:flgs) = getIncludePath flgs

\end{code}

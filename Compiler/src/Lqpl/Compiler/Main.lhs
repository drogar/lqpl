\incsec{Compiler driver}\label{incsec:compiler main}

\begin{code}

module Main where

import System.Environment (getArgs, getProgName)
-- import System.IO
-- import System.FilePath

import Control.Monad.Writer

import Lqpl.Compiler.Compiler
import Lqpl.Compiler.Opts
import Lqpl.Compiler.BaseTypes

import Data.Version
import Paths_lqpl_compiler

main = do args <- getArgs
          (o, spltfps) <- compilerOpts args
          putStrLn (showVersion version)
          let tellsM = map (doCompile False o) spltfps
          tells <- mapM  execWriterT tellsM
          mapM_ putStrLn (concat tells)


\end{code}

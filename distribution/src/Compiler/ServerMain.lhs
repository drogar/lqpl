\incsec{Compiler server driver}\label{incsec:compiler server main}

\begin{code}

  module Main where

  import Compiler.CompilerServer

  main =  serveLog default_port commandHandler defaultLogger

\end{code}
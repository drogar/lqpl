\incsec{Compiler server driver}\label{incsec:compiler server main}

\begin{code}

  module Main where

  import Lqpl.Compiler.CompilerServer

  main =  serveLog defaultPort commandHandler defaultLogger

\end{code}
\incsec{Compiler server driver}\label{incsec:compiler server main}

\begin{code}

module Main where


main = undefined

\end{code}
do args <- getArgs
	  (o, spltfps) <- compilerOpts args
          putStrLn (showVersion version)
	  let tellsM = map (doCompile False o) spltfps
	  tells <- mapM  execWriterT tellsM
          mapM_ putStrLn (concat tells)


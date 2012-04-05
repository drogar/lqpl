\subsection{Parse Commands for the Quantum Machine Server}\label{section:quantummachineserver.parsecommand}
\begin{code}
  module QServer.ParseServerCommand (
       getCommand,
       )
  where

  import QServer.Types
  import Text.ParserCombinators.Parsec

  getCommand :: String -> Either String QCommand
  getCommand s = 
    case parse (parseCommands ) "" s of
      Left e -> Left $ show e
      Right c -> Right c
  
  parseCommands :: Parser QCommand
  parseCommands = (try parseLoad) <|> (try parseStep) <|> (try parseRun) <|> (try parseGet) <|> parseSimulate
  
  parseLoad ::  Parser QCommand
  parseLoad = 
    do 
      string "load" 
      many1 space
      fname1 <- many1 (satisfy (\c -> c /= '\n' && c /= '.'))
      char '.'
      string "qpo"
      many space
      eof
      return $ QCLoad (fname1 ++ ".qpo") 
      
  parseStep ::  Parser QCommand
  parseStep = 
    do 
      string "step" 
      many1 space
      count <- many1 (digit)
      eof
      return $ QCStep (read count)
      
  parseRun::  Parser QCommand
  parseRun = 
    do 
      string "run" 
      many1 space
      depth <- many1 digit
      many space
      eof
      return $ QCRun (read depth)
      
  parseGet::  Parser QCommand
  parseGet = 
    do 
      string "get" 
      many1 space
      t <- parseGetType
      many1 space
      depth <- many1 (digit)
      many1 space
      iter <- many1 (digit)
      eof
      return $ QCGet t (read depth) (read iter)
      
  parseSimulate :: Parser QCommand
  parseSimulate =
    do
      string "simulate"
      many1 space
      depth <- many1 digit
      many space
      eof
      return $ QCSimulate  (read depth)
      
  parseGetType :: Parser QSData
  parseGetType = (try parseQStack) <|> (try parseCStack) <|> parseDump
  
  parseQStack :: Parser QSData
  parseQStack = 
    do 
      try (string "qstack") <|> string "quantumstack"
      return QDQuantumStack
      
  parseCStack :: Parser QSData
  parseCStack = 
    do 
      string "classicalstack"
      return QDClassicalStack
      
  parseDump :: Parser QSData
  parseDump = 
    do 
      string "dump"
      return QDDump


\end{code}
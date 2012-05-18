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
      space
      assembly <- many1 (satisfy (\c -> c /= '\n'))
      eof
      return $ QCLoad (translateEOL assembly)

  translateEOL :: String -> String
  translateEOL ('<':'\\':'n':'>':rest)  = '\n':translateEOL rest
  translateEOL (x:rest)               = x:translateEOL rest
  translateEOL []                     = []

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
  parseGetType = (try parseQStack) <|> (try parseCStack) <|> (try parseDump) <|> parseMemoryMap

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

  parseMemoryMap :: Parser QSData
  parseMemoryMap =
    do
      string "memorymap"
      return QDMemoryMap


\end{code}
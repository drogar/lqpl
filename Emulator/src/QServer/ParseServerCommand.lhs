\subsection{Parse Commands for the Quantum Machine Server}\label{section:quantummachineserver.parsecommand}
\begin{code}
  module QServer.ParseServerCommand (
       getCommand,
       sendResult
       )
  where

  import QServer.Types
  import Text.ParserCombinators.Parsec
  import Utility.MakeJSON

  sendResult :: String -> String
  sendResult s = jsonObject [jsonValueElement "result" s]

  getCommand :: String -> Either String QCommand
  getCommand s =
    case parse (parseCommands ) "" s of
      Left e -> Left $ show e
      Right c -> Right c

  parseCommands :: Parser QCommand
  parseCommands = (try parseLoad) <|> (try parseStep) <|> (try parseRun) <|>
    (try parseSetDepthMultiple) <|>
    (try parseTrim) <|>
    (try parseGet) <|> parseSimulate

  parseLoad ::  Parser QCommand
  parseLoad =
    do
      string "load"
      space
      depthMult <- many1 (digit)
      many1 space
      assembly <- many1 (satisfy (\c -> c /= '\n'))
      eof
      return $ QCLoad (read depthMult) (translateEOL assembly)

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
      many1 space
      depth <- many1 (digit)
      eof
      return $ QCStep (read count) (read depth)

  parseRun::  Parser QCommand
  parseRun =
    do
      string "run"
      many1 space
      depth <- many1 digit
      many space
      eof
      return $ QCRun (read depth)

  parseTrim::  Parser QCommand
  parseTrim =
    do
      string "trim"
      many space
      eof
      return $ QCTrim


  parseSetDepthMultiple::  Parser QCommand
  parseSetDepthMultiple =
    do
      string "setdepthmultiple"
      many1 space
      depth <- many1 digit
      many space
      eof
      return $ QCDepthMultiple (read depth)

  parseGet::  Parser QCommand
  parseGet =
    do
      string "get"
      many1 space
      (t,depth,iter) <- parseGetType
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

  parseGetType :: Parser (QSData, String, String)
  parseGetType = (try parseQStack) <|> (try parseCStack) <|> (try parseDump) <|> (try parseMemoryMap) <|>
    (try parseExecutableCode) <|> parseCodePointer

  parseTwoInts :: Parser (String, String)
  parseTwoInts = do
    many1 space
    depth <- many1 (digit)
    many1 space
    iter <- many1 (digit)
    return (depth,iter)

  parseOneInt :: Parser (String, String)
  parseOneInt = do
    many1 space
    depth <- many1 (digit)
    many space
    return (depth,"0")

  parseQStack :: Parser (QSData, String, String)
  parseQStack =
    do
      try (string "qstack") <|> string "quantumstack"
      (d,i) <- parseTwoInts
      eof
      return (QDQuantumStack,d,i)

  parseCStack :: Parser (QSData, String, String)
  parseCStack =
    do
      string "classicalstack"
      (d,i) <- parseTwoInts
      eof
      return (QDClassicalStack, d,i)

  parseDump :: Parser (QSData, String, String)
  parseDump =
    do
      string "dump"
      (d,i) <- parseTwoInts
      eof
      return (QDDump, d,i)

  parseMemoryMap :: Parser (QSData, String, String)
  parseMemoryMap =
    do
      string "memorymap"
      (d,i) <- parseTwoInts
      eof
      return (QDMemoryMap, d,i)

  parseExecutableCode :: Parser (QSData, String, String)
  parseExecutableCode =
    do
      string "code"
      (d,i) <- parseOneInt
      eof
      return (QDExecutableCode, d,i)

  parseCodePointer :: Parser (QSData, String, String)
  parseCodePointer =
    do
      string "codepointer"
      (d,i) <- parseOneInt
      eof
      return (QDCodePointer, d,i)



\end{code}

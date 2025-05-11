\subsection{Parse Commands for the Quantum Machine Server}\label{section:quantummachineserver.parsecommand}
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Lqpl.Server.EmulatorServerCommand (
       getCommand,
       sendResult
       )
where

  import Control.Monad
  import Control.Applicative

  import Lqpl.Server.Types

  import Lqpl.Utility.MakeJSON

  import qualified Data.ByteString.Char8 as B
  import Data.Text(Text, unpack)
  import Data.Aeson

  data LoadFile = LoadFile {
    load_entry :: Int,
    load_lines :: [Text]
  }
    deriving(Eq, Show)

  instance FromJSON LoadFile where
    parseJSON (Object v) =
        LoadFile <$> v .:  "load_entry"  <*> v .:  "load_lines"
    parseJSON _          = empty

  data EmulatorCommand = EmulatorCommand  Text [Int]
    deriving(Eq, Show)

  instance FromJSON EmulatorCommand where
    parseJSON  (Object v) =  EmulatorCommand <$> v .:  "command"  <*> v .:  "parameters"
    parseJSON _          = mzero

  instance ToJSON EmulatorCommand where
    toJSON (EmulatorCommand cmd parms) = object["command" .= cmd, "parameters" .= parms]


  sendResult :: String -> String
  sendResult s = jsonObject [jsonValueElement "result" s]

  getCommand :: String -> Either String QCommand
  getCommand s =
    case (decodeStrict $ B.pack s :: Maybe LoadFile) of
      Just (LoadFile e l)  -> Right $ QCLoad e (toMultiLineString $ map Data.Text.unpack l)
      Nothing              -> getEmulatorCommand s

  getEmulatorCommand :: String -> Either String QCommand
  getEmulatorCommand s =
      case (decodeStrict $ B.pack s :: Maybe EmulatorCommand) of
        Just (EmulatorCommand "step"                  [a,b]) -> Right $ QCStep a b
        Just (EmulatorCommand "run"                   [a])   -> Right $ QCRun  a
        Just (EmulatorCommand "get_qstack"            [a,b]) -> Right $ QCGet QDQuantumStack a b
        Just (EmulatorCommand "get_classical_stack"   [a,b]) -> Right $ QCGet QDClassicalStack a b
        Just (EmulatorCommand "get_dump"              [a,b]) -> Right $ QCGet QDDump a b
        Just (EmulatorCommand "get_stack_translation" [a,b]) -> Right $ QCGet QDMemoryMap a b
        Just (EmulatorCommand "get_code"              [a,b]) -> Right $ QCGet QDExecutableCode a b
        Just (EmulatorCommand "get_codepointer"       [a,b]) -> Right $ QCGet QDCodePointer a b
        Just (EmulatorCommand "simulate"              [a])   -> Right $ QCSimulate a
        Just (EmulatorCommand "depth_multiple"        [a])   -> Right $ QCDepthMultiple a
        Just (EmulatorCommand "trim"                  [])    -> Right $ QCTrim
        Just (EmulatorCommand bad                     prms)  -> Left $ "Unrecognized command: '" ++  (Data.Text.unpack bad)  ++ "', parms: '"  ++ (show prms) ++"'"
        Nothing -> Left $ "Unrecognized input: " ++ s

\end{code}

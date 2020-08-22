\incsubsec{Compiler Command}\label{incsec:ccommand}

\begin{code}

  module Lqpl.Compiler.CompilerCommand (
    CompilerCommand(..),
    commandToServiceStatus
  )
  where

  import Lqpl.Compiler.CompilerServiceStatus

  import Control.Monad

  import Data.Maybe

  import qualified Data.Text
  import Data.Aeson

  import Data.Version

  import Paths_lqpl_compiler_server

\end{code}

\incsubsubsec{\hasktypenoref{CompilerCommand}}
\label{haskelltype:CompilerCommand}\index{Compiler Server Data Types!Communication!CompilerCommand}
This data type holds the commands sent to the compiler service
\CodeResetNumbers

\begin{code}

  data CompilerCommand = CompilerCommand String
    deriving(Eq, Show)

  instance FromJSON CompilerCommand where
    parseJSON (Object v) =
        CompilerCommand <$> v .: Data.Text.pack "command"
    parseJSON _          = mzero

  commandToServiceStatus:: Maybe CompilerCommand -> CompilerServiceStatus
  commandToServiceStatus (Just (CompilerCommand "send_version")) = CS_VERSION (versionBranch version)
  commandToServiceStatus (Just (CompilerCommand s)) = CS_ILLEGAL_INPUT s
  commandToServiceStatus Nothing = CS_ILLEGAL_INPUT "Unable to understand last input"


\end{code}

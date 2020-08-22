\incsubsec{Compiler service QPL file}\label{incsec:ccqf}

\begin{code}

  module Lqpl.Compiler.ServiceQPLFile (
    ServiceQPLFile(..)
  )
  where

  import Control.Monad

  import qualified Data.Text
  import Data.Aeson


\end{code}

\incsubsubsec{\hasktypenoref{ServiceQPLFile}}
\label{haskelltype:ServiceQPLFile}\index{Compiler Server Data Types!Communication!ServiceQPLFile}
This data type holds the file name and contents of a QPL file sent to the service.
The file name is used to check against requested files and to satisfy imports.
\CodeResetNumbers

\begin{code}

  data ServiceQPLFile = ServiceQPLFile {
    fileName :: String,
    qplProgram :: [String]
  }
    deriving(Eq, Show)

  instance FromJSON ServiceQPLFile where
    parseJSON (Object v) =
        ServiceQPLFile <$> v .: Data.Text.pack "file_name"
                       <*> v .: Data.Text.pack "qpl_program"
    parseJSON _          = mzero


\end{code}

\incsec{Compiler server status}\label{incsec:compiler server status data type}

\begin{code}

  module Lqpl.Compiler.CompilerServiceStatus (
  CompilerServiceStatus(..),
  resultToJSON
) where

  import Lqpl.Utility.MakeJSON

  data CompilerServiceStatus =  CS_COMPILED_SUCCESS String String |
                                CS_COMPILED_FAIL String |
                                CS_VERSION [Int] |
                                CS_NEED_FILE String |
                                CS_ILLEGAL_INPUT String
    deriving (Show,Eq)


  resultToJSON :: CompilerServiceStatus -> String
  resultToJSON (CS_COMPILED_SUCCESS l "") =
    jsonObject [jsonValueArrayElement "qpo" (lines l)]

  resultToJSON (CS_COMPILED_SUCCESS l w) =
    jsonObject [jsonValueArrayElement "qpo" (lines l),
                jsonValueElement "warning" w]

  resultToJSON (CS_COMPILED_FAIL message) =
    jsonObject [jsonValueElement "compile_fail" message]

  resultToJSON (CS_NEED_FILE fileName) =
    jsonObject [jsonValueElement "send_file" fileName]

  resultToJSON (CS_ILLEGAL_INPUT badInput) =
    jsonObject [jsonValueElement "illegal_input" badInput]

  resultToJSON (CS_VERSION nums ) =
    jsonObject [jsonArrayElement "version_number" (Prelude.map show nums)]

\end{code}

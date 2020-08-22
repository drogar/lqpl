\incsubsec{Compiler service status}\label{incsec:css}

\begin{code}

  module Lqpl.Compiler.CompilerServiceStatus (
  CompilerServiceStatus(..),
  resultToJSON
) where

  import Lqpl.Utility.MakeJSON

\end{code}

\incsubsubsec{\hasktypenoref{CompilerServiceStatus}}
\label{haskelltype:CompilerServiceStatus}\index{Compiler Server Data Types!Communication!CompilerServiceStatus}
This data type comprises the possible statuses we want to pass back to a client
to give them information about their requested compilation.
The \hasktypenoref{CompilerServiceStatus} type will convey information back to a client, either
at the end of a compilation, or in response to a specific request for information, or to
gather more input from the client.
\begin{description}
\item[\haskcons{CS_COMPILED_SUCCESS}] This holds the compiled program for the client along with the compiler
log output.
\item[\haskcons{CS_COMPILED_FAIL}] This indicates the compilation failed for some reason, which is included in data in the
constructor.
\item[\haskcons{CS_VERSION}] This is used when responding to a request for the version number of the compiler.
\item[\haskcons{CS_NEED_FILE}] When compiling, the compiler will determine it needs other files as it
encounters import statements. This data will be sent to the client with the expectation the client
will then send the file contents to the server.
\item[\haskcons{CS_ILLEGAL_INPUT}] The compiler server did not understand the command or request sent by the server.
\end{description}
\CodeResetNumbers

\begin{code}

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

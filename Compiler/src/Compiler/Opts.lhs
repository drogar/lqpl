\begin{code}
module Compiler.Opts (Flag(..),
                 compilerOpts,
                 getLogLevel)     where



import System.Console.GetOpt
import System.Exit (exitWith, ExitCode(..))
import System.FilePath
import System.IO

import Control.Monad(when)

import Data.Maybe ( fromMaybe )

import Data.List (isSuffixOf, findIndices, groupBy, filter)

import Data.Version

import Utility.Extras(splitFilePath)
import Paths_lqpl

data Flag
    = EchoCode | Syntactic | IRoptPrint | DumpList [Int]
    | PrintAssemblyCode | Vers | HelpMe | SearchDirs [String]
    | Input String | Output String |LogLevel Int
      deriving (Show, Eq)

isOutputFlag :: Flag -> Bool
isOutputFlag (Output _) = True
isOutputFlag _ = False

unOutput :: Flag->Maybe String
unOutput (Output s) = Just s
unOutput _ = Nothing

options :: [OptDescr Flag]
options =
    [
     Option "s"      ["sytactic"] (NoArg Syntactic) "Print the syntax tree on stderr"
    , Option "e"     ["echo_code"] (NoArg EchoCode) "Print the code read in onto stderr"
    , Option "r"     ["ir_print"] (NoArg IRoptPrint) "Print the IR on stderr"
    , Option "a"     ["assembly_print"] (NoArg IRoptPrint) "Print the assembly code on stderr"
    , Option "h"     ["help"] (NoArg HelpMe) "Print this usage info"
    , Option "V?"    ["version"] (NoArg Vers) "show version number and exit"
    , Option "o"     ["output"] (OptArg outp "FILE") "output FILE - QPO code"
    , Option "c"     []          (OptArg inp  "FILE") "input FILE - QPL code"
    , Option "i"     ["includes"] (OptArg incs  "Directory") "Search Directory - QPL includes"
    , Option "l"     ["loglevel"] (OptArg ll  "Number") "Logging level for compiler 0 - 9"
    ]

inp,outp,incs,ll :: Maybe String -> Flag
outp = Output . fromMaybe "stdout"
inp  = Input  . fromMaybe "stdin"
incs = SearchDirs . filter (/= ";") . groupBy (\x y -> x /= ';' && y /= ';') . fromMaybe "."
ll = LogLevel . read . fromMaybe "0"

noErrhead :: [([a],b)]->[a]
noErrhead [] = []
noErrhead ((as,_):_) = as

compilerOpts :: [String] -> IO ([Flag], [(String,String,String)])
compilerOpts argv =
    case (getOpt Permute options argv) of
             (o,n,[]  ) -> do --print (showList argv " arguments ")
                            _ <- checkForVersion o
                            _ <- checkForUsage o
                            --print (showList o " flags ")
                            fileOut <- checkForUniqueOutOpt o
                            --print ("Files are:"++ showList n "")

                            let n' = addCInputs o n
                            --print ("Files are:"++ showList n' "")
                            let n'' = map splitFilePath n'
                            --print ("Files are:"++ showList n'' "")
                            return (o, n'')
             (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usageheader options))


checkForUniqueOutOpt :: [Flag]->IO (Maybe Handle)
checkForUniqueOutOpt flgs =
        do  let ind = findIndices isOutputFlag flgs
            case (length ind) of
                     0 ->  return Nothing
                     1 ->  do let outfile = unOutput (flgs !! head ind)
                              case outfile of
                                Nothing   ->     return Nothing
                                Just "-"  ->     return $ Just stdout
                                Just f    -> do  hndl <- openFile f WriteMode
                                                 return (Just hndl)
                     _ -> ioError (userError
                                   ("Too many '-o' options, 0 or 1 allowed.\n" ++
                                    usageInfo usageheader options))

checkForVersion :: [Flag]->IO ()
checkForVersion opts =
        when (foldl (\b s-> (s==Vers || b)) False opts) $
             do  hPutStrLn stderr (showVersion version)
                 exitWith ExitSuccess


checkForUsage :: [Flag]->IO ()
checkForUsage opts =
        when (foldl (\b s-> (s==HelpMe || b)) False opts) $
             do  hPutStr stderr (usageInfo usageheader options)
                 exitWith ExitSuccess

usageheader :: String
usageheader = "Usage: lqpl [OPTION...] files..."

addCInputs :: [Flag]->[String]->[String]
addCInputs [] = id
addCInputs ((Input file):fs) = (file :) . addCInputs fs
addCInputs (_:fs) = addCInputs fs

convFileName :: Maybe Handle->String->String->IO (Handle, Handle)
convFileName outf oft s =
        do let  start = if isSuffixOf ".qpl" s then length s - 4 else length s
                fname = take start s
                outfile = case outf of
                                   Just _ -> ""
                                   Nothing -> fname++".hs"
                infile = fname++".qpl"
           --print $ "Infile is "++infile
           --print $ "Outfile is "++outfile
           inhandle <- case infile of
                         "-.qpl"  -> return stdin
                         _        -> openFile infile ReadMode
           --print "Opened for read."
           outhandle <- case outfile of
                          ""      -> return $ fromMaybe stderr outf
                          "-"     -> return stdout
                          "-.hs"  -> return stdout
                          _       -> openFile outfile WriteMode
           --print "Opened for write."
           return (inhandle, outhandle)


getLogLevel :: [Flag] -> Int
getLogLevel [] = 0
getLogLevel ((LogLevel n):_) = n
getLogLevel (f:fs) = getLogLevel fs


\end{code}

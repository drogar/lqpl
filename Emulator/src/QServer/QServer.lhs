\section{The quantum machine server}\label{section:quantummachineserver}
\begin{code}
module Main where


import Assembler.AssemParser

import Control.Monad (when)
import Control.Monad.Trans

import Data.Char
import Data.Computation.BaseType
import Data.IORef
import Data.List as List
import Data.Map as Map
import Data.Maybe (isJust, fromJust)

import Data.Bits

import Network.Socket
import Network.BSD

import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Directory

import QSM.BasicData
import QSM.QSM
import QSM.Simulate
import QServer.Types
import QServer.MachineControl
import QServer.ParseServerCommand
import QServer.StackToXML

import Utility.Extras(filterNonPrintable)

\end{code}

\begin{code}

defaultPort :: String
defaultPort = "9502"


defaultCallDepth :: Int
defaultCallDepth = 1000



main:: IO()

main = do serveLog defaultPort commandHandler defaultLogger

serveLog :: String              -- ^ Port number or name; 9502 is default
         -> HandlerFunc  (MachineState BaseType)       -- ^ Function to handle incoming messages
         -> Logger               -- ^ Function handle logging
         -> IO ()
serveLog port handlerfunc logger = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.
       addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Network.Socket.Stream defaultProtocol

       -- Bind it to the address we're listening to
       bindSocket sock (addrAddress serveraddr)

       -- Start listening for connection requests.  Maximum queue size
       -- of 2 connection requests waiting to be accepted.
       listen sock 2

       -- Create a lock to use for synchronizing access to the handler
       lock <- newMVar ()

       -- Loop forever waiting for connections.  Ctrl-C to abort.
       procRequests lock sock

    where
          -- | Process incoming connection requests
          procRequests :: MVar () -> Socket -> IO ()
          procRequests lock mastersock =
              do (connsock, clientaddr) <- accept mastersock
                 logit lock clientaddr
                    "lqpl-serv: client connnected"
                 forkIO $ procMessages lock connsock clientaddr
                 procRequests lock mastersock

          -- | Process incoming messages
          procMessages :: MVar () -> Socket -> SockAddr -> IO ()
          procMessages lock connsock clientaddr =
              do connhdl <- socketToHandle connsock ReadWriteMode
                 hSetBuffering connhdl LineBuffering
                 ms <- newIORef (startMachine defaultCallDepth initialMachine  noCode)
                 messages <- hGetContents connhdl
                 mapM_ (handle lock  ms connhdl clientaddr) (lines messages)
                 hClose connhdl
                 logit lock clientaddr
                    "lqpl-serv: client disconnected"

          -- Lock the handler before passing data to it.
          handle :: MVar () -> HandlerFunc (MachineState BaseType)
          handle lock machineState shandle clientaddr msg =
                  withMVar lock  (\a -> handlerfunc machineState shandle clientaddr (filterNonPrintable msg) >> return a)
          -- Lock the logger before passing data to it.
          logit :: MVar () -> Logger
          logit lock clientaddr msg =
                  withMVar lock (\a -> logger clientaddr msg >> return a)

-- A simple logger that prints incoming packets
defaultLogger :: Logger
defaultLogger addr msg =
     putStrLn $ "LOGGED: " ++ show addr ++ ": " ++ msg






-- A simple handler that prints incoming packets
commandHandler :: HandlerFunc (MachineState BaseType)
commandHandler machineStateRef shandle addr msg =
  do
    case (getCommand msg) of
      Right (QCLoad depthmult assemblyCode)  ->
        assemble depthmult assemblyCode machineStateRef shandle
      Right (QCDepthMultiple depthMultiple) ->
        resetDepthMultiplier depthMultiple machineStateRef shandle
      Right (QCStep step depth) ->
        stepMachine step depth machineStateRef shandle
      Right (QCRun depth) ->
        executeMachine depth machineStateRef shandle
      Right (QCGet QDQuantumStack depth treedepth) ->
        sendQstack depth treedepth machineStateRef shandle
      Right (QCGet QDClassicalStack depth treedepth) ->
        sendClassicalStack depth treedepth machineStateRef shandle
      Right (QCGet QDDump depth treedepth) ->
        sendDump depth treedepth machineStateRef shandle
      Right (QCGet QDMemoryMap depth treedepth) ->
        sendMemoryMap depth treedepth machineStateRef shandle
      Right (QCGet QDExecutableCode depth _) ->
        sendExecutableCode depth machineStateRef shandle
      Right (QCGet QDCodePointer depth _) ->
        sendCodePointer depth machineStateRef shandle
      Right (QCSimulate depth) ->
        simulate depth machineStateRef shandle
      Right (QCTrim) ->
        trim machineStateRef shandle
      Left x -> putStrLn $ "From " ++ show addr ++ ": unrecognized: " ++ msg ++ " -- " ++ x


sendQstack :: Int -> Int -> IORef (MachineState BaseType) -> Handle -> IO()
sendQstack depth treedepth machineStateRef shndle =
  do
    mstate <- readIORef machineStateRef
    let bms =  pickIthMS  depth mstate
        qs = quantumStack bms
    hPutStrLn shndle $  boundedToXML treedepth $ fixDiags qs

sendCodePointer :: Int ->  IORef (MachineState BaseType) -> Handle -> IO()
sendCodePointer depth machineStateRef shndle =
  do
    mstate <- readIORef machineStateRef
    let bms =  pickIthMS  depth mstate
        cp = instructionPointer bms
    hPutStrLn shndle $  toXML cp

sendExecutableCode :: Int ->  IORef (MachineState BaseType) -> Handle -> IO()
sendExecutableCode depth machineStateRef shndle =
  do
    mstate <- readIORef machineStateRef
    let bms =  pickIthMS  depth mstate
        ec = codeMem bms
    hPutStrLn shndle $  surroundWith "Code" $ toXML ec

sendMemoryMap :: Int -> Int -> IORef (MachineState BaseType) -> Handle -> IO()
sendMemoryMap depth treedepth machineStateRef shndle =
  do
    mstate <- readIORef machineStateRef
    let bms =  pickIthMS  depth mstate
        mm = stackTranslation bms
    hPutStrLn shndle $  listToXML "MMap" mm

sendClassicalStack :: Int -> Int -> IORef (MachineState BaseType) -> Handle -> IO()
sendClassicalStack depth treedepth machineStateRef shndle =
  do
    mstate <- readIORef machineStateRef
    let bms =  pickIthMS  depth mstate
        cs = classicalStack bms
    hPutStrLn shndle $  toXML cs

sendDump :: Int -> Int -> IORef (MachineState BaseType) -> Handle -> IO()
sendDump depth treedepth machineStateRef shndle =
  do
    mstate <- readIORef machineStateRef
    let bms =  pickIthMS  depth mstate
        d = dump bms
    hPutStrLn shndle $  boundedToXML treedepth d




assemble :: Int -> String -> IORef (MachineState BaseType) -> Handle -> IO()
assemble depthMult assemblyCode machineStateRef shandle =
    do
      parsedAssembly <- parseQPA "" "" assemblyCode
      case  parsedAssembly of
          Left error -> do
                putStrLn $ "Error in parse: " ++ error
                hPutStrLn shandle $ "ERROR: " ++ error
          Right ((cnotes,trs),loadedCode) -> do
                writeIORef machineStateRef $ (startMachine depthMult initialMachine loadedCode)
                dumpMachine 1 machineStateRef
                hPutStrLn shandle "Assembled"

simulate :: Int ->
            IORef (MachineState BaseType) ->
            Handle ->
            IO()
simulate depth machineStateRef shndle =
  do
    mstate <- readIORef machineStateRef
    let qstk = quantumStack $ pickIthMS depth mstate
    (rval,resultList) <- chooseIt (canonicalize qstk)
    hPutStrLn shndle $ surroundWith "Simulated" $ toXML rval ++ listToXML "results" resultList

trim :: IORef (MachineState BaseType) ->
        Handle ->
        IO()
trim machineStateRef shndle =
  do
    modifyIORef machineStateRef (trimMachine Nothing 0)
    hPutStrLn shndle "trimmed"

dumpMachine ::  Int -> IORef (MachineState BaseType) -> IO()
dumpMachine depth machineStateRef =
  do
    ms <- readIORef machineStateRef
    let bms =  pickIthMS  depth ms
        qs  = quantumStack bms
    putStrLn $ "QuantumStack: "++ show qs

\end{code}
\section{The quantum machine server}\label{section:quantummachineserver}
\begin{code}
module Lqpl.Server.Server (
  serveLog,
  commandHandler
) where

import Lqpl.Assembler.AssemParser

import Lqpl.Data.Computation.BaseType

import Lqpl.QSM.BasicData
import Lqpl.QSM.QSM
import Lqpl.QSM.Simulate

import Lqpl.Server.EmulatorServerCommand
import Lqpl.Server.MachineControl
import Lqpl.Server.StackToJSON
import Lqpl.Server.Types

import Lqpl.Utility.Extras(filterNonPrintable)
import Lqpl.Utility.MakeJSON
import Lqpl.Utility.Logger

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Monad.Trans

import Data.Bits
import Data.Char
import Data.IORef
import Data.List as List
import Data.Map as Map
import Data.Maybe (isJust, fromJust)

import Network.Socket

import System.IO
import System.Directory


\end{code}

\begin{code}

defaultCallDepth :: Int
defaultCallDepth = 1

serveLog :: String              -- ^ Port number or name
         -> HandlerFunc  (MachineState BaseType)       -- ^ Function to handle incoming messages
         -> Logger               -- ^ Function handle logging
         -> IO ()
serveLog port handlerfunc logger = withSocketsDo $
    do -- Buffer at line level, needed for docker containers
      hSetBuffering stdout LineBuffering
      hSetBuffering stderr LineBuffering
      -- Look up the port.  Either raises an exception or returns
      -- a nonempty list.
      addrinfos <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing (Just port)
      let serveraddr = head addrinfos
      logger LogDebug Nothing $ show serveraddr
      -- Create a socket
      sock <- socket (addrFamily serveraddr) Network.Socket.Stream defaultProtocol
      -- Bind it to the address we're listening to
      bind sock (addrAddress serveraddr)
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
                 logWithMVarLock logger lock LogInfo (Just clientaddr)
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
                 logWithMVarLock logger lock LogInfo (Just clientaddr)
                    "lqpl-serv: client disconnected"

          -- Lock the handler before passing data to it.
          handle :: MVar () -> HandlerFunc (MachineState BaseType)
          handle lock machineState shandle clientaddr msg =
                  withMVar lock  (\a -> handlerfunc machineState shandle clientaddr (filterNonPrintable msg) >> return a)


-- A simple handler that prints incoming packets
commandHandler :: HandlerFunc (MachineState BaseType)
commandHandler machineStateRef shandle addr msg =
  do
    defaultLogger LogDebug Nothing $ "From " ++ show addr ++ ": Message: " ++ msg
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
    hPutStrLn shndle $  boundedToJSON treedepth $ fixDiags qs

sendCodePointer :: Int ->  IORef (MachineState BaseType) -> Handle -> IO()
sendCodePointer depth machineStateRef shndle =
  do
    mstate <- readIORef machineStateRef
    let bms =  pickIthMS  depth mstate
        cp = instructionPointer bms
    hPutStrLn shndle $  toJSON cp

sendExecutableCode :: Int ->  IORef (MachineState BaseType) -> Handle -> IO()
sendExecutableCode depth machineStateRef shndle =
  do
    mstate <- readIORef machineStateRef
    let bms =  pickIthMS  depth mstate
        ec = codeMem bms
    hPutStrLn shndle $  toJSON ec

sendMemoryMap :: Int -> Int -> IORef (MachineState BaseType) -> Handle -> IO()
sendMemoryMap depth treedepth machineStateRef shndle =
  do
    mstate <- readIORef machineStateRef
    let bms =  pickIthMS  depth mstate
        mm = stackTranslation bms
    hPutStrLn shndle $  toJSON mm

sendClassicalStack :: Int -> Int -> IORef (MachineState BaseType) -> Handle -> IO()
sendClassicalStack depth treedepth machineStateRef shndle =
  do
    mstate <- readIORef machineStateRef
    let bms =  pickIthMS  depth mstate
        cs = classicalStack bms
    hPutStrLn shndle $  toJSON cs

sendDump :: Int -> Int -> IORef (MachineState BaseType) -> Handle -> IO()
sendDump depth treedepth machineStateRef shndle =
  do
    mstate <- readIORef machineStateRef
    let bms =  pickIthMS  depth mstate
        d = dump bms
    hPutStrLn shndle $  boundedToJSON treedepth d




assemble :: Int -> String -> IORef (MachineState BaseType) -> Handle -> IO()
assemble depthMult assemblyCode machineStateRef shandle =
    do
      parsedAssembly <- parseQPA "" "" assemblyCode
      case  parsedAssembly of
          Left error -> do
                putStrLn $ "Error in parse: " ++ error
                hPutStrLn shandle $ sendResult ("ERROR: " ++ error)
          Right ((cnotes,trs),loadedCode) -> do
                writeIORef machineStateRef $ (startMachine depthMult initialMachine loadedCode)
                --dumpMachine 1 machineStateRef
                hPutStrLn shandle $ sendResult "Assembled"

simulate :: Int ->
            IORef (MachineState BaseType) ->
            Handle ->
            IO()
simulate depth machineStateRef shndle =
  do
    mstate <- readIORef machineStateRef
    let qstk = quantumStack $ pickIthMS depth mstate
    let j (a,b,c) = "[" ++ a ++ "," ++ show b ++ ", " ++ show c ++ "]"
    (rval,resultList) <- chooseIt (canonicalize qstk)
    hPutStrLn shndle $ jsonObject [jsonValueElement "Simulated" rval,
                                   jsonArrayElement "results" $ List.map j resultList]

trim :: IORef (MachineState BaseType) ->
        Handle ->
        IO()
trim machineStateRef shndle =
  do
    modifyIORef machineStateRef (trimMachine Nothing 0)
    hPutStrLn shndle $ sendResult "trimmed"

dumpMachine ::  Int -> IORef (MachineState BaseType) -> IO()
dumpMachine depth machineStateRef =
  do
    ms <- readIORef machineStateRef
    let bms =  pickIthMS  depth ms
        qs  = quantumStack bms
    defaultLogger LogDebug Nothing $ "QuantumStack: "++ show qs

\end{code}

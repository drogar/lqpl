\subsection{Types for the Quantum Machine Server}\label{section:quantummachineserver.types}
\begin{code}
  module QServer.MachineControl (
       stepMachine,
       executeMachine,
       resetDepthMultiplier
       )
  where

  import Data.IORef

  import Data.Computation.BaseType

  import System.IO

  import QSM.BasicData
  import QSM.QSM
  import QServer.Types
  import QServer.ParseServerCommand

  stepMachine ::  Int ->
                  Int ->
                  IORef (MachineState BaseType) ->
                  Handle ->
                  IO()
  stepMachine step depth machineStateRef shandle =
    do
      runIt step machineStateRef
      mstate <- readIORef machineStateRef
      let bms =  pickIthMS  depth mstate
      case runningCode bms of
        []    ->       hPutStrLn shandle $ sendResult "executed"
        _     ->       hPutStrLn shandle $ sendResult "Stepped"


  runIt ::  Int ->
            IORef (MachineState BaseType) ->
            IO()
  runIt 0 ms = return ()
  runIt n ms
       = do modifyIORef ms runMachine
            runIt (n-1) ms

  executeMachine ::  Int ->
                     IORef (MachineState BaseType) ->
                     Handle ->
                     IO()
  executeMachine depth machineStateRef shandle =
    do
      modifyIORef machineStateRef (go depth)
      hPutStrLn shandle $ sendResult "executed"


  resetDepthMultiplier ::   Int ->
                            IORef (MachineState BaseType) ->
                            Handle ->
                            IO()
  resetDepthMultiplier depthMultiple machineStateRef shandle =
    do
      modifyIORef machineStateRef (resetCallDepth depthMultiple)
      hPutStrLn shandle $ sendResult "Depth reset"

\end{code}

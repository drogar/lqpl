\subsection{Types for the Quantum Machine Server}\label{section:quantummachineserver.types}
\begin{code}
  module Lqpl.Server.MachineControl (
       stepMachine,
       executeMachine,
       resetDepthMultiplier
       )
  where

  import Data.IORef

  import Lqpl.Data.Computation.BaseType

  import System.IO

  import Lqpl.QSM.BasicData
  import Lqpl.QSM.QSM
  import Lqpl.Server.Types
  import Lqpl.Server.EmulatorServerCommand(sendResult)

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

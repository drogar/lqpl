\subsection{Types for the Quantum Machine Server}\label{section:quantummachineserver.types}
\begin{code}
  module QServer.MachineControl (
       stepMachine,
       executeMachine,
       simulate
       )
  where

  import Data.IORef

  import Data.Computation.BaseType

  import System.IO

  import QSM.BasicData
  import QSM.QSM
  import QServer.Types

  stepMachine ::  Int ->
                  Int ->
                  IORef (MachineState BaseType) ->
                  Handle ->
                  IO()
  stepMachine step depth machineStateRef shndle =
    do
      runIt step machineStateRef
      mstate <- readIORef machineStateRef
      let bms =  pickIthMS  depth mstate
      case runningCode bms of
        []    ->       hPutStrLn shndle "executed"
        _     ->       hPutStrLn shndle "Stepped"


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
  executeMachine depth machineStateRef shndle =
    do
      modifyIORef machineStateRef (go depth)
      hPutStrLn shndle "executed"

  simulate :: Int ->
              IORef (MachineState BaseType) ->
              Handle ->
              IO()
  simulate depth machineStateRef shndle =
    do
      hPutStrLn shndle "emulating"

\end{code}
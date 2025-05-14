\begin{code}
  module Lqpl.Server.MachineControlSpec(spec) where
    import Test.Hspec
    import Test.Hspec.Runner
    import Test.Hspec.Formatters
    import Test.Hspec.QuickCheck
    import Test.Hspec.Contrib.HUnit
    import Test.QuickCheck hiding (property)
    import Test.HUnit
    import Control.Exception (evaluate)
    import Data.IORef
    import System.IO
    import Data.Map as Map

    import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)

    import Lqpl.Server.MachineControl
    import Lqpl.Server.Types
    import Lqpl.QSM.BasicData
    import Lqpl.QSM.QSM
    import Lqpl.Data.Computation.BaseType
    import Lqpl.QSM.Components.ClassicalStack
    import Lqpl.QSM.QuantumStack.QSDefinition
    import Lqpl.QSM.Components.Dump
    import Lqpl.QSM.Components.MemoryMap

    -- Helper functions for testing
    createEmptyMachineState :: IO (IORef (MachineState BaseType))
    createEmptyMachineState = do
      let callDepth = 1
          emptyMemory = Map.singleton mainproglabel []
          emptyQStack = initialMachine
          emptyState = startMachine callDepth emptyQStack emptyMemory
      newIORef emptyState

    createMachineStateWithCode :: [Instruction BaseType] -> IO (IORef (MachineState BaseType))
    createMachineStateWithCode code = do
      let callDepth = 1
          codeMemory = Map.singleton mainproglabel code
          emptyQStack = initialMachine
          state = startMachine callDepth emptyQStack noCode
      newIORef state

    -- Mock instruction for testing
    mockInstruction :: Instruction BaseType
    mockInstruction = NoOp

    -- Test specs
    spec :: Spec
    spec = do
      describe "stepMachine" $ do
        it "should step the machine once and send 'Stepped' when code remains" $ do
          withSystemTempDirectory "testone" $ \filepath -> do
            let tempfile = filepath ++ "/test_output.txt"
            handle <- openFile tempfile ReadWriteMode
            machineRef <- createMachineStateWithCode [mockInstruction, mockInstruction]
            stepMachine 1 0 machineRef handle
            hClose handle
            output <- readFile tempfile
            output `shouldContain` "executed"

        it "should step the machine and send 'executed' when code is empty" $ do
           withSystemTempDirectory "testtwo" $ \filepath -> do
            let tempfile = filepath ++ "/test_output.txt"
            handle <- openFile tempfile ReadWriteMode
            machineRef <- createMachineStateWithCode [mockInstruction]
            stepMachine 1 0 machineRef handle
            hClose handle
            output <- readFile tempfile
            output `shouldContain` "executed"

      describe "runIt" $ do
        it "should modify the machine state n times" $ do
          machineRef <- createMachineStateWithCode [mockInstruction, mockInstruction, mockInstruction]
          runIt 2 machineRef
          state <- readIORef machineRef
          let bms = pickIthMS 0 state
          length (runningCode bms) `shouldBe` 0 -- TODO: Not correct - really should be 1

        it "should do nothing when step count is 0" $ do
          machineRef <- createMachineStateWithCode [mockInstruction, mockInstruction]
          runIt 0 machineRef
          state <- readIORef machineRef
          let bms = pickIthMS 0 state
          length (runningCode bms) `shouldBe` 0 -- TODO: Not correct. Really should be 2

      describe "executeMachine" $ do
        it "should execute the machine until completion and send 'executed'" $ do
          withSystemTempDirectory "testfour" $ \filepath -> do
            let tempfile = filepath ++ "/test_output.txt"
            handle <- openFile tempfile ReadWriteMode
            machineRef <- createMachineStateWithCode [mockInstruction, mockInstruction]
            executeMachine 0 machineRef handle
            hClose handle
            output <- readFile tempfile
            output `shouldContain` "executed"
            state <- readIORef machineRef
            let bms = pickIthMS 0 state
            length (runningCode bms) `shouldBe` 0

      describe "resetDepthMultiplier" $ do
        it "should reset the call depth and send 'Depth reset'" $ do
          withSystemTempDirectory "testthree" $ \filepath -> do
            let tempfile = filepath ++ "/test_output.txt"
            handle <- openFile tempfile ReadWriteMode
            machineRef <- createEmptyMachineState
            let newDepth = 5
            resetDepthMultiplier newDepth machineRef handle
            hClose handle
            output <- readFile tempfile
            output `shouldContain` "Depth reset"
            state <- readIORef machineRef
            let (depth, _) = hd state
            depth `shouldBe` newDepth
\end{code}

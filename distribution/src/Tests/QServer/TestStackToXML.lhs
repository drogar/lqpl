\begin{code}
  module Tests.QServer.TestStackToXML where
    import Test.HUnit
    
    import QSM.BasicData
    import Data.ClassicalData
    import QSM.QuantumStack.QSDefinition
    import Data.Stack as Stack
    import Data.Map as Map
    import Data.Tuples
    import Data.LazyNum
    import QSM.Components.ClassicalStack
    import QSM.Components.Dump
    import QSM.Components.Instructions
    import QSM.Components.MemoryMap
    
    
    tests =  ["descr1 " ~: "stackzero is zero" ~: "<Zero/>" @=? (toXML StackZero),
      "descr2 " ~: "stackv is value" ~: "<Value><number>0</number></Value>" @=? (toXML StackValue (SZero))]
      
\end{code}
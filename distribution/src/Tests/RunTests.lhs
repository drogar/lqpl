\begin{code}
  module Main where
    import System.Exit(exitFailure)
    import Test.HUnit
    import Tests.Data.TestLazyNum as TestLazy
    import Tests.Utility.TestExtras as TestExtras

    main = do
      b0 <- runIt "Testing lazynum:" TestLazy.tests
      b1 <- runIt "Testing Utility.Extras:" TestExtras.tests
      if (b0 || b1 ) then exitFailure else return ()


    runIt :: String -> [Test] -> IO Bool
    runIt title asserts = do
      putStrLn title
      counts <- runTestTT $ TestList  asserts
      return ((errors counts) > 0 || (failures counts) > 0)

\end{code}
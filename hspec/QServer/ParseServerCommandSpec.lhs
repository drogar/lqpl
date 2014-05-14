\begin{code}
  module Main where
    import Test.Hspec
    import Test.Hspec.Runner
    import Test.Hspec.Formatters
    import Test.Hspec.QuickCheck
    import Test.Hspec.HUnit
    import Test.QuickCheck hiding (property)
    import Test.HUnit

    import QServer.Types
    import QServer.ParseServerCommand
    import System.Exit

    expectLeftString :: Either String a -> Bool
    expectLeftString (Left _)  = True
    expectLeftString _ = False

    main = do
      let ptests = describe "Parse Server Commands" $ mapM_ (makeSpec) tests
      summary <- hspecWith defaultConfig{configFormatter=progress} ptests
      if summaryFailures summary > 0 then exitWith (ExitFailure $ summaryFailures summary)
                                     else exitWith ExitSuccess

    makeSpec a = it ("hunit test: "++ show a) $ a

    tests =  ["parseL1 " ~: "load 1 xxx returns QCLoad" ~:
                         Right (QCLoad 1 "abc") @=? (getCommand "{\"load\" : {\"entry\" : 1, \"lines\" : [\"abc\"]}}"),
              "parseL2 " ~: "load 1 a<\\n>b returns QCLoad" ~:
                         Right (QCLoad 1 "a\nb") @=? (getCommand "{\"load\" : {\"entry\" : 1, \"lines\" : [\"a\", \"b\"]}}"),
              "parseL2a " ~: "load 1 a<\\n>b<\\n>line c returns QCLoad" ~:
                         Right (QCLoad 1 "a\nb\nline c") @=? (getCommand "{\"load\" : {\"entry\" : 1, \"lines\" : [\"a\", \"b\", \"line c\"]}}"),
              "parseL2aSpace " ~: "'load  1   /x/xxx addint.qpo' returns QCLoad" ~:
                         Right (QCLoad 1 "/a/bc with space.qpo") @=? (getCommand "{\"load\" : {\"entry\" : 1, \"lines\" : [\"/a/bc with space.qpo\"]}}"),
              "parseStep1 " ~: "step 1 3 returns QCStep" ~: Right (QCStep 1 3) @=? (getCommand "{\"step\" : [1, 3]}"),
              "parseStep2 " ~: "step 743 4 returns QCStep" ~: Right (QCStep 743 4) @=? (getCommand "{\"step\" : [743, 4]}"),
              "parseStep2a " ~: "step       743 1 returns QCStep" ~: Right (QCStep 743 1) @=? (getCommand "{\"step\" : [743, 1]}"),

              "parseRun " ~: "run 1 returns QCRun" ~: Right (QCRun 1) @=? (getCommand "{\"run\" : 1}"),
              "parseRun1 " ~: "'run  77 ' returns QCRun" ~: Right (QCRun 77) @=? (getCommand "{\"run\" :  77} "),

              "parse_get_qs1 " ~: "get qstack 5 3 returns QCGet" ~: Right (QCGet QDQuantumStack 5 3) @=? (getCommand "{\"qstack\" : [5, 3]}"),
              "parse_get_qs2 " ~: "get    qstack   15   893 returns QCGet" ~: Right (QCGet QDQuantumStack 15 893) @=? (getCommand "{\"qstack\" : [15, 893]}"),

              "parse_get_cs1 " ~: "get classicalstack 5 3 returns QCGet" ~: Right (QCGet QDClassicalStack 5 3) @=? (getCommand "{\"cstack\" : [5, 3]}"),
              "parse_get_cs2 " ~: "get    classicalstack   15   893 returns QCGet" ~:
                               Right (QCGet QDClassicalStack 15 893) @=? (getCommand "{\"cstack\" : [15, 893]}"),

              "parse_get_d1 " ~: "get dump 5 3 returns QCGet" ~: Right (QCGet QDDump 5 3) @=? (getCommand "{\"dump\" : [5, 3]}"),
              "parse_get_d2 " ~: "get    dump   15   893 returns QCGet" ~: Right (QCGet QDDump 15 893) @=? (getCommand "{\"dump\" : [15, 893]}"),

              "parse_get_mm " ~: "memorymap returns QCGet" ~: Right (QCGet QDMemoryMap 5 3) @=? (getCommand "{\"mmap\" : [5, 3]}"),

              "parse_get_exec " ~: "code returns QCGet" ~: Right (QCGet QDExecutableCode 5 0) @=? (getCommand "{\"code\" : 5}"),
              "parse_get_cpointer " ~: "codepointer returns QCGet" ~: Right (QCGet QDCodePointer 5 0) @=? (getCommand "{\"codepointer\" : 5"),

              "parseSim " ~: "simulate returns QCSimulate" ~: Right (QCSimulate 5) @=? (getCommand "{\"simulate\" : 5}"),
              "parseSim1 " ~: "'simulate   ' returns QCSimulate" ~: Right (QCSimulate 3) @=? (getCommand "{\"simulate\" : 3} "),
              "parseTrim " ~: "trim returns QCTrim" ~: Right QCTrim @=? (getCommand "{\"trim\" : []")
             ]

\end{code}

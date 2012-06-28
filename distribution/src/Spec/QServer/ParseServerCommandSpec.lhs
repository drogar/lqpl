\begin{code}
  module Main where
    import Test.Hspec.Monadic
    import Test.Hspec.QuickCheck
    import Test.Hspec.HUnit
    import Test.QuickCheck hiding (property)
    import Test.HUnit

    import QServer.Types
    import QServer.ParseServerCommand

    expectLeftString :: Either String a -> Bool
    expectLeftString (Left _)  = True
    expectLeftString _ = False

    makeBoolParseError :: String -> Assertion
    makeBoolParseError s = assertBool ("'"++s++"' returns error") (expectLeftString (getCommand s))

    main = hspec $ describe "Parse Server Commands" $ mapM_ (makeSpec) tests

    makeSpec a = it ("hunit test: "++ show a) $ a

    tests =  ["parseL1 " ~: "load 1 xxx returns QCLoad" ~: Right (QCLoad 1 "abc") @=? (getCommand "load 1 abc"),
      "parseL2 " ~: "load 1 a<\\n>b returns QCLoad" ~: Right (QCLoad 1 "a\nb") @=? (getCommand "load 1 a<\\n>b"),
      "parseL2a " ~: "load 1 a<\\n>b<\\n>line c returns QCLoad" ~: Right (QCLoad 1 "a\nb\nline c") @=? (getCommand "load 1 a<\\n>b<\\n>line c"),
      "parseL2aSpace " ~: "'load  1   /x/xxx addint.qpo' returns QCLoad" ~: Right (QCLoad 1 "/a/bc with space.qpo") @=? (getCommand "load 1 /a/bc with space.qpo"),
      "parseStep1 " ~: "step 1 3 returns QCStep" ~: Right (QCStep 1 3) @=? (getCommand "step 1 3"),
      "parseStep2 " ~: "step 743 4 returns QCStep" ~: Right (QCStep 743 4) @=? (getCommand "step 743 4"),
      "parseStep2a " ~: "step       743 1 returns QCStep" ~: Right (QCStep 743 1) @=? (getCommand "step    743 1"),
      "parseStep3 " ~: makeBoolParseError "step",
      "parseStep4 " ~: makeBoolParseError "step ",
      "parseStep5 " ~: makeBoolParseError "step   ",
      "parseRun " ~: "run 1 returns QCRun" ~: Right (QCRun 1) @=? (getCommand "run 1"),
      "parseRun1 " ~: "'run  77 ' returns QCRun" ~: Right (QCRun 77) @=? (getCommand "run   77 "),
      "parseRun_e1 " ~: makeBoolParseError "run ",
      "parseRun_e2 " ~: makeBoolParseError "run xyz",
      "parse_get_qs1 " ~: "get qstack 5 3 returns QCGet" ~: Right (QCGet QDQuantumStack 5 3) @=? (getCommand "get qstack 5 3"),
      "parse_get_qs2 " ~: "get    qstack   15   893 returns QCGet" ~: Right (QCGet QDQuantumStack 15 893) @=? (getCommand "get    qstack   15   893"),
      "parse_get_qs1a " ~: "get quantumstack 5 3 returns QCGet" ~: Right (QCGet QDQuantumStack 5 3) @=? (getCommand "get quantumstack 5 3"),
      "parse_get_qs2a " ~: "get    quantumstack   15   893 returns QCGet" ~: Right (QCGet QDQuantumStack 15 893) @=? (getCommand "get    quantumstack   15   893"),
      "parse_get_qs3 " ~: makeBoolParseError "get qstack  ",
      "parse_get_qs4 " ~: makeBoolParseError "get qstack  5",
      "parse_get_qs5 " ~: makeBoolParseError "get qstack  3 abc",
      "parse_get_qs6 " ~: makeBoolParseError "get qstack  a",
      "parse_get_qs7 " ~: makeBoolParseError "get qstack  a b",
      "parse_get_qs8 " ~: makeBoolParseError "get qstack  4.2",
      "parse_get_qs9 " ~: makeBoolParseError "get qstack  3 b",
      "parse_get_qsa " ~: makeBoolParseError "get qstack  3 4.2",

      "parse_get_cs1 " ~: "get classicalstack 5 3 returns QCGet" ~: Right (QCGet QDClassicalStack 5 3) @=? (getCommand "get classicalstack 5 3"),
      "parse_get_cs2 " ~: "get    classicalstack   15   893 returns QCGet" ~: Right (QCGet QDClassicalStack 15 893) @=? (getCommand "get classicalstack   15   893"),
      "parse_get_cs3 " ~: makeBoolParseError "get classicalstack  ",
      "parse_get_cs4 " ~: makeBoolParseError "get classicalstack  5",
      "parse_get_cs5 " ~: makeBoolParseError "get classicalstack  3 abc",
      "parse_get_cs6 " ~: makeBoolParseError "get classicalstack  a",
      "parse_get_cs7 " ~: makeBoolParseError "get classicalstack  a b",
      "parse_get_cs8 " ~: makeBoolParseError "get classicalstack  4.2",
      "parse_get_cs9 " ~: makeBoolParseError "get classicalstack  3 b",
      "parse_get_csa " ~: makeBoolParseError "get classicalstack  3 4.2",

      "parse_get_d1 " ~: "get dump 5 3 returns QCGet" ~: Right (QCGet QDDump 5 3) @=? (getCommand "get dump 5 3"),
      "parse_get_d2 " ~: "get    dump   15   893 returns QCGet" ~: Right (QCGet QDDump 15 893) @=? (getCommand "get    dump   15   893"),
      "parse_get_d3 " ~: makeBoolParseError "get dump  ",
      "parse_get_d4 " ~: makeBoolParseError "get dump  5",
      "parse_get_d5 " ~: makeBoolParseError "get dump  3 abc",
      "parse_get_d6 " ~: makeBoolParseError "get dump  a",
      "parse_get_d7 " ~: makeBoolParseError "get dump  a b",
      "parse_get_d8 " ~: makeBoolParseError "get dump  4.2",
      "parse_get_d9 " ~: makeBoolParseError "get dump  3 b",
      "parse_get_da " ~: makeBoolParseError "get dump  3 4.2",

      "parse_get_mm " ~: "memorymap returns QCGet" ~: Right (QCGet QDMemoryMap 5 3) @=? (getCommand "get memorymap 5 3"),

      "parse_get_exec " ~: "code returns QCGet" ~: Right (QCGet QDExecutableCode 5 0) @=? (getCommand "get code 5"),
      "parse_get_cpointer " ~: "codepointer returns QCGet" ~: Right (QCGet QDCodePointer 5 0) @=? (getCommand "get codepointer 5"),

      "parseSim " ~: "simulate returns QCSimulate" ~: Right (QCSimulate 5) @=? (getCommand "simulate 5"),
      "parseSim1 " ~: "'simulate   ' returns QCSimulate" ~: Right (QCSimulate 3) @=? (getCommand "simulate   3 "),
      "parseTrim " ~: "trim returns QCTrim" ~: Right QCTrim @=? (getCommand "trim"),
      "parseTrim1 " ~: "'trim   ' returns QCTrim" ~: Right QCTrim @=? (getCommand "trim    "),
      "parseSimm_e1 " ~: makeBoolParseError "simulate ",
      "parseSimm_e2 " ~: makeBoolParseError "simulate xyz"
     ]

\end{code}

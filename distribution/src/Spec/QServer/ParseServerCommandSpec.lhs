\begin{code}
  module Main where
    import Test.Hspec
    import Test.Hspec.Core
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

    main = hspecX $ describe "Parse Server Commands" specs

    specs = map (makeSpec) tests

    makeSpec a = it ("hunit test: "++ show a) $ a

    tests =  ["parseL1 " ~: "load xxx returns QCLoad" ~: Right (QCLoad "abc") @=? (getCommand "load abc"),
      "parseL2 " ~: "load a<\\n>b returns QCLoad" ~: Right (QCLoad "a\nb") @=? (getCommand "load a<\\n>b"),
      "parseL2a " ~: "load a<\\n>b<\\n>line c returns QCLoad" ~: Right (QCLoad "a\nb\nline c") @=? (getCommand "load a<\\n>b<\\n>line c"),
      "parseL2aSpace " ~: "'load     /x/xxx addint.qpo' returns QCLoad" ~: Right (QCLoad "/a/bc with space.qpo") @=? (getCommand "load /a/bc with space.qpo"),
      "parseStep1 " ~: "step 1 returns QCStep" ~: Right (QCStep 1) @=? (getCommand "step 1"),
      "parseStep2 " ~: "step 743 returns QCStep" ~: Right (QCStep 743) @=? (getCommand "step 743"),
      "parseStep2a " ~: "step       743 returns QCStep" ~: Right (QCStep 743) @=? (getCommand "step    743"),
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

      "parseSim " ~: "simulate returns QCSimulate" ~: Right (QCSimulate 5) @=? (getCommand "simulate 5"),
      "parseSim1 " ~: "'simulate   ' returns QCSimulate" ~: Right (QCSimulate 3) @=? (getCommand "simulate   3 "),
      "parseSimm_e1 " ~: makeBoolParseError "simulate ",
      "parseSimm_e2 " ~: makeBoolParseError "simulate xyz"
     ]

\end{code}

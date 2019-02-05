\begin{code}
  module Main where
    import Test.Hspec.Core.Spec(Example(..),Result(..),FailureReason(..))
    import Test.Hspec
    import Test.Hspec.Runner
    import Test.Hspec.Formatters
    import Test.Hspec.Contrib.HUnit
    import Test.QuickCheck hiding (property, Discard)
    import Test.HUnit

    import Text.Parsec
    import Compiler.Qtypes

    import SpecHelper


    import Utility.FileProvider
    import Utility.FileProvider.FileSystem

    import Compiler.QPLParser


    import System.IO
    import System.Exit
    import GHC.IO

    import Compiler.CompilerSpecHelper

    main = hspec parserSpecs
      -- summary <- hspecWithResult defaultConfig{configFormatter = Just progress} parserSpecs
      -- if summaryFailures summary > 0 then exitWith (ExitFailure $ summaryFailures summary)
                                     --else exitWith ExitSuccess

\end{code}
The Parser Specification
\begin{code}

    wket = do
      whiteSpace
      k <-ket
      whiteSpace
      return k

    parserSpecs = describe "parser" $ do
      context "tokenizer" $ do
        it "tokenizes '|0>' and '|1>' as kets"
          $ testParse "Kets" "|0>" (unsafePerformIO $ runParserT ket [] "" "|0>")
        context "operators" $
          mapM_ (checkTokenValidator id (\s -> "parses the operator '"++s++"'") reservedOp) ops
        context "symbols" $ do
          mapM_ (checkTokenValidator id (\c -> "parses the symbol '"++c++"'") symbolDiscard) $ map (:[]) symbols
        context "Reserved Words" $ do
          mapM_ (checkTokenValidator id (\s -> "parses the reserved word '"++s++"'") reserved) reserveds
        context "Quantum Gates" $ do
          mapM_ (checkParse id (\s -> "parses the gate '"++s++"'") quantumGate) $ allGates
        context "numbers " $ do
          mapM_ (checkParse show (\n -> "parses the number '"++n++"'") integer) $ numbers
        context "identifiers " $ do
          mapM_ (checkParse id (\s -> "parses the identifier '"++s++"'") identifier) $ legalVariables
        context "Types and Constructors " $ do
          mapM_ (checkParse id (\s -> "parses the Constructor or TypeID '"++s++"'") constructor) $ legalConstructors
        context "comments " $ do
          it "should accept /* and */ as delimiters for multiline comments" $
            testTokenValidator "/* comments" (unsafePerformIO $ runParserT (exhaustParser $ whiteSpace ) [] "" "/* cm \n cms */")
          it "should accept // as a line comment" $
            testTokenValidator "// comments" (unsafePerformIO $ runParserT (exhaustParser $ whiteSpace ) [] "" "// cm \n")
          it "should ignore items within /* and */" $
            testParseResult (unsafePerformIO $ runParserT (exhaustParser $ wket) [] "" "/* junk */|0>") (unsafePerformIO $ runParserT (exhaustParser $ wket) [] "" "|0>")
          it "should ignore lines starting with //" $
              testParseResult (unsafePerformIO $ runParserT (exhaustParser $ wket) [] "" "// junk \n|0>") (unsafePerformIO $ runParserT (exhaustParser $ wket) [] "" "|0>")
        context "miscellaneous tokenizers" $ do
          it "parses 'a' as filename a"
            $ testParse "filenames" "a" (unsafePerformIO $ runParserT validFileName [] "" "a")
          it "parses 'a  \\n' as filename a"
            $ testParse "filenames" "a" (unsafePerformIO $ runParserT validFileName [] "" "a  \n")
          it "parses 'ab  \\n' as filename ab"
            $ testParse "filenames" "ab" (unsafePerformIO $ runParserT validFileName [] "" "ab  \n")
          it "parses 'a b  \\n' as filename 'a b'"
            $ testParse "filenames" "a b" (unsafePerformIO $ runParserT validFileName [] "" "a b  \n")
          it "parses 'file.ext' as filename file.ext"
            $ testParse "filenames" "file.ext" (unsafePerformIO $ runParserT validFileName [] "" "file.ext")
          it "parses 'include\\nexclude' as filename include"
            $ testParse "filenames" "include" (unsafePerformIO $ runParserT validFileName [] "" "include\nexclude")
      context "Parser" $ do
        context "terms" $
          mapM_ (uncurry (checkParseToResult (\s -> "parses the term '"++s++"'") term ) ) testTerms
        context "terms  as expressions " $
         mapM_ (uncurry (checkParseToResult (\s -> "parses the term '"++s++"'") expr ) ) testTerms
        context "Compound Expressions" $
          mapM_ (uncurry (checkParseToResult (\s -> "parses the expression '"++s++"'") expr ) ) testExpressions
        context "Statements" $
          mapM_ (uncurry (checkParseToResult (\s -> "parses the statement '"++s++"'") statement ) ) testStatements
        context "Data Definitions" $
          mapM_ (uncurry (checkParseToResult (\s -> "parses the data definition '"++s++"'") dataDef ) ) testDataDefs
        context "Procedure Definitions" $
          mapM_ (uncurry (checkParseToResult (\s -> "parses the procedure definition '"++s++"'") procDef ) ) testProcDefs
      context "Imports" $ do
        it "should produce a program from a single import" $
          (case (unsafePerformIO $ runParserT (exhaustParser  $ prog fpForTest) [] "" "#Import f") of
            Left e   -> expectationFailure $ " error: " ++ show e
            Right a  -> a `shouldBe` a)
        it "should produce the correct parse from an import" $
            (case (unsafePerformIO $ runParserT (exhaustParser  $ prog fpForTest) [] "" "#Import f") of
              Left e   -> expectationFailure $  " error: " ++ show e
              Right p  -> p `shouldBe` (parseResultOf "f"))
        it "should ignore leading and trailing blanks and produce the correct parse from an import" $
            (case (unsafePerformIO $ runParserT (exhaustParser  $ prog fpForTest) [] "" "#Import f\n \n") of
              Left e   -> expectationFailure $  " error: " ++ show e
              Right p  -> p `shouldBe` (parseResultOf "f"))
        it "should handle a complex program" $
            (case (unsafePerformIO $ runParserT (exhaustParser  $ prog fpForTest) [] "" "#Import g") of
              Left e   -> expectationFailure $  " error: " ++ show e
              Right p  -> p `shouldBe` (parseResultOf "g"))
        it "should continue the parse after an import" $
            (case (unsafePerformIO $ runParserT (exhaustParser  $ prog fpForTest) [] "" "#Import f\nap::(| ; d:Bool)= {skip}") of
              Left e   -> expectationFailure  $  " error: " ++ show e
              Right p  -> p `shouldBe` (((parseResultOf "f") ++ [ProcDef $ Procedure "ap" [] [] [ParameterDefinition "d" BOOL ] [] [Skip]])))
        it "should parse multiple imports" $
            (case (unsafePerformIO $ runParserT (exhaustParser  $ prog fpForTest) [] "" "#Import f\n#Import g") of
              Left e   -> expectationFailure $  " error: " ++ show e
              Right p  -> p `shouldBe` ((parseResultOf "f") ++ (parseResultOf "g")))
        it "should handle nested multiple imports" $
           case (unsafePerformIO $ runParserT (exhaustParser  $ prog fpForTest) [] "" "#Import top") of
              Left e   -> expectationFailure $  " error: " ++ show e
              Right p  -> p `shouldBe` (parseResultOf "top")
        it "should only import a file once." $
          testParseResult (unsafePerformIO $ runParserT (exhaustParser $ prog fpForTest) [] "" "#Import f") (unsafePerformIO $ runParserT (exhaustParser $ prog fpForTest) [] "" "#Import f\n#Import f")


\end{code}
Parser test data
\begin{code}

    identityGates = ["I1", "I2", "I456"]
    baseGates = identityGates ++ fixedGates
    inverseGates = map ("Inv-"++) baseGates
    allGates = baseGates ++ inverseGates
    numbers = [1,0,23, 9283719293, -3452]

    legalVariables = ["a", "abcde1234", "a'", "a'''''456","u3'4'", "xry''"] ++ (map (++"'") reservedKeyWords)

    legalConstructors = ["A", "CD", "Cdk9'", "CC'9hy", "X'", "Qubi", "Qubit'"] ++ (map (++"'") baseGates) ++
      (map (++"'") reservedTypes)

    fileNames = [("a", "a"), ("file.ext", "file.ext"), ("include\nDoNotInclude","include")]


    testTerms =  [("iden", Evar "iden"), ("23", Enum 23), ("true", Ebool True),
                  ("false", Ebool False), ("|0>", EQubit Zero), ("|1>", EQubit One),
                  ("(a)", Evar "a"),
                  ("Cons", Econs "Cons" []),
                  ("Cons()", Econs "Cons" []),
                  ("Cons(a,b)", Econs "Cons" [Evar "a", Evar "b"]),
                  ("sub()", Ecall "sub" [] [] []),
                  ("sub(a)", Ecall "sub" [] [Evar "a"] []),
                  ("sub(b, a mod b)", Ecall "sub" [] [Evar "b", Eapply Mod (Evar "a") (Evar "b")] []),
                  ("sub(a ; b)", Ecall "sub" [] [Evar "a"] ["b"]),
                  ("sub( ; b)", Ecall "sub" [] [] ["b"]),
                  ("sub(a ; )", Ecall "sub" [] [Evar "a"] []),
                  ("sub(a | b)", Ecall "sub" [Evar "a"] [Evar "b"] []),
                  ("sub( | b)", Ecall "sub" [] [Evar "b"] []),
                  ("sub(a | )", Ecall "sub" [Evar "a"] [] []),
                  ("sub(|  ; )", Ecall "sub" [] [] []),
                  ("sub(|  ; c )", Ecall "sub" [] [] ["c"]),
                  ("sub( | b ;  )", Ecall "sub" [] [Evar "b"] []),
                  ("sub(| b ; c )", Ecall "sub" [] [Evar "b"] ["c"]),
                  ("sub(a | ; )", Ecall "sub" [Evar "a"] [] []),
                  ("sub(a |  ; c )", Ecall "sub" [Evar "a"] [] ["c"]),
                  ("sub(a | b ;  )", Ecall "sub" [Evar "a"] [Evar "b"] []),
                  ("sub(a | b ; c )", Ecall "sub" [Evar "a"] [Evar "b"] ["c"])
                 ]

    testExpressions =  [("a >> 3",Eapply Oprshift (Evar "a") (Enum 3)) ,
                        ("a << 3", Eapply Oplshift (Evar "a") (Enum 3) ),
                        ("a + 3", Eapply Add (Evar "a") (Enum 3) ),
                        ("a - 3", Eapply Sub (Evar "a") (Enum 3) ),
                        ("a * 3", Eapply Mul (Evar "a") (Enum 3) ),
                        ("a / 3", Eapply Div (Evar "a") (Enum 3) ),
                        ("a || 3", Eapply Or (Evar "a") (Enum 3) ),
                        ("a =< 3", Eapply Ople (Evar "a") (Enum 3) ),
                        ("a == 3", Eapply Opeq (Evar "a") (Enum 3) ),
                        ("a ^ 3", Eapply Xor (Evar "a") (Enum 3) ),
                        ("a && 3", Eapply And (Evar "a") (Enum 3) ),
                        ("a < 3", Eapply Oplt (Evar "a") (Enum 3) ),
                        ("a > 3", Eapply Opgt (Evar "a") (Enum 3) ),
                        ("a mod 3", Eapply Mod (Evar "a") (Enum 3) ),
                        ("a rem 3", Eapply Rem (Evar "a") (Enum 3) ),
                        ("a =/= 3", Eapply Opneq (Evar "a") (Enum 3) ),
                        ("a >= 3", Eapply Opge (Evar "a") (Enum 3) ),
                        ("-a", Eminus (Evar "a")), ("~a", Enot (Evar "a")),
                        ("a << b * c - -d && e || ~f",
                         Eapply Or (Eapply And (Eapply Sub (Eapply Mul (Eapply Oplshift (Evar "a") (Evar "b")) (Evar "c")) (Eminus (Evar "d"))) (Evar "e")) (Enot (Evar "f")))
                        ]


    testStatements = [("a = 3", Assignment "a" (Enum 3)),
                      ("a=3", Assignment "a" (Enum 3)),
                      ("a=|1>", Assignment "a" (EQubit One)),
                      ("case a of C => {} ",CaseSt (Evar "a") [(CaseClause "C" [],[])]),
                      ("case a of C(f) => {} ",CaseSt (Evar "a") [(CaseClause "C" ["f"],[])]),
                      ("case a of C => {skip} ",CaseSt (Evar "a") [(CaseClause "C" [],[Skip])]),
                      ("case a of C => {zero} D(b) => {skip}",CaseSt (Evar "a") [(CaseClause "C" [],[ZeroStack]),(CaseClause "D" ["b"],[Skip])]),
                      ("case a of C => {zero} D(b,_) => {skip}",CaseSt (Evar "a") [(CaseClause "C" [],[ZeroStack]),(CaseClause "D" ["b","_"],[Skip])]),
                      ("case a of  D(b,_) => {skip}",CaseSt (Evar "a") [(CaseClause "D" ["b","_"],[Skip])]),
                      ("measure a of |0> => {zero} |1> => {skip}", Measure (Evar "a") [ZeroStack] [Skip]),
                      ("measure a of |1> => {zero} |0> => {skip}", Measure (Evar "a") [Skip] [ZeroStack]),
                      ("a := 3", UseAssign "a" (Enum 3)),
                      ("Had q", Call "Had"  [] [Evar "q"] ["q"] []),
                      ("Swap a b", Call "Swap"  [] [Evar "a", Evar "b"] ["a","b"] []),
                      ("use a,b in {skip}", Use ["a","b"] [Skip]),
                      ("use a", UseFromHere ["a"]),
                      ("(a,b) = sub()", Call "sub" [] [] ["a","b"] []),
                      ("(a,b) = sub(c,d)", Call "sub" [] [Evar "c", Evar "d"] ["a","b"] []),
                      ("(a,b) = sub(|)", Call "sub" [] [] ["a","b"] []),
                      ("(a,b) = sub(e,f|)", Call "sub" [Evar "e", Evar "f"] [] ["a","b"] []),
                      ("(a,b) = sub(|c,d)", Call "sub" [] [Evar "c", Evar "d"] ["a","b"] []),
                      ("(a,b) = sub(e,f|c,d)", Call "sub" [Evar "e", Evar "f"] [Evar "c", Evar "d"] ["a","b"] []),
                      ("sub",Call "sub" [] [] [] []),
                      ("sub a b c",Call "sub" [] [Evar "a", Evar "b", Evar "c"] ["a","b","c"] []),
                      ("sub() a b c",Call "sub" [] [Evar "a", Evar "b", Evar "c"] ["a","b","c"] []),
                      ("sub(e,f) a b c",Call "sub" [Evar "e", Evar "f"] [Evar "a", Evar "b", Evar "c"] ["a","b","c"] []),
                      ("sub(|) a b c",Call "sub" [] [Evar "a", Evar "b", Evar "c"] ["a","b","c"] []),
                      ("sub(e,f|) a b c",Call "sub" [Evar "e", Evar "f"] [Evar "a", Evar "b", Evar "c"] ["a","b","c"] []),
                      ("sub(|g,h) a b c",Call "sub" [] [Evar "g", Evar "h", Evar "a", Evar "b", Evar "c"] ["a","b","c"] []),
                      ("sub(e,f|g,h) a b c",Call "sub" [Evar "e", Evar "f"] [Evar "g", Evar "h", Evar "a", Evar "b", Evar "c"] ["a","b","c"] []),
                      ("sub(|  ; )", Call "sub" [] [] [] []),
                      ("sub(|  ; c )", Call "sub" [] [] ["c"] []),
                      ("sub( | b ;  )", Call "sub" [] [Evar "b"] [] []),
                      ("sub(| b ; c )", Call "sub" [] [Evar "b"] ["c"] []),
                      ("sub(a | ; )", Call "sub" [Evar "a"] [] [] []),
                      ("sub(a |  ; c )", Call "sub" [Evar "a"] [] ["c"] []),
                      ("sub(a | b ;  )", Call "sub" [Evar "a"] [Evar "b"] [] []),
                      ("sub(a | b ; c )", Call "sub" [Evar "a"] [Evar "b"] ["c"] []),
                      ("sub(a ; b)", Call "sub" [] [Evar "a"] ["b"] []),
                      ("sub( ; b)", Call "sub" [] [] ["b"] []),
                      ("sub(a ; )", Call "sub" [] [Evar "a"] [] []),
                      ("discard a", Discard ["a"]),
                      ("discard a,b,c", Discard ["a","b","c"]),
                      ("{skip ; zero;}", BlockStatement [Skip, ZeroStack]),
                      ("{skip ; zero}", BlockStatement [Skip, ZeroStack]),
                      ("{skip}", BlockStatement [Skip]),
                      ("{}", BlockStatement []),
                      ("if 1 => {skip} a => {zero} else => {skip;zero}", Guard [GuardClause (Enum 1) [Skip], GuardClause (Evar "a") [ZeroStack], GuardClause (Ebool True) [Skip, ZeroStack]]),
                      ("if 1 => {skip}  else => {skip;zero}",Guard [GuardClause (Enum 1) [Skip], GuardClause (Ebool True) [Skip, ZeroStack]]),
                      ("if else => {skip;zero}",BlockStatement [Skip, ZeroStack]),
                      ("{skip <= a} <= ~c", ControlledBy (BlockStatement [ControlledBy Skip [OneControl "a"]]) [ZeroControl "c"]),
                      ("skip <= a", ControlledBy Skip [OneControl "a"]),
                      ("skip <= a,b", ControlledBy Skip [OneControl "a", OneControl "b"]),
                      ("skip <= a,~b", ControlledBy Skip [OneControl "a",ZeroControl "b"]),
                      ("skip", Skip),
                      ("zero", ZeroStack)
                    ]

    testDataDefs = [("qdata Coin = {H|T}",
                      DataDef (TypeDefinition "Coin" []) [Constructor "H" [], Constructor "T" []]),
                    ("qdata List a = {Cons (b,List(c))|Nil}",
                      DataDef (TypeDefinition "List" ["a"]) [Constructor "Cons" [TypeVariable "b", DeclaredType "List" [TypeVariable "c"]], Constructor "Nil" []]),
                    ("qdata T a b c = {B(d,D(E(e))) | F(g,Qubit,Int,Bool, G(h, H(i, j)))}",
                        DataDef (TypeDefinition "T" ["a","b","c"]) [Constructor "B" [TypeVariable "d", DeclaredType "D" [DeclaredType "E" [TypeVariable "e"]]], Constructor "F" [TypeVariable "g", QUBIT, INT, BOOL, DeclaredType "G" [TypeVariable "h", DeclaredType "H" [TypeVariable "i", TypeVariable "j"]]]])
                    ]

    testProcDefs = [("app::(| ; )= {skip}",
                     Procedure "app" [] [] [] [] [Skip]),
                    ("app::(| ; d:Bool)= {skip}",
                     Procedure "app" [] [] [ParameterDefinition "d" BOOL ] [] [Skip]),
                    ("app::(| c:List(f);)= {skip}",
                     Procedure "app" [] [ParameterDefinition "c" (DeclaredType "List" [TypeVariable "f"])] [] [] [Skip]),
                    ("app::(| c:List(f); d:Bool)= {skip}",
                     Procedure "app" [] [ParameterDefinition "c" (DeclaredType "List" [TypeVariable "f"])] [ParameterDefinition "d" BOOL ] [] [Skip]),
                    ("app::(b:e| ; )= {skip}",
                     Procedure "app" [ParameterDefinition "b" (RigidVariable "e")] [] [] [] [Skip]),
                    ("app::(b:e| ; d:Bool)= {skip}",
                     Procedure "app" [ParameterDefinition "b" (RigidVariable "e")] [] [ParameterDefinition "d" BOOL ] [] [Skip]),
                    ("app::(b:e| c:List(f); )= {skip}",
                     Procedure "app" [ParameterDefinition "b" (RigidVariable "e")] [ParameterDefinition "c" (DeclaredType "List" [TypeVariable "f"])] [] [] [Skip]),
                    ("app::(b:e| c:List(f); d:Bool)= {skip}",
                     Procedure "app" [ParameterDefinition "b" (RigidVariable "e")] [ParameterDefinition "c" (DeclaredType "List" [TypeVariable "f"])] [ParameterDefinition "d" BOOL ] [] [Skip]),
                    ("app::()= {skip}",
                     Procedure "app" [] [] [] [] [Skip]),
                    ("app::(b:e, c:List(f); d:Bool)= {skip}",
                     Procedure "app" [] [ParameterDefinition "b" (RigidVariable "e"), ParameterDefinition "c" (DeclaredType "List" [TypeVariable "f"])] [ParameterDefinition "d" BOOL ] [] [Skip])
                    ]



\end{code}

Parser spec helpers.
\begin{code}


    symbolDiscard :: String -> Parser ()
    symbolDiscard s = do
      symbol s
      return ()

    testParseResult :: (Show a, Eq a) => Either ParseError a -> Either ParseError  a -> Expectation
    testParseResult (Right a) (Right b)  = a `shouldBe` b
    testParseResult _ _                  = expectationFailure "Parse failed"

    testParse :: Show a => String -> String -> Either ParseError  a -> Expectation
    testParse err t (Right t') =
          (t,t') `shouldSatisfy` (\(t,t') -> (t == (show t') || (show t == show t')))
    testParse err t (Left p)  = expectationFailure $ err ++ ": expected " ++ show t ++ ", but got "++ show p

    testMultipleParse :: Show a => String -> [String] -> Either ParseError [a] -> Expectation
    testMultipleParse err t (Right t') =
          (t,t') `shouldSatisfy` (\(t,t') -> (length t == length t') && (and $ zipWith (==) t (map show t')))

    testMultipleParse err t (Left p)  = expectationFailure $ err ++ ": expected " ++ show t ++ ", but got "++ show p

    --testTokenValidator :: String -> Either ParseError  () -> Test.Hspec.Core.Spec.Result
    testTokenValidator err (Right a)  = a `shouldBe` a
    testTokenValidator err (Left p)  = expectationFailure $ err ++ ": expected, but got "++ show p

    --checkTokenValidator :: (a -> String) -> (String -> String) -> (String -> Parser ()) -> a -> SpecM ()
    checkTokenValidator mkParseString mkItString  tstTokenizer inp =
      let pstring = mkParseString inp
          istring = mkItString pstring
      in it istring $ testTokenValidator "" (unsafePerformIO $ runParserT (exhaustParser (tstTokenizer pstring)) [] "" pstring)

    --checkParse :: Show a => (a -> String) -> (String -> String) -> Parser a -> a -> SpecM ()
    checkParse mkParseString mkItString  tstTokenizer inp =
      let pstring = mkParseString inp
          istring = mkItString pstring
      in it istring $ testParse "" pstring (unsafePerformIO $ runParserT (exhaustParser tstTokenizer) [] "" pstring)

    --checkParseToResult :: Show a => (String -> String) -> Parser a -> String -> a -> SpecM ()
    checkParseToResult mkItString  p inp res =
      let istring = mkItString inp
      in it istring $ testParse "" (show res) (unsafePerformIO $ runParserT (exhaustParser p) [] "" inp)

    exhaustParser ::  Parser a -> Parser a
    exhaustParser p = do
      r <- p
      inp <- getInput
      case inp of
        "" -> return r
        _  -> parserFail $  "Parser did not use all input, remaining is '"++inp++"'."

\end{code}

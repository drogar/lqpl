\begin{code}
  module Compiler.CompilerSpecHelper where


    import Utility.FileProvider
    import Utility.FileProvider.FileSystem
    import Compiler.QPLParser
    import Compiler.Qtypes

    testImportFiles = [("f", "qdata C = {H|T} \napp::(| ; )= {skip}", [DataDef (TypeDefinition "C" []) [Constructor "H" [], Constructor "T" []], ProcDef (Procedure "app" [] [] [] [] [Skip]) ]),
      ("g", "qdata L a = {N|C(a,L(a))} \n \n app::(l:L(a),m:L(a) ;o:L(a) )= \n {case l of //`whatever `\n  N => {skip} \n  C(h,t) => \n {skip} /*more whatev*/\n}", [DataDef (TypeDefinition "L" ["a"]) [Constructor "N" [], Constructor "C" [TypeVariable "a", DeclaredType "L" [TypeVariable "a"]]], ProcDef (Procedure "app" [] [ParameterDefinition "l" (DeclaredType "L" [TypeVariable "a"]),ParameterDefinition "m" (DeclaredType "L" [TypeVariable "a"])] [ParameterDefinition "o" (DeclaredType "L" [TypeVariable "a"])] [] [CaseSt (Evar "l") [(CaseClause "N" [],[Skip]),(CaseClause "C" ["h","t"],[Skip])]]) ]),
      ("top", "#Import bot \n#Import next",[DataDef (TypeDefinition "C" []) [Constructor "H" [], Constructor "T" []], ProcDef (Procedure "app" [] [] [] [] [Skip]) ]),
      ("bot", "qdata C={H|T}",[DataDef (TypeDefinition "C" []) [Constructor "H" [], Constructor "T" []]]),
      ("next", "#Import bot\n \n app::(| ; )= {skip}",[ProcDef (Procedure "app" [] [] [] [] [Skip])])
      ]

\end{code}
File Provider implementation for testing
\begin{code}

    fpForTest :: FileProvider
    fpForTest = FileProvider {
    fpDoesFileExist = \ f -> do
        let m = pickFpFrom testImportFiles f
        case m of
          Nothing  -> return False
          Just _   -> return True,
      fpReadFile = testFpReader,
      emptyProvider = "",
      currentFPDir = "",
      fpcombine = (++),
      getFirstFileInSearchPath = \p f -> do
        s <- testFpReader f
        return $ Just (f,s)
      }

    testFpReader f = do
      let ms = pickInputFrom testImportFiles f
      case ms of
        Nothing -> return ""
        Just s  -> return s
    parseResultOf :: String -> Program
    parseResultOf f = case pickParseFrom testImportFiles  f of
                        Nothing -> ([]::Program)
                        Just p  -> p

    pickFpFrom :: [(String,String,a)] -> String -> Maybe String
    pickFpFrom [] _ = Nothing
    pickFpFrom ((s,_,r):rest) s' | s == s' = Just s
                             | otherwise = pickFpFrom rest s'

    pickParseFrom :: (Show a, Eq a) => [(String,String,a)] -> String -> Maybe a
    pickParseFrom [] _ = Nothing
    pickParseFrom ((s,_,r):rest) s' | s == s' = Just r
                             | otherwise = pickParseFrom rest s'

    pickInputFrom :: [(String,String,a)] -> String -> Maybe String
    pickInputFrom [] _ = Nothing
    pickInputFrom ((s,r,_):rest) s' | s == s' = Just r
                             | otherwise = pickInputFrom rest s'



\end{code}
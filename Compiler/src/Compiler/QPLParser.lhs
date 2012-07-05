\begin{code}
  module Compiler.QPLParser where

    import Compiler.Qtypes
    import Utility.FileProvider
    import Text.Parsec
    import Text.Parsec.Expr
    import Text.Parsec.Char
    import Text.Parsec.Pos
    import qualified Text.Parsec.Token as P
    import Data.Functor.Identity
    import Control.Monad
    import Control.Monad.IO.Class

    import System.FilePath

    import System.IO

    parseQPL :: FileProvider->String->String->String->[String] -> IO Program
    parseQPL fp dir file s ss = do
      errOrProgram <- runParserT (prog fp) (dir:ss) file s
      case  errOrProgram of
        Left e     -> ioError $ userError $ show e
        Right p    -> return p



    type Parser a = ParsecT String [String] IO a

\end{code}
\subsection{Parser}
\begin{code}
    prog:: FileProvider -> Parser Program
    prog fp = do
      whiteSpace
      p <- (dataDef >>= \a -> return [a]) <|> (procDef >>= \a -> return $ [ProcDef a]) <|> imp fp
      rest <- (try (whiteSpace >> eof >> return [])) <|> prog fp
      return $ p ++ rest

    imp :: FileProvider -> Parser Program
    imp fp = try $ do
      whiteSpace
      string "#Import "
      whiteSpace
      impname <- validFileName
      currState <- getParserState
      --showInput "import - remaining:"
      let fils = stateUser currState
      if impname `elem` fils
        then return []
        else do
          newinp <- liftIO $ (getFirstFileInSearchPath fp) fils impname
          case newinp of
            Nothing -> parserFail $ "import not found:" ++ impname
            Just (f,contents)  -> do
              let fils' = impname:fils
                  newstate = State contents (newPos impname 0 0) fils'
              setParserState newstate
              --showInput "Process import: "
              rval <- prog fp
              --liftIO $ hPutStrLn stderr $ "Import gave: "++(show rval)
              --showInput "after import - remaining:"
              newfilss <- liftM stateUser getParserState
              setParserState currState{stateUser = newfilss}
              --showInput "After state reset - remaining inp: "
              return rval


    -- showInput :: String -> Parser ()
    -- showInput s = do
    --   inp <- getInput
    --   liftIO $ hPutStrLn stderr $ s ++ '\'':inp ++ "'"

    procDef :: Parser Procedure
    procDef = try $ do
      iden <- identifier
      reservedOp "::"
      (classicalParms, quantumParms, outParms) <- option ([],[],[]) $ parens $ do
        cParms <- option [] $ try $ do
          ps <- commaSep parameter
          symbol "|"
          return ps
        qParms <- commaSep parameter
        oParms <- option [] $ try $ semi >> commaSep parameter
        return (cParms, qParms, oParms)
      reservedOp "="
      BlockStatement ss <- stmtBlock
      return $ Procedure iden classicalParms quantumParms outParms [] ss

    parameter = do
      iden <- identifier
      colon
      t <- typevar
      case t of
        TypeVariable a  -> return $ ParameterDefinition iden (RigidVariable a)
        _               -> return $ ParameterDefinition iden t

    dataDef :: Parser GlobalDefinition
    dataDef = try $ do
      reserved "qdata"
      tname <- constructor
      tvars <- many identifier
      reservedOp "="
      cons <- braces $ ( do
        c <- constructor
        tvars <- option [] $ try $ parens $ commaSep1 typevar
        return $ Constructor c tvars
        ) `sepBy1` (symbol "|")
      return $ DataDef (TypeDefinition tname tvars) cons

    typevar = choice [tvarBuiltIn, tvarIdentifier, tvarDeclaredType]

    tvarIdentifier = try $ do
      iden <- identifier
      return $ TypeVariable iden

    tvarDeclaredType = try $ do
      c <- constructor
      tvars <- option [] $ try $ parens $ commaSep1 typevar
      return $ DeclaredType c tvars

    tvarBuiltIn = try $ (try $ reserved "Qubit" >> (return QUBIT)) <|>
      (try $ reserved "Int" >> return INT) <|> (reserved "Bool" >> return BOOL)


    statement :: Parser Statement
    statement = choice [stmtControlledBy, uncontrolledStatement]

    stmtControlledBy = try $ do
      scb <- uncontrolledStatement
      reservedOp "<="
      controls <- commaSep1 controller
      return $ ControlledBy scb controls

    controller = do
      modif <- optionMaybe $ try $ reservedOp "~"
      iden <- identifier
      case modif of
        Nothing  -> return $ OneControl iden
        Just _   -> return $ ZeroControl iden

    uncontrolledStatement = choice [stmtAssign,stmtSkip,stmtZero, stmtBlock,
        stmtIf, stmtCase, stmtDiscard, stmtMeasure, stmtUse, stmtCall]

    stmtAssign = try $ do
      iden <- identifier
      reservedOp "="
      e <- expr
      return $ Assignment iden e

    stmtSkip = try $ do
      reserved "skip"
      return Skip

    stmtZero = try $ do
      reserved "zero"
      return ZeroStack

    stmtBlock = try $ do
      ss <- braces $ semiSepEndBy statement
      return $ BlockStatement ss

    stmtIf = try $ do
      reserved "if"
      gs <- many $ do
              e <- expr
              reservedOp "=>"
              BlockStatement ss <- stmtBlock
              return $ GuardClause e ss
      reserved "else"
      reservedOp "=>"
      BlockStatement ss <- stmtBlock
      case gs of
        []   -> return $ BlockStatement ss
        _    -> return $ Guard $ gs ++ [GuardClause (Ebool True) ss]

    stmtMeasure = try $ do
      reserved "measure"
      e <- expr
      reserved "of"
      alts <- count 2 $ do
        k <- ket
        reservedOp "=>"
        BlockStatement ss <- stmtBlock
        return (k,ss)
      case alts of
        [("|0>",s0s), (_,s1s)] ->  return $ Measure e s0s s1s
        [("|1>",s1s), (_,s0s)] ->  return $ Measure e s0s s1s
        _                      ->  parserFail $ "Unexpected alternatives in measure: "++ show alts

    stmtDiscard = try $ do
      reserved "discard"
      ids <- commaSep1 identifier
      return $ Discard ids

    stmtCase = try $ do
      reserved "case"
      e <- expr
      reserved "of"
      alts <- many1 $ do
                 c <- constructor
                 patterns <- option [] $ try $ parens $  commaSep (try identifier <|> symbol "_")
                 reservedOp "=>"
                 BlockStatement ss <- stmtBlock
                 return (CaseClause c patterns, ss)
      return $ CaseSt e alts

    stmtUse = try $ choice [stmtUseAssign, stmtUseBlock]

    stmtUseAssign = try $ do
      iden <- identifier
      reservedOp ":="
      ex <- expr
      return $ UseAssign iden ex

    stmtUseBlock = try $ do
      reserved "use"
      ids <- commaSep1 identifier
      stmts <- optionMaybe $ try $ do
        reserved "in"
        BlockStatement ss <- stmtBlock
        return ss
      case stmts of
        Just sss  -> return $ Use ids sss
        Nothing   -> return $ UseFromHere ids

    stmtCall = try $ choice [stmtCallAssign, stmtCallWithOptionalIds]

    stmtCallAssign = try $ do
      ids <- parens $ commaSep1 identifier
      reservedOp "="
      call <- identifier
      (cexps, qexps,ids2)  <- option ([], [], []) callParameters
      return $ Call call cexps qexps ids []


    stmtCallWithOptionalIds = try $ do
      Call call cs qs idens os <- stmtBareCall
      ids <- many identifier
      return $ Call call cs (qs ++ map Evar ids) (idens ++ ids) os

    stmtBareCall = try $ do
      subrout <- try identifier <|> quantumGate
      (cexps, qexps,ids)  <- option ([], [], []) $ (try callClassicalparms) <|> callParameters
      return $ Call subrout cexps qexps ids []

    callClassicalparms = parens $ do
      cs <- commaSep expr
      return (cs,[],[])

    callParameters = parens $ do
      cexps <- option [] $  try $ do
        es <- commaSep expr
        symbol "|"
        return es
      qexps <- commaSep expr
      ids <- option [] $ try $ semi >> commaSep identifier
      return (cexps, qexps, ids)

    expr :: Parser Expression
    expr = buildExpressionParser expressionTable term
    expressionTable = [[prefix "-" Eminus ]
              , [binary "<<" (Eapply Oplshift) AssocLeft, binary ">>" (Eapply Oprshift) AssocLeft]
              , [binary "*" (Eapply Mul) AssocLeft, binary "/" (Eapply Div) AssocLeft,
                 binary "rem" (Eapply Rem) AssocLeft, binary "mod" (Eapply Mod) AssocLeft ]
              , [binary "+" (Eapply Add) AssocLeft, binary "-" (Eapply Sub)   AssocLeft ]
              , [binary "=/=" (Eapply Opneq) AssocLeft, binary "==" (Eapply Opeq)   AssocLeft ,
                 binary "=<" (Eapply Ople) AssocLeft, binary ">=" (Eapply Opge)   AssocLeft ,
                 binary "<" (Eapply Oplt) AssocLeft, binary ">" (Eapply Opgt)   AssocLeft ,
                 prefix "~" Enot]
              , [binary "&&" (Eapply And) AssocLeft]
              , [binary "||" (Eapply Or) AssocLeft, binary "^" (Eapply Xor) AssocLeft]
              ]

    binary  name fun assoc = Infix (do{ reservedOp name; return $ fun }) assoc
    prefix  name fun       = Prefix (do{ reservedOp name; return fun })
    postfix name fun       = Postfix (do{ reservedOp name; return fun })

    term :: Parser Expression
    term = choice [parens expr, tcall, tIdentifer, tnum, tbool "true", tbool "false", tket, tconstructor]

    tIdentifer = try $ do
      e <- identifier
      return $ Evar e

    tnum = try $ do
      e <- integer
      return $ Enum (fromInteger e)

    tbool t = try $ do
      reserved t
      case t of
        "true"  -> return $ Ebool True
        "false" -> return $ Ebool False
        a       -> parserFail $ "expecting to parse a bool value, not '"++a++"'"

    tcall = try $ do
      subrout <- identifier
      (cexps, qexps,ids)  <- callParameters
      return $ Ecall subrout cexps qexps ids

    tconstructor = try $ choice [tconsWithParms,tcons]

    tcons = try $ do
      c <- constructor
      return $ Econs c []

    tconsWithParms = try $ do
      c <- constructor
      es <- parens $ commaSep expr
      return $ Econs c es

    tket = try $ do
      k <- ket
      case k of
        "|0>" -> return $ EQubit Zero
        "|1>" -> return $ EQubit One



\end{code}
\subsection{Tokenizer}
\begin{code}
    -- NOTE: The ordering is important in this list to ensure single char ops dont
    --       pre-empt multi-char during tokenization
    -- Ordering of gates and reserved is not as we check the chars after.
    symbols = "(){},:;|_"
    reserveds = reservedKeyWords ++ reservedTypes
    reservedKeyWords = ["call","discard","of","if","rem","mod","measure","new","proc",
      "case","qdata","use","in","else","zero","true","false", "skip"]
    reservedTypes = ["Qubit","Int","Bool"]

    fixedGates = ["Had","Phase","Not","RhoX","MinusI",
      "Swap","Toffoli3","Rot","RhoY","RhoZ","T"]

    ops =   ["||" , "&&" , "==" , "=/=" , "=<" ,
            ">=" , ">>" , "<<" , "::" , "=>" , ":=" ,"<=", "=" ,  "*" , "-" , "/" ,  "+",
            "<" , ">" , "~" , "^"
            ]

    lqplDef :: P.GenLanguageDef String [String] IO
    lqplDef = P.LanguageDef
                   { P.commentStart   = "/*"
                   , P.commentEnd     = "*/"
                   , P.commentLine    = "//"
                   , P.nestedComments = True
                   , P.identStart     = lower
                   , P.identLetter    = alphaNum <|> oneOf "_'"
                   , P.opStart        = oneOf ""
                   , P.opLetter       = oneOf ""
                   , P.reservedOpNames= ops
                   , P.reservedNames  = reserveds
                   , P.caseSensitive  = True
                   }
    lexer = P.makeTokenParser lqplDef

    identifier = P.identifier lexer
    reserved = P.reserved lexer
    reservedOp = P.reservedOp lexer
    integer = P.integer lexer
    symbol = P.symbol lexer
    lexeme = P.lexeme lexer
    parens = P.parens lexer
    commaSep = P.commaSep lexer
    commaSep1 = P.commaSep1 lexer
    braces = P.braces lexer
    semi = P.semi lexer
    semiSep = P.semiSep lexer
    semiSep1 = P.semiSep1 lexer
    colon = P.colon lexer
    whiteSpace = P.whiteSpace lexer

    semiSepEndBy p = sepEndBy p semi


    ket =
      lexeme $ try $ do
        char '|'
        k <- char '0' <|> char '1'
        char '>'
        return $ '|':k:">"

    idengate =
      try $ do
        i <- char 'I'
        ds <- many1 digit
        notFollowedBy (P.identLetter lqplDef)
        return $ i:ds
    fixedGate =
      try $ do
        fg <- choice (map (try . string) fixedGates)
        notFollowedBy (P.identLetter lqplDef)
        return fg

    baseGate = idengate <|> fixedGate

    quantumGate =
      lexeme $ try $ do
        inv <-option [] $ try $ string "Inv-"
        bgate <- baseGate
        return $ inv ++ bgate

    constructor =
      lexeme $ try $ do
        name <- cons
        if (name `elem` reservedTypes)
          then unexpected ("Reserved Type or Constructor " ++ show name)
          else return name

    cons = do
      c <- upper
      cs <- many (P.identLetter lqplDef)
      return (c:cs)

    validFileName = do
       fname <- many1 $ try $ do
          c <- anyChar
          case c of
            '\n' -> parserFail "End of line"
            _ -> return c
       return $ makeValid $ removeTrailingSpaces fname

    removeTrailingSpaces = reverse . removeLeadingSpaces . reverse

    removeLeadingSpaces :: String -> String
    removeLeadingSpaces (' ':s) = removeLeadingSpaces s
    removeLeadingSpaces s = s


\end{code}
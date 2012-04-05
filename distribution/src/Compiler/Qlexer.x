{
module Compiler.Qlexer (
        Token(..),
        Alex(..),
        alexSetInput,
        AlexInput,
        AlexState(..),
        AlexPosn(..),
        showPosn,
        scanner,
        gettokens,
        qpllexer ) where
import Utility.FileProvider

import Compiler.ParseLexMonad
import Compiler.Qtypes
    }


$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]

$bindigit  = 0-1
$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\(\)\{\}\,\:\;\|\_]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol]

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\']

$octit     = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$nl        = [\n\r]


@gate =
        I $digit*|Had|CHad|T|CT|Phase|CPhase|Not|CNot|RhoX|CRhoX| MinusI |
        Swap|CSwap | Toffoli3|Rot |RhoY | CRhoY |  RhoZ | CRhoZ |UM
@reservedid =
        call|discard| of | if | rem | mod |
        measure|new|proc| Qubit| Int | Bool | case |
        qdata | use | in | else | zero | true | false

@ops =
        "=" |  "*" | "-" | "/" |  "+" |
        "||" | "&&" | "==" | "<" | ">" | "~" | "^" | "=/=" | "=<" |
        ">=" | ">>" | "<<" | "::" | "=>" | ":=" | "<="

@varid  = $small $idchar*

@consid = $large $idchar*

@number  = $digit $digit*


qpl :-

  $white+               { skip }
  "//".*                { skip }
  "`".*"`"              { skip }
  "|"[01]">"            { mkKet}
  "#"Import $white .*   { importTok}
  @number               { mkNumberTok}
  @ops                  { mkTok TkOperator }
  $symbol               { mkTok TkSymbol }
  ("Inv-")*@gate        { mkGateTok }
  @reservedid           { mkTok TkReserved }
  @varid                { mkTok TkId }
  @consid               { mkTok TkCons }
{
importTok :: FileProvider a => AlexInput a -> Int -> Alex a Token
importTok ai len
        = do newf <- addNewFile ai len
             case newf of
                Nothing -> alexMonadScan
                Just (fn, fc) ->  do alexAddInput ([AlexPn fn 0 1 1], ' ', [fc])
                                     alexMonadScan

addNewFile :: FileProvider a => AlexInput a -> Int -> Alex a (Maybe (a, String))
addNewFile (psn, _, s) len
        = do let importFile = makeFileProvider $ last $ tail $ words $ take len $ head s
             currentImps <- alexGetImpFiles
             if (importFile `elem` currentImps)
                then return Nothing
                else do dir <- alexGetInpDir
                        dirs <- alexGetImpDirs
                        liftio $ getFirstFileInSearchPath (dir:dirs) importFile



mkTok :: FileProvider a => (String->Token) -> AlexInput a -> Int -> Alex a Token
mkTok t (_,_,str) len = return (t (take len $ head str))

mkGateTok :: FileProvider a => AlexInput a -> Int -> Alex a Token
mkGateTok (_,_,str) len = return $ TkTransform (take len $ head str)


mkKet :: FileProvider a => AlexInput a -> Int -> Alex a Token
mkKet (_,_,str) len = return $ TkKet $ take (len-2) $ tail $ head str


mkNumberTok :: FileProvider a => AlexInput a -> Int -> Alex a Token
mkNumberTok (_,_,str) len =
     do case (take len $ head str) of
                  num -> return $ TkNumber (read num)

lexError s =
     do ((p:ps),_,(input:inps)) <- alexGetInput
        alexError (showPosn p ++ ": " ++ s ++
                   (if (not (null input))
                     then (" before " ++ show (head input))
                     else " at end of file"))

scanner str =
    runAlex str $ do let loop i
                             = do tok <- alexMonadScan;
                                  if tok == TkEof
                                     then return i
                                     else do loop $! (i+1)
                     loop 0

gettokens str =
   runAlex str $ do let loop toks
                            = do tok <- alexMonadScan;
                                 if tok == TkEof
                                    then return toks
                                    else do loop $! (tok:toks)
                    loop []

alexEOF :: FileProvider a => Alex a Token
alexEOF = return (TkEof)




type AlexAction a result = AlexInput a-> Int -> result



alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError inp' -> alexError "lexical error"
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan
    AlexToken inp' len action -> do
        alexSetInput inp'
        action inp len

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip input len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code input len
    = do alexSetStartCode code
         alexMonadScan

-- perform an action for this token, and set the start code to a new value
-- andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len
    = do alexSetStartCode code
         action input len

-- token :: (String -> Int -> token) -> AlexAction token
token t input len = return (t input len)

-- ignore this token, but increment the comment level
-- incCommentLevel :: Int -> AlexAction result
incCommentLevel input len =
    do alexIncCommentLevel
       alexMonadScan

startComment code input len =
    do alexSetStartCode code
       incCommentLevel input len

decCommentLevel input len =
    do { alexDecCommentLevel
       ; cl <- alexGetCommentLevel
       ; if (cl == 0)then do { alexSetStartCode 0 ;
                             alexMonadScan}
                     else alexMonadScan}




showPosn (AlexPn f  _ line col)
    = show ('(':f) ++ (')':(show line)) ++ ':': show col


qpllexer :: (Token -> Alex a b) -> Alex a b
qpllexer cont = alexMonadScan >>= \tok -> cont tok

 }

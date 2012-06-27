{
{-#OPTIONS_GHC -fno-warn-missing-signatures #-}
{-#OPTIONS_GHC -fno-warn-orphans #-}
{-#OPTIONS_GHC -fno-warn-monomorphism-restriction #-}
{-#OPTIONS_GHC -fno-warn-unused-binds #-}
module Assembler.AssemLexer (
        Token(..),
        Alex(..),
        alexSetInput,
        AlexInput,
        AlexState(..),
        AlexPosn(..),
        showPosn,
        scanner,
        gettokens,
        qpalexer ) where
import Assembler.LexMonad
    }



$whitechar = [ \t\n\r\f\v]

$bindigit  = 0-1
$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\(\)\,]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol]

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$idchar    = [$alpha $digit \']
$nl        = [\n\r]

@transforms = \! $idchar*  | \! \( $digit \) $idchar* | \! \- $idchar*

@constructors = \# $large $idchar*

@addresses  = \@ $idchar+ | \@ "^" $idchar+

@opcode =
        QLoad | QCons | QMove | QBind | QUnbind | QDiscard | QDelete |
        QPullup | Rename  | EnScope | DeScope |
        AddCtrl | QCtrl | UnCtrl | QApply |
        SwapD | Split | Measure |
        Use | Jump | CondJump | Call | Return | CGet | CApply |
        CPop | CLoad | CPut | NoOp

@ops =
        "*" | "-" | "/" | "%" | "+" | "--" | "%/" |
        "||" | "&&" | "==" | "<" | ">" | "~" | "^" | "=/=" | "=<" |
        ">=" | ">>" | "<<"

@label  = $idchar+

@number  = \-? $digit $digit*


qpa :-

  $white+               { skip }
  "//".*                { skip }
  "Compiler:" .*        { mkCompilerNote}
  "|"[01]">"            { mkKet}
  Start                 { mkStart }
  EndProc               { mkEnd }
  Trans                 { mkTrans}
  EndTrans              { mkEnd}
  True | False          { mkBoolTok}
  @number               { mkNumberTok}
  @ops                  { mkTok TkOperator }
  $symbol               { mkTok TkSymbol }
  @opcode               { mkTok TkOpcode }
  @addresses            { mkTok TkAddress }
  @label                { mkTok TkLabel }
  @constructors         { mkTok TkCons }
  @transforms           { mkTransform }
{


mkTok :: (String->Token) -> AlexInput -> Int -> Alex Token
mkTok t (_,_,_,str) len = return (t (take len $ head str))

mkCompilerNote :: AlexInput -> Int -> Alex Token
mkCompilerNote (_,_,_,str) len = return (TkCompilerNotes (take len $ head str))

mkStart  :: AlexInput -> Int -> Alex Token
mkStart _ _ = return TkStart

mkEnd  :: AlexInput -> Int -> Alex Token
mkEnd _ _ = return TkEnd

mkTrans  :: AlexInput -> Int -> Alex Token
mkTrans _ _ = return TkTrans

mkTransform ::AlexInput -> Int -> Alex Token
mkTransform (_,_,_,str) len =
   return $ TkTransform $ getGate $ tail $ take len $ head str

mkBoolTok :: AlexInput -> Int -> Alex Token
mkBoolTok (_,_,_,str) len =
     do case (take len $ head str) of
                  bv -> return $ TkBool (read bv)

mkKet :: AlexInput -> Int -> Alex Token
mkKet (_,_,_,str) len = return $ TkKet $ take (len-2) $ tail $ head str


mkNumberTok :: AlexInput -> Int -> Alex Token
mkNumberTok (_,_,_,str) len =
     do case (take len $ head str) of
                  num -> return $ TkNumber (read num)

lexError s =
     do ((p:ps),_,_,(input:inps)) <- alexGetInput
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

alexEOF :: Alex Token
alexEOF = return (TkEof)




type AlexAction result = AlexInput -> Int -> result



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


qpalexer :: (Token -> Alex a) -> Alex a
qpalexer cont = alexMonadScan >>= \tok -> cont tok

 }

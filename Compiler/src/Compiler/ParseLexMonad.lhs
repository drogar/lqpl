\begin{code}

module Compiler.ParseLexMonad where
import Compiler.Qtypes
import Utility.FileProvider
\end{code}
 The input type
\begin{code}

type AlexInput = ([AlexPosn],     -- current position,
                  Char,         -- previous char
                  [String])       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexInputString :: AlexInput -> String
alexInputString (p,c,s:ss) = s

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p:ps,c,[]) = Nothing
alexGetChar (p:ps,c,[[]]) = Nothing
alexGetChar (p:ps,_,[]:ss)  =
     alexGetChar (ps,' ',ss)
alexGetChar (p:ps,_,(c:s):ss)  =
    let p' = alexMove p c in p' `seq`
         Just (c, (p':ps, c, s:ss))
\end{code}

 Token positions

`Posn' records the location of a token in the input text.  It has three
fields: the address (number of chacaters preceding the token), line number
and column of a token within the file. `start_pos' gives the position of the
start of the file and `eof_pos' a standard encoding for the end of file.
`move_pos' calculates the new position after traversing a given character,
assuming the usual eight character tab stops.
\begin{code}

data AlexPosn = forall a. FileProvider a => AlexPn {alexCurrFile::a,
 address:: !Int, lineNum:: !Int, columnNum:: !Int}

instance Eq AlexPosn where
  (AlexPn a b c d) == (AlexPn a' b' c' d') = show a == show a' && b==b' && c==c' && d==d'

instance Show AlexPosn where
  show (AlexPn a b c d) = "AlexPn(" ++ show a ++ " "++ show b ++ " "++ show c ++ " "++ show d ++")"

alexStartPos :: AlexPosn
alexStartPos = AlexPn emptyProvider 0 1 1

alexLineNum :: AlexPosn -> Int
alexLineNum (AlexPn _ _ l _) = l

alexCharNum :: AlexPosn -> Int
alexCharNum (AlexPn _ _ _ c) = c

--alexCurrFile :: FileProvider a => AlexPosn -> a
--alexCurrFile (AlexPn f _ _ _) = f

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn f a l c) '\t' = AlexPn f (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn f a l c) '\n' = AlexPn f (a+1) (l+1)   1
alexMove (AlexPn f a l c) _    = AlexPn f (a+1)  l     (c+1)
\end{code}
The Monad type
\begin{code}

data AlexState = forall a. FileProvider a => AlexState {
        alexPos :: [AlexPosn],  -- position at current input location
        alexInp :: [String],     -- the current input
        alexChr :: !Char,      -- the character before the input
        alexScd :: !Int,       -- the current startcode
        alexClvl :: !Int,    -- BGG The comment level depth
        alexCdir :: a, -- The current directory - of input file.
        alexImps :: [a], -- List of imported / read files.
        alexSdirs :: [a] -- List of search dirs for imports.
    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> Alex a -> IO (Either String a)
runAlex input (Alex f)
   =  do ares <- f AlexState {alexPos = [alexStartPos],
                              alexInp = [input],
                              alexChr = '\n',
                              alexScd = 0,
                              alexClvl = 0,
                              alexCdir = currentFPDir,
                              alexImps = [],
                              alexSdirs = [currentFPDir]}
         return $ case ares of
                    Left msg -> Left msg
                    Right ( _, a ) -> Right a

newtype Alex a  = Alex { unAlex :: AlexState ->
                                   IO(Either String (AlexState , a)) }

instance Monad Alex where
  m >>= k  = Alex $ \s ->
                 do ua <- unAlex m s
                    case ua of
                            Left msg -> return $ Left msg
                            Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> return $ Right (s,a)

liftio :: IO b -> Alex b
liftio f = Alex $ \s ->
                do x<-f
                   return (Right (s,x))


alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alexPos=pos,alexChr=c,alexInp=inp} ->
        return $ Right (s, (pos,c,inp))

alexGetImpDirs :: FileProvider b => Alex [b]
alexGetImpDirs
    = Alex $  \s ->
        return $ Right (s, alexSdirs s)

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,inp)
 = Alex $ \s -> case s{alexPos=pos,alexChr=c,alexInp=inp,alexImps=map alexCurrFile pos} of
                  s@(AlexState{}) -> return $ Right (s, ())

alexAddInput :: AlexInput -> Alex ()
alexAddInput (pos,c,inp)
 = Alex $ \s -> case s{alexPos=pos ++ alexPos s,
                            alexChr=c,
                            alexInp=inp ++ alexInp s,
                            alexImps =  map alexCurrFile pos ++ alexImps s} of
                  s@(AlexState{}) -> return $ Right (s, ())

--alexEOF :: Alex a

alexError :: String -> Alex a
alexError message =
    Alex $ \s -> return $
                   Left (" Lexing Character "++
                         show (head $ head $ alexInp s)++
                         " at line " ++
                         show (alexLineNum $ head $ alexPos s) ++
                         " column " ++
                         show (alexCharNum $ head $ alexPos s) ++
                         " file " ++
                         show (alexCurrFile $ head $ alexPos s) ++
                         " context: " ++
                         take 30 (head $ alexInp s) ++
                         "\n" ++ message)

alexGetInpDir :: FileProvider b => Alex b
alexGetInpDir =  Alex $ \s@AlexState{alexCdir=d} ->
                          return $ Right (s, d)

alexGetImpFiles :: FileProvider b => Alex [b]
alexGetImpFiles  = Alex $ \s@AlexState{alexImps=imps} ->
                       return $ Right (s, imps)

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alexScd=sc} ->
                       return $ Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> return $ Right (s{alexScd=sc}, ())

alexGetCommentLevel :: Alex Int
alexGetCommentLevel = Alex $ \s@AlexState{alexClvl=cl} -> return $ Right (s, cl)

alexSetCommentLevel :: Int -> Alex ()
alexSetCommentLevel cl = Alex $ \s -> return $ Right (s{alexClvl=cl}, ())

alexIncCommentLevel :: Alex ()
alexIncCommentLevel  =
    do cl <- alexGetCommentLevel
       alexSetCommentLevel (cl + 1)

alexDecCommentLevel :: Alex ()
alexDecCommentLevel =
    do {  cl <- alexGetCommentLevel
        ; alexSetCommentLevel if cl > 0 then (cl - 1) else 0}

\end{code}
Useful token actions
\begin{code}

data Token
     =  TkKet String  | TkSymbol String
     | TkNumber Int
     | TkOperator String | TkReserved String
     | TkTransform String
     | TkTensor| TkId String
     | TkCons String
     | TkErr | TkEof | TkImp
       deriving Eq
instance Show Token where
     show (TkReserved s)  = "Res"++ s
     show (TkKet s)  = '|':s++"> "
     show (TkNumber i)  = show i
     show (TkSymbol s)    = "Sym"++s
     show (TkOperator s)  = "Op"++s
     show (TkTransform ut)
         = ut
     show (TkId s)      = "ID"++s
     show (TkCons s)    = "CID"++s
     show TkErr         = "ERROR TOKEN"
     show TkEof         = ""
     show TkTensor      = "Tensor "
     show TkImp         = "Importing"
\end{code}

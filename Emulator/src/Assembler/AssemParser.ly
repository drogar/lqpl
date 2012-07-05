\incsec{Parsing assembler with \happy.}\label{incsec:parsing}

> {
> {-#OPTIONS_GHC -fno-warn-missing-signatures #-}
> {-#OPTIONS_GHC -fno-warn-unused-binds #-}
> module Assembler.AssemParser where
> import Assembler.AssemLexer
> import Assembler.LexMonad
> import QSM.Components.Instructions
> import QSM.BasicData
> import QSM.Transformations
> import QSM.Components.ClassicalStack
> import QSM.QuantumStack.QSDefinition
> import Data.LazyNum
> import Data.List as List
> import Data.Map as Map hiding ((!))
> }
> %name qpa prog
>

> %tokentype { Token }
> 
> %token
>       START    {TkStart}
>       END      {TkEnd}
>       TRANS    {TkTrans}
>       GATE    {TkTransform $$}
>       COMPILERNOTE   {TkCompilerNotes $$}
>       LABEL   {TkLabel $$}
>       ADDRESS   {TkAddress $$}
>       CID     {TkCons $$}
>       QLOAD   {TkOpcode "QLoad" }
>       QCONS   {TkOpcode "QCons" }
>       QMOVE   {TkOpcode "QMove" }
>       QBIND   {TkOpcode "QBind" }
>       QUNBIND   {TkOpcode "QUnbind" }
>       QDISCARD  {TkOpcode "QDiscard" }
>       QDELETE  {TkOpcode "QDelete" }
>       QPULLUP   {TkOpcode "QPullup" }
>       RENAME    {TkOpcode "Rename" }
>       ENSCOPE    {TkOpcode "EnScope" }
>       DESCOPE    {TkOpcode "DeScope" }
>       ADDCTRL   {TkOpcode "AddCtrl" }
>       QCTRL   {TkOpcode "QCtrl" }
>       UNCTRL   {TkOpcode "UnCtrl" }
>       QAPPLY   {TkOpcode "QApply" }
>       SWAPD   {TkOpcode "SwapD" }
>       SPLIT   {TkOpcode "Split" }
>       MEASURE   {TkOpcode "Measure" }
>       USE     {TkOpcode "Use" }
>       JUMP   {TkOpcode "Jump" }
>       CONDJUMP   {TkOpcode "CondJump" }
>       CALL   {TkOpcode "Call" }
>       RETURN   {TkOpcode "Return" }
>       CGET   {TkOpcode "CGet" }
>       CPUT   {TkOpcode "CPut" }
>       CAPPLY   {TkOpcode "CApply" }
>       CPOP   {TkOpcode "CPop" }
>       CLOAD   {TkOpcode "CLoad" }
>       NOOPERATION   {TkOpcode "NoOp" }
>       '('     {TkSymbol "(" }
>       ')'     {TkSymbol ")" }
>       ','     {TkSymbol "," }
>       KET     {TkKet $$}
>	NUM     {TkNumber $$ }
>       BOOL    {TkBool $$}
>        "+"     {TkOperator "+"}
>        "-"     {TkOperator "-"}
>        "*"     {TkOperator "*"}
>        "/"     {TkOperator "/"}
>        "=="    {TkOperator "=="}
>        "<"     {TkOperator "<"}
>        ">"     {TkOperator ">"}
>        MOD     {TkOperator "%"}
>        REM     {TkOperator "%/"}
>        AND     {TkOperator "&&"}
>        OR      {TkOperator "||"}
>        NOT     {TkOperator "~"}
>        NEG     {TkOperator "--"}
>        XOR     {TkOperator "^"}
>        "=/="   {TkOperator "=/="}
>        "=<"    {TkOperator "=<"}
>        ">="    {TkOperator ">="}
>        "<<"    {TkOperator "<<"}
>        ">>"    {TkOperator ">>"}
>
> %monad {Alex}
> %lexer {qpalexer} {TkEof}
> %%
> prog::{(([String],Map String (Trans LazyNum)), Memory Basis )}
>  : procsAndTrans {((fst $ fst $1, Map.fromList $ (snd . fst) $1), Map.fromList $ snd $1) }
>
> procsAndTrans :: {(([String],[(String, Trans LazyNum)]), [(String, Code Basis)])}
>    : procsAndTrans cdir { (\ ((c,a1),a2) b -> ((b:c,a1),a2)) $1 $2 }
>    | procsAndTrans proc { (\ ((c,a1),a2) b -> ((c,a1),b:a2)) $1 $2 }
>    | procsAndTrans tran { (\ ((c,a1),a2) b -> ((c,b:a1),a2)) $1 $2 }
>    | {- empty -} {(([],[]),[])}
>
> cdir :: {String}
>  : COMPILERNOTE {$1}
>
> tran ::{(String, Trans LazyNum)}
>  : TRANS LABEL END {($2,i2by2)}
>
> proc :: {(String, Code Basis )}
>    : LABEL START instructions END {($1, (setlabels $ reverse $3))}
>
> instructions :: {[(Maybe String, Instruction Basis,  Maybe [String])] }
>    : instructions instruction {$2:$1}
>    | {- empty -} {[]}
>
> instruction :: {(Maybe String, Instruction Basis, Maybe [String])}
>    :  opAndOperands {(Nothing, fst $1, snd $1)}
>    | LABEL opAndOperands {(Just $1,fst $2, snd $2)}
>
>
> opAndOperands :: {(Instruction Basis,  Maybe [String])}
>    : QLOAD ADDRESS KET {(QLoad $2 (ketToBasis $3), Nothing)}
>    | QCONS ADDRESS CID {(QCons $2 (tail $3), Nothing)}
>    | QMOVE ADDRESS {(QMove $2, Nothing)}
>    | QBIND ADDRESS {(QBind  $2, Nothing)}
>    | QUNBIND ADDRESS ADDRESS {(QUnbind  $2 $3, Nothing)}
>    | QDISCARD ADDRESS {(QDiscard $2, Nothing)}
>    | QDELETE ADDRESS {(QDelete  $2,  Nothing)}
>    | QPULLUP ADDRESS {(QPullup $2, Nothing)}
>    | RENAME ADDRESS ADDRESS {(Rename $2 $3 , Nothing)}
>    | ENSCOPE {(EnScope , Nothing)}
>    | DESCOPE {(DeScope , Nothing)}
>    | QAPPLY GATE ADDRESS {(QApply 0 $2 $3, Nothing)}
>    | QAPPLY NUM GATE ADDRESS {(QApply $2 $3  $4, Nothing)}
>    | ADDCTRL {(AddCtrl, Nothing)}
>    | QCTRL NUM {(QCtrl $2, Nothing)}
>    | QCTRL {(QCtrl 1, Nothing)}
>    | UNCTRL {(UnCtrl, Nothing)}
>    | SWAPD {(SwapD, Nothing)}
>    | SPLIT ADDRESS LABEL clablist {(Split $2 0 (List.map fst $4), Just ($3 : (List.map snd $4)))}
>    | MEASURE ADDRESS LABEL LABEL LABEL{(Measure $2 0 0 0, Just [$3,$4,$5])}
>    | USE ADDRESS LABEL LABEL {(Use $2 0 0, Just [$3,$4])}
>    | JUMP LABEL {(Jump 0, Just [$2])}
>    | CONDJUMP LABEL {(CondJump 0, Just [$2])}
>    | CALL NUM  LABEL {(Call $2 $3, Nothing)}
>    | RETURN NUM {(Return  $2, Nothing)}
>    | CGET NUM {(CGet  $2, Nothing)}
>    | CPUT NUM {(CPut  $2, Nothing)}
>    | CPOP {(CPop, Nothing)}
>    | CAPPLY op {(CApply  $2, Nothing)}
>    | CLOAD NUM {(CLoad  (Left $2), Nothing)}
>    | CLOAD BOOL {(CLoad  (Right $2), Nothing)}
>    | NOOPERATION {(NoOp , Nothing)}
>    
>
> clablist :: {[((Constructor,Int),String)]}
>   : clab clablist {$1 : $2}
>   | {- empty -} {[]}
>
> clab :: {((Constructor,Int),String)}
>   : '(' CID ',' LABEL ')' {((tail $2,0),$4)}
>
> op ::{ClassicalOp}
>    : "+" {CAdd}
>    | "-" {CSub}
>    | "*" {CTimes}
>    | "/" {CDiv}
>    | "==" {CEq}
>    | "<" {CLt}
>    | ">" {CGt}
>    | MOD {CMod}
>    | REM {CRem}
>    | NEG {CNeg}
>    | AND {CAnd}
>    | OR {COr}
>    | NOT {CNot}
>    | XOR {CXor}
>    | "=/=" {CNeq}
>    | "=<" {CLte}
>    | ">=" {CGte}
>    | "<<" {CShl}
>    | ">>" {CShr}



> {

> happyError :: Alex a
> happyError = Alex $ \as -> 
>      return $ Left ("Parse error at position  " ++ 
>                     showPosn (head $ alex_pos as)++
>                     " '"++(head $ alex_inp as)++"'\n")
>
> qpa::Alex (([String],Map String (Trans LazyNum)), Memory Basis )
>
>
> parseQPA :: String->String->
>             String -> 
>             IO (Either String 
>		   (([String],Map String (Trans LazyNum)), Memory Basis ))
> parseQPA dir file s = parseProg qpa dir file s
>
> parseProg :: Alex a ->String->String->String -> IO (Either String a)
> parseProg parser dir file s = 
>     do res <-  unAlex parser (AlexState ([AlexPn file 0 1 1]) [s]  ' ' 0 0 dir [file] )
>
>        case res of
>             Right p -> return $ Right $ snd p
>             Left s' -> return $ Left s'
>
> }


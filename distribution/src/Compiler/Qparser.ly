\incsec{Parsing with \happy.}\label{incsec:parsing}
The lexing and parsing steps of this compiler are reasonably standard.

The standard parser generator for haskell, \happy\ is used. The
grammar for our language is significantly different from that
originally introduced in  \selingerqpl. The language design was done with
thought to the ways functional language syntax (e.g. haskell) works and
the ways in which general programmers often think about coding.

We provide an include facility which allows global definitions
of things such as types, generic functions and (planned)
unitary transformation definitions.

Borrowing from Haskell, an executable program will be one defined as
a group of "object programs" where there is exactly one procedure named
"main".

\incsubsec{Heading and tokens}\label{incsec:parsing:heading}
\happy\ files are composed of three parts. The module start,
the rules and any
functions for the module. This module, named \cde{Qparser} imports the
monadic lexer (see \fullref{incsec:lexerp}) and the basic types used to
represent
the code (see \fullref{incsec:qtypes}).

> {
> module Compiler.Qparser where
> import Compiler.ParseLexMonad
> import Compiler.Qlexer
> import Compiler.Qtypes
> }
> %name qpl prog
>


In this next part, we must define aliases for each token. \happy\ allows us to
define actual characters or strings as well as the more usual identifier like aliases.
By standard convention, the aliases are all in upper case.


> %tokentype { Token }
>
> %token
>       ASSIGN  {TkOperator "=" }
>       BOOL    {TkReserved "Bool" }
>       TRUE    {TkReserved "true" }
>       FALSE    {TkReserved "false" }
>       DISCARD {TkReserved "discard" }
>       GATE    {TkTransform $$}
>       ID      {TkId $$}
>       CID     {TkCons $$}
>       IF      {TkReserved "if" }
>       IN      {TkReserved "in" }
>       INT     {TkReserved "Int" }
>       MEASURE {TkReserved "measure" }
>       OF      {TkReserved "of" }
>       QUBIT    {TkReserved "Qubit" }
>       SKIP    {TkReserved "skip" }
>       CASE    {TkReserved "case" }
>       TENSOR  {TkOperator "\\o*"}
>       QDATA    {TkReserved "qdata" }
>       ELSE    {TkReserved "else" }
>       ZERO    {TkReserved "zero" }
>       USE     {TkReserved "use" }
>       UNITARYTRANS {TkOperator "*=" }
>       '{'     {TkSymbol "{" }
>       '}'     {TkSymbol "}" }
>       '('     {TkSymbol "(" }
>       ')'     {TkSymbol ")" }
>       ';'     {TkSymbol ";" }
>       ':'     {TkSymbol ":" }
>       '|'     {TkSymbol "|" }
>       DONTCARE {TkSymbol "_" }
>       "::"     {TkOperator "::" }
>        "=>"    {TkOperator "=>"}
>        "<="    {TkOperator "<="}
>       ','     {TkSymbol "," }
>       "|0>"     {TkKet "0" }
>       "|1>"     {TkKet "1" }
>       NUM     {TkNumber $$ }
>        "+"     {TkOperator "+"}
>        "-"     {TkOperator "-"}
>        "*"     {TkOperator "*"}
>        "/"     {TkOperator "/"}
>       USEASSIGN    {TkOperator ":="}
>        "=="    {TkOperator "=="}
>        "<"     {TkOperator "<"}
>        ">"     {TkOperator ">"}
>        MOD     {TkReserved "mod"}
>        REM     {TkReserved "rem"}
>        AND     {TkOperator "&&"}
>        OR      {TkOperator "||"}
>        NOT     {TkOperator "~"}
>        XOR     {TkOperator "^"}
>        "=/="   {TkOperator "=/="}
>        "=<"    {TkOperator "=<"}
>        ">="    {TkOperator ">="}
>        "<<"    {TkOperator "<<"}
>        ">>"    {TkOperator ">>"}
>
> %left TENSOR
> %left ';'


We now declare that the |Alex| monad  is used in the
parsing (see \fullref{incsec:lexerp}). We also
declare the function used for lexing and the end of file token.


> %monad {Alex}
> %lexer {qpllexer} {TkEof}
> %%


\incsubsec{Context Free Grammar}\label{incsec:parser:grammar}
Our context free grammar that we are using is:
\begin{Verbatim}[commandchars=\\\{\}]
To be done....
\end{Verbatim}

Our rule definitions mirror the context free grammar with the addition
of abstract syntax tree constructions.

> prog::{Program}
> prog : global_definitions {Program  (reverse $1)}
>
> global_definitions ::{[GlobalDefinition]}
>    : global_definitions global_definition {$2:$1}
>    | {- empty -} {[]}
>
> global_definition :: {GlobalDefinition}
>    : data_definition {DataDef $1}
>    | procedure_definition {ProcDef $1}

     | transform_definition {TransDef $1}

>
> data_definition :: {DataDefinition}
>    : type_definition ASSIGN '{' constructor_list '}' {DD $1 (reverse $4)}
>
> type_definition :: {TypeDefinition}
>    :  QDATA CID ids {TypeDefinition $2 $3}
>
> constructor_list :: {[Constructor]}
>    : constructor more_constructor_list {$1:$2}
>
> more_constructor_list ::{[Constructor]}
>    : '|' constructor more_constructor_list {$2:$3}
>    | {- empty -} {[]::[Constructor]}
>
> constructor :: {Constructor}
>    : CID '(' typevartype_list ')' {Constructor $1  $3}
>    | CID {Constructor $1 []}
>
> typevartype_list :: {[Qtype]}
>    : typevartype moretypevartype_list {$1:$2}
>
> moretypevartype_list ::  {[Qtype]}
>    : ',' typevartype moretypevartype_list {$2:$3}
>    | {- empty -} {[]}
>
>
> typevartype ::{Qtype}
>    : ID {TypeVariable $1}
>    | CID  {DeclaredType $1 []}
>    | CID '(' typevartype_list ')'  { DeclaredType $1 $3}
>    | builtintype  {$1}
>

Check docs how to use monad to decide which to do

> typevarfparm_list :: {[Qtype]}
>    : typevarfparm moretypevarfparm_list {$1:$2}
>
> moretypevarfparm_list ::  {[Qtype]}
>    : ',' typevarfparm moretypevarfparm_list {$2:$3}
>    | {- empty -} {[]}
>
>
> typevarfparm ::{Qtype}
>    : ID {RigidVariable $1}
>    | CID  {DeclaredType $1 []}
>    | CID '(' typevarfparm_list ')'  { DeclaredType $1 $3}
>    | builtintype  {$1}
>
>
> procedure_definition :: {Procedure}
>    : ID "::" '(' parameter_definitions '|'
>                  parameter_definitions ';' parameter_definitions  ')'
>          ASSIGN block {Procedure $1 $4 $6 $8 [] $11}
>    | ID "::" '(' parameter_definitions ';' parameter_definitions  ')'
>          ASSIGN block {Procedure $1 [] $4 $6 [] $9}
>    | ID "::" '(' ')' ASSIGN block {Procedure $1 [] [] [] [] $6}
>
>
> parameter_definitions ::{ [ParameterDefinition]}
>   : parameter_definition more_parameter_definitions {$1:$2}
>   | {- empty -} { [] }
>
> more_parameter_definitions ::{ [ParameterDefinition]}
>   : ',' parameter_definition more_parameter_definitions {$2:$3}
>   | {- empty -} { [] }
>
> parameter_definition :: {ParameterDefinition}
>    : ID ':' CID '(' typevarfparm_list ')' {ParameterDefinition $1 (DeclaredType $3  $5)}
>    | ID ':' CID {ParameterDefinition $1 (DeclaredType $3 [])}
>    | ID ':' builtintype {ParameterDefinition $1 $3}
>    | ID ':' ID {ParameterDefinition $1 (RigidVariable $3)}
>
> builtintype :: {Qtype}
>    : QUBIT { QUBIT}
>    | INT { INT}
>    | BOOL { BOOL}
>
>
> block :: {[Statement]}
>   : '{' stmtlist '}' {reverse $2}
>
> stmtlist::{[Statement]}
> stmtlist : stmtlist ';' stmt {$3:$1}
>          | stmtlist ';'      {$1}
>          | stmt      {[$1]}
>          | {- empty -} { [] }
>
> stmt :: {Statement}
>      : ID ASSIGN exp               {Assignment $1 $3}
>      | CASE exp OF cases   {CaseSt $2  $4}
>      | MEASURE exp OF zeroalt onealt {Measure  $2 $4 $5}
>      | MEASURE exp OF onealt zeroalt {Measure  $2 $5 $4}
>      | ID USEASSIGN exp            {UseAssign $1 $3}
>      | USE identifier_list IN block   {Use $2 $4}
>      | USE identifier_list            {UseFromHere $2 }
>      | '(' identifier_list ')' ASSIGN callable '(' opt_exp_list  ')'
>               {Call $5 [] $7  $2 []}
>      | '(' identifier_list ')' ASSIGN callable '(' opt_exp_list '|' opt_exp_list ')'
>               {Call $5 $7 $9  $2 []}
>      | callable  ids   {Call $1 [] (map Evar $2)  $2 [] }
>      | callable '(' opt_exp_list  ')' ids
>               {Call $1 $3 (map Evar $5)  $5 []}
>      | callable '(' opt_exp_list '|' opt_exp_list  ')' ids
>               {Call $1 $3 ($5 ++ (map Evar $7))  $7 []}
>      | callable '(' opt_exp_list '|' opt_exp_list  ';'
>                     opt_identifier_list  ')'
>               {Call $1 $3 $5 $7 []}
>      | callable '('  opt_exp_list  ';' opt_identifier_list ')'
>               {Call $1 [] $3 $5 []}
>      | DISCARD identifier_list {Discard $2}
>      | block {BlockStatement $1}
>      | IF guards   {Guard $2 }
>      | stmt cby control_list {ControlledBy $1 $3}
>      | SKIP                            {Skip}
>      | ZERO                       {ZeroStack}
>
>
>
> cby :: {Token}
> :    "<=" {$1}
>
> control_list::{[ControlType Identifier]}
>    : control more_control_list { $1:$2}
>
> more_control_list::{[ControlType Identifier]}
>    : ',' control more_control_list { $2:$3}
>    | {- empty -} {[]}
>
> control::{ControlType Identifier}
>    : NOT ID {ZeroControl $2}
>    | ID {OneControl $1}
>
>
>
> callable:: {String}
>   : ID  {$1}
>   | transform {$1}
>
> zeroalt ::  {[Statement]}
>   : "|0>" "=>" block {$3}
>
> onealt :: {[Statement]}
>   : "|1>" "=>" block { $3}
>
> guards ::{[GuardClause]}
>     : freeguards owguard      {$1 ++ [$2]}
>
> freeguards ::{[GuardClause]}
>     : {- empty -}   {[]}
>     | freeguard freeguards {$1:$2}
>
> freeguard ::{GuardClause}
>    : exp "=>" block {GuardClause $1 $3}
>
> owguard ::{GuardClause}
>    : ELSE "=>" block {GuardClause (Ebool True) $3}
>
>
> cases ::{[(CaseClause, [Statement])]}
> cases : case more_cases      {$1:$2}
>
> more_cases ::{[(CaseClause, [Statement])]}
>     : {- empty -} {[]}
>     | case more_cases {$1:$2}
>
> case :: {(CaseClause, [Statement])}
> case : caseclause "=>" block {($1, $3)}
>
> caseclause :: {CaseClause}
> caseclause :  CID '(' pattern_list ')' {CaseClause $1  $3}
>            | CID {CaseClause $1 []}
>
> pattern_list::{[Identifier]}
>    : pattern more_patterns  {$1:$2}
>
> more_patterns::{[Identifier]}
>    : ',' pattern more_patterns {$2:$3}
>    | {- empty -} {[]}
>
> pattern :: {Identifier}
>    : ID {$1}
>    | DONTCARE {"_"}
>
> exp::{Expression}
> exp  : exp0   {$1}
>
> exp0::{Expression}
> exp0 : exp0 or_op exp1 {Eapply $2 $1 $3}
>      | exp1 {$1}
>
> exp1::{Expression}
> exp1 : exp1 AND exp2 {Eapply And $1 $3}
>      | exp2 {$1}
>
> exp2::{Expression}
> exp2 : NOT exp2 {Enot $2}
>      | exp3 compare_op  exp3 {Eapply $2 $1 $3}
>      | "-" exp2 {Eminus $2}
>      | exp3 {$1}
>
> exp3::{Expression}
> exp3 : exp3 add_op exp4 {Eapply $2  $1 $3}
>      | exp4 {$1}
>
> exp4::{Expression}
> exp4 : exp4 mul_op  exp5 {Eapply $2 $1 $3}
>      | exp5 {$1}
>
> exp5::{Expression}
> exp5 : exp5 shift_op  exp6 {Eapply $2 $1 $3}
>      | exp6 {$1}
>
> exp6::{Expression}
> exp6   :  ID                  {Evar $1}
>        | NUM                  {Enum $1}
>        | TRUE                 {Ebool True}
>        | FALSE                {Ebool False}
>        | '(' exp ')'          {Ebracket $2}
>        | CID '(' exp_list ')' {Econs $1 $3}
>        | CID                  {Econs $1 []}
>        | ID '(' opt_exp_list ')'
>                               {Ecall $1 [] $3 []}
>        | ID '(' opt_exp_list ';' opt_identifier_list ')'
>                               {Ecall $1 [] $3 $5}
>        | ID '(' opt_exp_list '|' opt_exp_list ')'
>                               {Ecall $1  $3 $5 []}
>        | ID '(' opt_exp_list  '|' opt_exp_list ';' opt_identifier_list ')'
>                               {Ecall $1 $3 $5 $7}
>        | "|0>"                {EQubit Zero}
>        | "|1>"                {EQubit One}
>
>
>
> opt_exp_list::{[Expression]}
>    : exp_list {$1}
>    | {- empty -} {[]}
>
>
> exp_list::{[Expression]}
>    : exp more_exp_list {$1:$2}
>
> more_exp_list::{[Expression]}
>   : ',' exp more_exp_list {$2:$3}
>   | {- empty -} {[]}
>
> or_op::{BinOp}
> or_op: OR   {Or}
>        | XOR {Xor}
>
> compare_op::{BinOp}
> compare_op:"=="   {Opeq}
>        | "<" {Oplt}
>        | ">" {Opgt}
>        | "=<" {Ople}
>        | ">=" {Opge}
>        | "=/=" {Opneq}

> add_op::{BinOp}
> add_op:"+"   {Add}
>        | "-" {Sub}

> mul_op::{BinOp}
> mul_op:"*"   {Mul}
>        | "/" {Div}
>        | MOD {Mod}
>        | REM {Rem}

> shift_op::{BinOp}
> shift_op:">>"   {Oprshift}
>        | "<<" {Oplshift}
>
>
> ids::{[Identifier]}
>    : ID more_ids {$1:$2}
>    | {- empty -} {[]}
>
> more_ids::{[Identifier]}
>    :  ID more_ids {$1:$2}
>    | {- empty -}     {[]::[Identifier]}
>
> opt_identifier_list::{[Identifier]}
>    : identifier_list {$1}
>    | {- empty -}   {[]::[Identifier]}
>
> identifier_list::{[Identifier]}
>    : ID more_idlist {$1:$2}
>
> more_idlist::{[Identifier]}
>    : ',' ID more_idlist {$2:$3}
>    | {- empty -}     {[]::[Identifier]}
>
>
> transform::{String}
> transform: GATE {$1}
>        | transform TENSOR transform {$1 ++ "\\o*" ++ $3}
>



\incsubsec{Parsing support functions}\label{incsec:parsing:support}
We  are left with defining the minimal set of support functions, including
a minimalistic error routine.


> {
> happyError :: Alex a
> happyError = Alex $ \as ->
>      return $ Left ("Parse error at position  " ++
>                     showPosn (head $ alex_pos as)++
>                     " PrevChar:"++(['"',(alex_chr as),'"']) ++
>                     " next 20 chars:[" ++
>                     (take 20 $ head $ alex_inp as) ++ "]\n")
>
> qpl::Alex Program
>
>
> parseQPL :: String->String->String->[String] -> IO Program
> parseQPL dir file s ss = parseProg qpl dir file s ss
>
> parseProg :: Alex a ->String->String->String -> [String] -> IO a
> parseProg parser dir file s ss =
>     do res <- unAlex parser (AlexState ([AlexPn file 0 1 1]) [s]  ' ' 0 0 dir [file] ss )
>        case res of
>             Right p -> return $ snd p
>             Left s' -> error s'
>
> }


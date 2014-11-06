/*
 * Soonho Kong (soonhok@cs.cmu.edu)
 * Wei Chen (weichen1@andrew.cmu.edu)
 */

%{
open Batteries
open Basic
%}

%token LB RB LC RC LP RP EQ PLUS MINUS AST SLASH COMMA COLON SEMICOLON
%token LT LTE GT GTE DDT CARET
%token SIN COS TAN
%token ASIN ACOS ATAN
%token SINH COSH TANH
%token LOG EXP
%token TRUE FALSE
%token AND OR
%token EOF

%token STATE CONTROL
%token <float> FNUM
%token <string> ID

%left PLUS MINUS
%left AST SLASH
%left NEG
%right CARET

%start main

%type <Ast.program> main

%%

ffnum:
    | FNUM { $1 }
    | MINUS FNUM { 0.0 -. $2 }
;

exp:
 | ID                     { Var $1 }
 | ffnum                   { Num $1 }
 | LP exp RP              { $2 }
 | exp PLUS exp           { Add [$1; $3] }
 | exp MINUS exp          { Sub [$1; $3] }
 | MINUS exp %prec NEG    {
   match $2 with
   | Num n -> Num (0.0 -. n)
   | _ -> Neg $2
 }
 | exp AST exp                  { Mul [$1; $3] }
 | exp SLASH exp                { Div ($1, $3) }
 | EXP LP exp RP                { Exp $3 }
 | exp CARET exp                { Pow ($1, $3) }
 | SIN LP exp RP                { Sin $3 }
 | COS LP exp RP                { Cos $3 }
 | TAN LP exp RP                { Tan $3 }
 | ASIN LP exp RP               { Asin $3 }
 | ACOS LP exp RP               { Acos $3 }
 | ATAN LP exp RP               { Atan $3 }
 | SINH LP exp RP               { Sinh $3 }
 | COSH LP exp RP               { Cosh $3 }
 | TANH LP exp RP               { Tanh $3 }
;

/* boolean expression */
formula:
  | TRUE                { True }
  | FALSE               { False }
  | ID                  { FVar $1 }
  | LP formula RP       { $2 }
  | exp EQ exp          { Eq  ($1, $3) }
  | exp GT exp          { Gt  ($1, $3) }
  | exp LT exp          { Lt  ($1, $3) }
  | exp GTE exp         { Ge ($1, $3) }
  | exp LTE exp         { Le ($1, $3) }
  | formula OR formula        { Or [$1; $3] }
  | formula AND formula       { And [$1; $3] }
;

params:
    | /**/ { [] }
    | exp { [$1] }
    | exp COMMA params { $1::$3 }
;

id:
    | ID { $1 }
;

ids:
    | /**/ { [] }
    | id { [$1] }
    | id COMMA ids { $1::$3 }
;

stmt:
    | ID EQ exp SEMICOLON                                     { [Ast.Assign ($1, $3)] }
;

stmt_list:
    | /**/ { [] }
    | stmt stmt_list { $1@$2 }
;

main:
    | stmt_list                      { $1 }
;

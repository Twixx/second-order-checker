%{
    open Ast
%}
/* ML1 */
%token PLUS EVALTO MINUS MULT IS LESS THAN NOT
%token AST CROSS HYPHEN

%token IF THEN ELSE TRUE FALSE

/* ML2 */
%token VDASH
%token LET EQ IN

/* ML3 */
%token FUN RARROW

/* ML4 */
%token REC

/* ContML4 */
%token GTGT DRARROW LETCC UNDERSCORE

%%

%public judgment:
   j = drop_ctx { fun _ -> j }
drop_ctx:
    Env VDASH Exp GTGT Cont EVALTO Val { EvalTo($1, $5, $3, $7) }
  | Env VDASH Exp EVALTO Val { EvalTo($1, RetK, $3, $5) }
  | Val DRARROW Cont EVALTO Val { AppK($3, $1, $5) }

  | SInt PLUS SInt IS SInt { AppBOp(Plus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MULT SInt IS SInt { AppBOp(Mult, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MINUS SInt IS SInt { AppBOp(Minus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt LESS THAN SInt IS TRUE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool (Bool true)) }
  | SInt LESS THAN SInt IS FALSE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool (Bool false)) }
  /* abbreviations for less than */
  | SInt IS LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $5, Value_of_bool (Bool true)) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6, Value_of_bool (Bool false)) }

  | Env VDASH Exp error { errAt 4 "Syntax error: 'evalto' or '>>' expected" }
  | Env VDASH Exp EVALTO error { errAt 5 "Syntax error: value expected" }
  | Env VDASH Exp GTGT error { errAt 5 "Syntax error: continuation expected" }
  | Env VDASH Exp GTGT Cont error { errAt 6 "Syntax error: 'evalto' expected" }
  | Env VDASH Exp GTGT Cont EVALTO error { errAt 7 "Syntax error: value expected" }
  | Val DRARROW error { errAt 3 "Syntax error: continuation expected" }
  | Val DRARROW Cont error { errAt 4 "Syntax error: 'evalto' expected" }
  | Val DRARROW Cont EVALTO error { errAt 5 "Syntax error: value expected" }

  | SInt PLUS error { errAt 3 "Syntax error: natural number expected" }
  | SInt PLUS SInt error { errAt 4 "Syntax error: 'is' expected" }
  | SInt PLUS SInt IS error { errAt 5 "Syntax error: natural number expected" }
  | SInt MULT error { errAt 3 "Syntax error: natural number expected" }
  | SInt MULT SInt error { errAt 4 "Syntax error: 'is' expected" }
  | SInt MULT SInt IS error { errAt 5 "Syntax error: natural number expected" }
  | SInt MINUS error { errAt 3 "Syntax error: natural number expected" }
  | SInt MINUS SInt error { errAt 4 "Syntax error: 'is' expected" }
  | SInt MINUS SInt IS error { errAt 5 "Syntax error: natural number expected" }
(*
partialj :
    Env VDASH Exp EVALTO QM { In_EvalTo($1, RetK, $3) }
  | Env VDASH Exp GTGT Cont EVALTO QM { In_EvalTo($1, $5, $3) }
  | Val DRARROW Cont EVALTO QM { In_AppK($3, $1) }
  | SInt PLUS SInt IS QM { In_AppBOp(Plus, Value_of_int $1, Value_of_int $3) }
  | SInt MULT SInt IS QM { In_AppBOp(Mult, Value_of_int $1, Value_of_int $3) }
  | SInt MINUS SInt IS QM { In_AppBOp(Minus, Value_of_int $1, Value_of_int $3) }
/*  | SInt IS LESS THAN SInt { In_AppBOp(Lt, Value_of_int $1, Value_of_int $5) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6) }
*/
  | Env VDASH Exp error { errAt 4 "Syntax error: 'evalto' or '>>' expected" }
  | Env VDASH Exp EVALTO error { errAt 5 "Syntax error: '?' expected" }
  | Env VDASH Exp GTGT error { errAt 5 "Syntax error: continuation expected" }
  | Env VDASH Exp GTGT Cont error { errAt 6 "Syntax error: 'evalto' expected" }
  | Env VDASH Exp GTGT Cont EVALTO error { errAt 7 "Syntax error: '?' expected" }
  | Val DRARROW error { errAt 3 "Syntax error: continuation expected" }
  | Val DRARROW Cont error { errAt 4 "Syntax error: 'evalto' expected" }
  | Val DRARROW Cont EVALTO error { errAt 5 "Syntax error: '?' expected" }
  | SInt PLUS error { errAt 3 "Syntax error: natural number expected" }
  | SInt PLUS SInt error { errAt 4 "Syntax error: 'is' expected" }
  | SInt PLUS SInt IS error { errAt 5 "Syntax error: '?' expected" }
  | SInt MULT error { errAt 3 "Syntax error: natural number expected" }
  | SInt MULT SInt error { errAt 4 "Syntax error: 'is' expected" }
  | SInt MULT SInt IS error { errAt 5 "Syntax error: '?' expected" }
  | SInt MINUS error { errAt 3 "Syntax error: natural number expected" }
  | SInt MINUS SInt error { errAt 4 "Syntax error: 'is' expected" }
  | SInt MINUS SInt IS error { errAt 5 "Syntax error: '?' expected" }
*)
Env:
    /* empty */ { Empty }
  | LCID EQ Val Env2 { List.fold_left (fun env (id, v) -> Bind(env, Var id, v)) Empty (($1,$3)::$4) }
  | LCID error { errAt 2 "Syntax error: '=' expected" }
  | LCID EQ error { errAt 3 "Syntax error: value expected" }

Env2:
    /* empty */ { [] }
  | COMMA LCID EQ Val Env2 { ($2, $4) :: $5 }
  | error { errAt 1 "Syntax error: comma expected" }
  | COMMA error { errAt 2 "Syntax error: variable expected" }
  | COMMA LCID error { errAt 3 "Syntax error: '=' expected" }
  | COMMA LCID EQ error { errAt 4 "Syntax error: value expected" }

Exp:
  | LongExp { $1 }
  | Exp1 { $1 }
  | Exp1 BinOp1 LongExp { BinOp($2, $1, $3) }
  | Exp2 BinOp2 LongExp { BinOp($2, $1, $3) }
  | Exp3 BinOp3 LongExp { BinOp($2, $1, $3) }

LongExp:
  | IF Exp THEN Exp ELSE Exp { If($2, $4, $6) }
  | LET LCID EQ Exp IN Exp { Let(Var $2, $4, $6) }
  | LET REC LCID EQ FUN LCID RARROW Exp IN Exp { LetRec(Var $3, Var $6, $8, $10) }
  | FUN LCID RARROW Exp { Abs(Var $2, $4) }
  | LETCC LCID IN Exp { LetCc(Var $2, $4) }

Exp1:
  | Exp1 BinOp1 Exp2 { BinOp($2, $1, $3) }
  | Exp2 { $1 }

Exp2:
  | Exp2 BinOp2 Exp3 { BinOp($2, $1, $3) }
  | Exp3 { $1 }

Exp3:
    Exp3 BinOp3 Exp4 { BinOp($2, $1, $3) }
  | Exp4 { $1 }

Exp4:  /* function application:
          argument is an atomic expression without unary minus */
    Exp4 AExp { App($1, $2) }
  | MinExp { $1 }

BinOp1:
    LANGLE { Lt }

BinOp2:
    CROSS { Plus }
  | HYPHEN { Minus }

BinOp3:
    AST { Mult }

MinExp:
    HYPHEN INTL { Exp_of_int (Int (- $2)) }
  | AExp { $1 }

AExp:
    INTL { Exp_of_int (Int $1) }
  | TRUE { Exp_of_bool (Bool true) }
  | FALSE { Exp_of_bool (Bool false) }
  | LCID { Exp_of_Var (Var $1) }
  | LPAREN Exp RPAREN { $2 }
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }

SInt: /* signed int */
    INTL { Int $1 }
  | HYPHEN INTL { Int (- $2) }

Val:
    SInt { Value_of_int $1 }
  | TRUE { Value_of_bool (Bool true) }
  | FALSE { Value_of_bool (Bool false) }
  | LPAREN Env RPAREN LSBRA FUN LCID RARROW Exp RSBRA { Fun($2, Var $6, $8) }
  | LPAREN Env RPAREN LSBRA REC LCID EQ FUN LCID RARROW Exp RSBRA
      { Rec($2, Var $6, Var $9, $11) }
  | LSBRA Cont RSBRA { ContF $2 }

BinOp: BinOp1 {$1} | BinOp2 {$1} | BinOp3 {$1}

Hole:  UNDERSCORE { RetK }

OptCont: GTGT Cont { $2 }
  | /* empty */ { RetK }
Cont:
  | Hole { RetK }
  | LBRA Env VDASH Hole BinOp1 Exp2 RBRA OptCont
     { EvalRK($2, $6, $5, $8) }
  | LBRA Env VDASH Hole BinOp2 Exp3 RBRA OptCont
     { EvalRK($2, $6, $5, $8) }
  | LBRA Env VDASH Hole BinOp3 Exp4 RBRA OptCont
     { EvalRK($2, $6, $5, $8) }
  | LBRA Env VDASH Hole BinOp LongExp RBRA OptCont
     { EvalRK($2, $6, $5, $8) }
  | LBRA Val BinOp Hole RBRA OptCont { AppOpK($2, $3, $6) }
  | LBRA Env VDASH IF Hole THEN Exp ELSE Exp RBRA OptCont
     { BranchK($2, $7, $9, $11) }
  | LBRA Env VDASH LET LCID EQ Hole IN Exp RBRA OptCont
     { LetBodyK($2, Var $5, $9, $11) }
  | LBRA Env VDASH Hole AExp RBRA OptCont { EvalArgK($2, $5, $7) }
  | LBRA Val Hole RBRA OptCont { AppFunK($2, $5) }

  | LBRA error { errAt 2 "Syntax error: '_' or value or environment expected" }
  | LBRA Env VDASH error { errAt 4 "Syntax error: '_', 'if', or 'let' expected" }
  | LBRA Env VDASH Hole error { errAt 5 "Syntax error: '+', '-', '*', '<', or expression expected" }
  | LBRA Env VDASH Hole BinOp1 error { errAt 6 "Syntax error: expression expected" }
  | LBRA Env VDASH Hole BinOp1 Exp2 error { errAt 7 "Syntax error: '}' expected" }
  | LBRA Env VDASH Hole BinOp2 error { errAt 6 "Syntax error: expression expected" }
  | LBRA Env VDASH Hole BinOp2 Exp3 error { errAt 7 "Syntax error: '}' expected" }
  | LBRA Env VDASH Hole BinOp3 error { errAt 6 "Syntax error: expression expected" }
  | LBRA Env VDASH Hole BinOp3 Exp4 error { errAt 7 "Syntax error: '}' expected" }
  | LBRA Env VDASH Hole BinOp LongExp error { errAt 7 "Syntax error: '}' expected" }
  | LBRA Val error { errAt 3 "Syntax error: '+', '-', '*', or '<' expected" }
  | LBRA Val BinOp error { errAt 4 "Syntax error: '_' expected" }
  | LBRA Val BinOp Hole error { errAt 5 "Syntax error: '}' expected" }
  | LBRA Env VDASH IF error { errAt 5 "Syntax error: '_' expected" }
  | LBRA Env VDASH IF Hole error { errAt 6 "Syntax error: 'then' expected" }
  | LBRA Env VDASH IF Hole THEN error
     { errAt 7 "Syntax error: expression expected" }
  | LBRA Env VDASH IF Hole THEN Exp error
     { errAt 8 "Syntax error: 'else' expected" }
  | LBRA Env VDASH IF Hole THEN Exp ELSE error
     { errAt 9 "Syntax error: expression expected" }
  | LBRA Env VDASH IF Hole THEN Exp ELSE Exp error
     { errAt 10 "Syntax error: '}' expected" }
  | LBRA Env VDASH LET error
     { errAt 5 "Syntax error: variable name expected" }
  | LBRA Env VDASH LET LCID error
     { errAt 6 "Syntax error: '=' expected" }
  | LBRA Env VDASH LET LCID EQ error
     { errAt 7 "Syntax error: '_' expected" }
  | LBRA Env VDASH LET LCID EQ Hole error
     { errAt 8 "Syntax error: 'in' expected" }
  | LBRA Env VDASH LET LCID EQ Hole IN error
     { errAt 9 "Syntax error: expression expected" }
  | LBRA Env VDASH LET LCID EQ Hole IN Exp error
     { errAt 10 "Syntax error: '}' expected" }
  | LBRA Env VDASH Hole AExp error { errAt 6 "Syntax error: '}' expected" }
  | LBRA Val Hole error { errAt 4 "Syntax error: '}' expected" }

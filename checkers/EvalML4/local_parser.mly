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
%token REC

/* ML4 */
%token MATCH WITH BAR COLCOL

%%

%public judgment:
    j = drop_ctx { fun _ -> j}

drop_ctx:
    Env VDASH Exp EVALTO Val { EvalTo($1, $3, $5) }
  | SInt PLUS SInt IS SInt { AppBOp(Plus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MULT SInt IS SInt { AppBOp(Mult, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MINUS SInt IS SInt { AppBOp(Minus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt LESS THAN SInt IS TRUE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool (Bool true)) }
  | SInt LESS THAN SInt IS FALSE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool (Bool false)) }
  /* abbreviations for less than */
  | SInt IS LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $5, Value_of_bool (Bool true)) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6, Value_of_bool (Bool false)) }

  | Env VDASH Exp error { errAt 4 "Syntax error: 'evalto' expected" }
  | Env VDASH Exp EVALTO error { errAt 5 "Syntax error: value expected" }
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
    Env VDASH Exp EVALTO QM { In_EvalTo($1, $3) }
  | SInt PLUS SInt IS QM { In_AppBOp(Plus, Value_of_int $1, Value_of_int $3) }
  | SInt MULT SInt IS QM { In_AppBOp(Mult, Value_of_int $1, Value_of_int $3) }
  | SInt MINUS SInt IS QM { In_AppBOp(Minus, Value_of_int $1, Value_of_int $3) }
/*  | SInt IS LESS THAN SInt { In_AppBOp(Lt, Value_of_int $1, Value_of_int $5) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6) }
*/
  | Env VDASH Exp error { errAt 4 "Syntax error: 'evalto' expected" }
  | Env VDASH Exp EVALTO error { errAt 5 "Syntax error: '?' expected" }
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
  | Exp3 COLCOL LongExp { Cons($1, $3) }  /* left op. of :: is Exp3 (not Exp2) */
  | Exp3 BinOp3 LongExp { BinOp($2, $1, $3) } 
  | Exp4 BinOp4 LongExp { BinOp($2, $1, $3) } 

  | Exp1 BinOp1 error { errAt 3 "Syntax error: expression expected" }
  | Exp3 COLCOL error { errAt 3 "Syntax error: expression expected" }
  | Exp3 BinOp3 error { errAt 3 "Syntax error: expression expected" }
  | Exp4 BinOp4 error { errAt 3 "Syntax error: expression expected" }

LongExp: 
  | IF Exp THEN Exp ELSE Exp { If($2, $4, $6) }
  | LET LCID EQ Exp IN Exp { Let(Var $2, $4, $6) }
  | LET REC LCID EQ FUN LCID RARROW Exp IN Exp { LetRec(Var $3, Var $6, $8, $10) }
  | FUN LCID RARROW Exp { Abs(Var $2, $4) }
  | MATCH Exp WITH LSBRA RSBRA RARROW Exp BAR LCID COLCOL LCID RARROW Exp
      { if $9 = $11 
	then errBtw 9 11 "These variables shouldn't be the same"
	else Match($2, $7, Var $9, Var $11, $13) }

  | IF error { errAt 2 "Syntax error: expression expected" }
  | IF Exp error { errAt 3 "Syntax error: 'then' expected" }
  | IF Exp THEN error { errAt 4 "Syntax error: expression expected" }
  | IF Exp THEN Exp error { errAt 5 "Syntax error: 'else' expected" }
  | IF Exp THEN Exp ELSE error { errAt 6 "Syntax error: expression expected" }
  | LET error { errAt 2 "Syntax error: variable name or 'rec' expected" }
  | LET LCID error { errAt 3 "Syntax error: '=' expected" }
  | LET LCID EQ error { errAt 4 "Syntax error: expression expected" }
  | LET LCID EQ Exp error { errAt 5 "Syntax error: 'in' expected" }
  | LET LCID EQ Exp IN error { errAt 6 "Syntax error: expression expected" }
  | LET REC error { errAt 3 "Syntax error: variable name expected" }
  | LET REC LCID error { errAt 4 "Syntax error: '=' expected" }
  | LET REC LCID EQ error { errAt 5 "Syntax error: 'fun' expected" }
  | LET REC LCID EQ FUN error { errAt 6 "Syntax error: variable name expected" }
  | LET REC LCID EQ FUN LCID { errAt 7 "Syntax error: '->' expected" }
  | LET REC LCID EQ FUN LCID RARROW error { errAt 8 "Syntax error: expression expected" }
  | LET REC LCID EQ FUN LCID RARROW Exp error { errAt 9 "Syntax error: 'in' expected" }
  | LET REC LCID EQ FUN LCID RARROW Exp IN error { errAt 10 "Syntax error: expression expected" }
  | FUN error { errAt 2 "Syntax error: variable name expected" }
  | FUN LCID error { errAt 3 "Syntax error: '->' expected" }
  | FUN LCID RARROW error { errAt 4 "Syntax error: expression expected" }
  | MATCH error { errAt 2 "Syntax error: expression expected" }
  | MATCH Exp error { errAt 3 "Syntax error: 'with' expected" }
  | MATCH Exp WITH error { errAt 4 "Syntax error: '[]' expected" }
  | MATCH Exp WITH LSBRA error { errAt 5 "Syntax error: ']' expected" }
  | MATCH Exp WITH LSBRA RSBRA error { errAt 6 "Syntax error: '->' expected" }
  | MATCH Exp WITH LSBRA RSBRA RARROW error { errAt 7 "Syntax error: expression expected" }
  | MATCH Exp WITH LSBRA RSBRA RARROW Exp error { errAt 8 "Syntax error: '|' expected" }
  | MATCH Exp WITH LSBRA RSBRA RARROW Exp BAR error { errAt 9 "Syntax error: variable expected" }
  | MATCH Exp WITH LSBRA RSBRA RARROW Exp BAR LCID error { errAt 10 "Syntax error: '::' expected" }
  | MATCH Exp WITH LSBRA RSBRA RARROW Exp BAR LCID COLCOL error
      { errAt 11 "Syntax error: variable expected" }
  | MATCH Exp WITH LSBRA RSBRA RARROW Exp BAR LCID COLCOL LCID error
      { errAt 12 "Syntax error: '->' expected" }
  | MATCH Exp WITH LSBRA RSBRA RARROW Exp BAR LCID COLCOL LCID RARROW error
      { errAt 13 "Syntax error: expression expected" }

Exp1:
  | Exp1 BinOp1 Exp2 { BinOp($2, $1, $3) }
  | Exp2 { $1 }

Exp2:
  | Exp3 COLCOL Exp2 { Cons($1, $3) }
  | Exp3 { $1 }

Exp3:
  | Exp3 BinOp3 Exp4 { BinOp($2, $1, $3) }
  | Exp4 { $1 }

Exp4:
    Exp4 BinOp4 Exp5 { BinOp($2, $1, $3) }
  | Exp5 { $1 }

Exp5:  /* function application: 
          argument is an atomic expression without unary minus */
    Exp5 AExp { App($1, $2) }
  | MinExp { $1 }

BinOp1:
    LANGLE { Lt }

BinOp3:
    CROSS { Plus }
  | HYPHEN { Minus }

BinOp4:
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
  | LSBRA RSBRA { Nil }

  | LPAREN error { errAt 2 "Syntax error: expression expected" }
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }
  | LSBRA error { errAt 2 "Syntax error: ']' expected" }

SInt: /* signed int */
    INTL { Int $1 }
  | HYPHEN INTL { Int (- $2) }

Val:
    AVal { $1 }
  | AVal COLCOL Val { ConsV($1, $3) }

  | AVal COLCOL error { errAt 3 "Syntax error: value expected" }

AVal:
    SInt { Value_of_int $1 }
  | TRUE { Value_of_bool (Bool true) }
  | FALSE { Value_of_bool (Bool false) }
  | LSBRA RSBRA { NilV }
  | LPAREN Env RPAREN LSBRA FUN LCID RARROW Exp RSBRA { Fun($2, Var $6, $8) }
  | LPAREN Env RPAREN LSBRA REC LCID EQ FUN LCID RARROW Exp RSBRA 
      { Rec($2, Var $6, Var $9, $11) }
  | LPAREN Val RPAREN { $2 }

  | LSBRA error { errAt 2 "Syntax error: ']' expected" }
  | LPAREN Env RPAREN error { errAt 4 "Syntax error: '[' expected" }
  | LPAREN Env RPAREN LSBRA error { errAt 5 "Syntax error: 'fun' or 'rec' expected" }
  | LPAREN Env RPAREN LSBRA FUN error { errAt 6 "Syntax error: variable expected" }
  | LPAREN Env RPAREN LSBRA FUN LCID error { errAt 7 "Syntax error: '->' expected" }
  | LPAREN Env RPAREN LSBRA FUN LCID RARROW error { errAt 8 "Syntax error: expression expected" }
  | LPAREN Env RPAREN LSBRA FUN LCID RARROW Exp error { errBtw 4 9 "Syntax error: unmatched brackets" }

  | LPAREN Env RPAREN LSBRA REC error { errAt 6 "Syntax error: variable expected" }
  | LPAREN Env RPAREN LSBRA REC LCID error { errAt 7 "Syntax error: '=' expected" }
  | LPAREN Env RPAREN LSBRA REC LCID FUN error { errAt 8 "Syntax error: variable expected" }
  | LPAREN Env RPAREN LSBRA REC LCID FUN LCID error { errAt 9 "Syntax error: '->' expected" }
  | LPAREN Env RPAREN LSBRA REC LCID FUN LCID RARROW error { errAt 10 "Syntax error: expression expected" }
  | LPAREN Env RPAREN LSBRA REC LCID FUN LCID RARROW Exp error { errBtw 4 11 "Syntax error: unmatched brackets" }
  | LPAREN Val error { errBtw 1 3 "Syntax error: unmatched parenthesis" }

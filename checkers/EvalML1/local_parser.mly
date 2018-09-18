/* ML1 */
%token PLUS EVALTO MINUS MULT IS LESS THAN NOT
%token AST CROSS HYPHEN (* LT *)

%token IF THEN ELSE TRUE FALSE

%%

%public judgment:
    j = drop_ctx { fun _ -> j }

drop_ctx:
    Exp EVALTO Val { Ast.(EvalTo($1, $3)) }
  | SInt PLUS SInt IS SInt { Ast.(AppBOp(Plus, (Value_of_int $1), (Value_of_int $3), (Value_of_int $5))) }
  | SInt MULT SInt IS SInt { Ast.(AppBOp(Mult, (Value_of_int $1), (Value_of_int $3), (Value_of_int $5))) }
  | SInt MINUS SInt IS SInt { Ast.(AppBOp(Minus, (Value_of_int $1), (Value_of_int $3), (Value_of_int $5))) }
  | SInt LESS THAN SInt IS TRUE { Ast.(AppBOp(Lt, (Value_of_int $1), (Value_of_int $4), (Value_of_bool ((Bool true))))) }
  | SInt LESS THAN SInt IS FALSE { Ast.(AppBOp(Lt, (Value_of_int $1), (Value_of_int $4), (Value_of_bool ((Bool false))))) }
  /* abbreviations for less than */
  | SInt IS LESS THAN SInt { Ast.(AppBOp(Lt, (Value_of_int $1), (Value_of_int $5), (Value_of_bool ((Bool true))))) }
  | SInt IS NOT LESS THAN SInt { Ast.(AppBOp(Lt, (Value_of_int $1), (Value_of_int $6), (Value_of_bool ( (Bool false))))) }

  | Exp error { errAt 2 "Syntax error: 'evalto' expected" }
  | Exp EVALTO error { errAt 3 "Syntax error: value expected" }
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
    Exp EVALTO QM { In_EvalTo($1) }
  | SInt PLUS SInt IS QM { In_AppBOp(Plus, Value_of_int $1, Value_of_int $3) }
  | SInt MULT SInt IS QM { In_AppBOp(Mult, Value_of_int $1, Value_of_int $3) }
  | SInt MINUS SInt IS QM { In_AppBOp(Minus, Value_of_int $1, Value_of_int $3) }
/*  | SInt IS LESS THAN SInt { In_AppBOp(Lt, Value_of_int $1, Value_of_int $5) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6) }
*/
  | Exp error { errAt 2 "Syntax error: 'evalto' expected" }
  | Exp EVALTO error { errAt 3 "Syntax error: '?' expected" }
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

Exp:
  | LongExp { $1 }
  | Exp1 { $1 }
  | Exp1 BinOp1 LongExp { Ast.((BinOp($2, $1, $3))) }
  | Exp2 BinOp2 LongExp { Ast.((BinOp($2, $1, $3))) }
  | Exp3 BinOp3 LongExp { Ast.((BinOp($2, $1, $3))) }

  | Exp1 BinOp1 error { errAt 3 "Syntax error: expression expected" }
  | Exp2 BinOp2 error { errAt 3 "Syntax error: expression expected" }
  | Exp3 BinOp3 error { errAt 3 "Syntax error: expression expected" }

LongExp:
  | IF Exp THEN Exp ELSE Exp { Ast.((If ($2, $4, $6))) }

  | IF error { errAt 2 "Syntax error: expression expected" }
  | IF Exp error { errAt 3 "Syntax error: 'then' expected" }
  | IF Exp THEN error { errAt 4 "Syntax error: expression expected" }
  | IF Exp THEN Exp error { errAt 5 "Syntax error: 'else' expected" }
  | IF Exp THEN Exp ELSE error { errAt 6 "Syntax error: expression expected" }

Exp1:
  | Exp1 BinOp1 Exp2 { Ast.((BinOp($2, $1, $3))) }
  | Exp2 { $1 }

Exp2:
  | Exp2 BinOp2 Exp3 { Ast.((BinOp($2, $1, $3))) }
  | Exp3 { $1 }

Exp3:
    Exp3 BinOp3 AExp { Ast.((BinOp($2, $1, $3))) }
  | AExp { $1 }

BinOp1:
    LANGLE { Ast.(Lt) }

BinOp2:
    CROSS { Ast.(Plus) }
  | HYPHEN { Ast.(Minus) }

BinOp3:
    AST { Ast.(Mult) }

AExp:
    SInt { Ast.((Exp_of_int $1)) }
  | TRUE { Ast.((Exp_of_bool ((Bool true)))) }
  | FALSE { Ast.((Exp_of_bool ((Bool false)))) }
  | LPAREN Exp RPAREN { $2 }

  | LPAREN error { errAt 2 "Syntax error: expression expected" }
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }

SInt: /* signed int */
    INTL { Ast.((Int $1)) }
  | HYPHEN INTL { Ast.((Int (- $2))) }

Val:
    SInt { Ast.((Value_of_int $1)) }
  | TRUE { Ast.((Value_of_bool ((Bool true)))) }
  | FALSE { Ast.((Value_of_bool ( (Bool false)))) }

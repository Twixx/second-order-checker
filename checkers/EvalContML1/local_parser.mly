%{
    open Ast
%}

/* ML1 */
%token PLUS EVALTO MINUS MULT IS LESS THAN NOT
%token AST CROSS HYPHEN

%token IF THEN ELSE TRUE FALSE

/* ContML1 */
%token GTGT UNDERSCORE DRARROW

%%

%public judgment:
   j = drop_ctx { fun _ -> j }
drop_ctx:
    Exp GTGT Cont EVALTO Val { EvalTo($3, $1, $5) }
  | Exp EVALTO Val { EvalTo(RetK, $1, $3) }
  | Val DRARROW Cont EVALTO Val { AppK($3, $1, $5) }

  | SInt PLUS SInt IS SInt { AppBOp(Plus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MULT SInt IS SInt { AppBOp(Mult, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MINUS SInt IS SInt { AppBOp(Minus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt LESS THAN SInt IS TRUE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool (Bool true)) }
  | SInt LESS THAN SInt IS FALSE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool (Bool false)) }
  /* abbreviations for less than */
  | SInt IS LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $5, Value_of_bool (Bool true)) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6, Value_of_bool (Bool false)) }

  | Exp error { errAt 2 "Syntax error: 'evalto' expected" }
  | Exp EVALTO error { errAt 3 "Syntax error: value expected" }
  | Exp GTGT error { errAt 3 "Syntax error: continuation expected" }
  | Exp GTGT Cont error { errAt 4 "Syntax error: 'evalto' expected" }
  | Exp GTGT Cont EVALTO error { errAt 5 "Syntax error: value expected" }

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
    Exp GTGT Cont EVALTO QM { In_EvalTo($3, $1) }
  | Exp EVALTO QM { In_EvalTo(RetK, $1) }
  | Val DRARROW Cont EVALTO QM { In_AppK($3, $1) }
  | SInt PLUS SInt IS QM { In_AppBOp(Plus, Value_of_int $1, Value_of_int $3) }
  | SInt MULT SInt IS QM { In_AppBOp(Mult, Value_of_int $1, Value_of_int $3) }
  | SInt MINUS SInt IS QM { In_AppBOp(Minus, Value_of_int $1, Value_of_int $3) }
/*  | SInt IS LESS THAN SInt { In_AppBOp(Lt, Value_of_int $1, Value_of_int $5) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6) }
*/
  | Exp error { errAt 2 "Syntax error: 'evalto' expected" }
  | Exp EVALTO error { errAt 3 "Syntax error: '?' expected" }
  | Exp GTGT error { errAt 3 "Syntax error: continuation expected" }
  | Exp GTGT Cont error { errAt 4 "Syntax error: 'evalto' expected" }
  | Exp GTGT Cont EVALTO error { errAt 5 "Syntax error: '?' expected" }
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
  | Exp1 BinOp1 LongExp { BinOp($2, $1, $3) }
  | Exp2 BinOp2 LongExp { BinOp($2, $1, $3) }
  | Exp3 BinOp3 LongExp { BinOp($2, $1, $3) }

LongExp:
  | IF Exp THEN Exp ELSE Exp { If($2, $4, $6) }

Exp1:
  | Exp1 BinOp1 Exp2 { BinOp($2, $1, $3) }
  | Exp2 { $1 }

Exp2:
  | Exp2 BinOp2 Exp3 { BinOp($2, $1, $3) }
  | Exp3 { $1 }

Exp3:
    Exp3 BinOp3 AExp { BinOp($2, $1, $3) }
  | AExp { $1 }

BinOp1:
    LANGLE { Lt }

BinOp2:
    CROSS { Plus }
  | HYPHEN { Minus }

BinOp3:
    AST { Mult }

AExp:
    SInt { Exp_of_int $1 }
  | TRUE { Exp_of_bool (Bool true) }
  | FALSE { Exp_of_bool (Bool false) }
  | LPAREN Exp RPAREN { $2 }
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }

SInt: /* signed int */
    INTL { Int $1 }
  | HYPHEN INTL { Int (- $2) }

Val:
    SInt { Value_of_int $1 }
  | TRUE { Value_of_bool (Bool true) }
  | FALSE { Value_of_bool (Bool false) }

BinOp: BinOp1 {$1} | BinOp2 {$1} | BinOp3 {$1}

Hole:  UNDERSCORE { RetK }

OptCont: GTGT Cont { $2 }
  | /* empty */ { RetK }
  | GTGT error { errAt 2 "Syntax error: continuation expected" }
Cont:
  | Hole { RetK }
  | LBRA Hole BinOp1 Exp2 RBRA OptCont
     { EvalRK($4, $3, $6) }
  | LBRA Hole BinOp2 Exp3 RBRA OptCont
     { EvalRK($4, $3, $6) }
  | LBRA Hole BinOp3 AExp RBRA OptCont
     { EvalRK($4, $3, $6) }
  | LBRA Hole BinOp LongExp RBRA OptCont
     { EvalRK($4, $3, $6) }
  | LBRA Val BinOp Hole RBRA OptCont { AppOpK($2, $3, $6) }
  | LBRA IF Hole THEN Exp ELSE Exp RBRA OptCont
     { BranchK($5, $7, $9) }

  | LBRA error { errAt 2 "Syntax error: '_' or value or 'if' expected" }
  | LBRA Hole error { errAt 3 "Syntax error: '+', '-', '*', or '<' expected" }
  | LBRA Hole BinOp1 error { errAt 4 "Syntax error: expression expected" }
  | LBRA Hole BinOp1 Exp2 error { errAt 5 "Syntax error: '}' expected" }
  | LBRA Hole BinOp2 error { errAt 4 "Syntax error: expression expected" }
  | LBRA Hole BinOp2 Exp3 error { errAt 5 "Syntax error: '}' expected" }
  | LBRA Hole BinOp3 error { errAt 4 "Syntax error: expression expected" }
  | LBRA Hole BinOp3 AExp error { errAt 5 "Syntax error: '}' expected" }
  | LBRA Hole BinOp LongExp error { errAt 5 "Syntax error: '}' expected" }
  | LBRA Val error { errAt 3 "Syntax error: '+', '-', '*', or '<' expected" }
  | LBRA Val BinOp error { errAt 4 "Syntax error: '_' expected" }
  | LBRA Val BinOp Hole error { errAt 5 "Syntax error: '}' expected" }
  | LBRA IF error { errAt 3 "Syntax error: '_' expected" }
  | LBRA IF Hole error { errAt 4 "Syntax error: 'then' expected" }
  | LBRA IF Hole THEN error { errAt 5 "Syntax error: expression expected" }
  | LBRA IF Hole THEN Exp error { errAt 6 "Syntax error: 'else' expected" }
  | LBRA IF Hole THEN Exp ELSE error { errAt 7 "Syntax error: expression expected" }
  | LBRA IF Hole THEN Exp ELSE Exp error { errAt 8 "Syntax error: '}' expected" }

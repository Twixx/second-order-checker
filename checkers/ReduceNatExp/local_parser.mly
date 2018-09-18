%token PLUS REDUCETO MREDUCETO DREDUCETO MULT IS
%token AST CROSS S Z

%%
%public judgment:
    j = dropctx { fun _ -> j }

%inline dropctx:
    Exp REDUCETO Exp { Ast.OneStepTo($1, $3) }
  | Exp MREDUCETO Exp { Ast.MultiStepTo($1, $3) }
  | Exp DREDUCETO Exp { Ast.OneStepToD($1, $3) }
  | Nat PLUS Nat IS Nat { Ast.PlusIs($1, $3, $5) }
  | Nat MULT Nat IS Nat { Ast.MultIs($1, $3, $5) }

  | Exp error { errAt 2 "Syntax error" }
  | Exp REDUCETO error { errAt 3 "Syntax error: expression expected" }
  | Exp MREDUCETO error { errAt 3 "Syntax error: expression expected" }
  | Exp DREDUCETO error { errAt 3 "Syntax error: expression expected" }
  | Nat error { errAt 2 "Syntax error" }
       /* shift/reduce conflict with Judgment: Exp error */
  | Nat PLUS error { errAt 3 "Syntax error: natural number expected" }
  | Nat PLUS Nat error { errAt 4 "Syntax error: 'is' expected" }
  | Nat PLUS Nat IS error { errAt 5 "Syntax error: natural number expected" }
  | Nat MULT error { errAt 3 "Syntax error: natural number expected" }
  | Nat MULT Nat error { errAt 4 "Syntax error: 'is' expected" }
  | Nat MULT Nat IS error { errAt 5 "Syntax error: natural number expected" }

(*
partialj:
    Exp REDUCETO Exp { In_OneStepTo($1, $3) }
  | Exp MREDUCETO Exp { In_MultiStepTo($1, $3) }
  | Exp DREDUCETO QM { In_OneStepToD $1 }
  | Nat PLUS Nat IS QM { In_PlusIs($1, $3) }
  | Nat MULT Nat IS QM { In_MultIs($1, $3) }

  | Exp error { errAt 2 "Syntax error" }
  | Exp REDUCETO error { errAt 3 "Syntax error: expression expected" }
  | Exp MREDUCETO error { errAt 3 "Syntax error: expression expected" }
  | Exp DREDUCETO error { errAt 3 "Syntax error: '?' expected" }
  | Nat error { errAt 2 "Syntax error" }
       /* shift/reduce conflict with partialj: Exp error */
  | Nat PLUS error { errAt 3 "Syntax error: natural number expected" }
  | Nat PLUS Nat error { errAt 4 "Syntax error: 'is' expected" }
  | Nat PLUS Nat IS error { errAt 5 "Syntax error: '?' expected" }
  | Nat MULT error { errAt 3 "Syntax error: natural number expected" }
  | Nat MULT Nat error { errAt 4 "Syntax error: 'is' expected" }
  | Nat MULT Nat IS error { errAt 5 "Syntax error: '?' expected" }
*)

Exp:
    Exp CROSS MExp { (Ast.P ($1, $3)) }
  | MExp { $1 }
  | Exp CROSS error { errAt 3 "Syntax error: expression expected" }

MExp:
    MExp AST AExp { (Ast.M ($1, $3)) }
  | AExp { $1 }
  | MExp AST error { errAt 3 "Syntax error: expression expected" }

AExp:
    Nat { (Ast.Exp_of_Nat $1) }
  | LPAREN Exp RPAREN { $2 }
  | LPAREN error { errAt 1 "Syntax error: expression expected" }
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }

Nat:
    Z { Ast.Z }
  | S LPAREN Nat RPAREN { (Ast.S $3) }
  | S LPAREN Nat error { errBtw 2 4 "Syntax error: unmatched parenthesis" }
  | S LPAREN error { errAt 3 "Syntax error: natural number expected after S(" }
  | S error { errAt 2 "Syntax error: opening parenthesis expected after S" }



%token PLUS EVALTO MULT IS
%token AST CROSS S Z

%%

%public judgment:
    exp EVALTO nat { fun _ -> Ast.EvalTo($1, $3) }
  | nat PLUS nat IS nat { fun _ -> Ast.PlusIs($1, $3, $5) }
  | nat MULT nat IS nat { fun _ -> Ast.MultIs($1, $3, $5) }

  | exp error { errAt 2 "Syntax error: '*', '+', or 'evalto' expected" }
  | exp EVALTO error { errAt 3 "Syntax error: natural number expected" }
  | nat error { errAt 2 "Syntax error: 'plus', 'times', '+', '*', or 'evalto' expected" }
       /* intentional shift/reduce conflict with Judgment: exp error */
  | nat PLUS error { errAt 3 "Syntax error: natural number expected" }
  | nat PLUS nat error { errAt 4 "Syntax error: 'is' expected" }
  | nat PLUS nat IS error { errAt 5 "Syntax error: natural number expected" }
  | nat MULT error { errAt 3 "Syntax error: natural number expected" }
  | nat MULT nat error { errAt 4 "Syntax error: 'is' expected" }
  | nat MULT nat IS error { errAt 5 "Syntax error: natural number expected" }

exp:
    exp CROSS mexp { (Ast.P ($1, $3)) }
  | mexp { $1 }
  | exp CROSS error { errAt 3 "Syntax error: expression expected" }

mexp:
    mexp AST aexp { (Ast.M ($1, $3)) }
  | aexp { $1 }
  | mexp AST error { errAt 3 "Syntax error: expression expected" }

aexp:
    nat { (Ast.Exp_of_Nat $1) }
  | LPAREN exp RPAREN { $2 }
  | LPAREN error { errAt 1 "Syntax error: expression expected" }
  | LPAREN exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }

nat:
    Z { Ast.Z }
  | S LPAREN nat RPAREN { (Ast.S $3) }
  | S LPAREN nat error { errBtw 2 4 "Syntax error: unmatched parenthesis" }
  | S LPAREN error { errAt 3 "Syntax error: natural number expected after S(" }
  | S error { errAt 2 "Syntax error: opening parenthesis expected after S" }



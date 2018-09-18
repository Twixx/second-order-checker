%token PLUS MULT IS
%token S Z

%%

%public judgment:
  | nat PLUS nat IS nat { fun _ -> Ast.PlusIs ($1, $3, $5) }
  | nat MULT nat IS nat { fun _ -> Ast.MultIs ($1, $3, $5) }

  | nat error { errAt 2 "Syntax error: 'plus' or 'times' expected" }
  | nat PLUS error { errAt 3 "Syntax error: natural number expected" }
  | nat PLUS nat error { errAt 4 "Syntax error: 'is' expected" }
  | nat PLUS nat IS error { errAt 5 "Syntax error: natural number expected" }
  | nat MULT error { errAt 3 "Syntax error: natural number expected" }
  | nat MULT nat error { errAt 4 "Syntax error: 'is' expected" }
  | nat MULT nat IS error { errAt 5 "Syntax error: natural number expected" }

nat:
    Z { Ast.Z }
  | S LPAREN nat RPAREN { (Ast.S $3) }
  | S LPAREN nat error { errBtw 2 4 "Syntax error: unmatched parenthesis" }
  | S LPAREN error { errAt 3 "Syntax error: natural number expected after S(" }
  | S error { errAt 2 "Syntax error: opening parenthesis expected after S" }
  | LCID { errAt 1 "Syntax error" }
  | INTL { errAt 1 "Syntax error" }



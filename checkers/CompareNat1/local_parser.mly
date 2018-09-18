%token IS LESS THAN
%token S Z

%%

%public judgment:
    nat IS LESS THAN nat { fun _ -> Ast.Lt($1, $5) }
  | nat error { errAt 2 "Syntax error: 'is' expected" }
  | nat IS error { errAt 3 "Syntax error: 'less' expected" }
  | nat LESS error { errAt 4 "Syntax error: 'than' expected" }
  | nat IS LESS THAN error { errAt 5 "Syntax error: natural number expected" }

nat:
    Z { Ast.Z }
  | S LPAREN nat RPAREN { (Ast.S $3) }
  | S LPAREN nat error { errBtw 2 4 "Syntax error: unmatched parenthesis" }
  | S LPAREN error { errAt 3 "Syntax error: natural number expected after S(" }
  | S error { errAt 2 "Syntax error: opening parenthesis expected after S" }


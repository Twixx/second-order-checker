open Parser

let v = [
  (* game-specific keywords *)
  ("evalto", EVALTO);
  
  ("minus", MINUS);
  ("times", MULT);
  ("plus", PLUS);
  ("is", IS);
  ("less", LESS);
  ("than", THAN);
  ("not", NOT);

  (* ML1 expressions *)
  ("true", TRUE);
  ("false", FALSE);
  ("if", IF);
  ("then", THEN);
  ("else", ELSE);
  
  ("*", AST);
  ("+", CROSS);
  ("-", HYPHEN);

  (* ML2 judgments and expressions *)
  ("|-", VDASH);

  ("let", LET);
  ("in", IN);
  ("=", EQ);
] 


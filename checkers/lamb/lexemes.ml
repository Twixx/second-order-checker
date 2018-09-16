open Parser
let v = [
    ("\\", LAMBDA);
    (".", DOT);
    ("letrec", LETREC);
    ("=", EQ);
    ("in", IN);
    ("(", LPAREN);
    (")", RPAREN);
    ("->", IS);
]

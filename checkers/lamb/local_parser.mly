%token LAMBDA DOT LETREC IN EQ IS
%%
%public judgment:
    e1 = term IS e2 = term
    {
        fun (ctx : Ast.parsing_ctx) ->
            Ast.DeriveTo (e1 ctx, e2 ctx)
    }

term:
    LAMBDA id = LCID DOT t = term
    {
        fun ctx ->
            Ast.(Term_of_Value (Ast.Lamb (t (add_bound ctx id))))

    }
    | LETREC x = LCID y = LCID EQ t1 = term IN t2 = term
    {
        fun ctx ->
            Ast.(LetRec (t1 (add_bounds ctx [x; y]), t2 (add_bound ctx x)))
    }
    | e = app
    { e }

app:
    e1 = app e2 = id
    {
        fun ctx ->
            Ast.App (e1 ctx, e2 ctx)
    }
    | e = id
    { e }

id:
    id = LCID
    {
        fun ctx ->
            Ast.(Term_of_Value (Var_ (TERM, lookup_ctx ctx id)))
    }
    | LPAREN e = term RPAREN
    { e }

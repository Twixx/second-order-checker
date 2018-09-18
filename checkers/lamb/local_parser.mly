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
            let open Ast in
            let var =
            { term = Lamb (t (add_bound ctx id));
              pos = ($startpos, $endpos) }
            in
            { term = Term_of_Value var;
              pos = ($startpos, $endpos) }

    }
    | LETREC x = LCID y = LCID EQ t1 = term IN t2 = term
    {
        fun ctx ->
            let open Ast in
            { term = LetRec (t1 (add_bounds ctx [x; y]), t2 (add_bound ctx x));
              pos = ($startpos, $endpos) }
    }
    | e = app
    { e }

app:
    e1 = app e2 = id
    {
        fun ctx ->
            let open Ast in
            { term = App (e1 ctx, e2 ctx);
              pos = ($startpos, $endpos) }
    }
    | e = id
    { e }

id:
    id = LCID
    {
        fun ctx ->
            let var =
            let open Ast in
            { term = Var_ (TERM, lookup_ctx ctx id);
              pos = ($startpos, $endpos) }
            in
            { term = Term_of_Value var;
              pos = ($startpos, $endpos) }
    }
    | LPAREN e = term RPAREN
    { e }

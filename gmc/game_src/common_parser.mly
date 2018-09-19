%{
    let errBtw i j s =
        raise (Ast.ParseError (s, Parsing.((rhs_start_pos i, rhs_end_pos j))))

    let errAt i s = errBtw i i s
%}

%token LANGLE RANGLE
%token LBRA RBRA
%token LSBRA RSBRA
%token BY
%token SEMI COMMA LPAREN RPAREN
%token <int> INTL
%token EOF
%token <string> LCID UCID ID

%start entrypoint

%type <Ast.ast> entrypoint
%%

entrypoint:
    e = ast
    {
        let ctx = Ast.new_ctx in
        let ast = e ctx in
        Ast.gen_free_vars ast
    }

rulename:
    name = LCID | name = UCID | name = ID
    {
        (name, ($startpos, $endpos))
    }

ast:
    e1 = concl BY e2 = rulename LBRA e3 = premises RBRA
    {
        fun ctx ->
            let premises =
                match List.map (fun f -> f 0 ctx) e3  with
                | [] -> Ast.Empty_
                | l -> Ast.Premises_ l
            in
            let jpos = ($startpos(e1), $endpos(e1)) in
            let bound, concl = e1 0 ctx in
            ((bound, concl, jpos), e2, premises, ($startpos, $endpos))
    }

premises:
    { [] }
    | l = premise_list { l }

premise_list:
    p = premise SEMI prev = premise_list { p :: prev }
    | p = premise SEMI? { [p] }

concl:
    LANGLE vars = separated_nonempty_list(COMMA, LCID) RANGLE LSBRA concl = concl RSBRA
    {
        fun count ctx ->
            concl (count + (List.length vars)) (Ast.add_bounds ctx vars)
    }
    | judg = judgment { fun count ctx -> (count, judg ctx) }

premise:
    LANGLE vars = separated_nonempty_list(COMMA, LCID) RANGLE LSBRA j = premise RSBRA
    {
        fun count ctx ->
            j (count + List.length vars) (Ast.add_bounds ctx vars)
    }
    | ast = ast
    {
        fun count ctx ->
            (count, ast ctx)
    }

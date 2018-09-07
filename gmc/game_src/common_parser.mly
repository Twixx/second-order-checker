%token LPAREN RPAREN
%token LBRA RBRA
%token LSBRA RSBRA
%token BY
%token LAMBDA
%token LETREC EQ IN
%token DOT
%token IS
%token SEMI COMA
%token DQUOTE
%token EOF
%token <string> ID
%token <Ast.rule_name> RULENAME

%start entrypoint

%type <Ast.ast> entrypoint
%%

entrypoint:
    e = ast EOF
    {
        let ctx = Ast.new_ctx in
        let ast = e ctx in
        Ast.gen_free_vars ast
    }

rule:
    name = RULENAME
    { (name, ($startpos, $endpos)) }

ast:
    e1 = concl BY e2 = rule LBRA e3 = separated_list(SEMI, premise) RBRA
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

concl:
    LPAREN vars = separated_nonempty_list(COMA, ID) RPAREN LSBRA concl = concl RSBRA
    {
        fun count ctx ->
            concl (count + (List.length vars)) (Ast.add_bounds ctx vars)
    }
    | judg = judgment { fun count ctx -> (count, judg ctx) }

premise:
    LPAREN vars = separated_nonempty_list(COMA, ID) RPAREN LSBRA j = premise RSBRA
    {
        fun count ctx ->
            j (count + List.length vars) (Ast.add_bounds ctx vars)
    }
    | ast = ast
    {
        fun count ctx ->
            (count, ast ctx)
    }

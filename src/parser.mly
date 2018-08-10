%token SHEADER JHEADER RHEADER
%token IN
%token <Char.t> CHAR
%token <string> LCID UCID RULENAME
%token DEF
%token LPAREN RPAREN
%token LANGLE RANGLE
%token LBRACKET RBRACKET
%token OR
%token SEMI COMA
%token <char * int> VAR
%token EOF

%start entrypoint
%type <Ast.game> entrypoint

%%

entrypoint:
    SHEADER defs = list(term_def) JHEADER j = list(judgement) RHEADER r = rules EOF
    {
        let ctor_table = Ast.new_symb_table () in
        let ctors = Ast.apply ctor_table defs in
        let judgs = Ast.apply ctor_table j in
        let rules = Ast.apply ctor_table r in
        { Ast.syms = ctor_table; Ast.bnf_def = ctors; Ast.judgs = judgs; rules = rules}
    }

term_def:
    c = CHAR IN name = LCID opt = option(ctors_def)
    {
        fun ctor_table ->
            let ctors =
                match opt with
                | None -> []
                | Some l -> Ast.apply ctor_table l
            in
            {Ast.id = Char.code c; Ast.name = name; Ast.ctors = ctors}
    }

ctors_def:
    DEF l = separated_nonempty_list(OR, ctor) {l}

meta_par:
    LANGLE params = separated_nonempty_list(COMA, CHAR) RANGLE
    {params}

term_par:
    LBRACKET params = separated_list(COMA, CHAR) RBRACKET
    {
        fun ctx ->
            let rec lookup n ctx c =
                match ctx with
                | [] -> raise (Ast.UnboundParam c)
                | h :: t -> if h = c then n else lookup (n+1) t c
            in List.map (lookup 0 ctx) params
    }

term:
    id = CHAR opt = option(term_par)
    {
        fun ctx ->
            let params =
                match opt with
                | Some l -> l ctx
                | None -> [] 
            in
            (Char.code id, params)
    }

ctor:
    | name = UCID opt = option(meta_par) LPAREN t = separated_list(COMA, term) RPAREN
    {
        fun ctor_table ->
            let ctx =
                match opt with
                | Some l -> l
                | None -> []
            in
            let params = Ast.apply ctx t in
            let id = Ast.add_ctor name ctor_table in
            Ast.Constructor (id, List.length ctx, params)
    }
    | c = CHAR { fun _ -> Ast.Sub (Char.code c) }

judgement:
    name = UCID LPAREN l = separated_list(COMA, CHAR) RPAREN
    { fun table ->
        let id = Ast.add_judg name table in
        (id, List.map Char.code l)
    }

rules:
    l = separated_list(SEMI, rule) { l }

rule:
    prems = list(expr) r = RULENAME c = expr
    {
        fun table ->
            let param = (table, []) in
            {Ast.premises = Ast.apply param prems; Ast.concl = c param; Ast.name = r}
    }

expr:
    | name = UCID opt = option(meta_par) LPAREN l = separated_list(COMA, expr) RPAREN
    {
        fun (table, ctx) ->
            let id = Ast.get_ctor_id name table in
            let new_ctx = 
                match opt with
                | None -> ctx
                | Some vars -> (List.rev vars) @ ctx
            in
            Ast.Ctor (id, List.length l, Ast.apply (table, new_ctx) l)
    }
    | v = VAR opt = option(var_params)
    {
        let (c, i) = v in
        fun (table, ctx) ->
            let params = 
                match opt with
                | None -> []
                | Some l -> Ast.apply (table, ctx) l
            in
            Ast.Var ((Char.code c, i), params)
    }
    | LPAREN vars = separated_nonempty_list(COMA, CHAR) RPAREN LBRACKET e = expr RBRACKET
    {
        fun (table, ctx) ->
            let new_ctx = (List.rev vars) @ ctx in
            Ast.Abs (e (table, new_ctx))
    }

var_params:
    LBRACKET l = separated_list(COMA, var_param) RBRACKET { l }

var_param:
    | e = expr
    {
        fun (table, ctx) ->
            Ast.Expr (e (table, ctx))
    }
    | id = CHAR
    {
        fun (table, ctx) ->
            let rec lookup n ctx c =
                match ctx with
                | [] -> raise (Ast.UnboundParam c)
                | h :: t -> if h = c then n else lookup (n+1) t c
            in Ast.Bound (lookup 0 ctx id)
    }
    
(* FIXME: accept anything as top level rule like var or ctor that is not judg *)
(* FIXME: check that type alias exists *)

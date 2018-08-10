%token SHEADER JHEADER RHEADER
%token IN
%token <Char.t> CHAR
%token <string> LCID UCID
%token DEF
%token LPAREN RPAREN
%token LANGLE RANGLE
%token LBRACKET RBRACKET
%token OR
%token SEMI COMA
%token EOF

%start entrypoint
%type <Ast.game> entrypoint

%%

entrypoint:
    SHEADER defs = nonempty_list(term_def) JHEADER RHEADER EOF
    {
        let ctor_table = Ast.new_ctor_table () in
        let ctors = Ast.apply ctor_table defs in
        { Ast.syms = ctor_table; Ast.bnf_def = ctors }
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
    LANGLE params = separated_list(COMA, CHAR) RANGLE
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

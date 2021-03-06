%token SHEADER JHEADER RHEADER
%token <string> MLCODE
%token IN WITH VAR TRUE FALSE
%token <string> LCID UCID RULENAME
%token DEF
%token LPAREN RPAREN
%token LANGLE RANGLE
%token LBRACKET RBRACKET
%token OR
%token SEMI COMMA
%token <string * int * int> VARNAME
%token <Ast.qexp list> QEXP
%token EOF

%start entrypoint
%type <Ast.game> entrypoint
%type <Ast.variable> varname

%%

entrypoint:
    SHEADER defs = syncat+
    JHEADER judgs = judgment+
    RHEADER rules = separated_nonempty_list(SEMI, rule)
    ml_code = MLCODE? EOF
    {
        { bnf_ast = defs;
        judgs = judgs;
        rules = rules;
        ml_code = ml_code }
    }

syncat:
    syms = symbol_list IN tname = UCID vars = var_symbols syns = syncat_defs
    {
        { Ast.syn_symbols = syms;
          Ast.var_symbols = vars;
          Ast.catname = (tname, ($startpos(tname), $endpos(tname)));
          Ast.defs = syns; }
    }
    | syms = symbol_list IN tname = LCID
    {
        { Ast.syn_symbols = syms;
          Ast.var_symbols = [];
          Ast.catname = (tname, ($startpos(tname), $endpos(tname)));
          Ast.defs = [] }
    }

symbol_list:
    symb = LCID { [(symb, ($startpos, $endpos))] }
    | s = LCID COMMA l = symbol_list { (s, ($startpos(s), $endpos(s))) :: l}
    | s = LCID SEMI l = symbol_list { (s, ($startpos(s), $endpos(s))) :: l}

var_symbols:
    (* empty *) { [] }
    | WITH VAR v = symbol_list { v }

syncat_defs:
    (* empty *) { [] }
    | DEF l = separated_nonempty_list(OR, syncat_def) {l}

syncat_def:
    | name = UCID ctx = meta_par
      t = loption(delimited(LPAREN, separated_list(COMMA, ctor_param), RPAREN))
    {
        let info = ($startpos, $endpos) in
        Ast.CtorDef ((name, ctx, t), info)
    }
    | name = LCID
    {
       let info = ($startpos, $endpos) in
       Ast.Symbol (name, info)
    }

meta_par:
    l = loption(delimited(LANGLE, separated_list(COMMA, mpar), RANGLE))
    { List.rev l }

mpar:
    id = LCID { (id, ($startpos, $endpos)) }

bound_params:
    (* empty *) { [] }
    | LBRACKET params = symbol_list RBRACKET
    {
        List.rev params
    }

ctor_param:
    name = LCID params = bound_params
    {
        let info = ($startpos, $endpos) in
        (name, params, info)
    }

judgment:
    name = UCID args = loption(delimited(LPAREN, symbol_list, RPAREN))
    {
        let info = ($startpos, $endpos) in
        (name, args, info)
    }

rule:
    prems = premise* r = RULENAME c = topexpr
    {
        let info = ($startpos(r), $endpos(r)) in
        { Ast.premises = prems;
          Ast.concl = c;
          Ast.name = (r, info); }
    }

premise:
    t = topexpr { Judgment t }
    | e = QEXP { QExp e }

topexpr:
    name = UCID LPAREN l = separated_list(COMMA, expr) RPAREN
    {
        let info = ($startpos, $endpos) in
        let judg = (name, l) in
        (* add an empty context *)
        ([], judg, info)
    }
    | LPAREN vars = separated_nonempty_list(COMMA, mpar) RPAREN LBRACKET t = topexpr RBRACKET
    {
        let ctx, expr, info = t in
        (ctx @ (List.rev vars), expr, info)
    }

expr:
    name = UCID vars = meta_par
    l = loption(delimited(LPAREN, separated_list(COMMA, expr), RPAREN))
    {
        let info = ($startpos, $endpos) in
        (Ast.Ctor (name, vars, l), info)

    }
    | TRUE vars = meta_par { (Ast.Bool true, ($startpos, $endpos)) }
    | FALSE vars = meta_par { (Ast.Bool false, ($startpos, $endpos)) }
    | v = varname params = var_params
    {
            let info = ($startpos, $endpos) in
            (Ast.Var (v, params), info)
    }

varname:
    name = LCID { (name, -1, 0) }
    | v = VARNAME { v }

var_params:
    (* empty *) { [] }
    | LBRACKET l = separated_list(COMMA, expr) RBRACKET { l }

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
        (* Free variable are saved as de bruijn indices
         * They are kept in the free_vars list
         * positive indices are bound variables
         * negative ones are the negation of the positions of the free ones in
         * the list regardless of the context *)

        (* These functions shift the index of a free variable to its real ones
         * they keep track of the index of the first free variable
         * then substract the negative index to it to have a possible index
         * greater than all the bound ones *)
        let rec shift_concl free_idx (len, Ast.DeriveTo (n1, n2), info) =
            (len, Ast.DeriveTo (shift_node (free_idx + len) n1, shift_node (free_idx + len) n2), info)
        and shift_prems free_idx = function
            | Ast.Premises_ prems ->
                    let shift_prem (len, ast) = (len, shift_ast (free_idx + len) ast) in
                    Ast.Premises_ (List.map shift_prem prems)
            | Ast.Empty_ -> Ast.Empty_
        and shift_ast free_idx (concl, r, prems, info) =
            let concl' = shift_concl free_idx concl in
            let prems' = shift_prems free_idx prems in
            (concl', r, prems', info)
        and shift_node free_idx node =
            let updated =
                let open Ast in
                match node.term with
                | Lamb t -> Lamb (shift_node (free_idx + 1) t)
                | Term_of_Value t -> Term_of_Value (shift_node (free_idx) t)
                | LetRec (t1, t2) ->
                        LetRec (shift_node (free_idx + 2) t1, shift_node (free_idx + 1) t2)
                | App (t1, t2) -> App (shift_node free_idx t1, shift_node free_idx t2)
                | Var_ (tag, id) when id < 0 -> Var_ (tag, free_idx - id)
                | Var_ _ | Metavar_ _ -> node.term
            in
            { node with term = updated }
        in
        let free_vars = ref [] in
        let ast = e (free_vars, []) in
        shift_ast (-1) ast
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
        fun count (free, ctx) ->
            concl (count + (List.length vars)) (free, List.rev_append vars ctx)
    }
    | judg = judgment { judg }

premise:
    LPAREN vars = separated_nonempty_list(COMA, ID) RPAREN LSBRA j = premise RSBRA
    {
        fun count (free, ctx) ->
            j (count + List.length vars) (free, List.rev_append vars ctx)
    }
    | ast = ast
    {
        fun count ctx ->
            (count, ast ctx)
    }

judgment:
    DQUOTE e1 = term IS e2 = term DQUOTE
    {
        fun count ctx ->
            (count, Ast.DeriveTo (e1 ctx, e2 ctx))
    }

term:
    LAMBDA id = ID DOT t = term
    {
        fun (free, ctx) ->
            let var =
            { Ast.term = Ast.Lamb (t (free, id :: ctx));
              Ast.pos = ($startpos, $endpos) }
            in
            { Ast.term = Ast.Term_of_Value var;
              Ast.pos = ($startpos, $endpos) }

    }
    | LETREC x = ID y = ID EQ t1 = term IN t2 = term
    {
        fun (free, ctx) ->
            { Ast.term = Ast.LetRec (t1 (free, y :: x :: ctx), t2 (free, x :: ctx));
              Ast.pos = ($startpos, $endpos) }
    }
    | e = app
    { e }

app:
    e1 = app e2 = id
    {
        fun ctx ->
            { Ast.term = Ast.App (e1 ctx, e2 ctx);
              Ast.pos = ($startpos, $endpos) }
    }
    | e = id
    { e }

id:
    id = ID
    {
        (* First look if the variable is bound *)
        (* If not, look if we saw it before and then just use the negation of
         * the index in the list *)
        (* If not, add it to the free variables list *)
        fun (free, ctx) ->
            let rec lookup_free n ctx =
                match ctx with
                | [] -> free := !free @ [id]; n
                | h :: t -> if h = id then n else lookup_free (n - 1) t
            in
            let rec lookup_bound n ctx =
                match ctx with
                | [] -> lookup_free (-1) !free
                | h :: t -> if h = id then n else lookup_bound (n + 1) t
            in
            let var =
            { Ast.term = Ast.Var_ (Ast.TERM, lookup_bound 0 ctx);
              Ast.pos = ($startpos, $endpos) }
            in
            { Ast.term = Ast.Term_of_Value var;
              Ast.pos = ($startpos, $endpos) }
    }
    | LPAREN e = term RPAREN
    { e }

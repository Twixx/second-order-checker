open Printf

type ctor_infos =
    { term_name : string;
      ctx_name  : string;
      arity     : int;
      var_tags  : int list list;
      var_ar    : int list; }

type builtin_infos =
    { bltin_ctx         : string;
      bltin_term        : string;
      bltin_type        : string; }

(* Builtin sections *)
let builtin_term_def t =
    sprintf "| %s of %s" t.bltin_term t.bltin_type

let builtin_ctx_def t =
    sprintf "| %s of %s" t.bltin_ctx t.bltin_type

let builtin_closed t =
    sprintf "| %s _" t.bltin_term

let builtin_shift = builtin_closed

let shift_builtin_ctor_ast t =
    sprintf "| %s _ -> term" t.bltin_term

let head_normalize_builtin_fun t =
    sprintf "| %s c -> %s c" t.bltin_ctx t.bltin_term

let builtin_ctx_fun t =
    sprintf "| %s p -> Some (%s p)" t.bltin_term t.bltin_ctx

let builtin_unify_one_fun t =
    sprintf "| %s p1, %s p2 when p1 = p2 ->\n\t\tcreate_eos sols" t.bltin_term t.bltin_term

(* Judgments section *)
let judg_def (name, arity) =
    if arity = 0 then sprintf "| %s" name
    else
        sprintf "| %s of %s" name
            (String.concat " * " (List.init arity (fun _ -> "term")))

let shift_judg_ast (name, arity) =
    if arity = 0 then sprintf "| %s -> %s" name name
    else
        let l = List.init arity (fun i -> sprintf "n%i" i) in
        let params = String.concat ", " l in
        let shifted = String.concat ", shift " l in
        sprintf "| %s (%s) -> %s (shift %s)" name params name shifted

(* Constructors sections *)
let term_def v =
    if v.arity = 0 then sprintf "| %s" v.term_name
    else
        sprintf "| %s of %s" v.term_name
            (String.concat " * " (List.init v.arity (fun _ -> "term")))

let ctx_def v =
    if v.arity = 0 then sprintf "| %s" v.ctx_name
    else
        sprintf "| %s of %s" v.ctx_name
            (String.concat " * " (List.init v.arity (fun _ -> "metavar")))

let shift_ctor_ast v =
    if v.arity = 0 then sprintf "| %s -> term" v.term_name
    else
        let l = List.init v.arity (sprintf "t%i") in
        let params = String.concat ", " l in
        let shift p ar = sprintf "shift %i %s" ar p in
        let shifted = String.concat ", " (List.map2 shift l v.var_ar) in
        sprintf "| %s (%s) -> %s (%s)" v.term_name params v.term_name shifted

let is_closed_fun v =
    if v.arity = 0 then sprintf "| %s -> true" v.term_name
    else
        let l = List.init v.arity (sprintf "n%i") in
        let params = String.concat ", " l in
        let is_closed p ar = sprintf "(is_closed %s (idx + %i))" p ar in
        let right = String.concat " && " (List.map2 is_closed l v.var_ar) in
        sprintf "| %s (%s) -> %s" v.term_name params right

let shift_fun v =
    if v.arity = 0 then sprintf "| %s -> term" v.term_name
    else
        let l = List.init v.arity (sprintf "n%i") in
        let params = String.concat ", " l in
        let shift p ar = sprintf "shift d (c + %i) %s" ar p in
        let shifted = String.concat ", " (List.map2 shift l v.var_ar) in
        sprintf "| %s (%s) -> %s (%s)" v.term_name params v.term_name shifted

let head_normalize_fun tag_names v =
    if v.arity = 0 then sprintf "| %s -> %s" v.ctx_name v.term_name else
    let l = List.init v.arity (sprintf "m%i") in
    let params = String.concat ", " l in
    let gen_shift i = sprintf "\t\tlet par%i = List.map (shift 1 0) par%i in\n" (i + 1) i in
    let max_ar = List.fold_left max 0 v.var_ar in
    let decl = String.concat "" (List.init max_ar gen_shift) in
    let gen_vars tags =
        let mparams = List.mapi (fun i tag -> sprintf "(<var> (%s, %i)) ::" tag_names.(tag) i) (List.rev tags) in
        String.concat " " mparams
    in
    let meta m tags = sprintf "<metavar> (%s, %s par%i)" m (gen_vars tags) (List.length tags) in
    let rparams = String.concat ", " (List.map2 meta l v.var_tags) in
    sprintf "| %s (%s) ->\n %s\t\t%s (%s)" v.ctx_name params decl v.term_name rparams

let ctx_fun v =
    if v.arity = 0 then sprintf "| %s -> Some %s" v.term_name v.ctx_name else
    let l = (List.init v.arity (fun _ -> "pick_fresh_name ()")) in
    sprintf "| %s _ -> Some (%s (%s))" v.term_name v.ctx_name (String.concat ", " l)

let unify_one_fun v =
    if v.arity = 0 then
        sprintf "| %s, %s ->\n\t\tcreate_eos sols" v.term_name v.term_name
    else if v.arity = 1 then
        sprintf "| %s n1, %s n2 ->\n\t\tunify_one (n1, n2, threshold + %i) sols"
        v.term_name v.term_name (List.hd v.var_ar)
    else
        let l = List.init v.arity (fun i -> (sprintf "m%i" i, sprintf "n%i" i)) in
        let l1, l2 = List.split l in
        let params = String.concat ", " l1 in
        let params2 = String.concat ", " l2 in
        let triple ar (l, r) = sprintf "(%s, %s, threshold + %i)" l r ar in
        let rparams = List.map2 triple v.var_ar l in
        sprintf "| %s (%s), %s (%s) ->\n\t\tunify_head %s [%s] (create_eos sols)"
            v.term_name params v.term_name params2 (List.hd rparams)
            (String.concat "; " (List.tl rparams))

(* Rule sections *)
let rule_enum (_, _, _, enum) = "| " ^ enum
let rule_map_list (_, _, name, enum) = sprintf "(\"%s\", %s);" name enum

let indent = Str.global_replace (Str.regexp_string "\n") "\n    "
let indent2 = Str.global_replace (Str.regexp_string "\n") "\n        "

module QMap = Map.Make(struct type t = int let compare = compare end)
let match_rule ctors judgs tags builtins (premises, conclusion, name, enum) =
    let rec emit_expr expr =
        match expr with
        | Game.Ctor (id, exprs) ->
                if exprs = [] then ctors.(id).term_name
                else
                    sprintf "%s (%s)" ctors.(id).term_name
                        (String.concat ", " (List.map emit_expr exprs))
        | Bool (cat_id, b) ->
                sprintf "%s %s" builtins.(cat_id).bltin_term (string_of_bool b)
        | Var (var_id, params) ->
                sprintf "<metavar> (%i, [%s])" var_id
                    (String.concat "; " (List.map emit_expr params))
        | Bound (tag, _, i) ->
                sprintf"<var> (%s, %i)" tags.(tag) i
    in
    let gen_judgexpr_match i bound (id, exprs) =
        let gen_pattern j expr =
            sprintf "let m%i_%i = %s in" i j (emit_expr expr)
        in
        let name, ar = judgs.(id) in
        let patterns = String.concat "\n" (List.mapi gen_pattern exprs) in
        let l = List.init ar (fun j -> sprintf "n%i_%i" i j) in
        let p = String.concat ", " l in
        let match_par =
            if l = [] then sprintf "| %s -> ()" name
            else sprintf "| %s (%s) -> (%s)" name p p
        in
        let except =
            if (Array.length judgs) > 1 then "\n| _ -> raise (JudgError (pos))"
            else ""
        in
        let body = "\nmatch concl with\n" ^ match_par ^ except in
        let cons = List.init ar (fun j -> sprintf "(m%i_%i, n%i_%i, %i)" i j i j bound) in
        (sprintf "\nlet (%s) =%s\nin\n%s" p (indent body) patterns, cons)
    in
    let rec gen_premises i bodies consts match_l qexps = function
        | Game.Judg (bound, id, exprs) :: tl ->
            let decomp = sprintf "\nlet (ctx_len, ((concl_len, concl, pos), _, _, _)) = j%i in" i in
            let check = sprintf "\nif (ctx_len + concl_len) <> %i then raise (AbsError (ctx_len + concl_len, %i, pos));" bound bound in
            let body, cons = gen_judgexpr_match i bound (id, exprs) in
            let new_bodies = (decomp ^ check ^ body) :: bodies in
            let new_consts = List.rev_append cons consts in
            let new_match_l = (sprintf "j%i" i) :: match_l in
            gen_premises (i + 1) new_bodies new_consts new_match_l qexps tl
        | Game.QExp e :: tl ->
                let new_qexpr = e :: qexps in
                gen_premises i bodies consts match_l new_qexpr tl
        | [] -> List.(rev bodies, rev consts, rev match_l, rev qexps, i)
    in
    let gen_concl (bound, id, exprs) =
        let decomp = "\nlet (concl_len, concl, pos) = concl in" in
        let check = sprintf "\nif concl_len <> %i then raise (AbsError (concl_len, %i, pos));" bound bound in
        let body, cons = gen_judgexpr_match 0 bound (id, exprs) in
        (decomp ^ check ^ body, cons)
    in
    let concl_body, concl_const = gen_concl conclusion in
    let result = gen_premises 1 [] concl_const [] [] premises in
    let prem_bodies, consts, match_l, qexps, ar = result in
    let prem_err = sprintf "raise (PremError (List.length premises, %i))" ar in
    let m_body = String.concat "" prem_bodies in
    let unif = sprintf "\nunify [%s]" (String.concat "; " consts) in
    let jlist = String.concat "; " match_l in
    let prem_match =
        sprintf "\nbegin match premises with\n| [%s] ->%s%s\n| _ -> %s\nend"
            jlist (indent2 m_body) (indent2 unif) prem_err
    in
    let rule_body = (indent2 concl_body) ^ (indent2 prem_match) in
    let unif_body = sprintf "| %s ->%s" enum rule_body in

    if qexps = [] then unif_body ^ indent2 "\n|> (function None -> false | _ -> true)"
    else
    let add_qmap m = function
        | Game.QVar (cat, i) -> QMap.add i cat m
        | QStr _ -> m
    in
    let qmap = List.fold_left (fun m l -> List.fold_left add_qmap m l) QMap.empty qexps in
    let m = match QMap.max_binding_opt qmap with None -> 0 | Some (m, _) -> (m + 1) in
    let rec concat_qexp acc = function
            | Game.QVar (_, i) :: tl -> concat_qexp (sprintf "%sv%i" acc i) tl
            | QStr str :: tl -> concat_qexp (acc ^ str) tl
            | [] -> sprintf "\n(%s)" acc
    in
    let qexps_bodies = List.map (concat_qexp "") qexps in
    let qexps_body = String.concat " &&" qexps_bodies in
    let match_var i cat =
        sprintf "\nlet v%i = match vars.(%i) with <closed> (%s v) -> v | _ -> raise (UnknownError __LINE__) in" i i builtins.(cat).bltin_term
    in
    let build_vars = QMap.fold (fun k v vars -> vars ^ (match_var k v)) qmap "" in
    let fun_def = sprintf "\n|>\nlet check_qexpr vars =%s\nin check_qexprs %i check_qexpr" (indent (build_vars ^ qexps_body)) m in
    unif_body ^ (indent2 fun_def)

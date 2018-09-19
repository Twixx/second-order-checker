(* Exceptions *)
type info = Ast.info
exception CtorAlreadyDefined of string * info
exception UndefinedCtor of string * info
exception UndefinedJudg of string * info
exception CatAlreadyDefined of string * info
exception SymbolAlreadyDefined of string * info
exception VarAlreadyDefined of string * info
exception WrongVarDef of string * string * info
exception UndefinedSymbol of string * info
exception CycleDetected of string
exception DiamondDetected of string * string
exception MultipleAncestors of string * string * info
exception InvalidBuiltin of string * info
exception UndeclaredQVar of Ast.variable * info
exception InvalidQVar of Ast.variable * string * info
exception WrongArityVar of Ast.variable * int * int * info
exception WrongArityCtor of string * int * int * info
exception WrongMetaArityCtor of string * int * int * info
exception InvalidCat of string * string * info
exception RuleAlreadyDefined of string * info
exception UnboundParam of string * info
exception InvalidBoundParam of string * info
exception UndefinedBool of info

type symbol_id = int
type ctor_id = int
type cat_id = int

(* New AST once checked *)
type var_id = int
and expr =
    | Ctor of ctor_id * expr list
    | Bool of cat_id * bool
    | Var of var_id * expr list
    | Bound of cat_id * cat_id * int

type judg = ctor_id * expr list
type topexpr = int * ctor_id * expr list
type qexpr = QVar_btin of cat_id * var_id | QVar of var_id  | QStr of string
type premise = Judg of topexpr | QExp of qexpr list

module type GAME_AST = sig val game : Ast.game end
module type GAME = sig
    (* Contains the indexed names of categories *)
    val cat_names   : string array
    (* Number of built-in categories (they come at first in category arrays *)
    val builtin_num : int
    (* Constructor infos: name, category of metavariables,
     * category and list of index of bound variables in order for parameters *)
    val ctor_infos  : (string * cat_id * cat_id list * (cat_id * int list) list) array
    (* Judgment infos: name, category of parameters *)
    val judg_infos  : (string * cat_id list) array
    (* List of all injection constructors *)
    val sub_ctors   : (int * int) array

    val rule_infos  : (premise list * topexpr * Ast.rule_name) list
end

module MakeGame (G : GAME_AST) : GAME = struct
    let bnf_ast = G.game.bnf_ast
    let def_num = List.length bnf_ast

    (* Build the table that associates category ID with category *)
    (* Table to keep track of all the names already defined and associated IDs *)
    let sym_nametbl = Hashtbl.create 16
    let var_nametbl = Hashtbl.create 16
    let ctor_nametbl = Hashtbl.create 16

    let cat_names = Array.make def_num ""
    (* List of the parent categories for each category *)
    let parents = Array.make def_num []

    let get_sym_id name pos =
        match Hashtbl.find_opt sym_nametbl name with
        | Some id -> id
        | None -> raise (UndefinedSymbol (name, pos))

    let get_var_id name pos =
        match Hashtbl.find_opt var_nametbl name with
        | Some id -> id
        | None -> raise (UndefinedSymbol (name, pos))

    (* Check quickly if the name is user-defined or built-in *)
    let is_builtin id =
        let first = cat_names.(id).[0] in
        (Char.lowercase_ascii first = first)

    let is_valid_builtin_name name =
        if name.[0] = (Char.lowercase_ascii name.[0]) then
            match name with
            | "int" | "bool" -> true
            | _ -> false
        else true

    (* Collect everything from the BNF *)
    let cat_names, builtin_num, sym_cats, var_tags, var_cat_defs, ctor_infos =
        (* Keep track of defined categories *)
        let cat_table = Hashtbl.create def_num in
        let check_cat pos name =
            match Hashtbl.find_opt cat_table name with
            | Some _ -> raise (CatAlreadyDefined (name, pos))
            | None -> Hashtbl.add cat_table name ()
        in
        (* Build the array of category names with built-in first,
         * return the definitons in the same order *)
        let rec extract_cats builtin userdef btin_defs defs = function
            | { Ast.catname = (name, pos) } as hd :: tl ->
                    check_cat pos name;
                    if hd.defs = [] then begin
                        if not (is_valid_builtin_name name) then
                            raise (InvalidBuiltin (name, pos));
                        extract_cats (name :: builtin) userdef (hd :: btin_defs) defs tl
                    end else begin
                        extract_cats builtin (name :: userdef) btin_defs (hd :: defs) tl
                    end
            | [] -> (builtin @ userdef, btin_defs @ defs, List.length builtin)
        in
        let names, def_list, builtin_num = extract_cats [] [] [] [] bnf_ast in
        List.iteri (fun i name -> cat_names.(i) <- name) names;

        (* Build the tables that associate symbol with ID and category
         * and the category of a (meta)variable definiton (which is its tag) *)
        (* The hashtables count increases at the same time than the lists, then
            * adding the count as id matches the corresponding element in the
            * generated list *)
        let add_name names cid (name, pos) =
            let r1 = Hashtbl.find_opt sym_nametbl name in
            let r2 = Hashtbl.find_opt var_nametbl name in
            match r1, r2 with
            | None, None ->
                    Hashtbl.add names name (Hashtbl.length names);
                    cid
            | _ -> raise (SymbolAlreadyDefined (name, pos))
        in
        let rec exctract_syns cid infos tags = function
            | { Ast.syn_symbols = s; var_symbols = v } :: tl ->
                    let new_infos = infos @ List.map (add_name sym_nametbl cid) s in
                    let new_tags = tags @ List.map (add_name var_nametbl cid) v in
                    exctract_syns (cid + 1) new_infos new_tags tl
            | [] -> (infos, tags)
        in
        let r1, r2 = exctract_syns 0 [] [] def_list in
        let sym_cats, var_tags = Array.(of_list r1, of_list r2) in

        (* Extract the constructors *)
        (* Collect parent categories graph and real variable categories *)
        (* Temporary array that stores the position and the category of the
         * definition of a variables if it exists *)
        let var_cat_defs = Array.make (Array.length var_tags) None in
        let map_arg vars (name, params, pos) =
            let rec lookup n ctx (p, ppos) =
                match ctx with
                | [] -> raise (UnboundParam (p, ppos))
                | (h, _) :: t when h = p -> n
                | _ :: t -> lookup (n + 1) t (p, ppos)
            in
            let param_infos = List.map (lookup 0 vars) params in
            (sym_cats.(get_sym_id name pos), param_infos)
        in
        let add_ctor pos cid (name, vars, params) =
            match Hashtbl.find_opt ctor_nametbl name with
            | Some _ -> raise (CtorAlreadyDefined (name, pos))
            | None ->
                    Hashtbl.add ctor_nametbl name (Hashtbl.length ctor_nametbl);
                    let cats = List.map (fun (n, p) -> var_tags.(get_var_id n p)) vars in
                    (name, cid, cats, List.map (map_arg vars) params)
        in
        let get_ctor cid ctors = function
            | Ast.CtorDef (ctor, pos) -> ctors @ [add_ctor pos cid ctor]
            | Ast.Symbol (name, pos) ->
                    begin try
                        let id = sym_cats.(get_sym_id name pos) in
                        parents.(id) <- cid :: parents.(id)
                    with UndefinedSymbol _ ->
                        let vid = get_var_id name pos in
                        if var_cat_defs.(vid) = None then var_cat_defs.(vid) <- Some (cid, pos)
                        else raise (VarAlreadyDefined (name, pos))
                    end; ctors
        in
        let get_defs i r = List.fold_left (get_ctor i) [] Ast.(r.defs) in
        let ctors_infos = Array.of_list List.(concat (mapi get_defs def_list)) in
        (cat_names, builtin_num, sym_cats, var_tags, var_cat_defs, ctors_infos)

    (* look for bool builtin to add constructors for true/false *)
    let bool_cat =
        let rec lookup_bool n =
            if n = builtin_num then -1
            else if cat_names.(n) = "bool" then n
            else lookup_bool (n + 1)
        in
        lookup_bool 0

    let get_bool_cat pos =
        if bool_cat >=0 then bool_cat
        else raise (UndefinedBool pos)

    let ctor_num = Array.length ctor_infos

    let get_ctor_id name pos =
        match Hashtbl.find_opt ctor_nametbl name with
        | Some id -> id
        | None -> raise (UndefinedCtor (name, pos))

    (* Subcategories *)
    (* Check that the categories hierarchy is a multitree *)
    module VMap = Map.Make(struct type t = int let compare = compare end)
    type vexted_tag = UNVISITED | VISITED | VISITING
    (* Array of Map of all parent categories associated with the distance *)
    let reachable = Array.make def_num VMap.empty
    let () =
        let visited = Array.make def_num UNVISITED in
        let rec check_tree id =
            (* Merge the maps and increase the distance by one *)
            let rec build_union id acc = function
                | hd :: tl ->
                        let f k v1 v2 = raise (DiamondDetected (cat_names.(k), cat_names.(id))) in
                        let increased = VMap.map ((+) 1) reachable.(hd) in
                        build_union id (VMap.union f increased acc) tl
                | [] -> acc
            in
            begin match visited.(id) with
            | UNVISITED ->
                    visited.(id) <- VISITING;
                    List.iter check_tree parents.(id);
                    (* Add the category to its own parent list and merge all the parent ones *)
                    reachable.(id) <- build_union id (VMap.singleton id 0) parents.(id);
                    visited.(id) <- VISITED
            | VISITED -> ()
            | VISITING -> raise (CycleDetected (cat_names.(id)))
            end
        in
        (* Visit all the categories *)
        for i = 0 to (def_num - 1) do check_tree i done

    let is_subcat cid1 cid2 =
        VMap.mem cid1 reachable.(cid2)

    (* Check and update variables definition categories *)
    let var_cats =
        let check_cat i var =
            let tag = var_tags.(i) in
            match var with
            (* If not defined, then it has the same category than its type *)
            | None -> tag
            (* Otherwise, it should be a subcategory of its tag *)
            | Some (cat, pos) ->
                if is_subcat tag cat then cat
                else
                    raise (WrongVarDef (cat_names.(tag), cat_names.(cat), pos))
        in
        Array.mapi check_cat var_cat_defs

    (* Building injection constructors and an array of indices to access them quickly *)
    (* The indices array containts the index of the first injection constructor
     * for each sub-category, the others follow it in order of the list in the parents array
     * (see the function get_sub_id in build_ctor_chain below) *)
    let sub_ctors, sub_ctors_ids =
        let rec gen_ctors i ids ctors =
            if i >= def_num then List.(rev ids, rev ctors)
            else
                let par = parents.(i) in
                (* Sum the number of constructors generated *)
                let id = match ids with [] -> 0 | p :: _ -> p in
                let new_ids = (id + (List.length par)) :: ids in
                (* Build the constructors from all the parents *)
                let new_ctors = List.rev_append (List.map (fun id -> (id, i)) par) ctors in
                gen_ctors (i + 1) new_ids new_ctors
        in
        (* The IDs array should start at 0 *)
        let ids, ctors = gen_ctors 0 [0] [] in
        Array.(of_list ctors, of_list ids)

    let build_ctor_chain expected_cid cid expr =
        (* Get id of an injection constructor *)
        let get_sub_id id1 id2 =
            (* Get the index of the parent category in the parent list *)
            let rec lookup i = function
                | hd :: tl when hd = id1 -> i
                | hd :: tl -> lookup (i + 1) tl
                | _ -> raise Not_found
            in
            ctor_num + sub_ctors_ids.(id2) + (lookup 0 parents.(id2))
            (* Build the injection constructor id *)
            (* It directly follows the regular constructors,
             * then, we add the index of the first injection constructor,
             * then we add the index of the parent in the list.
             * The index cannot be used directly here, but it represants what
             * would be the index of the injection constructor if we
             * concatenate the regular and the injection ones *)
        in
        (* Look from which parent the parent is reachable
         * and repeat to process until we reach it *)
        let rec path_to path vertices =
            let v = List.find (fun v -> VMap.mem expected_cid reachable.(v)) vertices in
            if v = expected_cid then v :: path
            else path_to (v :: path) parents.(v)
        in
        (* Build the chain of constructors from a path *)
        let rec build_ctor inner = function
            | id1 :: (id2 :: _ as tl) ->
                    Ctor (get_sub_id id1 id2, [build_ctor inner tl])
            | _ -> inner
        in
        let path = path_to [] [cid] in
        build_ctor expr path

    (* Find the lowest common ancestor in the graph *)
    let common_cat pos id1 id2 =
        (* Merge the two parents maps keeping only the distance of one of them *)
        let f _ x y = match x, y with Some v, Some _ -> Some v | _ -> None in
        let common = VMap.merge f reachable.(id1) reachable.(id2) in
        (* Extract the (one) closest from the list *)
        let rec extract_min acc l =
            match acc, l with
            | (min, others), hd :: tl ->
                    if snd hd < snd min then extract_min (hd, min :: others) tl
                    else extract_min (min, hd :: others) tl
            | _ -> acc
        in
        match VMap.bindings common with
        (* if there are some common parents *)
        | hd :: tl ->
                (* extract the closest one *)
                let (min, _), rest = extract_min (hd, []) tl in
                (* Ensure that all the others are parents of this one *)
                (* It means the ancestor is unique *)
                if List.for_all (fun (id, _) -> is_subcat id min) rest then min
                else raise (MultipleAncestors (cat_names.(id1), cat_names.(id2), pos))
        (* if no common parents *)
        | [] -> raise (InvalidCat (cat_names.(id2), cat_names.(id1), pos))

    (* Extract Judgments *)
    let judg_num = List.length G.game.judgs
    (* Array of category and list of parameter categories *)
    let judg_infos = Array.make judg_num ("", [])
    let judg_nametbl = Hashtbl.create judg_num
    let () =
        let add_judg id (name, args, pos) =
            (* Check the name is different from a constructor as well *)
            let r1 = Hashtbl.find_opt ctor_nametbl name in
            let r2 = Hashtbl.find_opt judg_nametbl name in
            match r1, r2 with
            | None, None ->
                    Hashtbl.add judg_nametbl name id;
                    let map_arg (name, pos) = sym_cats.(get_sym_id name pos) in
                    judg_infos.(id) <- (name, List.map map_arg args)
            | _, _ -> raise (CtorAlreadyDefined (name, pos))
        in
        List.iteri add_judg G.game.judgs

    let get_judg_id name pos =
        match Hashtbl.find_opt judg_nametbl name with
        | Some id -> id
        | None -> raise (UndefinedJudg (name, pos))

    (* Check the rules *)
    (* Done in two passes:
        * - The first one checks all the categories and collect the categories of
        *   variables and build the new AST
        * - The second one, once we know all the categories, adds the injection
        *   constructors where it is needed *)
    module RuleSet = Set.Make(struct type t = string let compare = compare end)
    let rule_infos =
        (* Keep track of rule names *)
        let rule_set = ref RuleSet.empty in
        (* Variable table: it keeps track of the category of a variable and the
         * category of all the parameters, it is cleaned for each rule *)
        let local_var_names = Hashtbl.create 8 in
        let check_rulename (rule, pos) =
            if RuleSet.mem rule !rule_set then
                raise (RuleAlreadyDefined (rule, pos))
            else
                rule_set := RuleSet.add rule !rule_set;
        in
        (* Find the most common category for a list of parameters *)
        let rec cat_union (name, i, p) pos current cats2 =
            match current, cats2 with
            | [], [] -> []
            | cid :: tl, (cid2, pos) :: tl2 ->
                    (common_cat pos cid cid2) :: cat_union (name, i, p) pos tl tl2
            | _ ->
                    let ar1 = List.length current in
                    let ar2 = List.length cats2 in
                    raise (WrongArityVar ((name, i, p), ar1, ar2, pos))
        in
        (* Update the informations of a variable, returns the id *)
        let add_var name pos (id, i, p) (cat, cats) =
            match Hashtbl.find_opt local_var_names (id, i, p) with
            | None ->
                    let count = Hashtbl.length local_var_names in
                    let cats = fst (List.split cats) in
                    Hashtbl.add local_var_names (id, i, p) (count, cat, cats);
                    count
            | Some (vid, ccat, current) ->
                    let new_cats = cat_union (name, i, p) pos current cats in
                    Hashtbl.replace local_var_names (id, i, p) (vid, ccat, new_cats);
                    vid
        in
        (* Check that all the metavars of a context are of the same categories
         * than the expected ones *)
        let rec check_ctx name pos (expected : int list) vars =
            match expected, vars with
            | [], [] -> ()
            | hd1 :: tl1, (hd2, pos) :: tl2 ->
                    let cat = var_tags.(get_var_id hd2 pos) in
                    if hd1 = cat then check_ctx name pos tl1 tl2
                    else raise (InvalidCat (cat_names.(cat), cat_names.(hd1) , pos))
            | _, _ ->
                    let var_len = List.length vars in
                    let ctx_len = List.length expected in
                    raise (WrongMetaArityCtor (name, ctx_len, var_len, pos));
        in
        (* Check an expression given a context, return the category and the new
         * node. bound_allowed is set to false when the expression cannot be a
         * bound variable *)
        let rec check_expr bound_allowed ctx expr pos =
            let check_ctor_params name metavars params args =
                let check_ctor_param (param, pos) (expected_cid, bound) =
                    (* Create the new context *)
                    let bound_meta = List.(map (nth metavars) bound) in
                    (* Get the category of the expression *)
                    let cid, expr = check_expr false (bound_meta @ ctx) param pos in
                    (* Compare it *)
                    if is_subcat expected_cid cid then expr
                    else raise (InvalidCat (cat_names.(expected_cid), cat_names.(cid), pos))
                in
                try List.map2 check_ctor_param params args
                with Invalid_argument _ ->
                    let ar, ar2 = List.(length params, length args) in
                    raise (WrongArityCtor (name, ar, ar2, pos))
            in
            let rec check_var_params cids exprs = function
                | [] -> (List.(rev cids, rev exprs))
                | (param, pos) :: tl ->
                    let cid, expr = check_expr true ctx param pos in
                    check_var_params ((cid, pos) :: cids) (expr :: exprs) tl
            in
            let rec lookup n ctx id =
                match ctx with
                | [] -> raise (UnboundParam (id, pos))
                | (h, _) :: t -> if h = id then n else lookup (n + 1) t id
            in
            match expr with
            | Ast.Bool b ->
                    let cat_id = get_bool_cat pos in
                    (cat_id, Bool (cat_id, b))
            | Ast.Ctor (name, vars, params) ->
                    let id = get_ctor_id name pos in
                    let _, cid, expected_ctx, args = ctor_infos.(id) in
                    (* Check arity and categories of metavariables *)
                    check_ctx name pos expected_ctx vars;
                    (* Check the parameters *)
                    (cid, Ctor (id, check_ctor_params name vars params args))
            | Ast.Var ((name, i, p), params) ->
                    try
                        let id = get_sym_id name pos in
                        let cids, exprs = check_var_params [] [] params in
                        let cid = sym_cats.(id) in
                        let var_id = add_var name pos (id, i, p) (cid, cids) in
                        (cid, Var (var_id, exprs))
                    with UndefinedSymbol _ ->
                        let vid = get_var_id name pos in
                        if bound_allowed && i = -1 && p = 0 && List.length params = 0 then
                            let tag, cat = (var_tags.(vid), var_cats.(vid)) in
                            (cat, Bound (tag, cat, lookup 0 ctx name))
                        else
                            raise (InvalidBoundParam (name, pos))
        in
        (* Check the judgements *)
        let check_judg_expr ctx ((name, params), pos) =
            (* Get the id and the defined categories of the parameters *)
            let id = get_judg_id name pos in
            let args = snd judg_infos.(id) in
            (* Get the actual categories of arguments and compare them *)
            (* If arity or category is different, stop *)
            let check expected_cid (param, pos) =
                let cid, expr = check_expr false ctx param pos in
                if is_subcat expected_cid cid then expr
                else raise (InvalidCat (cat_names.(cid), cat_names.(expected_cid), pos))
            in
            try (id, List.map2 check args params)
            with Invalid_argument _ ->
                let ar, ar2 = List.(length params, length args) in
                raise (WrongArityCtor (name, ar, ar2, pos))
        in
        (* For an abstracted judgement (bound variables, judgment) *)
        let check_topexpr (ctx, (name, params), pos) =
            let id, params = check_judg_expr ctx ((name, params), pos) in
            (List.length ctx, id, params)
        in
        (* Check a quoted expression: it should be of a built-in category *)
        let rec check_qexpr = function
            | Ast.QVar ((name, i, p), pos) :: tl ->
                    let id = get_sym_id name pos in
                    let cid = sym_cats.(id) in
                    let vid =
                        begin match Hashtbl.find_opt local_var_names (id, i, p) with
                        | Some (vid, _, _) -> vid
                        | None -> raise (UndeclaredQVar ((name, i, p), pos))
                        end
                    in
                    if is_builtin cid then
                        QVar_btin (cid, vid) :: check_qexpr tl
                    else
                        (QVar vid) :: check_qexpr tl
                    (*raise (InvalidQVar ((name, i, p), cat_names.(cid), pos));*)
            | Ast.QStr hd :: tl -> QStr hd :: check_qexpr tl
            | [] -> []
        in
        (* Check first the judgements to collect variables and then quoted
         * expressions *)
        let rec check_premises judgs qexprs = function
            | Ast.Judgment topexpr :: tl ->
                    let judg = Judg (check_topexpr topexpr) in
                    check_premises (judg :: judgs) qexprs tl
            | Ast.QExp qexpr :: tl ->
                    (* Don't check it yet *)
                    check_premises judgs (qexpr :: qexprs) tl
            | [] ->
                    (* Check the qexprs now and append them to the judgements *)
                    let build_qexpr e = QExp (check_qexpr e) in
                    List.(rev_append judgs (rev_map build_qexpr qexprs))
        in
        (* Injection of injection constructors *)
        let rec inject_expr infered_cats expected_cat expr =
            match expr with
            | Ctor (id, exprs) ->
                    let _, cat, _, expected_params = ctor_infos.(id) in
                    let expect_cats = fst (List.split expected_params) in
                    let params = List.map2 (inject_expr infered_cats) expect_cats exprs in
                    build_ctor_chain expected_cat cat (Ctor (id, params))
            | Var (vid, exprs) ->
                    let cat, expected_cats = infered_cats.(vid) in
                    let params = List.map2 (inject_expr infered_cats) expected_cats exprs in
                    build_ctor_chain expected_cat cat (Var (vid, params))
            | Bound (_, cat, _)
            | Bool (cat, _) -> build_ctor_chain expected_cat cat expr
        in
        let inject_topexpr var_names (arity, id, exprs) =
            let expect_cats = snd judg_infos.(id) in
            (arity, id, List.map2 (inject_expr var_names) expect_cats exprs)
        in
        let inject_premise infered_cats prem =
            match prem with
            | QExp _ -> prem
            | Judg j -> Judg (inject_topexpr infered_cats j)
        in
        (* For each rule *)
        let check_rule (rule : Ast.rule) =
            (* Reset the variables table *)
            Hashtbl.clear local_var_names;
            (* Check that name is not already defined *)
            let (name, pos) = Ast.(rule.name) in
            check_rulename (name, pos);
            (* Check and build the conclusion *)
            let concl = check_topexpr Ast.(rule.concl) in
            (* Check and build the premises *)
            let premises = check_premises [] [] Ast.(rule.premises) in
            (* Build the array of final variable categories *)
            let infered_cats = Array.make (Hashtbl.length local_var_names) (0,[]) in
            Hashtbl.iter (fun _ (id, cat, cats) -> infered_cats.(id) <- (cat, cats)) local_var_names;
            (* Inject the injection contructors *)
            let concl = inject_topexpr infered_cats concl in
            let premises = List.map (inject_premise infered_cats) premises in
            (premises, concl, name)
        in
        List.map check_rule G.game.rules
end


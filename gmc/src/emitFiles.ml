open EmitSection

exception InvalidDirectory
exception InvalidFile of string
exception TemplateNotFound of string

module EmitFiles (G : Game.GAME) = struct
    let builtin_subs = Hashtbl.create 9
    (* Substitutions for names used in the generated code, to quickly fix in
    * case of collision, these names can be used in the emitted code, they will
    * be substuted after *)
    let _ = List.iter (fun (kwd, sub) -> Hashtbl.add builtin_subs kwd sub)
                [ "<metavar>", "Metavar_";
                  "<var>",  "Var_";
                  "<var_fname>",  "var_";
                  "<meta_fname>", "meta_";
                  "<proj>", "Proj";
                  "<closed>", "Closed_";
                  "<cvar>", "CVar_";
                  "<premises>", "Premises_";
                  "<empty>", "Empty_";
                ]

    (* Substute all the names defined above in a string *)
    let replace_builtin_names str =
        let sub k v str =
            let r = Str.regexp_string k in
            Str.global_replace r v str
        in
        Hashtbl.fold sub builtin_subs str

    (* Functions to build names from user defined ones *)
    let builtin_from_catname str =
        (String.capitalize_ascii str)

    let term_from_ctor str = "" ^ str
    let ctx_from_ctor str = "C" ^ str
    let fn_from_ctor str = "_" ^ str

    let enum_from_name str =
        let uc = String.uppercase_ascii str in
        String.map (fun c -> if c = '-' then '_' else c) uc

    (* Build once the constructors emit informations (all the names) *)
    let build_ctor (name, _, ctx, pars) =
        { term_name = term_from_ctor name;
          ctx_name = ctx_from_ctor name;
          arity = List.length pars;
          var_tags = List.(map (fun (_, l) -> map (nth ctx) l) pars);
          var_ar = List.(map (fun (_, l) -> length l) pars);
          create_fn = fn_from_ctor name; }

    (* Same for the injection constructors *)
    let build_sub_ctor (id1, id2) =
        let n1 = G.cat_names.(id1) in
        let n2 = G.cat_names.(id2) in
        let name = n1 ^ "_of_" ^ n2 in
        { term_name = term_from_ctor name;
          ctx_name = ctx_from_ctor name;
          create_fn = fn_from_ctor name;
          arity = 1; var_ar = [0]; var_tags = [[]]; }

    (* Merge the constructors and the injection ones *)
    (* Since the IDs used in the new AST for the injection constructors start
     * directly after the regular ones, they can be accessed indifferently *)
    let ctor_emit_infos =
        let ctor_emit_infos = Array.map build_ctor G.ctor_infos in
        let subctor_emit_infos = Array.map build_sub_ctor G.sub_ctors in
        Array.append ctor_emit_infos subctor_emit_infos

    (* Build the built-in types emit informations *)
    let builtin_emit_infos =
        let build_builtin i =
            let str = builtin_from_catname G.cat_names.(i) in
            {  bltin_term = builtin_from_catname str;
               bltin_ctx = ctx_from_ctor str;
               bltin_type = G.cat_names.(i);
               bltin_create_fn = fn_from_ctor str; }
        in
        Array.init G.builtin_num build_builtin

    let rule_emit_infos = List.map (fun (p, c, n) -> (p, c, n, enum_from_name n)) G.rule_infos
    let judg_emit_infos =
        let build_judg (name, params) =
            (term_from_ctor name, List.length params)
        in
        Array.map build_judg G.judg_infos

    let cat_emit_infos = Array.map enum_from_name G.cat_names

    (* Look for a section, generate a new line by calling gen on each element
     * of the array restoring the previous indentation of the section before *)
    let emit_array_pattern arr section gen str =
        let r = Str.regexp ("\\([ \t\r]*\\)" ^ section) in
        let indent = Str.global_replace (Str.regexp_string "\n") "\n\\\\1" in
        let l = Array.fold_left (fun acc c -> acc @ [indent (gen c)]) [] arr in
        let subs = "\\1" ^ (String.concat "\n\\1" l) in
        Str.replace_first r subs str

    (* specialized version of the previous function *)
    let emit_ctor_pattern = emit_array_pattern ctor_emit_infos
    let emit_judg_pattern = emit_array_pattern judg_emit_infos
    let emit_cat_pattern = emit_array_pattern cat_emit_infos
    let emit_builtin_pattern = emit_array_pattern builtin_emit_infos

    (* Same, but for the list of rules *)
    let emit_rule_pattern section gen str =
        let r = Str.regexp ("\\([ \t\r]*\\)" ^ section) in
        let indent = Str.global_replace (Str.regexp_string "\n") "\n\\\\1" in
        let l = List.map (fun x -> indent (gen x)) rule_emit_infos in
        let subs = "\\1" ^ (String.concat "\n\\1" l) in
        Str.replace_first r subs str

    let open_file folder filename =
        let filename = Filename.concat folder filename in
        try
            open_out filename
        with _ -> raise (InvalidFile filename)

    let load_template filename =
        try
            let ic = open_in filename in
            let n = in_channel_length ic in
            let s = Bytes.create n in
            really_input ic s 0 n;
            close_in ic;
            Bytes.to_string s
        with _ -> raise (TemplateNotFound filename)

    (* Replace all the sections and then the names *)
    let emit_ast ast =
        emit_rule_pattern "<rule_enum>" rule_enum ast |>
        emit_rule_pattern "<rule_map_list>" rule_map_list |>
        emit_ctor_pattern "<term_def>" term_def |>
        emit_cat_pattern "<term_tags>" (fun t -> "| " ^ t) |>
        emit_ctor_pattern "<ctx_def>" ctx_def |>
        emit_judg_pattern "<judg_def>" judg_def |>
        emit_ctor_pattern "<ctor_functions>" ctor_function |>
        emit_builtin_pattern "<builtin_term_def>" builtin_term_def |>
        emit_builtin_pattern "<builtin_ctx_def>" builtin_ctx_def |>
        emit_builtin_pattern "<builtin_functions>" builtin_functions |>
        emit_judg_pattern "<shift_judg_ast>" shift_judg_ast |>
        emit_ctor_pattern "<shift_ctor_ast>" shift_ctor_ast |>
        emit_builtin_pattern "<shift_builtin_ctor_ast>" shift_builtin_ctor_ast |>
        replace_builtin_names

    let emit_unify unify =
        emit_ctor_pattern "<is_closed_fun>" is_closed_fun unify |>
        emit_ctor_pattern "<shift_fun>" shift_fun |>
        emit_ctor_pattern "<head_normalize_fun>" (head_normalize_fun cat_emit_infos) |>
        emit_ctor_pattern "<ctx_fun>" ctx_fun |>
        emit_ctor_pattern "<unify_one_fun>" unify_one_fun |>
        emit_builtin_pattern "<builtin_closed>" builtin_closed |>
        emit_builtin_pattern "<builtin_shift>" builtin_shift |>
        emit_builtin_pattern "<head_normalize_builtin_fun>" head_normalize_builtin_fun |>
        emit_builtin_pattern "<builtin_ctx_fun>" builtin_ctx_fun |>
        emit_builtin_pattern "<builtin_unify_one_fun>" builtin_unify_one_fun |>
        replace_builtin_names

    let emit_checker checker =
        let f = match_rule ctor_emit_infos judg_emit_infos cat_emit_infos builtin_emit_infos in
        emit_rule_pattern "<match_rule>" f checker |>
        replace_builtin_names

    let emit_files folder =
        if not (Sys.file_exists folder) || not (Sys.is_directory folder) then
            raise InvalidDirectory;

        (* Load the templates *)
        let ast_template = load_template "templates/ast.template" in
        let unify_template = load_template "templates/unify.template" in
        let checker_template = load_template "templates/checker.template" in

        (* Open files for writing *)
        let ast_f = open_file folder "ast.ml" in
        let checker_f = open_file folder "checker.ml" in
        let unify_f = open_file folder "unify.ml" in

        (* Writing in files the generated code from templates *)
        output_string ast_f (emit_ast ast_template);
        output_string unify_f (emit_unify unify_template);
        output_string checker_f (emit_checker checker_template);
        close_out ast_f;
        close_out unify_f;
        close_out checker_f
end

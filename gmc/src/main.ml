open Lexing
open Parsing

exception ArgumentMissing
exception FileNotFound of string

let open_file f =
    try open_in f
    with _ -> raise (FileNotFound f)

let main () =
    let input, folder =
        if Array.length Sys.argv <> 3 then raise ArgumentMissing
        else Sys.argv.(1), Sys.argv.(2)
    in
    let file = open_file input in
    let lexbuf = Lexing.from_channel file in
    try
        (* Parse the file *)
        let game = Parser.entrypoint Lexer.token lexbuf in
        (* Check and build the structure for generation *)
        let module Game = Game.MakeGame(struct let game = game end) in
        (* Emit the OCaml files in the given directory *)
        let module Emitter = EmitFiles.EmitFiles(Game) in
        Emitter.emit_files folder
    with
    | Parser.Error ->
            let str = lexeme lexbuf in
            let pos = Lexer.get_info lexbuf in
            Printf.printf "Syntax error at %s: \"%s\"\n" (Ast.print_info pos) str;
            exit 1
    | Lexer.LexError (msg, str, pos) ->
            Printf.printf "Lexer error at %s \"%s\":\n%s\n" (Ast.print_info pos) str msg;
            exit 1

let () =
    let open Printf in
    let print_err pos str =
        printf "Error at %s:\n%s\n" (Ast.print_info pos) str;
        exit 1
    in
    let var_name (name, i, p) =
        if i >= 0 then sprintf "%s%i%s" name i (String.make p '\'')
        else name ^ (String.make p '\'')
    in
    try main ()
    with
    | ArgumentMissing ->
            print_string "A command line argument is missing\n"; exit 1
    | FileNotFound f ->
            printf "Unable to find the game file: %s\n" f; exit 1
    | Game.CtorAlreadyDefined (str, pos) ->
            print_err pos (sprintf "Constructor %s is already defined" str)
    | Game.UndefinedCtor (str, pos) ->
            print_err pos (sprintf "Constructor %s is not defined" str)
    | Game.UndefinedJudg (str, pos) ->
            print_err pos (sprintf "Judgment %s is not defined" str)
    | Game.UnboundParam (name, pos) ->
            print_err pos (name ^ " is unbounded")
    | Game.InvalidBoundParam (name, pos) ->
            print_err pos (name ^ " is a category variable, it cannot be used as a variable")
    | Game.SymbolAlreadyDefined (name, pos) ->
            print_err pos (sprintf "Symbol %s is already defined" name)
    | Game.UndefinedSymbol (name, pos) ->
            print_err pos (sprintf "Symbol %s is not defined\n" name)
    | Game.WrongArityVar (var, def, ar, pos) ->
            print_err pos (sprintf "Arity mismatch, variable %s expects %i argument(s), %i given" (var_name var) ar def)
    | Game.WrongArityCtor (name, def, ar, pos) ->
            print_err pos (sprintf "Arity mismatch, constructor %s expects %i argument(s), %i given" name ar def)
    | Game.WrongMetaArityCtor (name, def, ar, pos) ->
            print_err pos (sprintf "Arity mismatch (meta), constructor %s expects %i meta-argument(s), %i given" name ar def)
    | Game.InvalidCat (t1, t2, pos) ->
            print_err pos (sprintf "Category %s is expected, %s is not a sub-category of %s" t2 t1 t2)
    | Game.RuleAlreadyDefined (name, pos) ->
            print_err pos (sprintf "Rule %s is already defined" name)
    | Game.CatAlreadyDefined (name, pos) ->
            print_err pos (sprintf "Category %s is already defined" name)
    | Game.VarAlreadyDefined (name, pos) ->
            print_err pos (sprintf "Variable %s is already defined" name)
    | Game.WrongVarDef (tag, cat, pos) ->
            print_err pos (sprintf "This variable must be of a %s sub-category, %s is not" tag cat)
    | Game.CycleDetected cat ->
            printf "Category %s: categories cannot be recursive\n" cat; exit 1
    | Game.DiamondDetected (cat1, cat2) ->
            printf "There is more than one way to build a %s from a %s\n" cat2 cat1;
            exit 1
    | Game.MultipleAncestors (cat1, cat2, pos) ->
            print_err pos (
            "The category of this parameter cannot be infered.\n" ^
            sprintf "This parameter has been defined of category %s and " cat1 ^
            sprintf "there is not only one lowest common parent category between %s and %s" cat1 cat2)
    | Game.InvalidBuiltin (name, pos) ->
            print_err pos (sprintf "Invalid built-in category: %s" name)
    | Game.InvalidQVar (var, pos) ->
            print_err pos (sprintf "$%s must be a first-order variable to be quoted" (var_name var))
    | Game.UndeclaredQVar (var, pos) ->
            print_err pos (sprintf "$%s is not declared" (var_name var))
    | Game.UndefinedBool pos ->
            print_err pos "Built-in bool category should be used to allow usage of true/false constructors"
    | EmitFiles.InvalidDirectory ->
            print_string "Path is not a valid directory\n"; exit 1
    | EmitFiles.InvalidFile f ->
            printf "Unable to open the file %s to write\n" f; exit 1
    | EmitFiles.TemplateNotFound f ->
            printf "Unable to open the template file: %s\n" f; exit 1

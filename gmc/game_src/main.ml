open Checker
open Parsing

let _ =
    let lexbuf = Lexing.from_channel stdin in
    try
        let result = Parser.entrypoint Lexer.token lexbuf in
        check_ast result 0
    with
    | Parser.Error ->
            let str = Lexing.lexeme lexbuf in
            let pos = Lexer.get_info lexbuf in
            Printf.printf "Syntax error at %s: \"%s\"\n" (Ast.print_info pos) str
    | Lexer.LexError (msg, str, pos) ->
            Printf.printf "Lexer error at %s \"%s\": %s\n" (Ast.print_info pos) str msg
    | Ast.ParseError (msg, pos) ->
            Printf.printf "Syntax error at %s: \"%s\"\n" (Ast.print_info pos) msg

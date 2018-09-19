{
    open Parser        (* The type token is defined in parser.mli *)
    open Lexing
    open Parsing
    exception LexError of string * string * (position * position)

    let get_info lexbuf = (lexeme_start_p lexbuf, lexeme_end_p lexbuf)
    let lex_error msg lexbuf =
        let escaped = String.escaped msg in
        raise (LexError (escaped, lexeme lexbuf, get_info lexbuf))

    let newline lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
            { pos with pos_lnum = pos.pos_lnum + 1;
                pos_bol = pos.pos_cnum }

    let build_var name i p =
        if i = "" then (name, -1, String.length p)
        else (name, int_of_string i, String.length p)

    let buffer = Buffer.create 8
}

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let alpha = lowercase | uppercase
let digit = ['0'-'9']
let alphanum = alpha | digit
rule token = parse
    | '\n'                        { newline lexbuf; token lexbuf}
    | [' ' '\t']                  { token lexbuf }     (* skip blanks *)
    | "(*"                        { comment 1 lexbuf }
    | '`'                         { Buffer.clear buffer; qexpr [] lexbuf }
    | "in"                        { IN }
    | "::="                       { DEF }
    | "[Syntax]"                  { SHEADER }
    | "[Judgments]"               { JHEADER }
    | "[Rules]"                   { RHEADER }
    | "[ML]" (_+ as code)         { MLCODE code }
    | "with"                      { WITH }
    | "var"                       { VAR }
    | "true"                      { TRUE }
    | "false"                     { FALSE }
    | '-'+ (uppercase (alphanum | '-')* as id)
                                  { RULENAME id }
    | uppercase alpha* as id      { UCID id }
    | lowercase+ as name (digit+ as i) ('\''* as p)
                                  { VARNAME (build_var name i p) }
    | lowercase+ as name ('\''+ as p)
                                  { VARNAME (build_var name "" p) }
    | lowercase+ as id            { LCID id }
    | ';'                         { SEMI }
    | ','                         { COMMA }
    | '|'                         { OR }
    | '('                         { LPAREN }
    | ')'                         { RPAREN }
    | '<'                         { LANGLE }
    | '>'                         { RANGLE }
    | '['                         { LBRACKET }
    | ']'                         { RBRACKET }
    | eof                         { EOF }
    | _                           { lex_error "unexpected char" lexbuf }

(* Comments *)
and comment n = parse
    "(*" { comment (n+1) lexbuf }
    | "*)" { if n = 1 then token lexbuf else comment (n-1) lexbuf }
    | '\n' { newline lexbuf; comment n lexbuf }
    | eof  { lex_error "non terminated comment" lexbuf }
    | _    { comment n lexbuf }

(* Quoted expressions *)
and qexpr acc = parse
    '`' {
            (* Create the last string of code if the buffer is not empty *)
            if Buffer.length buffer = 0 then QEXP acc
            else QEXP (acc @ [QStr (Buffer.contents buffer)])
    }
    | '$' (lowercase+ as name) (digit* as i) ('\''* as p) {
            (* Create a string a code if the buffer is not empty,
            * Create the variable, reset the buffer *)
            let var = build_var name i p in
            let v = Ast.QVar (var, get_info lexbuf) in
            if Buffer.length buffer = 0 then qexpr (acc @ [v]) lexbuf
            else
                let str = Buffer.contents buffer in
                Buffer.clear buffer;
                qexpr (acc @ [QStr str; v]) lexbuf
    }
    | ("\\$" | "\\`") as s { Buffer.add_string buffer s; qexpr acc lexbuf }
    | '\n'                 { newline lexbuf; Buffer.add_char buffer '\n'; qexpr acc lexbuf }
    | eof                  { lex_error "non terminated quoted expression" lexbuf }
    | _ as c               { Buffer.add_char buffer c; qexpr acc lexbuf }

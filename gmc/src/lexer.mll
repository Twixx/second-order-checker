{
        open Parser        (* The type token is defined in parser.mli *)
        open Lexing
        open Parsing
        exception LexError of string
        exception ParseError of string

        let error msg start finish  =
            Printf.sprintf "(line %d: char %d..%d): %s" start.pos_lnum
            (start.pos_cnum - start.pos_bol) (finish.pos_cnum - finish.pos_bol)
            msg

        let lex_error msg lexbuf =
            let s = error (msg ^ (lexeme lexbuf)) (lexeme_start_p lexbuf)
                    (lexeme_end_p lexbuf)
            in
            raise (LexError s)

        let newline lexbuf =
            let pos = lexbuf.lex_curr_p in
            lexbuf.lex_curr_p <-
                { pos with pos_lnum = pos.pos_lnum + 1;
                   pos_bol = pos.pos_cnum }

        let build_var name i = (name, if i = "" then (-1) else int_of_string i)
        let get_info lexbuf = (lexeme_start_p lexbuf, lexeme_end_p lexbuf)

        let comment_depth = ref 0
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
          | "(*"                        { comment_depth := 1; comment lexbuf }
          | '`'                         { Buffer.clear buffer; qexpr [] lexbuf }
          | "in"                        { IN }
          | "::="                       { DEF }
          | "[Syntax]"                  { SHEADER }
          | "[Judgments]"               { JHEADER }
          | "[Rules]"                   { RHEADER }
          | "with"                      { WITH }
          | "var"                       { VAR }
          | '-'+ (uppercase (alphanum | '-')* as id)
                                        { RULENAME id }
          | uppercase alpha* as id      { UCID id }
          | lowercase+ as name (digit+ as i)
                                        { VARNAME (build_var name i) }
          | lowercase+ as id            { LCID id }
          | ';'                         { SEMI }
          | ','                         { COMA }
          | '|'                         { OR }
          | '('                         { LPAREN }
          | ')'                         { RPAREN }
          | '<'                         { LANGLE }
          | '>'                         { RANGLE }
          | '['                         { LBRACKET }
          | ']'                         { RBRACKET }
          | eof                         { EOF }
          | _                           { lex_error "Unexpected char: " lexbuf }

(* Comments *)
and comment = parse
"*)"                { decr comment_depth;
                        if !comment_depth = 0 then token lexbuf
                        else comment lexbuf }
| "(*"              { incr comment_depth; comment lexbuf}
| '\n'              { newline lexbuf; comment lexbuf}
| eof               { lex_error "non terminated comment" lexbuf }
| _                 { comment lexbuf }

(* Quoted expressions *)
and qexpr acc = parse
'`' {
        (* Create the last string of code if the buffer is not empty *)
        if Buffer.length buffer = 0 then QEXP acc
        else QEXP (acc @ [QStr (Buffer.contents buffer)])
}
| '$' (lowercase+ as name) (digit* as i) {
        (* Create a string a code if the buffer is not empty,
         * Create the variable, reset the buffer *)
        let name, i = build_var name i in
        let v = Ast.QVar ((name, i), get_info lexbuf) in
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

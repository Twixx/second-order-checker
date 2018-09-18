{
    open Parser (* Common tokens *)
    open Lexing

    exception LexError of string * string * (position * position)

    let get_info lexbuf = (lexeme_start_p lexbuf, lexeme_end_p lexbuf)
    let lex_error msg lexbuf =
        let escaped = String.escaped msg in
        raise (LexError (escaped, lexeme lexbuf, get_info lexbuf))

    let tbl =
        let tbl = Hashtbl.create 1024 in
        List.iter (fun (word, token) -> Hashtbl.add tbl word token) Lexemes.v;
        tbl

    let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
        { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

rule token = parse
    (* ignore spacing and newline characters *)
    [' ' '\009' '\012' '\r']+     { token lexbuf }
    (* ignore spacing and newline characters *)
    | [' ' '\009' '\012' '\r']* '\n'    { newline lexbuf; token lexbuf }
    (* ignore // and the following characters until the end of the line *)
    | "//" [^ '\n']* '\n' { newline lexbuf; token lexbuf }
    (* special symbols *)
    | "[" { LSBRA }
    | "]" { RSBRA }
    | "{" { LBRA }
    | "}" { RBRA }
    | ";" { SEMI }
    | "," { COMA }
    | "<" { LANGLE }
    | ">" { RANGLE }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "by" { BY }
    | "(*" { comment 1 lexbuf }
    (* lowercase names *)
    | ['a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']* as name
        { try Hashtbl.find tbl name with _ -> LCID name }
    (* Uppercase names added for EvalML6 *)
    | ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']* as name
        { try Hashtbl.find tbl name with _ -> UCID name }
    (* other alphabetical names, including rule names, which can contain hyphen *)
    | ['A'-'Z' 'a'-'z' '_']+ ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    | ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'' '-']* as name
        { try Hashtbl.find tbl name with _ -> ID name }
    (* Possible and allowed symbols *)
    | "-d->"
    | ['!' '"' '#' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '+'
    '@' '^' '`' '~' '|']+ | '\'' | ',' | '\\' | '(' | ')' as sym
        { try Hashtbl.find tbl sym with _ -> ID sym }
    | ['0' - '9']+ as intl { INTL (int_of_string intl) }
    | eof { EOF }
    | _ { lex_error "unexpected character" lexbuf }

and comment n = parse
    "(*" { comment (n+1) lexbuf }
    | "*)" { if n = 1 then token lexbuf else comment (n-1) lexbuf }
    | '\n' { newline lexbuf; comment n lexbuf }
    | eof  { lex_error "non terminated comment" lexbuf }
    | _    { comment n lexbuf }

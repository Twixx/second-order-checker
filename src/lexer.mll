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

        let comment_depth = ref 0
}

rule token = parse
          | '\n'                        { newline lexbuf; token lexbuf}
          | [' ' '\t']                  { token lexbuf }     (* skip blanks *)
          | "(*"                        { comment_depth := 1; comment lexbuf }
          | "in"                        { IN }
          | "::="                       { DEF }
          | "[Syntax]"                  { SHEADER }
          | "[Judgments]"               { JHEADER }
          | "[Rules]"                   { RHEADER }
          | ['-']+ (['A'-'Z'] ['a'-'z' 'A'-'Z' '-' '1'-'9']* as id)
                                        { RULENAME id }
          | ['A'-'Z'] ['a'-'z' 'A'-'Z']* as id
                                        { UCID id }
          | ['a'-'z'] ['a'-'z']+ as id  { LCID id }
          | (['a'-'z'] as c) (['1'-'9']+ as i)
                                        { VAR (c, int_of_string i) }
          | ['a'-'z'] as c              { CHAR c }
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

and comment = parse
"*)"                { decr comment_depth;
                        if !comment_depth = 0 then token lexbuf
                        else comment lexbuf }
| "(*"              { incr comment_depth; comment lexbuf}
| '\n'              { newline lexbuf; comment lexbuf}
| eof               { lex_error "non terminated comment" lexbuf }
| _                 { comment lexbuf }

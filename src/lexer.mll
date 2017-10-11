{
    open Parser
    module L = Lexing
    exception SyntaxError of string

    let incr_linenum lexbuf =
        let pos = lexbuf.L.lex_curr_p in
        lexbuf.L.lex_curr_p <- {
            pos with
            L.pos_lnum = pos.L.pos_lnum + 1;
            L.pos_bol = pos.L.pos_cnum;
        }
}

let boolean = "#t" | "#f"
let integer = ['+' '-']? ['0'-'9']+
let identifier = ['a'-'z' 'A'-'Z' '!' '?' '-' '+'] ['0'-'9' 'a'-'z' 'A'-'Z' '!' '?' '-' '+']*

rule token = parse
    | [' ' '\t']+ { token lexbuf }
    | '\n' { incr_linenum lexbuf; token lexbuf }

    | ''' { QUOTE_MARK }
    | "quote" { QUOTE }
    | "lambda" { LAMBDA }
    | "if" { IF }
    | "set!" { SET }
    | "call/cc" { CALLCC }

    | '(' { LEFT_PAREN }
    | ')' { RIGHT_PAREN }

    | boolean { BOOLEAN (L.lexeme lexbuf = "#t") }
    | integer { INTEGER (int_of_string (L.lexeme lexbuf)) }
    | identifier { IDENTIFIER (L.lexeme lexbuf) }

    | ';' { comment lexbuf }
    | eof { EOF }

    | _ as c { raise (SyntaxError ("Unexpected char: " ^ (Char.escaped c))) }

and comment = parse
    | eof { EOF }
    | '\n' { incr_linenum lexbuf; token lexbuf }
    | _ { comment lexbuf }

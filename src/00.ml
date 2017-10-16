type stream = {
    mutable line_num: int;
    mutable chr: char list;
    chan: in_channel;
}

let read_char (stm:stream) : char =
    match stm.chr with
        | [] ->
            let c = input_char stm.chan in
            (if c = '\n' then stm.line_num <- stm.line_num + 1);
            c
        | c::rest ->
            stm.chr <- rest;
            c

let unread_char (stm:stream) (c:char) : unit =
    stm.chr <- (c::stm.chr)

let rec is_white (c:char) : bool =
    match c with
        | ' ' | '\t' | '\n' -> true
        | _ -> false
and is_digit (c:char) : bool =
    match c with
        | '0'..'9' -> true
        | _ -> false
and is_symstartchar (c:char) : bool =
    match c with
        | 'a'..'z' | 'A'..'Z' | '*'|'/'|'>'|'<'|'='|'?'|'!'|'-'|'+' -> true
        | _ -> false
and is_delimiter (c:char) : bool =
    match c with
        | '(' | ')' | '{' | '}' | ';' | '"' -> true
        | c -> is_white c

let rec eat_whitespace (stm:stream) : unit =
    let c = read_char stm in
    if is_white c
    then eat_whitespace stm
    else unread_char stm c

type lobject =
    | Fixnum of int
    | Boolean of bool
    | Symbol of string
    | Nil
    | Pair of lobject * lobject

exception SyntaxError of string
let unexpected (c:char) : exn =
    SyntaxError ("Unexpected char " ^ (Char.escaped c))

let rec read_fixnum (stm:stream) (acc:string) : lobject =
    let nc = read_char stm in
    if is_digit nc
    then read_fixnum stm (acc ^ (Char.escaped nc))
    else (
        unread_char stm nc;
        Fixnum (int_of_string acc)
    )
and read_symbol (stm:stream) (acc:string) : lobject =
    let nc = read_char stm in
    if is_delimiter nc
    then (unread_char stm nc; Symbol acc)
    else read_symbol stm (acc ^ (Char.escaped nc))
and read_boolean (stm:stream) : lobject =
    match read_char stm with
        | 't' -> Boolean true
        | 'f' -> Boolean false
        | c -> raise (unexpected c)
and read_sexp (stm:stream) : lobject =
    eat_whitespace stm;
    match read_char stm with
        | c when is_symstartchar c -> read_symbol stm (Char.escaped c)
        | c when is_digit c -> read_fixnum stm (Char.escaped  c)
        | '~' -> read_fixnum stm (Char.escaped '~')
        | '#' -> read_boolean stm
        | c -> raise (unexpected c)

let print_sexp (e:lobject) : unit =
    let rec to_string (e:lobject) : string =
        match e with
            | Fixnum n -> string_of_int n
            | Boolean b -> if b then "#t" else "#f"
            | Symbol s -> s
            | Nil -> "nil"
            | Pair p when is_list p -> list_to_string e
            | Pair p when is_simple_pair p -> pair_to_string e
    in
    print_endline (to_string e)

let rec repl (stm:stream) : unit =
    print_string "> ";
    flush stdout;
    print_sexp (read_sexp stm);
    print_newline ();
    repl stm

let () =
    let stm = { chr=[]; line_num=1; chan=stdin } in
    repl stm

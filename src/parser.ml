open Ast

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
        | 'a'..'z' | 'A'..'Z'
        | '*' | '/' | '>' | '<' | '=' | '?' | '!' | '-' | '+' -> true
        | _ -> false
and is_delimiter (c:char) : bool =
    match c with
        | '(' | ')' | '{' | '}' | ';' | '"' -> true
        | c -> is_white c


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
        | c -> failwith ("unexpected char " ^ (Char.escaped c))
and read_pair (stm:stream) : lobject =
    eat_whitespace stm;
    match read_char stm with
        | ')' -> Nil
        | c ->
            unread_char stm c;
            let car = read_sexp stm in
            let cdr = read_pair stm in
            Pair (car, cdr)
and read_sexp (stm:stream) : lobject =
    eat_whitespace stm;
    match read_char stm with
        | c when is_symstartchar c -> read_symbol stm (Char.escaped c)
        | c when is_digit c -> read_fixnum stm (Char.escaped  c)
        | '~' -> read_fixnum stm (Char.escaped '~')
        | '#' -> read_boolean stm
        | '(' -> read_pair stm
        | c -> failwith ("unexpected char " ^ (Char.escaped c))
and eat_whitespace (stm:stream) : unit =
    let c = read_char stm in
    if is_white c
    then eat_whitespace stm
    else unread_char stm c


let rec build_ast (sexp:lobject) : exp =
    match sexp with
        | Primitive _ -> failwith "this can't happen"
        | Fixnum _ | Boolean _ | Nil -> Literal sexp
        | Symbol s -> Var s
        | Pair _ when is_list sexp -> (
                match pair_to_list sexp with
                    | [Symbol "if"; cond; t; f] ->
                        If (build_ast cond, build_ast t, build_ast f)
                    | [Symbol "and"; c1; c2] -> And (build_ast c1, build_ast c2)
                    | [Symbol "or"; c1; c2] -> Or (build_ast c1, build_ast c2)
                    | [Symbol "set!"; Symbol n; e] -> Defexp (Val (n, build_ast e))
                    | [Symbol "apply"; fnexp; args] when is_list args ->
                        Apply (build_ast fnexp, build_ast args)
                    | fnexp::args -> Call (build_ast fnexp, List.map build_ast args)
                    | [] -> failwith "parser error: poorly formed expression"
            )
        | Pair _ -> Literal sexp

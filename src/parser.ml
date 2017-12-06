open Batteries
open Ast

let explode = String.explode

let is_digit = Char.is_digit

let is_whitespace = Char.is_whitespace

let is_letter = Char.is_letter

let rec is_num = function
    | '.' -> true
    | h when is_digit h -> true
    | _ -> false


and is_initial = function
    | '.'
    |'+'
    |'-'
    |'@'
    |'!'
    |'$'
    |'%'
    |'&'
    |'*'
    |'/'
    |':'
    |'<'
    |'='
    |'>'
    |'?'
    |'~'
    |'_'
    |'^' ->
        true
    | c when is_letter c -> true
    | _ -> false


and is_subsequent = function
    | c when is_initial c -> true
    | c when is_digit c -> true
    | _ -> false


let rec trim_space = function
    | h :: t when is_whitespace h -> trim_space t
    | x -> x


type parsing = (datum * char list, string) result

let bind_parsing f r = match r with Ok (d, cs) -> f (d, cs) | Bad _ -> r

let checker label =
    bind_parsing (fun (d, cs) ->
        match cs with
            | [] | ')' :: _ | ']' :: _ -> Ok (d, cs)
            | h :: t when is_whitespace h -> Ok (d, trim_space t)
            | h :: _ -> Bad (Printf.sprintf "[%s] unexpected char '%c'" label h) )


let parse (src: string) : datum =
    let rec parse_any chars : parsing =
        let cs = trim_space chars in
        match cs with
            | [] -> Ok (Nil, [])
            | '(' :: t -> parse_list ')' t
            | '[' :: t -> parse_list ']' t
            | '\'' :: t -> parse_quote t
            | '#' :: t -> parse_bool t
            | '"' :: t -> parse_str t
            | h :: _ when is_digit h -> parse_num cs
            | h :: _ when is_initial h -> parse_sym cs
            | h :: _ -> Bad ("unexpected char: " ^ Char.escaped h)
    and parse_quote chars =
        bind_parsing
            (fun (d, cs) ->
                 match d with
                     | Nil -> Bad "quote"
                     | _ ->
                         let quote = Lst [Sym "quote"; d] in
                         Ok (quote, cs))
            (parse_any chars)
    and parse_bool chars =
        let aux cs =
            match cs with
                | 't' :: t | 'T' :: t -> Ok (Bool true, t)
                | 'f' :: t | 'F' :: t -> Ok (Bool false, t)
                | _ -> Bad "bool"
        in
        aux chars |> checker "bool"
    and parse_str chars =
        let rec aux acc = function
            | [] -> Bad "str"
            | '"' :: t -> Ok (Str (to_str acc), t)
            | h :: t -> aux (h :: acc) t
        and to_str l =
            l |> List.map Char.escaped |> List.rev |> String.concat ""
        in
        aux [] chars |> checker "str"
    and parse_num chars =
        let rec aux acc cs =
            match cs with
                | h :: t when is_num h -> aux (h :: acc) t
                | _ -> Ok (Num (to_num acc), cs)
        and to_num l =
            l |> List.map Char.escaped |> List.rev |> String.concat ""
            |> float_of_string
        in
        aux [] chars |> checker "num"
    and parse_sym chars =
        let rec aux acc cs =
            match cs with
                | h :: t when is_subsequent h -> aux (h :: acc) t
                | _ -> Ok (Sym (to_sym acc), cs)
        and to_sym l =
            l |> List.map Char.escaped |> List.rev |> String.concat ""
        in
        aux [] chars |> checker "sym"
    and parse_list m chars =
        let rec aux acc cs =
            match cs with
                | [] -> Bad "list"
                | h :: t when h = m -> Ok (Lst (List.rev acc), t)
                | _ ->
                    match parse_any cs with
                        | Ok (_, []) -> Bad "expect ), got EOF"
        | Ok (exp, t) -> aux (exp :: acc) t
        | Bad msg -> Bad msg
    in
    aux [] chars
  in
  match parse_any (explode src) with
  | Ok (exp, []) -> exp
  | Ok (_, h :: _) -> failwith ("expect EOF, got " ^ Char.escaped h)
  | Bad msg -> failwith msg


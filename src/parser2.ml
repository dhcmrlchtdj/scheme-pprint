type datum =
    | Nil
    | Bool of bool
    | Num of float
    | Str of string
    | Sym of string
    | Lst of datum list
[@@deriving show]

let to_string (exp:datum) : string = show_datum exp

let parse_true = string_return "#t" (Bool true)
let parse_false = string_return "#f" (Bool true)
let parse_num = pfloat |>> fun s -> Num (float_of_string s)
let parse_str = pstring |>> fun s -> Str s

let parse (src:string) : datum =
    let rec parse_any chars : parsing =
        let cs = trim_space chars in
        match cs with
            | [] -> Ok (Nil, [])
            | '('::t -> parse_list t
            | '\''::t -> parse_quote t
            | '#'::t -> parse_bool t
            | '"'::t -> parse_str t
            | h::_ when is_digit h -> parse_num cs
            | h::_ when is_initial h -> parse_sym cs
            | h::_ -> Error ("unexpected char: " ^ (Char.escaped h))
    and parse_quote chars =
        bind_parsing (fun (d, cs) ->
            match d with
                | Nil -> Error "quote"
                | _ ->
                    let quote = Lst [Sym "quote"; d] in
                    Ok (quote, cs)
        ) (parse_any chars)
    and parse_bool chars =
        let aux cs =
            match cs with
                | 't'::t | 'T'::t -> Ok (Bool true, t)
                | 'f'::t | 'F'::t -> Ok (Bool false, t)
                | _ -> Error "bool"
        in
        aux chars
        |> checker "bool"
    and parse_str chars =
        let rec aux acc = function
            | [] -> Error "str"
            | '"'::t -> Ok (Str (to_str acc), t)
            | h::t -> aux (h::acc) t
        and to_str l =
            l
            |> List.map Char.escaped
            |> List.rev
            |> String.concat ""
        in
        aux [] chars
        |> checker "str"
    and parse_num chars =
        let rec aux acc cs =
            match cs with
                | h::t when is_num h -> aux (h::acc) t
                | _ -> Ok (Num (to_num acc), cs)
        and to_num l =
            l
            |> List.map Char.escaped
            |> List.rev
            |> String.concat ""
            |> float_of_string
        in
        aux [] chars
        |> checker "num"
    and parse_sym chars =
        let rec aux acc cs =
            match cs with
                | h::t when is_subsequent h -> aux (h::acc) t
                | _ -> Ok (Sym (to_sym acc), cs)
        and to_sym l =
            l
            |> List.map Char.escaped
            |> List.rev
            |> String.concat ""
        in
        aux [] chars
        |> checker "sym"
    and parse_list chars =
        let rec aux acc cs =
            match cs with
                | [] -> Error "list"
                | ')'::t -> Ok (Lst (List.rev acc), t)
                | _ ->
                    (match parse_any cs with
                        | Ok (_, []) -> Error "expect ), got EOF"
                        | Ok (exp, t) -> aux (exp::acc) t
                        | Error msg -> Error msg)
        in
        aux [] chars
    in
    match parse_any (explode src) with
        | Ok (exp, []) -> exp
        | Ok (_, h::_) -> failwith ("expect EOF, got " ^ (Char.escaped h))
        | Error msg -> failwith msg


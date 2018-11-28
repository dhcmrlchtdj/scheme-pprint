open! Containers

type rr = (Ast.t * char list, string) result

let parse (src : string) : Ast.t =
    let rec parse_aux chs : rr =
        Result.map
        match chs with
            | [] -> Ok (Ast.Nil, [])
            | '('::t -> parse_list t
            | '#'::t -> parse_bool t
            | '"'::t -> parse_str t
            | 
            | h :: _ -> Error ("unexpected char " ^ Char.escaped h)
    in
    let chs = String.to_list src in
    let ast = parse_aux chs in
    match ast with
        | Ok (x, []) -> x
        | Ok (_, h :: _) -> failwith ("expect EOF, got " ^ Char.escaped h)
        | Error msg -> failwith msg

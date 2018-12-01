open! Containers

let parse (tokens : Token.t list) : Ast.t =
    let rec aux (acc : Ast.expression list) (t : Token.t list) =
        match parse_expr t with
            | Ok (Some expr, tt) -> aux (expr :: acc) tt
            | Ok (None, []) -> Ok (List.rev acc)
            | Ok (None, _) -> Error "[parse aux] never"
            | Error s -> Error s
    and parse_expr _ = failwith "ha" in
    match aux [] tokens with Ok s -> s | Error s -> failwith s

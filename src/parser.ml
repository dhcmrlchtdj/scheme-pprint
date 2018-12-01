open! Containers
open Ast

let datum2expr (datum : Ast.datum) : Ast.expression =
    let rec aux = function
        | B x -> Bool x
        | N x -> Num x
        | S x -> Str x
        | Q x -> Variable x
        | L [Q "quote"; d] -> Quote d
        | L [Q "lambda"; L idents; exp] when is_symbol_list idents ->
            let params = symbol2ident [] idents in
            Lambda (params, aux exp)
        | L [Q "if"; cond; exp1] -> If (aux cond, aux exp1, None)
        | L [Q "if"; cond; exp1; exp2] -> If (aux cond, aux exp1, Some (aux exp2))
        | L [Q "set!"; Q id; exp] -> Set (id, aux exp)
        | L [Q "call/cc"; exp] -> CallCC (aux exp)
        | L (proc :: args) -> Application (aux proc, List.map aux args)
        | _ -> failwith "[datum2expr] unsupported"
    and is_symbol_list = function
        | [] -> true
        | Q _ :: t -> is_symbol_list t
        | _ -> false
    and symbol2ident acc = function
        | [] -> List.rev acc
        | Q h :: t -> symbol2ident (h :: acc) t
        | _ -> failwith "[symbol2ident] never"
    in
    aux datum


let tokens2datums (tokens : Token.t list) : Ast.datum list =
    let open Token in
    let rec aux (acc : Ast.datum list) (t : Token.t list) =
        match tokens2datum t with
            | Ok (Some datum, tt) -> aux (datum :: acc) tt
            | Ok (None, []) -> Ok (List.rev acc)
            | Ok (None, _) -> Error "[tokens2datums aux] never"
            | Error s -> Error s
    and tokens2datum = function
        | [] -> Ok (None, [])
        | BOOL x :: t -> Ok (Some (B x), t)
        | NUM x :: t -> Ok (Some (N x), t)
        | STR x :: t -> Ok (Some (S x), t)
        | SYMBOL x :: t -> Ok (Some (Q x), t)
        | QUOTE :: t -> read_quoted t
        | LEFT_PAREN :: t -> read_list_paren [] t
        | RIGHT_PAREN :: _ -> Error "[tokens2datum] unexpected ')'"
        | LEFT_BRACKET :: t -> read_list_bracket [] t
        | RIGHT_BRACKET :: _ -> Error "[tokens2datum] unexpected ']'"
    and read_quoted t =
        match tokens2datum t with
            | Ok (Some datum, tt) -> Ok (Some (L [Q "quote"; datum]), tt)
            | _ -> Error "[read_quoted] expect datum"
    and read_list_paren acc = function
        | RIGHT_PAREN :: tt -> Ok (Some (L (List.rev acc)), tt)
        | t ->
            (match tokens2datum t with
                | Ok (Some datum, tt) -> read_list_paren (datum :: acc) tt
                | _ -> Error "[read_list] expect datum")
    and read_list_bracket acc = function
        | RIGHT_BRACKET :: tt -> Ok (Some (L (List.rev acc)), tt)
        | t ->
            (match tokens2datum t with
                | Ok (Some datum, tt) -> read_list_bracket (datum :: acc) tt
                | _ -> Error "[read_list] expect datum")
    in
    match aux [] tokens with Ok s -> s | Error s -> failwith s


let parse (tokens : Token.t list) : Ast.t =
    let datums = tokens2datums tokens in
    let exprs = List.map datum2expr datums in
    exprs

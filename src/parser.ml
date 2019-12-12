open! Containers
open Ast
module T = Token

let datum2expr (datum : Ast.datum) : Ast.expression =
  let rec aux = function
    | B x -> Bool x
    | N x -> Num x
    | S x -> Str x
    | Q x -> Symbol x
    | L [ Q "quote"; d ] -> Quote d
    | L [ Q "lambda"; L idents; exp ] when is_symbol_list idents ->
      let params = symbol2ident [] idents in
      Lambda (params, aux exp)
    | L [ Q "if"; cond; exp1; exp2 ] -> If (aux cond, aux exp1, aux exp2)
    | L [ Q "set!"; Q id; exp ] -> Set (id, aux exp)
    | L [ Q "call/cc"; exp ] -> CallCC (aux exp)
    | L (proc :: args) -> Application (aux proc, List.map aux args)
    (* FIXME empty list is syntax error (?) *)
    | L [] -> Quote (L [])
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

let tokens2datums (tokens : T.t list) : Ast.datum list =
  let rec aux (acc : Ast.datum list) (t : T.t list) =
    match tokens2datum t with
      | Ok (Some datum, tt) -> aux (datum :: acc) tt
      | Ok (None, []) -> Ok (List.rev acc)
      | Ok (None, tt) -> aux acc tt
      | Error s -> Error s
  and tokens2datum = function
    | [] -> Ok (None, [])
    | T.COMMENT _ :: t -> Ok (None, t)
    | T.BOOL x :: t -> Ok (Some (B x), t)
    | T.NUM x :: t -> Ok (Some (N x), t)
    | T.STR x :: t -> Ok (Some (S x), t)
    | T.SYMBOL x :: t -> Ok (Some (Q x), t)
    | T.QUOTE :: t -> read_quoted t
    | T.LEFT_PAREN :: t -> read_list T.RIGHT_PAREN [] t
    | T.RIGHT_PAREN :: _ -> Error "[tokens2datum] unexpected ')'"
    | T.LEFT_BRACKET :: t -> read_list T.RIGHT_BRACKET [] t
    | T.RIGHT_BRACKET :: _ -> Error "[tokens2datum] unexpected ']'"
  and read_quoted t =
    match tokens2datum t with
      | Ok (Some datum, tt) -> Ok (Some (L [ Q "quote"; datum ]), tt)
      | _ -> Error "[read_quoted] expect datum"
  and read_list (p : T.t) (acc : Ast.datum list) = function
    | h :: t when T.equal h p -> Ok (Some (L (List.rev acc)), t)
    | t -> (
      match tokens2datum t with
        | Ok (Some datum, tt) -> read_list p (datum :: acc) tt
        | Ok (None, []) -> Error "[read_list] expect datum"
        | Ok (None, tt) -> read_list p acc tt
        | Error s -> Error ("[read_list] expect datum | " ^ s)
    )
  in
  match aux [] tokens with
    | Ok s -> s
    | Error s -> failwith s

let parse (tokens : T.t list) : Ast.t =
  let datums = tokens2datums tokens in
  let exprs = List.map datum2expr datums in
  exprs

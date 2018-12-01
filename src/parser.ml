open! Containers

let parse (tokens : Token.t list) : Ast.t =
    let open Token in
    let open Ast in
    let rec aux (acc : Ast.expression list) (t : Token.t list) =
        match parse_expr t with
            | Ok (Some expr, tt) -> aux (expr :: acc) tt
            | Ok (None, []) -> Ok (List.rev acc)
            | Ok (None, _) -> Error "[parse aux] never"
            | Error s -> Error s
    and parse_expr = function
        | [] -> Ok (None, [])
        | BOOL x :: t -> Ok (Some (Bool x), t)
        | NUM x :: t -> Ok (Some (Num x), t)
        | STR x :: t -> Ok (Some (Str x), t)
        | LEFT_PAREN :: SYMBOL "quote" :: t -> parse_quote t
        | LEFT_PAREN :: SYMBOL "lambda" :: t -> parse_lambda t
        | LEFT_PAREN :: SYMBOL "if" :: t -> parse_if t
        | LEFT_PAREN :: SYMBOL "set" :: t -> parse_set t
        | LEFT_PAREN :: SYMBOL "call/cc" :: t -> parse_callcc t
        | LEFT_PAREN :: t -> parse_application t
        | h :: _ -> Error ("[parse_expr] unknown token: " ^ Token.show h)
    (* (quote (1 2 3)) *)
    and parse_quote t = failwith "TODO"
    (* (lambda (param1 param2 ...) body) *)
    and parse_lambda t = failwith "TODO"
    (* (if cond then else)*)
    and parse_if t = failwith "TODO"
    (* (set! id expr)*)
    and parse_set t = failwith "TODO"
    (* (call/cc (lambda (k) k)) *)
    and parse_callcc t = failwith "TODO"
    (* (proc arg1 arg2 ...) *)
    and parse_application t = failwith "TODO" in
    match aux [] tokens with Ok s -> s | Error s -> failwith s

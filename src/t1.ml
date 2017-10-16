open Ast

type ret_value =
    | VBool of bool
    | VInt of int
    | VStr of string
type environ = (string * ret_value) list
let empty = []
let extend env k v = (k,v)::env
let lookup env k = List.assoc k env

let rec eval (exp:Ast.expression) (env:environ) : ret_value =
    match exp with
        | ExprBoolean c -> VBool c
        | ExprInteger i -> VInt i
        | ExprIdentifier i -> lookup env i
        | ExprQuote e -> eval e env
        | ExprLambda (param, exp) -> ()
        | ExprIf (i, t, e) ->
            (match eval i env with
                | VBool true -> eval t env
                | VBool false -> eval e env
                | _ ->)
        | ExprSet (v, e) ->
                extend
        | ExprCallCC e -> ()

let exec (exp:Ast.expression) : string =
    match eval exp empty with
        | VBool true -> "true"
        | VBool false -> "false"
        | VInt x -> string_of_int x
        | VStr s -> s


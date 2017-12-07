open Batteries
open Ast

type ret_value =
    | RNil
    | RBool of bool
    | RStr of string
    | RNum of float
    | RFunc
    | RLst of ret_value list

and environment = (string * ret_value) list

type ret = (environment * ret_value, string) result

let rec to_string = function
    | RNil -> ""
    | RBool b -> P.sprintf "%B" b
    | RStr s -> P.sprintf "%S" s
    | RNum f -> P.sprintf "%F" f
    | RFunc -> "#<procedure>"
    | RLst d ->
        let sub = String.concat " " (List.map to_string d) in
        P.sprintf "(%s)" sub


let env_get env k = List.assoc k env

and env_set env k v = (k, v) :: env

let eval env = function
    | Nil -> Ok (env, RNil)
    | Bool b -> Ok (env, RBool b)
    | Sym k -> Ok (env, env_get env k)
    | Str s -> Ok (env, RStr s)
    | Num f -> Ok (env, RNum f)
    | Lst _ -> Ok (env, RNil)


let run exp = match eval [] exp with Ok (_, v) -> to_string v | Bad s -> s

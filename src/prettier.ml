open Ast
module P = Printf

let rec to_string = function
    | Nil -> ""
    | Bool b -> P.sprintf "%B" b
    | Sym s -> P.sprintf "%s" s
    | Str s -> P.sprintf "%S" s
    | Num f -> P.sprintf "%F" f
    | Lst d ->
        let sub = String.concat " " (List.map to_string d) in
        P.sprintf "(%s)" sub


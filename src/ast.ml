module P = Printf

type datum =
    | Nil
    | Bool of bool
    | Sym of string
    | Str of string
    | Num of float
    | Lst of datum list
[@@deriving show]

let dump (exp: datum) : string = show_datum exp

let rec to_string = function
    | Nil -> ""
    | Bool b -> P.sprintf "%B" b
    | Sym s -> P.sprintf "%s" s
    | Str s -> P.sprintf "%S" s
    | Num f -> P.sprintf "%F" f
    | Lst d ->
        let sub = String.concat " " (List.map to_string d) in
        P.sprintf "(%s)" sub


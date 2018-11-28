type t =
    | Bool of bool
    | Char of char
    | Symbol of string
    | Str of string
    | Num of float
    | Lst of t list
[@@deriving show]

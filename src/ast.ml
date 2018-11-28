type t =
    | Nil
    | Bool of bool
    | Str of string
    | Num of float
    | Sym of string
    | Lst of t list
[@@deriving show]

type t =
    | LEFT_PAREN
    | RIGHT_PAREN
    | Bool of bool
    | Num of float
    | Str of string
    | Symbol of string
[@@deriving show]

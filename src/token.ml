type t =
    | LEFT_PAREN
    | RIGHT_PAREN
    | QUOTE
    | BOOL of bool
    | NUM of float
    | STR of string
    | SYMBOL of string
[@@deriving show]

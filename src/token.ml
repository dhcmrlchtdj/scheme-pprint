type t =
    | LEFT_PAREN
    | RIGHT_PAREN
    | DOT
    | QUOTE
    | SYMBOL of string
    | BOOL of bool
    | NUM of float
    | STR of string
[@@deriving show]

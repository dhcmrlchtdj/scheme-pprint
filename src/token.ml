type t =
    | LEFT_PAREN
    | RIGHT_PAREN
    | DOT
    | QUOTE
    | IDENT of string
    | BOOL of bool
    | NUM of float
    | STR of string
[@@deriving show]

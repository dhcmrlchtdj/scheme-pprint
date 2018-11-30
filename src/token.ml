type t =
    | LEFT_PAREN
    | RIGHT_PAREN
    | DOT
    | QUOTE
    | IDENTIFIER of string
    | KEYWORD of string
    | BOOLEAN of bool
    | NUMBER of float
    | CHARACTER of char
    | STRING of string
    | SYMBOL of string
[@@deriving show]

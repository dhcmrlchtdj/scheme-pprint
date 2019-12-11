type t =
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACKET
  | RIGHT_BRACKET
  | QUOTE
  | BOOL of bool
  | NUM of float
  | STR of string
  | SYMBOL of string
  | COMMENT of string
[@@deriving show]

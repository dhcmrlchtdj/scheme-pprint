type t = expression list [@@deriving show]

and expression =
    | Bool of bool
    | Num of float
    | Char of char
    | Str of string
    | Symbol of string
    | Quote of datum
    | Set of identifier * expression
    | If of expression * expression * expression option
    | Application of expression * expression list
    | Lambda of identifier list * expression list * expression list
    | Def of identifier * expression
    | DefFunc of identifier * identifier list * expression
    | Begin of expression list
[@@deriving show]

and identifier = string [@@deriving show]

and datum =
    | BOOLEAN of bool
    | NUMBER of float
    | STRING of string
    | SYMBOL of string
    | LIST of datum list
[@@deriving show]

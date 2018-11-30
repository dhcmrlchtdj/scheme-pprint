type t = expression list [@@deriving show]

and expression =
    | Bool of bool
    | Num of float
    | Str of string
    | Symbol of string
    | Quote of datum
    | Set of identifier * expression
    | If of expression * expression * expression option
    | Let of (identifier * expression) list * expression
    | Application of expression * expression list
    | Lambda of identifier list * expression
    | Define of identifier * expression
    | Begin of expression list
[@@deriving show]

and identifier = string [@@deriving show]

and datum =
    | SYM of string
    | S of string
    | B of bool
    | N of float
    | L of datum list
[@@deriving show]

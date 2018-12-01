type t = expression list [@@deriving show]

and expression =
    | Bool of bool
    | Num of float
    | Str of string
    | Symbol of string
    | Quote of datum
    | Lambda of identifier list * expression list
    | If of expression * expression * expression option
    | Set of identifier * expression
    | CallCC of expression
    | Application of expression * expression list
    | Begin of expression list
    | Let of (identifier * expression) list * expression
    | Define of identifier * expression
[@@deriving show]

and identifier = string [@@deriving show]

and datum =
    | SYM of string
    | S of string
    | B of bool
    | N of float
    | L of datum list
[@@deriving show]

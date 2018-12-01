type t = expression list [@@deriving show]

and expression =
    | Bool of bool
    | Num of float
    | Str of string
    | Variable of string
    | Quote of datum
    | Lambda of identifier list * expression
    | If of expression * expression * expression option
    | Set of identifier * expression
    | CallCC of expression
    | Application of expression * expression list
[@@deriving show]

and identifier = string [@@deriving show]

and datum =
    | B of bool
    | N of float
    | S of string
    (* symbol *)
    | Q of string
    | L of datum list
[@@deriving show]

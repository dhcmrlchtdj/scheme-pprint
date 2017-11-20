type datum =
    | Nil
    | Bool of bool
    | Sym of string
    | Str of string
    | Num of float
    | Lst of datum list
[@@deriving show]

let to_string (exp:datum) : string =
    show_datum exp

type program = form list [@@deriving show]

and form =
    | D of definition
    | E of expression
[@@deriving show]

and definition =
    | Def of variable * expression
    | DefFunc of variable * variable list * expression
    (* | DefFunc2 of variable * variable list * variable * expression *)
    (* | DefineSyntax of keyword * expression *)
    (* | LetSyntax of syntax_binding list * definition *)
    (* | LetSyntaxRec of syntax_binding list * definition *)
    | Begin of definition list
[@@deriving show]

and expression =
    | Constant of constant
    | Variable of variable
    | Quote of datum
    | Lambda of variable list * definition list * expression list
    (* | Lambda2 of (variable list) * variable * body *)
    (* | Lambda3 of variable * body *)
    | If of expression * expression * expression option
    | Set of variable * expression
    (* | LetSyntax of syntax_binding list * expression list *)
    (* | LetSyntaxRec of syntax_binding list * expression list *)
    | Application of expression * expression list
[@@deriving show]

and variable = identifier [@@deriving show]

and keyword = identifier [@@deriving show]

and constant =
    | CBool of bool
    | CNum of float
    | CChar of char
    | CStr of string
[@@deriving show]

and identifier = string [@@deriving show]

and datum =
    | Bool of bool
    | Num of float
    | Str of string
    | Symbol of string
    | Character of char
    | Lst of datum list
    (* | Vector of datum list *)
[@@deriving show]

type t = program [@@deriving show]

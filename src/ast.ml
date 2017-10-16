type expression =
    | ExprBoolean of bool
    | ExprInteger of int
    | ExprIdentifier of string
    | ExprNil
    | ExprCons of expression * expression
    | ExprQuote of expression
    | ExprLambda of (string list) * expression
    | ExprIf of expression * expression * expression
    | ExprSet of string * expression
    | ExprCallCC of expression

let f = Printf.sprintf
let rec to_string (exp:expression) : string =
    match exp with
        | ExprBoolean true -> "#t"
        | ExprBoolean false -> "#f"
        | ExprInteger i -> string_of_int i
        | ExprIdentifier i -> i
        | ExprNil -> "()"
        | ExprCons (h, t) ->
            f "(%s . %s)" (to_string h) (to_string t)
        | ExprQuote e -> f "(quote %s)" (to_string e)
        | ExprLambda (param, exp) ->
            f "(lambda (%s) %s)"
                (List.fold_left
                     (fun a b -> match a with
                           | "" -> b
                           | _ -> f "%s %s" a b)
                     "" param)
                (to_string exp)
        | ExprIf (i, t, e) -> f "(if %s %s %s)" (to_string i) (to_string t) (to_string e)
        | ExprSet (v, e) -> f "(set! %s %s)" v (to_string e)
        | ExprCallCC e -> f "(call/cc %s)" (to_string e)

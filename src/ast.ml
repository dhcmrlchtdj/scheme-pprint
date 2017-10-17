type lobject =
    | Fixnum of int
    | Boolean of bool
    | Symbol of string
    | Nil
    | Pair of lobject * lobject
    | Primitive of string * (lobject list -> lobject)
    | Quote of value
and value = lobject
and name = string
and def =
    | Val of name * exp
    | Exp of exp
and exp =
    | Literal of value
    | Var of name
    | If of exp * exp * exp
    | And of exp * exp
    | Or of exp * exp
    | Apply of exp * exp
    | Call of exp * exp list
    | Defexp of def


let rec is_list (e:lobject) : bool =
    match e with
        | Nil -> true
        | Pair (_, r) -> is_list r
        | _ -> false

let rec pair_to_list (e:lobject) : lobject list =
    match e with
        | Nil -> []
        | Pair(a, b) -> a::(pair_to_list b)
        | _ -> failwith "this can't happen"

let rec to_string (e:lobject) : string =
    match e with
        | Fixnum n -> string_of_int n
        | Boolean b -> if b then "#t" else "#f"
        | Symbol s -> s
        | Nil -> "nil"
        | Pair _ -> "(" ^ (pair_to_string e) ^ ")"
        | Primitive (n, _) -> "#<procedure " ^ n ^ ">"
and pair_to_string (e:lobject) : string =
    if is_list e
    then _list_to_string e
    else _pair_to_string e
and _list_to_string (e:lobject) : string =
    match e with
        | Pair (l, Nil) -> to_string l
        | Pair (l, r) -> (to_string l) ^ " " ^ (pair_to_string r)
        | _ -> failwith "this can't happen"
and _pair_to_string (e:lobject) : string =
    match e with
        | Pair (l, r) -> (to_string l) ^ " . " ^ (to_string r)
        | _ -> failwith "this can't happen"

let print_sexp (e:lobject) : unit =
    print_string (to_string e)

let print_value = print_sexp

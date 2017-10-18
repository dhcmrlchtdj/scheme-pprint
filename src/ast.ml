type expression =
    | Nil
    | If of expression * expression * expression


type datum =
    (* #t #T #f #F *)
    | Boolean of bool
    (* peculiar | initial subsequent* *)
    | Symbol of string
    (* num_sign? digit+ *)
    | Number of int
    | List of datum list

let rec is_letter = function
    | 'a'..'z' | 'A'..'Z' -> true
    | _ -> false
and is_digit = function
    | '0'..'9' -> true
    | _ -> false
and is_initial = function
    | c when is_letter c -> true
    | '!' | '$' | '%' | '&' | '*' | '/' | ':'
    | '<' | '=' | '>' | '?' | '~' | '_' | '^' -> true
    | _ -> false
and is_subsequent = function
    | c when is_initial c -> true
    | c when is_digit c -> true
    | '.' | '+' | '-' | '@' -> true
    | _ -> false
and is_peculiar = function
    | '+' | '-' -> true
    | _ -> false
and is_num_sign = function
    | '+' | '-' -> true
    | _ -> false

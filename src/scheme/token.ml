type token =
    | Boolean of bool
    | Symbol of string
    | Number of int
    | Left_Paren
    | Right_Paren

let to_string = function
    | Boolean b -> if b then "#b" else "#f"
    | Symbol s -> s
    | Number i -> string_of_int i
    | Left_Paren -> "("
    | Right_Paren -> ")"

let rec print = function
    | [] ->
        print_newline ()
    | h :: t ->
        print_string (to_string h);
        print_string " ";
        print t

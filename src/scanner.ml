open! Containers

(* https://www.scheme.com/tspl3/grammar.html *)

let is_symbol_initial = function
    | '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '~'
    |'_' | '^' | '.' | '+' | '-' | '@' ->
        true
    | c when Parse.is_alpha c -> true
    | _ -> false


let is_symbol_subsequent = function
    | c when Parse.is_num c -> true
    | c when is_symbol_initial c -> true
    | _ -> false


let is_bool_end = function
    | [] -> true
    | ')' :: _ -> true
    | '(' :: _ -> true
    | h :: _ when Parse.is_white h -> true
    | _ -> false


let scan (src : string) : Token.t list =
    let open Token in
    let rec aux (acc : Token.t list) (t : char list) =
        match scan_token t with
            | Ok (Some tok, tt) -> aux (tok :: acc) tt
            | Ok (None, []) -> Ok (List.rev acc)
            | Ok (None, _) -> Error "[scan aux] never"
            | Error s -> Error s
    and scan_token = function
        | [] -> Ok (None, [])
        | h :: t when Parse.is_white h -> scan_token t
        | '[' :: t -> Ok (Some LEFT_BRACKET, t)
        | ']' :: t -> Ok (Some RIGHT_BRACKET, t)
        | '(' :: t -> Ok (Some LEFT_PAREN, t)
        | ')' :: t -> Ok (Some RIGHT_PAREN, t)
        | '\'' :: t -> Ok (Some QUOTE, t)
        | '#' :: 't' :: t when is_bool_end t -> Ok (Some (BOOL true), t)
        | '#' :: 'T' :: t when is_bool_end t -> Ok (Some (BOOL true), t)
        | '#' :: 'f' :: t when is_bool_end t -> Ok (Some (BOOL false), t)
        | '#' :: 'F' :: t when is_bool_end t -> Ok (Some (BOOL false), t)
        | ';' :: t ->
            let s = scan_comment [] t in
            let f (x, tt) = (Some (COMMENT x), tt) in
            Result.map f s
        | '"' :: t ->
            let s = scan_str [] t in
            let f (x, tt) = (Some (STR x), tt) in
            Result.map f s
        | h :: t when Parse.is_num h ->
            let s = scan_num [h] t in
            let f (x, tt) = (Some (NUM x), tt) in
            Result.map f s
        | h :: t when is_symbol_initial h ->
            let s = scan_symbol [h] t in
            let f (x, tt) = (Some (SYMBOL x), tt) in
            Result.map f s
        | h :: _ -> Error ("[scan_token] unexpected char " ^ String.of_char h)
    and scan_str (acc : char list) = function
        | '\\' :: '\\' :: t -> scan_str ('\\' :: acc) t
        | '\\' :: '"' :: t -> scan_str ('"' :: acc) t
        | '"' :: t ->
            let x = acc |> List.rev |> String.of_list in
            Ok (x, t)
        | h :: t -> scan_str (h :: acc) t
        | [] -> Error "[scan_str] expect char, got EOF"
    and scan_num (acc : char list) = function
        | h :: t when Parse.is_num h -> scan_num (h :: acc) t
        | t ->
            let x = acc |> List.rev |> String.of_list |> Float.of_string in
            Ok (x, t)
    and scan_symbol (acc : char list) = function
        | h :: t when is_symbol_subsequent h -> scan_symbol (h :: acc) t
        | t ->
            let x = acc |> List.rev |> String.of_list in
            Ok (x, t)
    and scan_comment (acc : char list) = function
        | h :: t when not (Char.equal h '\n') -> scan_comment (h :: acc) t
        | t ->
            let x = acc |> List.rev |> String.of_list in
            Ok (x, t)
    in
  match aux [] (String.to_list src) with Ok s -> s | Error s -> failwith s

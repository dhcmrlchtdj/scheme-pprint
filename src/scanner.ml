open! Containers
module T = Token

(* https://www.scheme.com/tspl3/grammar.html *)

let is_symbol_initial = function
    | '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '~'
    |'_' | '^' ->
        true
    | c when Parse.is_alpha c -> true
    | _ -> false


let is_symbol_subsequent = function
    | '.' | '+' | '-' | '@' -> true
    | c when Parse.is_num c -> true
    | c when is_symbol_initial c -> true
    | _ -> false


let scan (src : string) : T.t list =
    let rec aux (acc : T.t list) (t : char list) =
        match scan_token acc t with
            | Ok (ts, []) -> Ok (List.rev ts)
            | Ok (ts, tt) -> aux ts tt
            | Error s -> Error s
    and scan_token (acc : T.t list) = function
        | [] -> Ok (acc, [])
        | h :: t when Parse.is_white h -> scan_token acc t
        | '(' :: t -> Ok (T.LEFT_PAREN :: acc, t)
        | ')' :: t -> Ok (T.RIGHT_PAREN :: acc, t)
        | '#' :: t ->
            let s = scan_bool t in
            let f (x, tt) = (T.Bool x :: acc, tt) in
            Result.map f s
        | '"' :: t ->
            let s = scan_str [] t in
            let f (x, tt) = (T.Str x :: acc, tt) in
            Result.map f s
        | h :: t when Parse.is_num h ->
            let s = scan_num [h] t in
            let f (x, tt) = (T.Num x :: acc, tt) in
            Result.map f s
        | h :: t when is_symbol_initial h ->
            let s = scan_symbol [h] t in
            let f (x, tt) = (T.Symbol x :: acc, tt) in
            Result.map f s
        | h :: _ -> Error ("[scan_token] unexpected char " ^ String.of_char h)
    and scan_bool = function
        | ['t'] -> Ok (true, [])
        | 't' :: ')' :: t -> Ok (true, ')' :: t)
        | 't' :: ' ' :: t -> Ok (true, t)
        | ['f'] -> Ok (false, [])
        | 'f' :: ')' :: t -> Ok (false, ')' :: t)
        | 'f' :: ' ' :: t -> Ok (false, t)
        | _ -> Error "[scan_bool] expect bool"
    and scan_str (acc : char list) = function
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
    in
    match aux [] (String.to_list src) with Ok s -> s | Error s -> failwith s

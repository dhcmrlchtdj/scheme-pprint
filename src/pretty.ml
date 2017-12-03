(**
 * http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
 * https://github.com/prettier/prettier-printer
*)

open Ast
module P = Printf

type doc =
    | Concat of doc list
    | Line of int
    | Text of string
[@@deriving show]

let dump (exp: doc) : string = show_doc exp

let rec to_string = function
    | Concat doc -> List.map to_string doc |> String.concat ""
    | Line i -> "\n" ^ BatString.repeat " " i
    | Text s -> s


let rec from_datum = function
    | Nil -> Text ""
    | Bool b -> Text (P.sprintf "%B" b)
    | Sym s -> Text (P.sprintf "%s" s)
    | Str s -> Text (P.sprintf "%S" s)
    | Num f -> Text (P.sprintf "%F" f)
    | Lst d ->
        let sub =
            d |> List.map from_datum
            |> List.map (function
                | Concat d -> d
                | Text s -> [Text s; Text " "]
                | Line i -> [Line i] )
            |> List.flatten |> List.map (function Line i -> Line (i + 4) | x -> x)
        in
        let sub2 = Concat sub in
        Concat [Text "("; Line 4; sub2; Line 0; Text ")"]


let print (exp: datum) : string =
    let d = from_datum exp in
    to_string d


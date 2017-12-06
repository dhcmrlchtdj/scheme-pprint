(**
 * http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
 * https://github.com/prettier/prettier-printer
*)

open Batteries
open Ast
module P = Printf

type doc =
    | Concat of doc list
    | Newline
    | Indent of int
    | Text of string
[@@deriving show]

let dump (exp: doc) : string = show_doc exp

let rec to_string = function
    | Concat doc -> List.map to_string doc |> String.concat ""
    | Newline -> "\n"
    | Indent i -> String.repeat " " i
    | Text s -> s


let rec simple_from_datum = function
    | Nil -> Text ""
    | Bool b -> Text (P.sprintf "%B" b)
    | Sym s -> Text (P.sprintf "%s" s)
    | Str s -> Text (P.sprintf "%S" s)
    | Num f -> Text (P.sprintf "%F" f)
    | Lst d ->
        let sub =
            let x = d |> List.map simple_from_datum in
            let len = List.length x - 1 in
            x
            |> List.mapi (fun i x ->
                if i = len then [x] else [x; Newline; Indent 0] )
            |> List.flatten |> List.map simple_add_indent |> fun x -> Concat x
        in
        Concat [Text "("; sub; Text ")"]


and simple_add_indent = function
    | Concat doc -> doc |> List.map simple_add_indent |> fun x -> Concat x
    | Newline -> Newline
    | Indent i -> Indent (i + 4)
    | Text s -> Text s


let print (exp: datum) : string =
    let d = simple_from_datum exp in
    to_string d


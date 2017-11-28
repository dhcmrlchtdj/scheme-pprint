(**
 * http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
 * https://github.com/prettier/prettier-printer
*)

open Ast
module P = Printf

type doc =
    | Concat of doc list
    | Line
    | Indent of int
    | Text of string
[@@deriving show]

let dump (exp: doc) : string = show_doc exp

let rec to_string (exp: doc) : string = ""

let rec from_datum = function
    | Nil -> Text ""
    | Bool b -> Text (P.sprintf "%B" b)
    | Sym s -> Text (P.sprintf "%s" s)
    | Str s -> Text (P.sprintf "%S" s)
    | Num f -> Text (P.sprintf "%F" f)
    | Lst d ->
        let sub = List.map from_datum d in
        let sub2 = Concat sub in
        Concat [Text "("; Line; sub2; Line; Text ")"]


let print (exp: datum) : string =
    let d = from_datum exp in
    dump d


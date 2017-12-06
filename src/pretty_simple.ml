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


let rec from_datum = function
    | Nil -> Text ""
    | Bool b -> Text (P.sprintf "%B" b)
    | Sym s -> Text (P.sprintf "%s" s)
    | Str s -> Text (P.sprintf "%S" s)
    | Num f -> Text (P.sprintf "%F" f)
    | Lst d ->
        let sub =
            let doc = d |> List.map from_datum in
            let len = List.length doc - 1 in
            doc
            |> List.mapi (fun i x ->
                if i = len then [x] else [x; Newline; Indent 0] )
            |> List.flatten |> List.map add_indent |> fun x -> Concat x
        in
        let doc = Concat [Text "("; sub; Text ")"] in
        Concat (simplify doc)


and add_indent = function
    | Concat doc -> doc |> List.map add_indent |> fun x -> Concat x
    | Newline -> Newline
    | Indent i -> Indent (i + 4)
    | Text s -> Text s


and simplify = function
    | Concat doc -> doc |> List.map simplify |> List.flatten
    | x -> [x]


let print (exp: datum) : string = exp |> from_datum |> to_string

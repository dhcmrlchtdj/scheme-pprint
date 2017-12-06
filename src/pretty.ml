open Batteries
open Ast
module P = Printf

type doc =
    | Group of doc
    | Concat of doc list
    | Newline of int
    | Text of string
[@@deriving show]

let dump (exp: doc) : string = show_doc exp

let rec to_string width = function
    | Text s -> s
    | Newline i -> P.sprintf "\n%s" (String.repeat " " i)
    | Group d ->
        let s = to_string_group d in
        let len = String.length s in
        if len <= width then s else to_string width d
    | Concat ds ->
        let _, dss =
            List.fold_left
                (fun (w, prev) next ->
                     let s = to_string w next in
                     let len = String.length s in
                     let r = match next with Newline i -> width - i | _ -> w - len in
                     (r, s :: prev))
                (width, []) ds
        in
        dss |> List.rev |> String.concat ""


and to_string_group = function
    | Text s -> s
    | Newline _ -> " "
    | Group d -> to_string_group d
    | Concat ds -> List.map to_string_group ds |> String.concat ""


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
            doc |> List.mapi (fun i x -> if i = len then [x] else [x; Newline 0])
            |> List.flatten |> List.map add_indent |> fun x -> Concat x
        in
        let doc = Concat [Text "("; sub; Text ")"] |> simplify in
        Group (Concat doc)


and add_indent = function
    | Group d -> Group (add_indent d)
    | Concat ds -> ds |> List.map add_indent |> fun x -> Concat x
    | Newline i -> Newline (i + 4)
    | Text s -> Text s


and simplify = function
    | Concat doc -> doc |> List.map simplify |> List.flatten
    | x -> [x]


let print (width: int) (exp: datum) : string =
    exp |> from_datum |> to_string width


(* exp |> from_datum |> dump *)

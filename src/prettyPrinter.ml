open! Containers

let expr2datum (expr : Ast.expression) : Ast.datum =
    let open Ast in
    let rec aux = function
        | Bool x -> B x
        | Num x -> N x
        | Str x -> S x
        | Symbol x -> Q x
        | Quote d -> L [Q "quote"; d]
        | Lambda (params, exp) ->
            let symbols = ident2symbol [] params in
            L [Q "lambda"; L symbols; aux exp]
        | If (cond, exp1, None) -> L [Q "if"; aux cond; aux exp1]
        | If (cond, exp1, Some exp2) -> L [Q "if"; aux cond; aux exp1; aux exp2]
        | Set (id, exp) -> L [Q "set!"; Q id; aux exp]
        | CallCC exp -> L [Q "call/cc"; aux exp]
        | Application (proc, args) -> L (aux proc :: List.map aux args)
    and ident2symbol acc = function
        | [] -> List.rev acc
        | h :: t -> ident2symbol (Q h :: acc) t
    in
    aux expr


module Document = struct
    module P = Printf

    type doc =
        | Newline of int
        | Text of string
        | Group of doc
        | Concat of doc list

    let max_width = 80

    let to_string =
        let rec aux used = function
            | Text s -> s
            | Newline i -> P.sprintf "\n%s" (String.repeat " " i)
            | Group d ->
                let s = to_string_group d in
                let len = String.length s in
                if len + used <= max_width then s else aux used d
            | Concat ds ->
                let _, dss =
                    List.fold_left
                        (fun (w, prev) next ->
                             let s = aux w next in
                             let r =
                                 match next with Newline i -> i | _ -> w + String.length s
                             in
                             (r, s :: prev) )
                        (used, [])
                        ds
                in
                dss |> List.rev |> String.concat ""
        and to_string_group = function
            | Text s -> s
            | Newline _ -> " "
            | Group d -> to_string_group d
            | Concat ds -> List.map to_string_group ds |> String.concat ""
        in
        aux 0


    let of_datum =
        let open Ast in
        let rec aux = function
            | Q s -> Text (P.sprintf "%s" s)
            | S s -> Text (P.sprintf "%S" s)
            | B b -> Text (if b then "#t" else "#f")
            | N f -> Text (P.sprintf "%F" f)
            | L ds ->
                let sub =
                    let doc = List.map aux ds in
                    let len = List.length doc - 1 in
                    doc
                    |> List.mapi (fun i x -> if i = len then [x] else [x; Newline 0])
                    |> List.flatten
                    |> List.map add_indent
                    |> fun x -> Concat x
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
        in
        aux


    let pretty datum = datum |> of_datum |> to_string
end

let to_string (ast : Ast.t) : string =
    let ast2str expr = expr |> expr2datum |> Document.pretty in
    let str = List.map ast2str ast in
    String.concat "\n" str

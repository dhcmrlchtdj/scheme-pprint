open! Containers

let rec expr2datum (expr : Ast.expression) : Ast.datum =
    let open Ast in
    match expr with
        | Bool x -> B x
        | Num x -> N x
        | Str x -> S x
        | Symbol x -> SYM x
        | Quote d -> L [SYM "quote"; d]
        | Lambda (is, es) ->
            (* (lambda (p1 p2 ...) body1 body2) *)
            let dis = List.map (fun x -> SYM x) is in
            let des = List.map expr2datum es in
            L (SYM "lambda" :: L dis :: des)
        | If (t, e1, e2) ->
            (* (if cond then else)*)
            let dt = expr2datum t in
            let de1 = expr2datum e1 in
            (match Option.map expr2datum e2 with
                | Some de2 -> L [SYM "if"; dt; de1; de2]
                | None -> L [SYM "if"; dt; de1])
        | Set (i, e) ->
            (* (set! id expr)*)
            let de = expr2datum e in
            L [SYM "set!"; SYM i; de]
        | CallCC e ->
            (* (call/cc (lambda (k) k)) *)
            let de = expr2datum e in
            L [SYM "call/cc"; de]
        | Application (e, es) ->
            (* (proc-expr a1 a2 ...) *)
            let de = expr2datum e in
            let des = List.map expr2datum es in
            L (de :: des)
        | Begin es ->
            (* (begin e1 e2 ...) *)
            let des = List.map expr2datum es in
            L (SYM "begin" :: des)
        | Let (ees, e) ->
            (* (let ([id expr] [i2 e2]) expr)*)
            let de = expr2datum e in
            let dees = List.map (fun (i, e) -> L [SYM i; expr2datum e]) ees in
            L [SYM "let"; L dees; de]
        | Define (i, e) ->
            (* (define id expr) *)
            let de = expr2datum e in
            L [SYM "define"; SYM i; de]


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
            | SYM s -> Text (P.sprintf "%s" s)
            | S s -> Text (P.sprintf "%S" s)
            | B b -> Text (P.sprintf "%B" b)
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

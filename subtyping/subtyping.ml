let ( let* ) = Result.bind

let flatten l =
  let rec loop acc l =
    match l with
      | [] -> Ok (List.rev acc)
      | Ok x :: l' -> loop (x :: acc) l'
      | Error e :: _ -> Error e
  in
  loop [] l

(* --- *)

type types =
  [ `Bool
  | `Top
  | `Arrow of types * types
  | `Record of (string * types) list
  ]
[@@deriving show]

type terms =
  [ `Bool of bool
  | `Var of string
  | `Abs of string * types * terms
  | `App of terms * terms
  | `Record of (string * terms) list
  | `RecordItem of terms * string
  ]
[@@deriving show]

(* type values = *)
(*   [ `Abs of string * types * terms *)
(*   | `Record of (string * terms) list *)
(*   ] *)
(* [@@deriving show] *)

type values_context = (string * terms) list

type types_context = (string * types) list

(* --- *)

let rec ( <: ) s t =
  (* S is subtype of T *)
  match (s, t) with
    | (_, _) when s = t -> true
    | (_, `Top) -> true
    | (`Arrow (p1, r1), `Arrow (p2, r2)) ->
      let p = p2 <: p1 in
      let r = r1 <: r2 in
      p && r
    | (`Record sub, `Record sup) ->
      let check (name, sup_typ) =
        match List.assoc_opt name sub with
          | None -> false
          | Some sub_typ -> sub_typ <: sup_typ
      in
      List.for_all check sup
    | _ -> false

let rec typeof (ctx : types_context) : terms -> (types, string) result
  = function
  | `Bool _ -> Ok `Bool
  | `Var name ->
    let t = List.assoc_opt name ctx in
    Option.to_result ~none:("Var(" ^ name ^ ")") t
  | `Abs (name, typ, exp) ->
    let ctx2 = (name, typ) :: ctx in
    let* r = typeof ctx2 exp in
    Ok (`Arrow (typ, r))
  | `App (m, n) -> (
    let* tm = typeof ctx m in
    let* tn = typeof ctx n in
    match tm with
      | `Arrow (t_arg, t_ret) when tn <: t_arg -> Ok t_ret
      | _ -> Error "App"
  )
  | `Record l ->
    let conv (name, exp) =
      let* typ = typeof ctx exp in
      Ok (name, typ)
    in
    let* typ = l |> List.map conv |> flatten in
    Ok (`Record typ)
  | `RecordItem (exp, name) -> (
    let* typ = typeof ctx exp in
    match typ with
      | `Record record ->
        let t = List.assoc_opt name record in
        Option.to_result ~none:"RecordItem" t
      | _ -> Error "RecordItem"
  )

(* --- *)

let rec valueof (ctx : values_context) : terms -> (terms, string) result
  = function
  | `Bool x -> Ok (`Bool x)
  | `Var name ->
    let t = List.assoc_opt name ctx in
    Option.to_result ~none:("Var(" ^ name ^ ")") t
  | `Abs (name, typ, exp) -> Ok (`Abs (name, typ, exp))
  | `App (m, n) -> (
    let* vm = valueof ctx m in
    let* vn = valueof ctx n in
    match vm with
      | `Abs (name, _, exp) ->
        let ctx2 = (name, vn) :: ctx in
        valueof ctx2 exp
      | _ -> Error "App"
  )
  | `Record l ->
    let conv (name, exp) =
      let* e = valueof ctx exp in
      Ok (name, e)
    in
    let* l2 = l |> List.map conv |> flatten in
    Ok (`Record l2)
  | `RecordItem (exp, name) -> (
    let* r = valueof ctx exp in
    match r with
      | `Record record ->
        let t = List.assoc_opt name record in
        Option.to_result ~none:"RecordItem" t
      | _ -> Error "RecordItem"
  )

(* --- *)

let eval expr =
  let* t = typeof [] expr in
  let* v = valueof [] expr in
  let ts = show_types t in
  let vs = show_terms v in
  let s = Printf.sprintf "type = %s\nvalues = %s" ts vs in
  Ok s

(* --- *)

  let%expect_test "eval" =
    let _ =
      let get_x =
        `Abs ("arg", `Record [ ("x", `Bool) ], `RecordItem (`Var "arg", "x"))
      in
      let record = `Record [ ("x", `Bool true); ("y", `Bool false) ] in
      let exp = `App (get_x, record) in
      let* s = eval exp in
      Printf.printf "%s" s;
      [%expect {|
        type = `Bool
        values = `Bool (true) |}];

      Ok ()
    in
    ()

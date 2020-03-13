let ( let* ) = Result.bind

type typ =
  | Tbool
  | Tarrow of typ * typ

let rec typ_to_string = function
  | Tbool -> "bool"
  | Tarrow (p, r) ->
    Printf.sprintf "(%s -> %s)" (typ_to_string p) (typ_to_string r)

type term =
  | True
  | False
  | If of term * term * term
  | Var of string
  | Abs of string * typ * term
  | App of term * term

let rec term_to_string = function
  | True -> "true"
  | False -> "false"
  | If (c, t1, t2) ->
    Printf.sprintf
      "(if %s %s %s)"
      (term_to_string c)
      (term_to_string t1)
      (term_to_string t2)
  | Var x -> Printf.sprintf "%s" x
  | Abs (x, t, m) ->
    Printf.sprintf "λ(%s:%s).%s" x (typ_to_string t) (term_to_string m)
  | App (m, n) -> Printf.sprintf "(%s %s)" (term_to_string m) (term_to_string n)

type val_context = (string * term) list

type typ_context = (string * typ) list

let rec typeof ctx = function
  | True -> Ok Tbool
  | False -> Ok Tbool
  | If (c, t1, t2) ->
    let* cc = typeof ctx c in
    if cc = Tbool
    then
      let* tt1 = typeof ctx t1 in
      let* tt2 = typeof ctx t2 in
      if tt1 = tt2 then Ok tt1 else Error "if t1 <> t2"
    else Error "if cond"
  | Var x ->
    let t = List.assoc_opt x ctx in
    Option.to_result ~none:"var" t
  | Abs (x, t, m) ->
    let ctx2 = (x, t) :: ctx in
    let* r_typ = typeof ctx2 m in
    Ok (Tarrow (t, r_typ))
  | App (m, n) -> (
    let* tm = typeof ctx m in
    let* tn = typeof ctx n in
    match tm with
      | Tarrow (t_arg, t_ret) when tn = t_arg -> Ok t_ret
      | _ -> Error "app"
  )

let rec valueof ctx = function
  | True -> True
  | False -> False
  | If (True, t1, _) -> valueof ctx t1
  | If (False, _, t2) -> valueof ctx t2
  | If (c, t1, t2) -> valueof ctx (If (valueof ctx c, t1, t2))
  | Var x -> (
    match List.assoc_opt x ctx with
      | Some l -> l
      | None -> Var x
  )
  | Abs (x, t, m) -> Abs (x, t, valueof ctx m)
  | App (m, n) -> (
    let m' = valueof ctx m in
    let n' = valueof ctx n in
    match m' with
      | Abs (x, _t, m) -> valueof ((x, n') :: ctx) m
      | _ -> App (m', n')
  )

let eval expr =
  let* t = typeof [] expr in
  let v = valueof [] expr in
  let ts = typ_to_string t in
  let vs = term_to_string v in
  let s = Printf.sprintf "(%s:%s)" vs ts in
  Ok s

let%expect_test "eval" =
  let _ =
    let* s = eval True in
    Printf.printf "%s" s;
    [%expect {| (true:bool) |}];
    let* s = eval (If (False, True, False)) in
    Printf.printf "%s" s;
    [%expect {| (false:bool) |}];
    let* s = eval (Abs ("x", Tbool, Var "x")) in
    Printf.printf "%s" s;
    [%expect {| (λ(x:bool).x:(bool -> bool)) |}];
    Ok ()
  in
  ()

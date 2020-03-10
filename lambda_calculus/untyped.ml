module DeBruijn = struct
  type term =
    | Var of int
    | Abs of term
    | App of term * term
end

type term =
  (* x *)
  | Var of string
  (* (λx.M) *)
  | Abs of string * term
  (* (M N) *)
  | App of term * term

let rec to_string = function
  | Var x -> Printf.sprintf "%s" x
  | Abs (x, m) -> Printf.sprintf "λ%s.%s" x (to_string m)
  | App (m, n) -> Printf.sprintf "(%s %s)" (to_string m) (to_string n)

type context = (string * term) list

let eval t =
  let rec aux ctx = function
    | Var x -> (
      match List.assoc_opt x ctx with
        | Some l -> l
        | None -> Var x
    )
    | Abs (x, m) -> Abs (x, aux ctx m)
    | App (m, n) -> (
      let m' = aux ctx m in
      let n' = aux ctx n in
      match m' with
        | Abs (x, m) -> aux ((x, n') :: ctx) m
        | _ -> App (m', n')
    )
  in
  t |> aux [] |> to_string

let%expect_test "eval" =
  Printf.printf "%s" (eval (Var "x"));
  [%expect {| x |}];
  Printf.printf "%s" (eval (Abs ("x", Var "x")));
  [%expect {| λx.x |}];
  let lc_true = Abs ("t", Abs ("f", Var "t")) in
  Printf.printf "true = %s" (eval lc_true);
  [%expect {| true = λt.λf.t |}];
  let lc_false = Abs ("t", Abs ("f", Var "f")) in
  Printf.printf "false = %s" (eval lc_false);
  [%expect {| false = λt.λf.f |}];
  let lc_if =
    Abs
      ( "cond",
        Abs
          ("then", Abs ("else", App (App (Var "cond", Var "then"), Var "else")))
      )
  in
  Printf.printf "if = %s" (eval lc_if);
  [%expect {| if = λcond.λthen.λelse.((cond then) else) |}];
  Printf.printf
    "if true T F = %s"
    (eval (App (App (App (lc_if, lc_true), Var "T"), Var "F")));
  [%expect {| if true T F = T |}];
  Printf.printf
    "if false T F = %s"
    (eval (App (App (App (lc_if, lc_false), Var "T"), Var "F")));
  [%expect {| if false T F = F |}];
  let church_zero = Abs ("s", Abs ("z", Var "z")) in
  let church_succ =
    Abs
      ( "n",
        Abs
          ("s", Abs ("z", App (Var "s", App (App (Var "n", Var "s"), Var "z"))))
      )
  in
  Printf.printf "church_zero = %s" (eval church_zero);
  [%expect {| church_zero = λs.λz.z |}];
  Printf.printf "church_one = %s" (eval (App (church_succ, church_zero)));
  [%expect {| church_one = λs.λz.(s z) |}];
  Printf.printf
    "church_two = %s"
    (eval (App (church_succ, App (church_succ, church_zero))));
  [%expect {| church_two = λs.λz.(s (s z)) |}];
  ()

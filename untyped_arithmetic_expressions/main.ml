type 'a term =
  | True : bool term
  | False : bool term
  | If : (bool term * 'a term * 'a term) -> 'a term
  | Zero : int term
  | Succ : int term -> int term
  | Pred : int term -> int term
  | IsZero : int term -> bool term

let eval t =
  let rec it_to_int acc = function
    | Zero -> acc
    | Succ t -> it_to_int (acc + 1) t
    | Pred t -> it_to_int (acc - 1) t
    | _ -> assert false
  in
  let to_string : type a. a term -> string = function
    | True -> "true"
    | False -> "false"
    | Zero -> "0"
    | Succ t -> t |> it_to_int 1 |> string_of_int
    | Pred t -> t |> it_to_int (-1) |> string_of_int
    | _ -> assert false
  in
  let rec aux : type a. a term -> a term = function
    | True -> True
    | False -> False
    | If (c, t1, t2) -> (
      match aux c with
        | True -> aux t1
        | False -> aux t2
        | _ -> assert false
    )
    | Zero -> Zero
    | Succ t -> (
      match aux t with
        | Pred t -> t
        | t -> Succ t
    )
    | Pred t -> (
      match aux t with
        | Succ t -> t
        | t -> Pred t
    )
    | IsZero t -> (
      match aux t with
        | Zero -> True
        | _ -> False
    )
  in
  t |> aux |> to_string

let%expect_test "eval" =
  Printf.printf "%s" (eval Zero);
  [%expect {| 0 |}];
  Printf.printf "%s" (eval (Succ Zero));
  [%expect {| 1 |}];
  Printf.printf "%s" (eval (Succ (Succ Zero)));
  [%expect {| 2 |}];
  Printf.printf "%s" (eval (Pred Zero));
  [%expect {| -1 |}];
  Printf.printf "%s" (eval (Pred (Pred Zero)));
  [%expect {| -2 |}];
  Printf.printf "%s" (eval (Succ (Pred Zero)));
  [%expect {| 0 |}];
  Printf.printf "%s" (eval (Pred (Succ Zero)));
  [%expect {| 0 |}];
  Printf.printf "%s" (eval (If (True, True, False)));
  [%expect {| true |}];
  Printf.printf "%s" (eval (If (False, True, False)));
  [%expect {| false |}];
  Printf.printf "%s" (eval (If (IsZero (Pred (Succ Zero)), True, False)));
  [%expect {| true |}];
  Printf.printf "%s" (eval (If (IsZero (Pred (Pred (Pred Zero))), True, False)));
  [%expect {| false |}];
  ()

type t =
    | Halt
    | Refer of int * int
    | Constant of int * int
    | Close of int * int * int
    | Test of int * int
    | Assign of int * int
    | Conti of int
    | Unate of int * int
    | Frame of int * int
    | Argument of int
    | Apply
    | Return
[@@deriving show]

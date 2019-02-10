type t =
    | Halt
    (* variable, next_expression *)
    | Refer of string * t
    (* data, next_expression *)
    | Constant of Ast.datum * t
    (* args, fn_body, next_expression *)
    | Closure of string list * t * t
    (* next_then, next_else *)
    | Test of t * t
    (* variable, next_expression *)
    | Assign of string * t
    (* next_expression *)
    | Continuate of t
    (* stack, variable *)
    | Nuate of stack list * string
    (* stack_return_expression, next_expression *)
    | Frame of t * t
    (* next_expression *)
    | Argument of t
    | Apply
    | Return
[@@deriving show]

and ret =
    | N
    | C of closure
    | B_bin of (ret * ret -> ret)
    | D of Ast.datum
[@@deriving show]

and env = (string * ret) list [@@deriving show]

and closure = t * env * string list [@@deriving show]

and stack = t * env * ret list [@@deriving show]

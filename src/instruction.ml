type t =
    | Halt
    (* variable, next_expression *)
    | Refer of string * t
    (* data, next_expression *)
    | Constant of Ast.datum * t
    (* args, fn_body, next_expression *)
    | Closure of string list * t * t
    (* next_then, next_else *)
    | Test of t * t option
    (* variable, next_expression *)
    | Assign of string * t
    (* next_expression *)
    | Continuation of t
    (* stack, variable *)
    (* | Restore of stack * string *)
    (* next_expression, stack_return_expression *)
    | Frame of t * t
    (* next_expression *)
    | Argument of t
    | Apply
    | Return
[@@deriving show]

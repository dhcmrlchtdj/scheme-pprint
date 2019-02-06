open! Containers

let compile (ast : Ast.t) : Instruction.t list =
    let open Ast in
    let open Instruction in
    let rec expr2inst (next : Instruction.t) = function
        | Symbol x -> Refer (x, next)
        | Bool x -> Constant (B x, next)
        | Num x -> Constant (Ast.N x, next)
        | Str x -> Constant (S x, next)
        | Quote d -> Constant (d, next)
        | Lambda (params, exp) ->
            let body = expr2inst Return exp in
            Closure (params, body, next)
        | If (cond, exp1, exp2) ->
            let then_next = expr2inst next exp1 in
            let else_next = expr2inst next exp2 in
            let next_cond = Test (then_next, else_next) in
            expr2inst next_cond cond
        | Set (id, exp) ->
            let set_next = Assign (id, next) in
            expr2inst set_next exp
        | CallCC exp ->
            let k_next = expr2inst Apply exp in
            let cc_next = Continuate k_next in
            (match next with Return -> cc_next | _ -> Frame (next, cc_next))
        | Application (proc, args) ->
            let fn_next = expr2inst Apply proc in
            let rec aux n = function
                | [] -> n
                | h :: t ->
                    let nn = expr2inst (Argument n) h in
                    aux nn t
            in
            let args_next = aux fn_next args in
            (match next with Return -> args_next | _ -> Frame (next, args_next))
    in
    List.map (expr2inst Halt) ast

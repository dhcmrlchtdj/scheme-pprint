open! Containers

let compile (ast : Ast.t) : Instruction.t =
    match ast with Ast.Str s -> Instruction.Str s | _ -> failwith "ha"

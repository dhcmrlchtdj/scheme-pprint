open! Containers

let run (ast : Instruction.t) : string =
    match ast with Instruction.Str s -> s

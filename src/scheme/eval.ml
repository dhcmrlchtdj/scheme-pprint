module Env = struct
    type t = int list
    let empty = []
end


open Ast


let eval (_:expression) =
    0

let print = print_int

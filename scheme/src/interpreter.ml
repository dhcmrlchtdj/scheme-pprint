open! Containers
module I = Instruction
module Assoc = List.Assoc

let env_update key value env =
    if Assoc.mem ~eq:String.equal key env
    then Assoc.set ~eq:String.equal key value env
    else raise Not_found


let rec env_extend (keys : string list) (values : I.ret list) (env : I.env) :
    I.env =
    match (keys, values) with
        | [], [] -> env
        | hkey :: tkey, hval :: tval ->
            let env2 = Assoc.set ~eq:String.equal hkey hval env in
            env_extend tkey tval env2
        | _, _ -> failwith "[env_extend] never"


let env_lookup key (env : I.env) = Assoc.get_exn ~eq:String.equal key env

let ret2string = function
    | I.N -> "nil"
    | I.C _ -> "<function>"
    | I.B_bin _ -> "<builtin function>"
    | I.D datum ->
        let rec p_datum = function
            | Ast.Q s -> Printf.sprintf "%s" s
            | Ast.S s -> Printf.sprintf "%S" s
            | Ast.B b -> if b then "#t" else "#f"
            | Ast.N f -> Printf.sprintf "%F" f
            | Ast.L ds ->
                Printf.sprintf "(%s)" (String.concat " " (List.map p_datum ds))
        in
        p_datum datum


let stdenv =
    [ ( "+"
      , I.B_bin
              (function
                  | I.D (Ast.N x), I.D (Ast.N y) -> I.D (Ast.N (x +. y))
                  | _ -> failwith "invalid") ) ]


let interpret (inst : Instruction.t) : string =
    let open Instruction in
    let rec aux
            (acc : I.ret)
            (inst : I.t)
            (env : I.env)
            (args : I.ret list)
            (stacks : I.stack list) : I.ret =
        match inst with
            | Halt -> acc
            | Refer (name, next) ->
                let value = env_lookup name env in
                aux value next env args stacks
            | Constant (datum, next) -> aux (D datum) next env args stacks
            | Closure (params, body, next) ->
                let fn = (body, env, params) in
                aux (C fn) next env args stacks
            | Test (n1, n2) ->
                let next =
                    match acc with N -> n2 | C _ -> n1 | D _ -> n1 | B_bin _ -> n1
                in
                aux acc next env args stacks
            | Assign (name, next) ->
                let env2 = env_update name acc env in
                aux acc next env2 args stacks
            | Continuate next ->
                let inst = Instruction.Nuate (stacks, "v") in
                let fn = (inst, [], ["v"]) in
                aux (C fn) next env args stacks
            | Nuate (stacks2, name) ->
                let acc2 = env_lookup name env in
                aux acc2 Return env args stacks2
            | Frame (r, next) ->
                let new_stacks = (r, env, args) :: stacks in
                aux acc next env [] new_stacks
            | Argument next ->
                let args = acc :: args in
                aux acc next env args stacks
            | Apply ->
                (match acc with
                    | N | D _ -> failwith "not a function"
                    | C (next, env2, params) ->
                        let env3 = env_extend params args env2 in
                        aux acc next env3 [] stacks
                    | B_bin fn ->
                        let acc2 =
                            match args with
                                | [x; y] -> fn (x, y)
                                | _ -> failwith "invalid bin fn"
                        in
                        aux acc2 Return env [] stacks)
            | Return ->
                (match stacks with
                    | (next, env2, args2) :: stacks2 -> aux acc next env2 args2 stacks2
                    | _ -> failwith "")
    in
    let r = aux N inst stdenv [] [] in
    ret2string r

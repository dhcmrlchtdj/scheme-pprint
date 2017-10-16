type vm_val =
    | I of int
    | C of vm_inst * (string list) * vm_env
and vm_inst =
    | Halt
    | Refer of string * vm_inst
    | Constant of vm_val * vm_inst
    | Closure of (string list) * vm_inst * vm_inst
    | Test of vm_inst * vm_inst
    | Assign of string * vm_inst
    | Continuation of vm_inst
    | Nuate of vm_stack * string
    | Frame of vm_inst * vm_inst
    | Argument of vm_inst
    | Apply
    | Return
and vm_env = (string * vm_val) list
and vm_rib = vm_val list
and vm_stack =
    | EmptyStack
    | Stack of vm_inst * vm_env * vm_rib * vm_stack


type expression =
    | Integer of int
    | Symbol of string
    | Lambda of (string list) * expression
    | If of expression * expression * expression
    | Set of string * expression
    | CallCC of expression
    | Apply2 of expression * (expression list)


let rec lookup (env:vm_env) (k:string) : vm_val = I 0
let rec extend (env:vm_env) (k:string) (v:vm_val) : vm_env = env

let conti (s:vm_stack) : vm_val = I 0


let rec compile (exp:expression) (next:vm_inst) : vm_inst = next


let rec null a = true


let rec vm (accumulator:vm_val) (next_inst:vm_inst) (curr_env:vm_env) (curr_rib:vm_rib) (curr_stack:vm_stack) : vm_val =
    match next_inst with
        (* return accumulator *)
        | Halt ->
            accumulator

        (* find var from curr_env *)
        (* place the value into accumulator *)
        (* set next_inst to inst *)
        | Refer (var, inst) ->
            let accu = lookup curr_env var in
            vm accu inst curr_env curr_rib curr_stack

        (* place obj into accumulator *)
        (* set next_inst to inst *)
        | Constant (obj, inst) ->
            vm obj inst curr_env curr_rib curr_stack

        (* create closure from body, vars, curr_env *)
        (* place the closure into accumulator *)
        (* set next_inst to inst *)
        | Closure (vars, body, inst) ->
            let accu = C (body, vars, curr_env) in
            vm accu inst curr_env curr_rib curr_stack

        (* test accumulator *)
        (* set next_inst to then or else *)
        | Test (then_, else_) ->
            let test = (match accumulator with
                | I _ -> true
                | C _ -> true) in
            let inst = if test then then_ else else_ in
            vm accumulator inst curr_env curr_rib curr_stack

        (* bind var to accumulator in curr_env *)
        (* set next_inst to inst *)
        | Assign (var, inst) ->
            let env = extend curr_env var accumulator in
            vm accumulator inst env curr_rib curr_stack

        (* create continuation from curr_stack *)
        (* place the continuation to accumulator *)
        (* set next_inst to inst *)
        | Continuation inst ->
            (* TODO *)
            let accu = conti curr_stack in
            vm accu inst curr_env curr_rib curr_stack

        (* set curr_stack to stack *)
        (* find var from curr_env *)
        (* place the value to accumulator *)
        (* set next_inst to Return *)
        | Nuate (stack, var) ->
            let accu = lookup curr_env var in
            vm accu Return curr_env curr_rib stack

        (* create frame from curr_env, curr_rib, ret as next_inst *)
        (* add the frame to curr_stack *)
        (* set current_rib to empty *)
        (* set next_inst to instruction *)
        | Frame (inst, ret) ->
            let stack = Stack (ret, curr_env, curr_rib, curr_stack) in
            vm accumulator inst curr_env [] stack

        (* add accumulator to curr_rib *)
        (* set next_inst to inst *)
        | Argument inst ->
            let rib = accumulator :: curr_rib in
            vm accumulator inst curr_env rib curr_stack

        (* apply closure in accumulator to curr_rib *)
        (* create new environment by closure's variable list and curr_rib *)
        (* set curr_env to the new environment *)
        (* set curr_rib to empty *)
        (* set next_inst to the closure's body *)
        | Apply ->
            vm accumulator body env [] curr_stack

        (* remove the first frame from curr_stack *)
        (* reset curr_env, curr_rib, next_inst, curr_stack *)
        | Return -> (
                match curr_stack with
                    | Stack (inst, env, rib, stack) ->
                        vm accumulator inst env rib stack
                    | _ -> failwith "inst(Return): empty stack"
            )


let evaluate (exp:expression) : vm_val =
    let ins = compile exp Halt in
    let ret = vm (I 0) ins [] [] EmptyStack in
    ret

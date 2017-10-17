open Ast

let rec lookup ((n, e):(string*lobject)) : lobject =
    match e with
        | Nil -> failwith "not found"
        | Pair (Pair (Symbol x, v), _) when x = n -> v
        | Pair (_, t) -> lookup (n, t)
        | _ -> failwith "this can't happen"
and bind ((n, v, e):(string*lobject*lobject)) : lobject =
    Pair (Pair (Symbol n, v), e)

let basis =
    let prim_plus = function
        | [Fixnum(a); Fixnum(b)] -> Fixnum(a+b)
        | _ -> failwith "type error +"
    in
    let prim_pair = function
        | [a; b] -> Pair(a, b)
        | _ -> failwith "type error pair"
    in
    let rec prim_list = function
        | [] -> Nil
        | car::cdr -> Pair (car, prim_list cdr)
    in
    let newprim acc (name, func) =
        bind (name, Primitive(name, func), acc)
    in
    List.fold_left newprim Nil [
        ("+", prim_plus);
        ("pair", prim_pair);
        ("list", prim_list);
    ]

let rec eval_sexp (sexp:lobject) (env:lobject) : (lobject * lobject) =
    match sexp with
        | Fixnum _
        | Boolean _
        | Primitive _
        | Nil -> (sexp, env)
        | Symbol name -> (lookup (name, env), env)
        | Pair _ when is_list sexp -> (
                match pair_to_list sexp with
                    | [Symbol "if"; cond; t; e] ->
                        (match eval_sexp cond env with
                            | (Boolean true, e2) -> eval_sexp t e2
                            | (Boolean false, e2) -> eval_sexp e e2
                            | _ -> failwith "eval if, type error")
                    | [Symbol "set!"; Symbol name; exp] ->
                        let (v, e) = eval_sexp exp env in
                        let ee = bind (name, v, e) in
                        (v, ee)
                    | [Symbol "pair"; l; r] ->
                        let (ll, env1) = eval_sexp l env in
                        let (rr, env2) = eval_sexp r env1 in
                        (Pair (ll, rr), env2)
                    | [Symbol "env"] ->
                        (env, env)
                    | (Symbol fn)::args ->
                        (match eval_sexp (Symbol fn) env with
                            | (Primitive (_, f), env2) -> (f args, env2)
                            | _ -> failwith "eval function")
                    | _ -> (sexp, env)
            )
        | Pair _ -> (sexp, env)

let rec eval (ast:exp) (env:lobject) : (value * lobject) =
    match ast with
        | Defexp d -> evaldef d env
        | e -> (evalexp e env, env)
and evaldef (d:def) (env:lobject) =
    match d with
        | Val (name, e) ->
            let v = evalexp e env in
            (v, bind (name, v, env))
        | Exp e -> (evalexp e env, env)
and evalexp (e:exp) (env:lobject) : 

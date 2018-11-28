open! Containers

let p f = function
    | `Stdin -> IO.read_all stdin |> f
    | `File file -> IO.File.read_exn file |> f


let print_ast s = s |> Parser.parse |> Ast.show |> print_endline

let print_inst s =
    s |> Parser.parse |> Compiler.compile |> Instruction.show |> print_endline


let print_val s =
    s |> Parser.parse |> Compiler.compile |> Vm.run |> print_endline


let () =
    let exe = Sys.argv.(0) in
    let usage () = Printf.printf "Usage: %s [-dast | -dinst] [file | -]\n" exe in
    let argv = Sys.argv |> Array.to_list |> List.tl in
    let aux = function
        | ["-h"] | ["--help"] -> usage ()
        | ["-dast"; "-"] -> p print_ast `Stdin
        | ["-dast"; file] -> p print_ast (`File file)
        | ["-dinst"; "-"] -> p print_inst `Stdin
        | ["-dinst"; file] -> p print_inst (`File file)
        | ["-"] -> p print_val `Stdin
        | [file] -> p print_val (`File file)
        | _ -> usage ()
    in
    aux argv

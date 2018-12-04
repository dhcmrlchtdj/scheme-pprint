open! Containers

let p f = function
    | `Stdin -> IO.read_all stdin |> f
    | `File file -> IO.File.read_exn file |> f


let print_token =
    p (fun s ->
        s
        |> Scanner.scan
        |> List.map Token.show
        |> String.concat "\n"
        |> print_endline )


let print_ast =
    p (fun s -> s |> Scanner.scan |> Parser.parse |> Ast.show |> print_endline)


let print_fmt =
    p (fun s ->
        s
        |> Scanner.scan
        |> Parser.parse
        |> PrettyPrinter.to_string
        |> print_endline )


let print_inst =
    p (fun s ->
        s
        |> Scanner.scan
        |> Parser.parse
        |> Compiler.compile
        |> List.map Instruction.show
        |> String.concat "\n"
        |> print_endline )


let print_val =
    p (fun s ->
        s
        |> Scanner.scan
        |> Parser.parse
        |> Compiler.compile
        |> Vm.run
        |> print_endline )


let () =
    let exe = Sys.argv.(0) in
    let usage () =
        Printf.printf "Usage: %s [-token | -ast | -fmt | -inst] [file | -]\n" exe
    in
    let argv = Sys.argv |> Array.to_list |> List.tl in
    let aux = function
        | ["-h"] -> usage ()
        | ["-token"; "-"] -> print_token `Stdin
        | ["-token"; file] -> print_token (`File file)
        | ["-ast"; "-"] -> print_ast `Stdin
        | ["-ast"; file] -> print_ast (`File file)
        | ["-fmt"; "-"] -> print_fmt `Stdin
        | ["-fmt"; file] -> print_fmt (`File file)
        | ["-inst"; "-"] -> print_inst `Stdin
        | ["-inst"; file] -> print_inst (`File file)
        | ["-"] -> print_val `Stdin
        | [file] -> print_val (`File file)
        | _ -> usage ()
    in
    aux argv

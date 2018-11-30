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
        |> Instruction.show
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
    let usage () = Printf.printf "Usage: %s [-dast | -dinst] [file | -]\n" exe in
    let argv = Sys.argv |> Array.to_list |> List.tl in
    let aux = function
        | ["-dtoken"; "-"] -> print_token `Stdin
        | ["-dtoken"; file] -> print_token (`File file)
        | ["-dast"; "-"] -> print_ast `Stdin
        | ["-dast"; file] -> print_ast (`File file)
        | ["-dinst"; "-"] -> print_inst `Stdin
        | ["-dinst"; file] -> print_inst (`File file)
        | ["-"] -> print_val `Stdin
        | [file] -> print_val (`File file)
        | _ -> usage ()
    in
    aux argv

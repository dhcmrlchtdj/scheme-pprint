let read_all (ic: in_channel) : string =
    let rec aux acc =
        match
            try
                let line = input_line ic in
                Some (line :: acc)
            with End_of_file -> None
        with
            | None -> acc
            | Some acc2 -> aux acc2
    in
    aux [] |> List.rev |> String.concat "\n"


let pp (s: string) : unit =
    let exp = Parser.parse s in
    print_endline s ;
    print_endline (Prettier.to_string exp) ;
    print_endline (Ast.to_string exp) ;
    ()


let () =
    let prog = Sys.argv.(0) in
    let usage () =
        Printf.printf "Usage: %s [file | -]\n" prog
    in
    let argv = Sys.argv |> Array.to_list |> List.tl in
    let aux = function
        | ["-h"] | ["--help"] -> usage ()
        | ["-"] ->
            let s = read_all stdin in
            pp s
        | [file] ->
            let ic = open_in file in
            let s = read_all ic in
            close_in_noerr ic ; pp s
        | [] | _ -> usage ()
    in
    aux argv


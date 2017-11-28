module P = Printf

let pp (ss: string) : unit =
    let s = ss |> String.trim in
    let exp = Parser.parse s in
    P.printf "--------input\n%S\n" s ;
    P.printf "--------oneline\n%s\n" (Ast.to_string exp) ;
    P.printf "--------dump\n%s\n" (Ast.dump exp) ;
    P.printf "--------pretty\n%s\n" (Pretty.print exp) ;
    ()


let () =
    let prog = Sys.argv.(0) in
    let usage () = P.printf "Usage: %s [file | -]\n" prog in
    let argv = Sys.argv |> Array.to_list |> List.tl in
    let aux = function
        | ["-h"] | ["--help"] -> usage ()
        | ["-"] -> BatIO.read_all BatIO.stdin |> pp
        | [file] -> BatFile.with_file_in file BatIO.read_all |> pp
        | [] | _ -> usage ()
    in
    aux argv

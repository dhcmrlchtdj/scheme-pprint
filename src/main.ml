let _ =
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.main Lexer.token lexbuf in
    print_endline (match ast with
        | Some x -> Ast.to_string x
        | None -> "None");

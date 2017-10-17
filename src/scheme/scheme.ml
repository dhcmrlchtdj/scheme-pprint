let () =
    let tokens = Lexer.read () in
    let ast = Parser.parse tokens in
    let value = Eval.eval ast in
    Eval.print value

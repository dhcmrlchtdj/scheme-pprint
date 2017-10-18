let () =
    let tokens = Lexer.scan () in
    let ast = Parser.parse tokens in
    let value = Eval.eval ast in
    Eval.print value

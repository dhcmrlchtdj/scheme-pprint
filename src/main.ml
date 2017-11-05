let () =
    let s = "(((call/cc (lambda (c) c)) (lambda (x) x)) 'HEY!)" in
    let exp = Parser.parse s in
    print_endline (Parser.to_string exp);
    ()

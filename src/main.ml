let () =
    let s = "(   fn 123.13.)" in
    let exp = Parser.parse s in
    print_endline (Parser.to_string exp);
    ()

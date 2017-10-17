open Parser

let rec repl (stm:stream) (env:Ast.lobject) : unit =
    print_string "> ";
    flush stdout;
    let sexp = read_sexp stm in
    let ast = build_ast sexp in
    let (answer, env2) = Eval.eval ast env in
    Ast.print_value answer;
    print_newline ();
    repl stm env2

let () =
    let stm = { chr=[]; line_num=1; chan=stdin } in
    repl stm Ast.Nil

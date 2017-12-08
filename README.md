# scheme pretty printer

---

```
$ make test

--------input
"(((call/cc (lambda (c) c)) (lambda (x) x)) 'HEY!)"

--------dump
(Ast.Lst
   [(Ast.Lst
       [(Ast.Lst
           [(Ast.Sym "call/cc");
             (Ast.Lst
                [(Ast.Sym "lambda"); (Ast.Lst [(Ast.Sym "c")]); (Ast.Sym "c")
                  ])
             ]);
         (Ast.Lst
            [(Ast.Sym "lambda"); (Ast.Lst [(Ast.Sym "x")]); (Ast.Sym "x")])
         ]);
     (Ast.Lst [(Ast.Sym "quote"); (Ast.Sym "HEY!")])])

--------oneline
(((call/cc (lambda (c) c)) (lambda (x) x)) (quote HEY!))

--------pretty_simple
(((call/cc
            (lambda
                (c)
                c))
        (lambda
            (x)
            x))
    (quote
        HEY!))

--------pretty
(((call/cc (lambda (c) c))
        (lambda (x) x))
    (quote HEY!))
```

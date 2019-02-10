# schemer

A <del>R5RS</del> VM

```sh
$ opam install ocamlbuild containers ppx_deriving # dep
$ opam install merlin ocamlformat ocp-indent # dev dep

$ make # build
$ ./main -h # usage

$ echo "#f" | ./main -
$ #f
$ echo "(+ (+ 1 2) 3)" | ./main -
$ 6.
$ echo "(((call/cc (lambda (c) c)) (lambda (x) x)) 'HEY)" | ./main -
$ HEY

$ make test # run example
```

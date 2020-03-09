# schemer

A <del>R5RS</del> VM

```sh
$ make dev

$ make build
$ ./_build/default/src/main.exe -h # usage

$ echo "#f" | ./_build/default/src/main.exe -
$ #f
$ echo "(+ (+ 1 2) 3)" | ./_build/default/src/main.exe -
$ 6.
$ echo "(((call/cc (lambda (c) c)) (lambda (x) x)) 'HEY)" | ./_build/default/src/main.exe -
$ HEY
```

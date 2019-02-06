mlis := $(patsubst %.ml,%,$(wildcard src/*.ml))
# prog := "(+ (+ 1 2) 3)"
# prog := "'HEY"
# prog := "(lambda (c) c)"
# prog := "((lambda (x) x) 'HEY)"
# prog := "(call/cc (lambda (c) c))"
# prog := "((call/cc (lambda (c) c)) (lambda (x) x))"
prog := "(((call/cc (lambda (c) c)) (lambda (x) x)) 'HEY)"

.PHONY: main
main: byte

.PHONY: test
test: main
	@ echo $(prog) ; echo ""
	@ echo $(prog) | ./main -fmt - ; echo ""
	@ echo $(prog) | ./main -token - ; echo ""
	@ echo $(prog) | ./main -ast - ; echo ""
	@ echo $(prog) | ./main -inst - ; echo ""
	@ echo $(prog) | ocamlrun -b ./main - ; echo ""

.PHONY: byte
byte: $(mlis)
	@ ocamlbuild -use-ocamlfind src/main.byte
	@ ln -sf ./main.byte ./main

.PHONY: $(mlis)
$(mlis):
	-@ ocamlbuild -use-ocamlfind $@.inferred.mli

.PHONY: clean
clean:
	@ ocamlbuild -clean
	@ rm -rf ./main
	@ rm -rf ./main.byte
	@ rm -rf ./main.native

.PHONY: fmt
fmt:
	ocamlformat -i src/*.ml
	ocp-indent -i src/*.ml

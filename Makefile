OCB_FLAGS := \
	-tag 'color(always)' \
	-tags safe_string,short_paths,strict_sequence,keep_locs,keep_docs,principal \
	-use-ocamlfind \
	-pkg ppx_deriving.std \
	-pkg batteries \
	-tags 'warn(+a-4-41-44),warn_error(-a+31)'
OCB := ocamlbuild $(OCB_FLAGS)

mlis := $(patsubst %.ml,%,$(wildcard src/*.ml))

test: main
	echo "(((call/cc (lambda (c) c)) (lambda (x) x)) 'HEY!)" | ./main.byte -
	echo "(define (main) (define (print-usage) (display \"\\nUsage:\\n./prime.ss <number>\\n\\n\")) (define args (cdr (command-line))) (if (not (= 1. (length args))) (print-usage) (let ((nth (string->number (car args)))) (if (not nth) (print-usage) (begin (display (stream-ref primes nth)) (newline))))))" | ./main.byte -

main: $(mlis)
	@$(OCB) src/main.byte

$(mlis):
	@$(OCB) $@.inferred.mli

clean:
	@ocamlbuild -clean

.PHONY: main clean $(mlis)

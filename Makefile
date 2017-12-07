OCB_FLAGS := \
	-tag 'color(always)' \
	-tag safe_string \
	-tag short_paths \
	-tag strict_sequence \
	-tag keep_locs \
	-tag keep_docs \
	-tag bin_annot \
	-tag principal \
	-tag thread \
	-tag nopervasives \
	-use-ocamlfind \
	-pkg ppx_deriving.std \
	-pkg batteries \
	-tags 'warn(+a-4),warn_error(-a+31)'
OCB := ocamlbuild $(OCB_FLAGS)

mlis := $(patsubst %.ml,%,$(wildcard src/*.ml))

main: $(mlis)
	@$(OCB) src/main.byte

test: main
	@echo "(((call/cc (lambda (c) c)) (lambda (x) x)) 'HEY!)" | ./main.byte -
	@echo "(define (main) (define (print-usage) (display \"\\nUsage:\\n./prime.ss <number>\\n\\n\")) (define args (cdr (command-line))) (if (not (= 1. (length args))) (print-usage) (let ((nth (string->number (car args)))) (if (not nth) (print-usage) (begin (display (stream-ref primes nth)) (newline))))))" | ./main.byte -
	@echo "(define (stream-car s) (car (force s)))" | ./main.byte -
	@echo "(lambda (x) x)" | ./main.byte -

$(mlis):
	@$(OCB) $@.inferred.mli

clean:
	@ocamlbuild -clean

.PHONY: main clean $(mlis)

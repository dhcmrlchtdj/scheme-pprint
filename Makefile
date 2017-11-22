OCB_FLAGS := \
	-tag 'color(always)' \
	-tags safe_string,short_paths,strict_sequence,keep_locs,keep_docs,principal \
	-use-ocamlfind \
	-pkg ppx_deriving.std \
	-pkg batteries \
	-tags 'warn(+a-4),warn_error(-a+31)'
OCB := ocamlbuild $(OCB_FLAGS)

mlis := $(patsubst %.ml,%,$(wildcard src/*.ml))

test: main
	echo "(((call/cc (lambda (c) c)) (lambda (x) x)) 'HEY!)" | ./main.byte -

main: $(mlis)
	@$(OCB) src/main.byte

$(mlis):
	@$(OCB) $@.inferred.mli

clean:
	@ocamlbuild -clean

.PHONY: main clean $(mlis)

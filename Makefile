OCB_FLAGS := \
	-tag 'color(always)' \
	-tags safe_string,short_paths,strict_sequence,keep_locs,keep_docs,principal \
	-use-ocamlfind -pkgs 'str,ppx_deriving.std' \
	-tags 'warn(+a),warn_error(-a+31)'
OCB := ocamlbuild $(OCB_FLAGS)

mlis := $(patsubst %.ml,%,$(wildcard src/*.ml))

main: $(mlis)
	@$(OCB) src/main.byte

$(mlis):
	@$(OCB) $@.inferred.mli

clean:
	@ocamlbuild -clean

.PHONY: main clean $(mlis)

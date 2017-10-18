OCB_FLAGS := -tag 'color(always)' \
	-tag 'warn(@A-4-27-60-39-33)' \
	-tags safe_string,strict_sequence,strict_formats,short_paths,keep_locs \
	-use-menhir -tag explain \
	-use-ocamlfind -pkgs 'str'
OCB := ocamlbuild $(OCB_FLAGS)

mlis := $(patsubst %.ml,%,$(wildcard src/*.ml))

main: $(mlis)
	@$(OCB) src/main.byte

$(mlis):
	@$(OCB) $@.inferred.mli

clean:
	@ocamlbuild -clean

.PHONY: main clean $(mlis)

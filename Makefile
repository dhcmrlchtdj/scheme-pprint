OCB_FLAGS := -tag 'color(always)' \
	-tag 'warn(@A-4-27-60-39-33)' \
	-tags safe_string,strict_sequence,strict_formats,short_paths,keep_locs \
	-use-menhir -tag explain \
	-use-ocamlfind -pkgs 'str'
OCB := ocamlbuild $(OCB_FLAGS)

main: scheme

clean:
	@ocamlbuild -clean

scheme_targets := $(patsubst %.ml,%,$(wildcard src/scheme/*.ml))
$(scheme_targets):
	@$(OCB) $@.inferred.mli
scheme: $(scheme_targets)
	@$(OCB) src/scheme/scheme.byte

.PHONY: main clean scheme $(scheme_targets)

test: main
	cat test.ss | ./main.byte

clean:
	@ocamlbuild -clean

OCB_FLAGS := -tag 'color(always)' \
	-tag 'warn(@A-4-27-60-39)' \
	-tag safe_string \
	-tag strict_sequence \
	-tag strict_formats \
	-tag short_paths \
	-tag keep_locs \
	-use-menhir \
	-use-ocamlfind \
	-pkgs 'str'
OCB := ocamlbuild $(OCB_FLAGS)

main:
	@$(OCB) src/main.{byte,inferred.mli}

.PHONY: test clean main

test: main
	cat test.ss | ./main.byte

clean:
	@ocamlbuild -clean

OCB_FLAGS := -tag 'color(always)' \
	-tag 'warn(@A-4-27-60-39-33)' \
	-tags safe_string,strict_sequence,strict_formats,short_paths,keep_locs \
	-use-menhir -tag explain \
	-use-ocamlfind -pkgs 'str'
OCB := ocamlbuild $(OCB_FLAGS)

main:
	@$(OCB) src/main.{byte,inferred.mli}

.PHONY: test clean main

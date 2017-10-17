OCB_FLAGS := -tag 'color(always)' \
	-tag 'warn(@A-4-27-60-39-33)' \
	-tags safe_string,strict_sequence,strict_formats,short_paths,keep_locs \
	-use-ocamlfind -pkgs 'str'
OCB := ocamlbuild $(OCB_FLAGS)

main:
	@$(OCB) src/main.{byte,inferred.mli}

clean:
	@ocamlbuild -clean

.PHONY: main clean

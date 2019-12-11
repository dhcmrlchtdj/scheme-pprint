(lang dune 2.0)

(name schemer)

(version 0.1.0)
(authors "H11 <nirisix@gmail.com>")
(maintainers "H11 <nirisix@gmail.com>")
(source
 (github dhcmrlchtdj/schemer))
(license MIT)

(package
 (name schemer)
 (synopsis "Blueprint for ocaml project")
 (depends
  (containers           (and (>= 2.7)))
  (ppx_deriving         (and :build (>= 4.4)))
  (dune                 (and :build (>= 2.0.0)))
  (ocamlformat          (and :dev (>= 0.12)))
  ))

(generate_opam_files true)

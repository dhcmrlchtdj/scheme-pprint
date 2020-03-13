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
 (synopsis "Interpreter")
 (depends
  (containers           (and (>= 2.7)))
  (ppx_deriving         (and :build (>= 4.4)))
  (dune                 (and :build (>= 2.0.0)))
  (ppx_expect           (and :with-test (>= v0.13.0)))
  (bisect_ppx           (and :with-test (>= 1.4.2)))
  (ocamlformat          (and :dev (>= 0.12)))
  ))

(generate_opam_files true)

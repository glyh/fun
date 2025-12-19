(lang dune 3.19)

(name fun)

(generate_opam_files true)

(source
 (github glyh/fun))

(authors "Yihang Liu")

(maintainers "Yihang Liu")

(license GPL-3.0-or-later)

(documentation https://github.com/glyh/fun)

(using menhir 3.0)

(package
 (name fun)
 (synopsis "A little functional language")
 (description "A little functional language")
 (depends
  dune
  (ocaml
   (>= 5.3))
  ; Library dependencies
  (linenoise
   (>= 1.5.1))
  menhir
  (ppx_deriving
   (>= 6.0.2))
  (ppx_deriving_yojson
   (>= 3.9.1))
  (ppx_here
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (sedlex
   (>= 3.4))
  ; Develop setup
  (alcotest
   (and
    (or :with-test :with-dev-setup)
    (>= 1.9.1)))
  (bisect_ppx
   (and
    (or :with-test :with-dev-setup)
    (>= 2.8.3)))
  (ocamlformat
   (and
    :with-dev-setup
    (>= 0.27.0)))
  (ocaml-lsp-server
   (and
    :with-dev-setup
    (>= 1.22.0))))
 (tags (compiler)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

(lang dune 3.19)

(name fun)

(generate_opam_files true)

(source
 (github username/reponame))

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
  (core
   (>= v0.17.1))
  menhir
  (sedlex
   (>= 3.4))
  (ppx_deriving
   (>= 6.0.2))
  (ppx_deriving_yojson
   (>= 3.9.1))
  ; Develop setup
  (ppx_inline_test 
   (and
    (or :with-test :with-dev-setup)
    (>= v0.17.0)))
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

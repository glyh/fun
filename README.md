# fun

`fun` is an experimental programming language compiler/interpreter written in OCaml.

The current implementation is built around `core_tt`, a dependently typed core with bidirectional elaboration, normalization by evaluation, implicit arguments, nominal ADTs, structural records/modules, pattern matching, and file imports.

```sh
dune build
dune test
dune exec fun
```

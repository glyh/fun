# fun

`fun` is an experimental programming language compiler/interpreter written in OCaml.

The current implementation is built around `core_tt`, a dependently typed core with
bidirectional elaboration, normalization by evaluation, implicit arguments, nominal
ADTs, structural records/modules, pattern matching, traits, algebraic effects,
mutable references, and a hygienic enforestation-based macro system.

```sh
dune build
dune test
dune exec fun            # REPL
```

## Design philosophy

**Consistency > Flexibility > Correctness.**

- **Consistency**: one construct for many roles (`struct` = record / module /
  namespace), types are values, and the core type system carries the language
  model directly.
- **Flexibility**: willing to trade theoretical properties such as parametricity
  for practical power. Type-case on open `Type` is acceptable.
- **Correctness**: still valued through bidirectional checking, normalization,
  unification, and regression tests — but not at the expense of the language's
  practical shape.

The pipeline is a single path from surface syntax to values:

```text
source → Raw_syntax → Enforest → Expand + Lower → Surface.t → Elaborate → Core.term → NbE → value
```

## Where things live

- **[`AGENTS.md`](AGENTS.md)** — how to work in this repo: build/test, source
  layout, conventions, and recurring gotchas.
- **[`docs/STATUS.md`](docs/STATUS.md)** — canonical snapshot of *what is built*.
  When another doc disagrees on completion status, STATUS wins.
- **[`docs/wayfinder/`](docs/wayfinder/)** — the direction map: what has been
  *decided*, what is still *open* (tickets), and what is still *fog*. Start at the
  [Fun compiler design map](docs/wayfinder/fun-design-map.md). This is
  also where the design detail for every completed direction lives (under
  `topics/`), plus the macro-system reference library (`macro-system/`).

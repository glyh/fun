# fun

`fun` is an experimental programming language compiler/interpreter. The implementation is
written in **C# (.NET 10)** under `dotnet/`.

The earlier OCaml prototype was **removed on 2026-09-25** once the port was measured as a
superset of it: `port-fails: 0` over every program in the repo, with the 34 cases where the two
disagreed being ones the prototype got wrong. Its history is in `git log` (`lib/`, `bin/`, the
`dune` files) and in `docs/wayfinder/tickets/`, which was written while it was the reference.

The language is built around `core_tt`, a dependently typed core with
bidirectional elaboration, normalization by evaluation, implicit arguments, nominal
ADTs, structural records/modules, pattern matching, traits, algebraic effects,
mutable references, and a hygienic enforestation-based macro system.

```sh
cd dotnet
dotnet build                                  # the compiler
cd dotnet && dotnet test test/Fun.Tests       # xUnit: internals (shapes, unifier, machine, budget)
cd dotnet && dotnet run --project test/Fun.Conformance   # the shared language suite
cd dotnet && dotnet run --project src/Fun.Cli            # REPL
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

- **[`CLAUDE.md`](CLAUDE.md)** — how to work in this repo: build/test, source
  layout, conventions, and recurring gotchas.
- **[`docs/STATUS.md`](docs/STATUS.md)** — canonical snapshot of *what is built*.
  When another doc disagrees on completion status, STATUS wins.
- **[`docs/wayfinder/`](docs/wayfinder/)** — the direction map: what has been
  *decided*, what is still *open* (tickets), and what is still *fog*. Start at the
  [Fun compiler design map](docs/wayfinder/fun-design-map.md). This is
  also where the design detail for every completed direction lives (under
  `topics/`), plus the macro-system reference library (`macro-system/`).

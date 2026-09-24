# Scripts

## `differential.sh` — the differential harness

Runs **every `.fun` program in the repo** through both runners and compares
outcomes, so "C# covers every feature the OCaml prototype has" is bounded
rather than sampled by the 736 shared cases.

```sh
dune build bin/differential.exe            # the OCaml half (bin/differential.ml)
dotnet build dotnet/test/Fun.Conformance   # the C# half (--file mode)
scripts/differential.sh
```

### What it enumerates

`find . -name '*.fun'` minus `_build`, `bin`, `obj` — i.e.
`test/conformance/cases/**` (736 cases + 54 `.unit-*.fun` units),
`dotnet/std/*.fun` (the port's prelude copy), and anything else (today: nothing
else exists; `test/**` has no `.fun` fixtures outside `cases/`).

### The two runners

- **OCaml** — `bin/differential.ml` (`_build/default/bin/differential.exe`),
  a new executable that mirrors `test/conformance/run_conformance.ml`'s
  `elaborate_case` + `describe_value` + `Elaborate.Ctx.run`, so a program is
  judged exactly as the conformance suite judges it. It reads the file's
  sibling `<name>.unit-<unit>.fun` units into a temp loader, so `import` cases
  run with their real imports.
- **C#** — `dotnet test/Fun.Conformance` gained a `--file <path>` mode that
  reuses the same `Driver.Elaborate` / `Driver.Describe` / `Driver.Run` shape as
  a normal case, with the suite's honest accounting: an unported path is a
  failure, an invariant failure is a failure.

Both print one line per program:

| Line | Meaning |
|---|---|
| `VALUE <s>` | the program evaluated to `<s>` |
| `OK` | the program elaborates; sibling `.expect` is `ok`, so it is not run |
| `ELAB <msg>` | expansion/elaboration failed |
| `EVAL <msg>` | evaluation failed |

A hang is not a line: the harness runs each half under `timeout -k 5 60` and
records `HANG` itself (the OCaml prototype hangs on some inputs).

### Normalization

Error texts differ between the runners, so the comparison is on **classes**
(`VALUE` / `OK` / `ELAB` / `EVAL` / `HANG`), never on message text. A value is
normalized identically on both sides: `describe_value` (OCaml) and
`Driver.Describe` (C#) both map an `I64` to its digits and a constructor to its
bare name. What that loses: two different *errors* of the same class count as
agreement — this harness cannot see a wording-only divergence, and it cannot see
a divergence in the *spine* of a constructor value (e.g. `Cons(1, Nil)` vs
`Cons`), because neither `describe_value` nor `Driver.Describe` prints spines.
Both are by design: the conformance suite's `.expect` never states spines, and
pinning error wording would make the suite untestable for a second
implementation.

### Classification

Agreement is decided first: same class (`VALUE` / `OK` / `ELAB` / `EVAL`), and
for a value the same normalized string. Only a disagreement is adjudicated,
against the case's `.expect`, to name the wrong side:

- `prototype-fails` — the prototype's outcome does not match `.expect`, the
  port's does.
- `port-fails` — the port's outcome does not match `.expect`, the prototype's
  does.
- `both-fail` — neither matches `.expect` (or a no-`.expect` file disagrees).

`hang` and `runner-err` are reported per file, never folded into agreement. A
conformance case that disagrees is annotated `[known prototype divergence]` when
`test/conformance/prototype-divergences.txt` lists it; a disagreement without
that tag is a new finding.

### Limits (printed, never silent)

- `.unit-*.fun` files are **skipped** (printed with reason): they are compilation
  units, imported by their sibling case, not standalone programs. The case that
  imports them *is* run, so they are covered transitively.
- `dotnet/std/*.fun` are prelude source, not programs; both runners reject them
  as programs (`ELAB` both sides), which is agreement, not a port gap.
- The Alcotest fixtures under `test/**` are OCaml strings, not `.fun` files, so
  they are not enumerable here (there are no `.fun` files outside `cases/`
  today).
- A program that needs macro-order or prelude-relative setup beyond what
  `import` + `open_prelude` give is not distinguished from a genuine failure;
  no such program exists outside the prelude files today.

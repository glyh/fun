# Conformance cases

Language-behaviour tests as data, so every implementation runs the same suite:
the OCaml prototype today, the .NET port next
([port-core-tt-to-dotnet](../../../docs/wayfinder/tickets/port-core-tt-to-dotnet.md)).
A case may only depend on what a program produces — never on compiler internals.

## Layout

```
cases/<area>/<name>.fun              the program (an expression)
cases/<area>/<name>.expect           what it must produce
cases/<area>/<name>.unit-<unit>.fun  an extra compilation unit, importable as "<unit>"
```

Areas are `values` (evaluation), `macros` (macro expansion), `imports`
(multi-unit) and `elaborate` (type checking only). A `# …` first line names the
case it came from.

## `.expect`

| Content | Meaning |
|---|---|
| `42`, `-7` | the program evaluates to that `I64` |
| `True`, `False`, any constructor name | it evaluates to that constructor |
| `ok` | it elaborates (type checks); the program is not run |
| `error` | it fails, at expansion, elaboration or evaluation |

The `.expect` states the language's behaviour as the domain model specifies it,
not whatever an implementation happens to do. A case the OCaml prototype gets
wrong is listed in `../prototype-divergences.txt` with the ticket for its defect:
the OCaml runner expects those cases to fail (and reports one that starts
passing), the .NET runner ignores the list.

`error` is deliberately coarse: an error's class and wording are
implementation-specific, and pinning them here would make the suite untestable
for a second implementation. Tests that assert a particular error stay in the
OCaml Alcotest suites.

## Running

```sh
dune test test/conformance      # this suite only
dune test                       # everything
```

A failure names the file, what was expected and what came out.

## Adding a case

Write the two files. Nothing to register — the runner walks the directory.
Keep programs small and self-contained; the prelude (`std`) is open.

This suite is the only place a language behaviour is tested — the Alcotest
binaries keep internal tests only (shapes, reflection round trips, budget
accounting, exact error constructors, types rather than values).

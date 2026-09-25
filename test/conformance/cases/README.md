# Conformance cases

Language-behaviour tests as data: 766 programs plus their expected results, run by
the C# port. They were written to be implementation-independent, so the earlier OCaml
prototype ran the same files — that is why the suite lives here and not under `dotnet/`,
and why `../prototype-divergences.txt` exists as a historical record.

A case may only depend on what a program produces — never on compiler internals.

## Layout

```text
cases/<area>/<name>.fun              the program (an expression)
cases/<area>/<name>.expect           what it must produce
cases/<area>/<name>.unit-<unit>.fun  an extra compilation unit, importable as "<unit>"
```

Areas are `values` (evaluation), `macros` (macro expansion), `imports`
(multi-unit) and `elaborate` (type checking only). A `# …` first line names the
case it came from.

## `.expect`

| Content | Meaning |
| --- | --- |
| `42`, `-7` | the program evaluates to that `I64` |
| `True`, `False`, any constructor name | it evaluates to that constructor |
| `ok` | it elaborates (type checks); the program is not run |
| `error` | it fails, at expansion, elaboration or evaluation |

The `.expect` states the language's behaviour as the domain model specifies it,
not whatever an implementation happens to do.

`error` is deliberately coarse: an error's class and wording are
implementation-specific. Tests that must assert a *particular* error belong in
`dotnet/test/Fun.Tests` (xUnit), not here.

## Running

```sh
cd dotnet && dotnet run --project test/Fun.Conformance
```

It prints `conformance: <n> cases, <k> failed`, naming each failure's file, what
was expected and what came out. Exit code is zero only when `k` is zero.

## Adding a case

Write the two files. Nothing to register — the runner walks the directory.
Keep programs small and self-contained; the prelude (`std`) is open.

This suite is the only place a *language behaviour* is tested. xUnit keeps the
internals — syntax shapes, reflection round trips, budget accounting, exact error
constructors, and types rather than values.

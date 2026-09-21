---
title: A bare constructor pattern resolves by name among the scrutinee's constructors
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-20
resolution: A bare constructor pattern head resolves through its binder or open, never by scrutinee-constructor name; fixed in the C# port only.
assignee:
blocked_by:
---

# A bare constructor pattern resolves by name among the scrutinee's constructors

Found while porting `match` to C# (2026-09-16). **Fixed in the C# port only**
(`dotnet/src/Fun.Compiler/Elaborator.Enum.cs`, `ResolveConstructorHead`); the
OCaml prototype keeps the defect.

## Decided (2026-09-16, user)

A bare constructor pattern head resolves **like any other name** - through its
binder or an open choice - never by name among the scrutinee type's
constructors. A constructor is a member of its type (glossary **Member**), so a
raw `enum`'s constructors are in scope bare only after `open`; the `type` macro
writes that open (`rec … = enum { … }; export …; open …`), which is why
`type`-declared constructors work bare. A head that resolves to anything other
than a constructor is an error.

A capitalised bare name in pattern position is still a constructor pattern and a
lower-case one a binder: that is enforestation's syntactic rule, unchanged.

## Defect

The prototype finds a bare head among the scrutinee's constructors whether or not
the constructor is in scope, and even when the name is bound to a value:

```
{ Color = enum { Red, Green }; match (Color.Green) { Red => 1, Green => 2 } }
```

gives 2 with no `open Color`, and

```
{ Color = enum { Red, Green }; open Color; Red = 5; match (Color.Green) { Red => 1, _ => 2 } }
```

still reads `Red` as the constructor.

## Conformance

- `elaborate/bare-constructor-pattern-needs-open` (error) and
  `elaborate/bare-constructor-pattern-shadowed-by-value` (error) are the two
  reproductions, listed in `test/conformance/prototype-divergences.txt`.
- `values/bare-constructor-pattern-after-open` (2) is the agreeing case.
- **Correction (2026-09-20): the claim above was wrong.** One existing case *did*
  rely on the by-name lookup, and it is why the rule was first thought settled:
  `imports/core-165` matched `{ Green => 2, Red => 1 }` with no `open` in scope, and
  passed in the prototype only through the defect. It surfaced when the port's
  residue fork stopped on it rather than guessing (a shared case and the decision
  cannot both hold). Resolved by making the case say what it means — the arms now
  qualify through the unit that declares the constructors
  (`C = import "color"; C.Red => 1, C.Green => 2`), which passes in **both**
  runners — and by pinning the rejected behaviour for that shape in
  `imports/bare-constructor-pattern-through-import` (error), listed as a divergence.
  The import shape is what slipped past the original audit: every case checked
  then was a raw `enum` in one unit, where patterns were qualified.
- No existing case relied on the by-name lookup: every raw-enum case qualifies its
  patterns.

## Resolution (2026-09-20)

Closed after re-running both runners on `main @ d58af64`.
`dune test --root . test/conformance` reports `690 cases, 0 failed, 19 known
prototype divergences`, so `elaborate/bare-constructor-pattern-needs-open` and
`elaborate/bare-constructor-pattern-shadowed-by-value` fail in the prototype as
listed; `cd dotnet && dotnet run --project test/Fun.Conformance --no-build` passes
both (neither is among the 13 unrelated residue failures), so the port is correct.

---
title: "Port: an atom pattern in a record field does not unify"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: an atom pattern in a record field does not unify

A well-typed program is rejected. Found by the integrator 2026-09-25 while re-testing
[the nested field pattern ticket](port-nested-field-patterns.md), whose probes had been invalid
(they used a comma in the struct declaration — see
[the reader loop](port-reader-loops-on-struct-field-comma.md) — and a bare `{…}` where the
language wants `R{…}`).

## The program

```fun
{ R = struct { f : I64; g : I64 };
  f = fn(x : I64, y : match (R{f = 1; g = x}) { R{f = 1; g = 2} => I64, _ => Char }) { y };
  f(5, 5) }
```

| shape of the field pattern | result |
| --- | --- |
| a bare binder, `R{f = 1; g = y}` | `VALUE 5` |
| a constructor, `R{f = 1; g = Some(z)}` (against `g : Option(I64)`) | `VALUE 5` |
| an **atom**, `R{f = 1; g = 2}` (against `g : I64`) | **`ELAB type mismatch: cannot unify VAtomTy with VAtomTy`** |

The expected answer is `5`: the field pattern is an atom, the field's type is that atom's type,
so the pattern matches everything and `y` is `5`. Both neighbouring shapes work, which is what
makes this a bug in the atom path rather than in record patterns as a whole.

## Why the message is the clue

`cannot unify VAtomTy with VAtomTy` — an atom *type* failing to unify with an atom *type*, with
no indication of which. Two readings, and the fix should say which is true rather than guess:

- the atom pattern's type is compared against the field's type with one side built wrongly (e.g.
  the atom *value's* type instead of the atom's declared type), so two atoms that should be equal
  are not; or
- the atom pattern is elaborated against the wrong expected type (the scrutinee's, not the field's).

The second would be the same class of mistake as the record-pattern path's own; the first is
local to the atom case. Both are one probe away: print the two `VAtomTy` payloads at the failing
unify.

## What to do

1. Reproduce, then **name the two atoms** in the error before changing anything — the message is
   currently useless for exactly this reason.
2. Fix the mismatch at its source. Do not special-case `I64` fields.
3. **Tests**: the three programs above, all `.expect 5`, as ordinary shared cases (nothing here is
   a prototype divergence — the prototype is deleted, and this is the port being wrong on a shape
   the suite never covered). The bare-binder and constructor forms are the controls that keep the
   fix honest.
4. Check the neighbouring positions while in there — a tuple field, a nested record, an atom
   against a `Char` — since a wrong expected type would show up in more than one place.

## Reading

- `dotnet/src/Fun.Compiler/Elaborator.Patterns.cs` — `ElaboratePattern`'s atom case, and
  `ElaborateRecordPattern`'s per-field call that supplies the field's type
- `dotnet/src/Fun.Compiler/Unify.cs` — the atom-type comparison the message comes from
- [the nested field pattern ticket](port-nested-field-patterns.md) — the probes that found this,
  and why they had to be rewritten twice

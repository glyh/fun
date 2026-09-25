---
title: "Port: a pattern head that is a member of a non-nominal reduces to a language error"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: a pattern head that is a member of a non-nominal reduces to a language error

Site 2 of the 2026-09-25 [re-sweep](port-unported-path-audit.md#re-sweep-2026-09-25) of the
refusal inventory, found by a read-only audit, re-probed by the integrator. One of **three
reachable refusals hiding behind rows the first audit marked fixed** — its row credits
[port-stage2-residue](port-stage2-residue.md), which fixed `elab-067`'s generative route only.

## The program, and both runners

```fun
{ M = module { pub f = fn(x : I64) { x } };
  g = fn(u : I64) { match (u) { M.f(a) => a, _ => 0 } };
  g(3) }
```

| runner | output |
| --- | --- |
| OCaml | `ELAB ElabError(NotANominalType)` |
| port | `ELAB not ported: not ported yet: a constructor pattern head that is a member of a non-nominal` |

`M.f` is a plain function used where a type name is expected, and the prototype says exactly that:
**not a nominal type**. The port refuses the same shape, so this is a gap, not parity.

## Why this is about reduction, not about the message

The neighbouring ruling is the constraint to respect: **any function reducing to a nominal is
accepted as a pattern-head type** — `Seq = fn(A : Type) { List(A) }` makes `Seq.Cons(h, t)` a
valid pattern head, because types are values
([pattern-head-accepts-type-formers](pattern-head-accepts-type-formers.md), closed, "intended").
So the head must be **tried**, by applying the entry to fresh metas until a nominal appears, and
`NotANominalType` is what you say when that reduction does not produce one. A port that simply
rejected "a head that is a member of a non-nominal" would break the alias the ruling blesses, so
check both directions before changing anything:
- `M.f` (a function whose result is `I64`) → error, as above;
- a function whose result *is* a nominal → still a valid head, via the existing
  `find_nominal_template_opt`-style reduction.

## Where the port refuses

`dotnet/src/Fun.Compiler/Elaborator.Enum.cs:43`, reached with the other two sites the audit
recorded for the same shape (`Elaborator.Match.cs:93,161`) — one of them may be the right place
for the error rather than the throw.

## What to do

1. Replace the refusal with the `NotANominalType`-class language error, raised where the head's
   reduction fails to produce a nominal.
2. Skip the throw rather than converting it if the reduction makes it unreachable (the audit's
   verdict 3) — say which you found.
3. **Test**: a shared case, `.expect error` — ordinary, since the prototype errors too. Add the
   **positive** companion as well: a `fn(A : Type) { List(A) }` alias used as a pattern head, which
   must keep working. That pair is what pins "reduce first, then complain".
4. Keep the honest accounting: an unported path stays a `NotImplementedException`.

## Reading

- `dotnet/src/Fun.Compiler/Elaborator.Enum.cs:43`, `Elaborator.Match.cs:93,161`
- [pattern-head-accepts-type-formers](pattern-head-accepts-type-formers.md) — the closed ruling
  that bounds the fix
- [the constructor-pattern-head divergence](port-stage2-residue.md) — the route already fixed, so
  this one is not credited to it twice
- the prototype's `NotANominalType` site and its `find_nominal_template_opt`

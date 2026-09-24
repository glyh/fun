---
title: "When an implicit lambda should bind its parameter rigidly, and when it should instantiate"
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# When an implicit lambda should bind its parameter rigidly, and when it should instantiate

**Not blocking anything.** Raised by the fork that implemented
[an implicit lambda checked against a function type instantiates](port-generalise-under-check.md)
(2026-09-24). It had to choose a gate to land that fix, chose one that keeps every suite
case green, and reports the corners its choice does not cover rather than leaving them in a
conversation. This ticket is those corners.

## The situation

An implicit lambda in check position has two possible treatments, and the language needs
both:

- **bind its parameter rigidly** — check the lambda as the ∀ it claims to be. This is what
  a *dependent* expected type needs: `[T : Type] -> T -> T` (whose body type-cases `T`) and
  `[A : Eq] -> …` (whose hidden dictionaries are among the mentions) are not inferable, and
  a value-level implicit `[n : I64]` must have its runtime value flow through the lambda.
- **instantiate** — insert the parameter first and infer, exactly as a *name* does
  ([check-against-implicit-type-inserts-first](check-against-implicit-type-inserts-first.md)).
  This is what a call site's parameter-independent expected type needs
  (`[A : Type] -> I64 -> ?r`).

The port now decides by **dependency**: `BindsImplicitParameterItself`
(`dotnet/src/Fun.Compiler/Elaborator.Implicits.cs`) binds the parameter itself when the
expected domain is not `Type` (dictionary / effect-row / value parameter), or when the
codomain, probed at a fresh meta, `Unify.Mentions` the parameter. Otherwise it instantiates.

That gate was chosen because the naive alternative broke **16** suite cases — the whole
type-case family, the value-level implicit case, and `(==)`/`(!=)` in `stage2.fun` — which
is evidence that both treatments are genuinely needed, not that one is an accident.

## The corners it does not cover

Neither is exercised by any suite case, so neither is a defect today — they are the places
where the gate's rule and the "right" rule could part company:

1. **A parameter-independent dictionary.** `[A : Type] -> [Eq(I64)] -> …` whose body needs
   the evidence: nothing about the *expected* type mentions `A`, so the gate instantiates,
   and then the hidden dictionary may not be insertable where the body needs it. The
   prototype's behaviour on such a program has not been probed.
2. **A written parameter type mentioning the parameter inside a non-inferable body** — an
   explicit annotation like `fn[T : Type](x : T) { … }` checked against a
   parameter-independent expected, where the *body* (not the expected type) is what forces
   `T` to be rigid.

## What settling this looks like

Probe both runners on the two shapes above — the prototype is a map, not the spec, and this
area has produced three recorded causes that probing overturned, so a program beats an
inference. Then either:

- **the gate is right** (the differences are unobservable in both implementations) → close
  this ticket saying so, with the programs; the gate stays as it is; or
- **a shape differs** → fix it the way the ruling
  [an implicit lambda checked against a function type instantiates](port-generalise-under-check.md)
  was fixed, and add the case (ordinary if the prototype agrees, listed in
  `prototype-divergences.txt` if it does not).

## Reading

- `dotnet/src/Fun.Compiler/Elaborator.Implicits.cs` — `BindsImplicitParameterItself`, the
  comment above it is the rationale
- [an implicit lambda checked against a function type instantiates](port-generalise-under-check.md)
  — the ruling, the corrected cause, and the hazard
- `dotnet/std/stage2.fun:23-24` — the `(==)`/`(!=)` definitions the naive version broke

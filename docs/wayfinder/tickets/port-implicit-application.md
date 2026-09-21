---
title: "Port: implicit application — f{ e } and applying a value of unknown function type"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: implicit application

The two **verified** real gaps from [the unported-path audit](port-unported-path-audit.md)
(section "Real gaps", G1) — the only two the audit reproduced in *both* runners, and
the integrator reproduced them again independently. Both are small and self-contained;
either is a good next implementation fork. Follow the
[porting conventions](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16) ·
convention 7 puts the work in its own partial file.

## 1. An implicit argument written `f{ e }`

`dotnet/src/Fun.Expand/Enforest.cs:387` refuses it:

```
throw new NotImplementedException("not ported yet: an implicit argument written f{ e }");
```

Reproduced (2026-09-20, both runners, case `expect` `1`):

```fun
{ f = fn[A : Type](x : A) { x }; f{I64}(1) }
```

Prototype: `1`. Port: `not ported yet: an implicit argument written f{ e }`.

Note the guard at that site already requires adjacency (`lhs.Span.End == postfix.Span.Start`)
and distinguishes `f{ e }` from `f { … }` (a brace group), so the shape is understood —
only the production is missing. The prototype reads it in `lib/expand/enforest.ml`.

## 2. Applying a value of unknown function type

`dotnet/src/Fun.Compiler/Elaborator.Implicits.cs:50` refuses it:

```
throw new NotImplementedException("not ported yet: applying a value of unknown function type");
```

Reproduced (2026-09-20, both runners, case `expect` `1`):

```fun
{ h = fn(g) { g[I64] }; 1 }
```

Prototype: `1`. Port: `not ported yet: applying a value of unknown function type`.

This is the implicit-argument analogue of a decision already taken for references
([a reference operation on a value of unknown type is "not a reference"](deref-of-unknown-type-is-not-a-reference.md),
closed 2026-09-20): the operation *infers* rather than refusing. So this should infer
the implicit function type and insert the meta, exactly as ordinary application infers
an arrow — check that reading against the prototype before implementing, and if it
disagrees, stop and report rather than guessing (convention 9).

## Tests

Both are shared conformance cases (convention 6 — a source-to-result test is a
conformance case). Use the programs above, `expect` `1`. Run them through both
runners before committing: the prototype is known to pass both (verified 2026-09-20),
so if one fails in the prototype that is a new finding, not a case to adjust.

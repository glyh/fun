---
title: "Port: the checker's unhandled-effect error, not a refusal"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: the checker's unhandled-effect error, not a refusal

Site 8 of the 2026-09-25 [re-sweep](port-unported-path-audit.md#re-sweep-2026-09-25) of the
refusal inventory, found by a read-only audit, re-probed by the integrator. One of **three
reachable refusals that hide behind rows the first audit marked fixed** — its row for this shape
credits [port-stage2-residue](port-stage2-residue.md), which closed a *different* route
(core-102's eager argument) and left this one.

## The program, and both runners

```fun
{ effect Abort = sig { stop : Unit -> I64 };
  f = fn(u : Unit) { perform Abort.stop(u); I64 };
  E = enum { C(f(())) };
  1 }
```

| runner | output |
| --- | --- |
| OCaml | `ELAB ElabError(EvaluationFailed "unhandled effect Abort.stop: no handler for it is in scope while inferring the form at <unknown>:1:98-1:115 (while type checking)")` |
| port | `ELAB not ported: not ported yet: the checker evaluated a term that performs Abort.stop` |

The **prototype has the answer**: an unhandled effect the *checker* stumbles into is a language
error naming the effect and the form it was inferring. The port refuses the same shape, so this
is a gap, not parity — and unlike most of this inventory it is a **class difference the shared
suite can already state**: after the fix both runners error, so the case is ordinary.

## Where the port refuses

`dotnet/src/Fun.Compiler/Nbe.Effects.cs:111`:

```csharp
throw new NotImplementedException($"not ported yet: the checker evaluated a term that performs {Describe(instance)}.{op}");
```

The port's *checker* errors do carry a source position (its budget form prints
`… in an evaluation while reading the type at <file>:1:57-1:64`), so the message shape the
prototype produces is reachable — this is a conversion plus the "which form" half, not new
machinery.

## What to do

1. Replace the refusal with the checker's unhandled-effect error, in the prototype's shape: the
   effect, the missing handler, and the form being inferred. Reuse whatever the port already uses
   to say *which* form the checker was reading (its budget message does).
2. Keep the honest accounting: a path that is genuinely not ported stays a
   `NotImplementedException`; this one is a language error both implementations agree on.
3. **Test**: a shared case, `.expect error` — ordinary, not a divergence entry, since the
   prototype errors for the *right* reason and the port must reach the same class. Add an xUnit
   test only if you want to pin *which* error (internals), which is what makes the class
   difference visible; the shared case cannot.
4. Check the neighbours while you are in the file: the audit sorted the remaining refusals, and
   the ones in `Nbe.Effects.cs` are the only reachable one here — do not convert the rest.

## Reading

- `dotnet/src/Fun.Compiler/Nbe.Effects.cs:111` — the refusal; `:57` `Term.Tunnel`, `:27`
  `TunnelFrame` for how a tunnelled request is already carried
- `dotnet/src/Fun.Compiler/Elaborator.Effects.cs` — the row/tunneling elaboration, and where the
  *checker* path arrives
- `lib/semantic/typecheck/` — the prototype's `EvaluationFailed … (while type checking)` shape
- [port-effects](port-effects.md) (closed) and [port-stage2-residue](port-stage2-residue.md) —
  what each did and did not cover, so the same row is not credited twice again

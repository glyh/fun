---
title: "Port: an enum's captures come from its payload values, not its payload terms"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: an enum's captures come from its payload values, not its payload terms

Verdict of [the unverified rows](port-unverified-rows.md), row 2 — a **real gap**, and
worse than a gap: the port **crashes** on it. Verified by the integrator 2026-09-24 at
`32aa27e`.

## The program

```fun
(fn(X : Type) { A = X; rec U = fn(B : Type) { enum { C(A) } }; U })(I64)
```

| runner | output |
|---|---|
| OCaml | `<lam>` — accepted |
| port | **unhandled** `Fun.Compiler.UnifyException: a variable outside the meta's spine escapes into its solution` (`Unify.cs:161` ← `Unify.Enum.cs:42` ← `Elaborator.Enum.cs:72`), and the conformance runner dies |

The same happens with a nominal payload (`T = enum { K(X) }; rec U = fn(A : Type) { enum { C(T) } }`).

## Cause (found by the probing fork, reproduced here)

The term-vs-value split: `InferEnum` computes `levels` from the payload **terms**
(`A` → `Var(A)`) but `CloseOver`s the payload **values** (`VVar(X)`), so `X ∉ levels`.
`PredictCaptures` (`Elaborator.RecTypes.cs:132`) has the identical name-based narrowing,
which both over- and under-captures. The `CompletePending` comparison at
`Elaborator.RecTypes.cs:62` (message on `:63`, *not* `:54` as the old line numbers said)
sits *after* `CloseOver`, so it is masked by it.

## Fix

- Compute a member's captures from the payload **values**, the way the prototype's
  `capture_payloads` does, so `levels` covers every variable a value mentions; then
  reconcile `PredictCaptures` with the same source so the name-based prediction cannot
  disagree with the use-based one.
- **Separately: the runner must not die.** `Program.cs` does not catch
  `UnifyException`, so an internal invariant failure aborts the whole conformance run and
  the count is lost. It must be reported as a failed case (an invariant failure is a
  port bug, never a language error — convention 2 says `FunException` is only for a
  genuine language error).

## Tests

- The program above: `expect` the value the prototype gives, as an **ordinary** shared
  case (the prototype accepts it — no divergence entry), and the nominal-payload variant
  from the probe.
- The `-rejects-other` style companion that keeps two instances separate is unaffected;
  re-run the whole suite after the change, since captures feed identity.

## Reading

- `dotnet/src/Fun.Compiler/Elaborator.Enum.cs` (`InferEnum`, `CloseOver`),
  `Elaborator.RecTypes.cs:62`/`:132`, `Unify.Enum.cs:42`, `Unify.cs:161`
- the prototype's `capture_payloads`

## Paused (2026-09-24) — nothing written, plan pinned

The implementation fork was stopped during its **diagnosis** phase by the work pause:
clean tree, **nothing committed**, so this ticket resumes exactly as written, from base
`1dec417`.

What its read confirmed, agreeing with the evidence above: `InferEnum` computes the
capture `levels` from the payload **terms** (via `FreeLevels`, which sees `Var(A)`'s
level) while `CloseOver` runs on the payload **values** (`VVar(X)`), so `X ∉ levels`;
and `PredictCaptures` reads only `NamedLevels`, i.e. names. The prototype's
`capture_payloads` feeds on *quoted values*, which is the shape to mirror.

The next step, as it left it:

1. Quote the payload values — `Nbe.Quote(ctx.Metas, ctx.Width, p)` — into one shared
   helper, `EnumCaptureLevels(ctx, payloadValues)`.
2. Use that helper in **both** places: replace `FreeLevels(payloadTerms)` in `InferEnum`,
   and elaborate + eval the payloads inside `PredictCaptures`'s stand-in context, so the
   name-based prediction and the use-based computation cannot disagree.
3. Add the shared case(s) (the program above, preferring an observable `I64`; plus the
   nominal-payload variant), then catch `UnifyException` in
   `dotnet/test/Fun.Conformance/Program.cs` so an invariant failure is a **failed case**
   rather than a dead run, and re-run the full suite — captures feed identity, so the
   `values/nominal-*` and `values/rec-*` cases are the ones at risk.

Files it identified: `dotnet/src/Fun.Compiler/Elaborator.Enum.cs` (~55-72),
`Elaborator.RecTypes.cs` (`CompletePending` ~62, `PredictCaptures` ~132),
`dotnet/test/Fun.Conformance/Program.cs`.

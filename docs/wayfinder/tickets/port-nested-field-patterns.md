---
title: "Port: a nested field pattern must work — it hangs today"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: a nested field pattern must work — it hangs today

Found by the fork that landed
[a match stuck on a known scrutinee's unknown part](port-stuck-match-sub-occurrence.md)
(2026-09-25, merged `d7bb999`), while probing for a third shape. It confirmed the hang is
**pre-existing** — it hangs with that fork's change stashed — so it is its own defect, and it is
**not** a consequence of the merged work. Re-probed, narrowed, and ruled on by the integrator
2026-09-25.

## The ruling (user, 2026-09-25)

> A **nested field pattern is supported**: `{ f = 1, g = Some(z) }` matches a record whose
> `g : Option(I64)`. It hangs today; that is the defect.

The alternative — keep the port's existing refusal and apply it to every field-pattern kind
(parity with the prototype's Phase 7B limit) — was put to the user as the recommendation and
**declined**. So this is the port being **ahead** of the prototype on purpose, not parity work:
the prototype cannot express the shape at all.

## Today's behaviour — three programs, integrator-verified

All three are demanded by the checker: the `match` is the second parameter's type, so it is
evaluated while reading `f`'s type. All three end in `f(Some(5), 5)`, which **passes `5` through
`y`** — so `5` falls out whatever the type-level match decides, exactly as in
`values/stuck-match-sub-occurrence` (whose `.expect` is `5`).

**(a) hangs the port** — a declared struct type, a constructor pattern in a field:

```fun
{ R = struct { f : I64, g : Option(I64) };
  f = fn(x : Option(I64), y : match ({ f = 1, g = x }) { { f = 1, g = Some(z) } => I64, _ => Char }) { y };
  f(Some(5), 5) }
```

| runner | output |
| --- | --- |
| OCaml | `ELAB Enforest_util.Error("binding f: unsupported pattern in Phase 7B match")` |
| port | **no line within 30 s** — a hang (`scripts/differential.sh` records `HANG` at 60 s) |

**(b) the same shape with an *atom* field pattern** — `{ f = 1, g = 2 }`, and `g : I64`:

| runner | output |
| --- | --- |
| OCaml | `ELAB … unsupported pattern in Phase 7B match` |
| port | `ELAB unsupported pattern` |

**(c) the same shape with no declared record type** — drop `R`, keep (a)'s text otherwise:

| runner | output |
| --- | --- |
| OCaml | `ELAB … unsupported pattern in Phase 7B match` |
| port | `ELAB unsupported pattern` |

## What the three pin down

The port hangs **only in (a)**. (b) and (c) reach the port's own refusal
(`dotnet/src/Fun.Expand/Enforest.Match.cs:134`, `throw new ExpandException("unsupported pattern")`),
so the guard exists and **(a) is a hole in the field-pattern path, not a missing feature** — which
matters for the fix: the sub-pattern must be carried through, and the hang is a symptom of that
path being half-written rather than a separate bug.

The ill-typed version of (a) that the fork first found (`g : I64` with `g = Some(z)`) hangs for
the same reason, so that type mismatch was a red herring; (a) above is well typed.

**The prototype's error is a limitation, not a verdict.** Both runners refuse nested field
patterns by design at the enforester (`lib/expand/enforest_pat.ml:39`), and the port is already
ahead on the neighbouring shape: a record pattern with **bare-binder** fields against a declared
struct type is an ordinary shared case (`values/core-154`, `core-156`, `core-178`, `core-179`,
`elab-023`), where the prototype has no counterpart.

## What to implement

1. **Make (a) terminate and work.** The sub-pattern has to be elaborated against the *field's*
   type and then carried through the decision tree, and — because the scrutinee's field may hold
   an unknown variable — through the stuck/deferral path the ruling on
   [a stuck match](port-stuck-match-sub-occurrence.md) describes. Start by instrumenting the hang;
   the merged fork's `Nbe.Match.cs`/`StuckMatch.cs` is **not** implicated (the pre-existing hang
   proves it), so look at where a record pattern's field sub-pattern is read: the enforester's
   record-pattern parse, `Elaborator.Patterns.cs`, and the match compiler.
2. **(b) must agree with (a).** One rule covers every field-pattern kind, so once nesting works a
   field pattern that is an atom must work too — it is refused today with `unsupported pattern`,
   which is the same hole seen from the other side.
3. **Do not widen the refusal back.** The ruling is explicit; if the sub-pattern path turns out to
   need a real design decision, STOP and report rather than reverting to `unsupported pattern`.

## Tests

- **A shared case for (a)** with `.expect` `5`, listed in `test/conformance/prototype-divergences.txt`
  naming this ticket — the prototype refuses the shape, so it cannot be an ordinary case. Precedent:
  `values/stuck-match-pruned-arm`, added the same day for the same reason (a shape only the port can
  elaborate).
- **A shared case for (b)**, the atom field pattern, whose `.expect` is also `5`: it agrees on the
  class today but for the wrong reason (`unsupported pattern` is not a verdict), so it is the case
  that will notice the hole closing.
- **xUnit**: the sub-pattern's elaboration directly — the internals half, since the shared suite can
  only state a value and the interesting property is that a nested field pattern survives
  enforestation → elaboration → the tree.
- Its real job, in all three: **a hang fails them**. A hang is the only class no `.expect` can
  state, and the port's runner counts it as a failure while `scripts/differential.sh` records
  `HANG` separately — so these are the guard against the class recurring.

## Follow-ups, recorded not guessed

- **The domain model must state this rule.** It is a *language* rule now, and the model is the
  specification while the prototype is supporting material; today the model is silent, so the
  divergence entry's `.expect` carries a port answer the spec does not yet assert. Record it in
  the surface/enforestation pass (`docs/wayfinder/topics/core-tt-domain-model-surface-enforestation.md`)
  before the case claims the model's authority.
- **Size: a feature, not a guard widening.** Enforestation, elaboration, the decision tree and the
  stuck-match deferral all see the sub-pattern. Do not brief a fork with "make it stop hanging";
  brief it with the ruling above.

## Reading

- `dotnet/src/Fun.Expand/Enforest.Match.cs:134` — the refusal (b) and (c) reach
- `dotnet/src/Fun.Compiler/Elaborator.Patterns.cs` — record patterns, and where a nested field
  pattern would be elaborated
- `dotnet/src/Fun.Compiler/Nbe.Match.cs`, `Nbe.StuckMatch.cs` — the deferral the record case must
  join, and [a match stuck on a known scrutinee's unknown part](port-stuck-match-sub-occurrence.md)
  — the ruling that a match inspecting an unknown part **waits**
- `dotnet/src/Fun.Compiler/Budget.cs` — the bound that should have turned this into an error rather
  than a hang
- `lib/expand/enforest_pat.ml:39` — the prototype's Phase 7B limit, i.e. why its error is not
  evidence

---
title: "Port: a constructor sub-pattern in a record field hangs instead of being refused"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: a constructor sub-pattern in a record field hangs instead of being refused

Found by the fork that landed
[a match stuck on a known scrutinee's unknown part](port-stuck-match-sub-occurrence.md)
(2026-09-25, merged `d7bb999`), while probing for a third shape. It confirmed the hang is
**pre-existing** — it hangs with that fork's change stashed — so it is its own defect, and it is
**not** a consequence of the merged work. Re-probed and narrowed by the integrator 2026-09-25.

## The three programs, and what each runner does

All three are demanded by the checker: the `match` is the second parameter's type, so it is
evaluated while reading `f`'s type.

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
| OCaml | `ELAB Enforest_util.Error("binding f: unsupported pattern in Phase 7B match")` |
| port | `ELAB unsupported pattern` |

**(c) the same shape with **no declared record type**** — drop `R`, keep (a)'s text otherwise:

| runner | output |
| --- | --- |
| OCaml | `ELAB … unsupported pattern in Phase 7B match` |
| port | `ELAB unsupported pattern` |

## What the three pin down

The port hangs **only in (a)**. (b) and (c) both reach the port's own refusal
(`Enforest.Match.cs:134`, `throw new ExpandException("unsupported pattern")`), so the guard
exists and this is a **hole in it, not a missing feature**: a field whose sub-pattern is a
constructor application reaches some other path and does not terminate. Narrowing it that far
is the point of the three programs — the search is the record-pattern elaboration path, not
"the checker budget in general", and the ill-typed version of (a) that the fork first found
(`g : I64` with `g = Some(z)`) hangs for the same reason, so the type mismatch is a red herring.

**The prototype's error is a limitation, not a verdict.** Both runners refuse nested field
patterns by design at the enforester (`lib/expand/enforest_pat.ml:39`,
`dotnet/src/Fun.Expand/Enforest.Match.cs:134`), and the port *is* ahead on the neighbouring
shape: a record pattern with **bare-binder** fields against a declared struct type is an
ordinary shared case (`values/core-154`, `core-156`, `core-178`, `core-179`, `elab-023`), where
the prototype has no counterpart. So (a) is a shape only the port can even attempt.

## Open design question — the fix does not depend on it, the test does

Whether a **nested** field pattern should be supported at all: the prototype cannot say (it
refuses the shape outright), and the port's atom case already refuses it, so parity says refuse
uniformly. The alternative is to support it as an extension the shared suite cannot cover
(then the prototype's refusal becomes a divergence). **Undecided — asked of the user
2026-09-25; record the ruling here before a fork reads this ticket.**

## What to do

1. **Reproduce with a timeout** and instrument rather than guess — the hang is in the port's
   record-pattern path, and the merged fork's `Nbe.Match.cs`/`StuckMatch.cs` is *not* implicated
   (the pre-existing hang proves it).
2. **Make it terminate.** If the ruling is "refuse uniformly", that is the same
   `ExpandException("unsupported pattern")` (b) already gives — the port's own rule, applied to
   every field-pattern kind rather than the atom one. If it is "support nesting", the fix is the
   sub-pattern's elaboration against the field's type, and the hang is a symptom of that being
   half-written.
3. **Make the class impossible to reintroduce silently.** A hang is the only class no `.expect`
   can state: the port's runner counts it as a failure and `scripts/differential.sh` records
   `HANG` separately, so the case below is the guard.
4. **Test** — a shared case is available either way, but its `.expect` follows the ruling:
   *refuse uniformly* → `elaborate/<name>` with `error` (the prototype errors too, so it is an
   **ordinary** case, not a divergence entry); *support nesting* → the port's value with the case
   listed in `prototype-divergences.txt`. Its real job is that a hang fails it.

## Reading

- `dotnet/src/Fun.Expand/Enforest.Match.cs:134` — the refusal the port already has
- `dotnet/src/Fun.Compiler/Elaborator.Patterns.cs` — record patterns, and where a nested field
  pattern would be elaborated
- `dotnet/src/Fun.Compiler/Budget.cs` and its `RunUnderBudget` callers — the bound that should
  have turned this into an error rather than a hang
- `lib/expand/enforest_pat.ml:39` — the prototype's Phase 7B limit, i.e. why its error is not
  evidence
- [checker-evaluation-budget](checker-evaluation-budget.md) (closed) — the intended bound

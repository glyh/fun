---
title: "Port: the observable budget cases the ruling named"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: the observable budget cases the ruling named

The internals-parity ruling (user, 2026-09-20) was: behavioural parity only, **budget
yes** — add `dotnet/test/Fun.Tests/BudgetTests.cs` for the three observable budget cases.
It still does not exist, and a coverage sweep on 2026-09-24 settled where each of the three
actually stands:

| behaviour | the prototype's test | covered in the port? |
|---|---|---|
| a macro body's overrun names the macro | `test/backend/test_core.ml:1399` | **yes** — `MacroTests.ADiventMacroRunsOutOfBudget` (`MacroTests.cs:34`) |
| an operator body's budget error carries the use site | `test_core.ml:1412` | **nowhere** — no xUnit fact, and the shared suite cannot express it (`.expect` is coarse: `error`) |
| `expand_decls`' budget error names the macro | `test_core.ml:2048` | **nowhere** |

`RecTests.cs:19` and `EffectTests.cs:49` cover the *checking*-budget side. So this ticket is
the missing part of a ruled item — not a new idea, and not a port bug.

## What to do

Write `dotnet/test/Fun.Tests/BudgetTests.cs` covering the two uncovered behaviours, driving
them through the port's own surface. Read the prototype's assertions first
(`test_core.ml:1399-1420` and `:2040-2060`) and mirror the **behaviour**, not the wording:
what is observable is that the error names the *macro* (and, for the operator case, the
*use site*) rather than only the budget — i.e. assert on the identifying part, not the whole
sentence, so the test does not pin prose the domain model does not fix.

This is xUnit rather than a conformance case for the reason the ruling gives: a shared case
can only say `error`, and the point of these three is *which* error. Note that in the ticket
so the exception to convention 6 is on the record.

## Reading

- `docs/wayfinder/tickets/port-parity-plan.md` — the internals-parity ruling ("behavioural
  only, budget yes, shapes no")
- `test/backend/test_core.ml:1399`, `:1412`, `:2048`; `dotnet/test/Fun.Tests/MacroTests.cs`,
  `RecTests.cs`, `EffectTests.cs`
- `dotnet/src/Fun.Compiler/Elaborator*.cs` — `RunUnderBudget` and the budget error

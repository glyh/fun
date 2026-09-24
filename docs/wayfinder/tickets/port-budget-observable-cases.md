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
| an operator body's budget error carries the use site | `test_core.ml:1412` | **landed, weaker** — `BudgetTests.AnOperatorBodyOverrunNamesTheOperatorUse` asserts the operator's name; the port drops the use/declaration spans the prototype prints → [a budget error names the outermost request](port-budget-attribution.md) |
| `expand_decls`' budget error names the macro | `test_core.ml:2048` | **blocked** — the port names the *outermost* request where the prototype names the innermost; the test is deliberately not landed → [a budget error names the outermost request](port-budget-attribution.md) |

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

## Status (2026-09-25)

Two of the three behaviours are now asserted, which is what the ruling asked for:

- the macro-body case was already covered by `MacroTests.ADiventMacroRunsOutOfBudget`;
- the **operator** case landed as `BudgetTests.AnOperatorBodyOverrunNamesTheOperatorUse`
  (`BudgetTests.cs:26`) — xUnit, not a shared case, for the reason the ruling gives (a shared
  case can only say `error`, and the point here is *which* error). It asserts the operator's
  **name**, not a span, because the port's budget errors carry no source position at all.
- the **`expand_decls`** case could not be landed honestly: the two runners name different
  macros, so any assertion would either enshrine the port's answer or be red. It is
  [its own ticket](port-budget-attribution.md), and knocking it off closes this one's third
  behaviour and the ruling's 184th test.

So the ruling's three observable budget cases stand at **two landed, one blocked on a real
message divergence** — not on nobody having written the test.

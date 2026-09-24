---
title: "Port: a budget error names the outermost request, not the one that overran"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: a budget error names the outermost request, not the one that overran

Found by the fork that landed [the observable budget cases](port-budget-observable-cases.md)
(2026-09-25). It stopped rather than land a test either way: asserting the port's answer
would enshrine the divergence, asserting the prototype's would be red.

## The divergence

```fun
{ macro spin(_) <diverging>;
  macro keep(d : List(Decl)) : List(Decl) { Syntax.expand_decls(d) };
  M = module { keep({ pub x = spin(0) }) };
  0 }
```

| runner | which macro the budget error names |
|---|---|
| OCaml | `spin#420` — the **innermost** application, the one whose body overran |
| port | `keep` — the **outermost** request |

Cause: `Budget.MacroApplication` (`dotnet/src/Fun.Compiler/Budget.cs:57`) uses the application
name only as the depth-0 request's demand, so a nested application never reaches the message,
while the prototype's `Eval_budget` raises the innermost `application.exceeded`.

## A second, weaker difference: the spans

For the operator shape the port names the operator (`… in the application of macro '~'`) but
carries **no source position**, where the prototype prints
`syntax operator "~" used at <file>:3:4-3:5, declared at <file>:2:8-2:11`. That is one
instance of a difference already recorded on the map as fog: *elaborator errors carry no
source location at all*. Fixing the attribution without the span still leaves the lesser
message, which is why they are noted together here and only the first is in scope.

## What to do

1. Settle the intended attribution — recommendation: the **innermost** application, as the
   prototype has it and as the budget's meaning implies (the work that overran, not the
   request that asked for it). The user can veto in a word; it is a message, not a semantic.
2. Fix `Budget.cs` accordingly and prove it with the nested program above.
3. Land the `expand_decls` assertion in `BudgetTests.cs` that is currently missing (the
   operator one landed; xUnit is 183 where the ruling's three behaviours wanted 184).
4. Spans: leave with the map's fog note unless the port's error spans are being done anyway
   — then this ticket is a natural first customer.

## Reading

- `dotnet/src/Fun.Compiler/Budget.cs:57` (`MacroApplication`), `BudgetTests.cs`
- [the observable budget cases](port-budget-observable-cases.md) — what landed, and why this
  one could not
- the prototype's `Eval_budget` (innermost `application.exceeded`)

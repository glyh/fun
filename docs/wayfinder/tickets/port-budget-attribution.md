---
title: "Port: a budget error should name the call stack of requests"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: a budget error should name the call stack of requests

Found by the fork that landed [the observable budget cases](port-budget-observable-cases.md)
(2026-09-25), which stopped rather than land a test either way. Then ruled on by the user the
same day — and the ruling is better than either option that was put to them.

## The divergence it started from

```fun
{ macro spin(_) <diverging>;
  macro keep(d : List(Decl)) : List(Decl) { Syntax.expand_decls(d) };
  M = module { keep({ pub x = spin(0) }) };
  0 }
```

| runner | which macro the budget error names |
|---|---|
| OCaml | `spin#420` — the innermost application, the one whose body overran |
| port | `keep` — the outermost request |

Cause: `Budget.MacroApplication` (`dotnet/src/Fun.Compiler/Budget.cs:57`) uses the application
name only as the depth-0 request's demand, so a nested application never reaches the message,
while the prototype's `Eval_budget` raises the innermost `application.exceeded`.

## Ruling (user, 2026-09-25): name the **call stack**

> shouldn't we name a callstack? if it's genuinely caused by a combination of different stuffs?

So the answer to "innermost or outermost" is **neither alone**. A budget overrun can be the
product of several applications rather than one, and a single frame misattributes it either
way. The error should carry the chain of requests that spent the budget — which also means the
original question was the wrong question, and this ticket is now bigger than a message fix.

## What has to be settled before implementing — **ask the user, one short question at a time**

1. **Order** — outermost→innermost (a stack trace's order, and the direction the port's message
   already reads in) or innermost first (the frame that overran).
2. **Frame content** — the macro's name, and its site. The port's *macro* budget errors carry
   **no source position** today, while its *checker* ones do
   (`calling loop, in an evaluation while reading the type at <file>:1:57-1:64`), so a stack of
   bare names is only half of what the prototype prints. Spans on macro budget errors are the
   map's recorded fog item (elaborator errors carry no location) — decide whether this ticket
   takes that on or leaves it.
3. **Scope** — one stack across both kinds of request (macro applications *and* checker
   demands, of which the checker form already names one) or a stack of macro applications only.
4. **Truncation** — deep nesting needs a cap, and the cap has to say it truncated.

## What to do once those are answered

1. Build the chain in `Budget`/`MacroApplication` instead of keeping only the depth-0 name.
2. Prove it with the program above: the port should name **both** `keep` and `spin`, in the
   settled order.
3. Land the assertion in `BudgetTests.cs` that is blocked today (xUnit is 183 where the ruling's
   three observable behaviours wanted 184).

## Reading

- `dotnet/src/Fun.Compiler/Budget.cs:57` (`MacroApplication`), `BudgetTests.cs`
- the prototype's `Eval_budget` and `application.exceeded`
- [the observable budget cases](port-budget-observable-cases.md) — what landed, and why this
  one could not

---
title: "Port: a budget error should name the call stack of requests"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-25
resolution: Closed by the integrator, 2026-09-25 - implemented by a fork (af13dca, merged a954bac) and verified by re-running both suites and the corrected program. The port now names keep before spin, each with its site, truncating at both ends with K = 3. xUnit 184/184, conformance 751 cases 0 failed. All four rulings implemented.
assignee:
blocked_by:
---

# Port: a budget error should name the call stack of requests

> ## Resolution (2026-09-25) — closed
>
> Implemented by a fork (`af13dca`, merged `a954bac`); verified by the integrator re-running
> both suites and the program by hand, not taken on the fork's word.
>
> **The program below was mis-transcribed, in this ticket and wherever it was quoted.**
> `macro spin(_) <diverging>` is not the language's surface syntax — there is no `<diverging>`
> annotation in either implementation (`grep -rn diverging` finds only the prototype's
> `test/backend/test_core.ml:1397` `diverging_body`), and **both runners reject the spelling**
> with `expected { body } after fn parameters`. The divergence it was standing for is real; the
> transcription was not. The corrected program — the prototype's own, from
> `test_core.ml:2051` — and what each runner now prints:
>
> ```fun
> { macro spin(_) { rec loop : I64 -> I64 = fn(n) { loop(n) }; loop(0) };
>   macro keep(d : List(Decl)) : List(Decl) { Syntax.expand_decls(d) };
>   M = module { keep({ pub x = spin(0) }) };
>   0 }
> ```
>
> | runner | output |
> | --- | --- |
> | OCaml | `ELAB expanding macro 'spin#412' exceeded the evaluation budget of 1000000 calls, calling loop#410 (…)` — the innermost application, as before |
> | port | `ELAB evaluation exceeded the budget of 1000000 calls while type checking: the application of macro 'keep' at <unknown>:1:153-1:157 / an evaluation / the application of macro 'spin' at <unknown>:1:168-1:175 / an evaluation / calling loop (…)` |
>
> **All four rulings, as built:** outermost → innermost; frames `<request> at <site>`, with macro
> applications now carrying their application site's span (threaded from the expander's use sites
> through `IMacroRuntime.ApplyExpr/ApplyDecls` → `Loader.ApplyMacro`; the type-aware path passes
> `call.Span`, the declaration path `call.Head.Span`, which is why the snippet the fork first
> quoted has no position); one stack across macro applications *and* checker demands; and
> both-ends truncation at **K = 3** — verified on a three-deep nest
> (`keep` / `an evaluation` / `mid` / `… 1 more …` / `spin` / `an evaluation` / `calling loop`).
> `Budget` keeps a `List<Frame>` pushed and popped in `Start`'s `finally`, cleared when the
> outermost request begins.
>
> **Counts:** xUnit `183 → 184/184`, conformance `751 cases, 0 failed` (unchanged — this ticket
> adds no shared case on purpose: a shared case can only say `error`, and the point is *which*
> error, which is the exception the ruling records for xUnit). That closes the last of the
> ruling's three observable budget behaviours and its 184th test.
>
> **One deliberate presentation choice, recorded rather than left implicit:** the evaluator's
> re-entry produces `an evaluation` frames interleaved between the macro frames. They are kept
> because they are truthful nesting and ruling 3 put checker demands *in* the stack — not a new
> decision, but a reader of the message should know it was chosen.
>
> **One artifact, not a defect:** the snippet's sites read `<unknown>:…` because the conformance
> driver elaborates a file it was given no name for; the line and column are the real ones.

Found by the fork that landed [the observable budget cases](port-budget-observable-cases.md)
(2026-09-25), which stopped rather than land a test either way. Then ruled on by the user the
same day — and the ruling is better than either option that was put to them.

## The divergence it started from

```fun
{ macro spin(_) { rec loop : I64 -> I64 = fn(n) { loop(n) }; loop(0) };
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

**Follow-up ruled 2026-09-25: the order is outermost → innermost** ("like a stacktrace"), so
the port must name `keep` before `spin` in the program below. Three sub-questions remain —
frame content, scope, truncation — and they are asked one at a time, in the order below.

## All four settled (user, 2026-09-25) — ready to implement

| # | sub-question | ruling |
| --- | --- | --- |
| 1 | order | outermost → innermost ("like a stacktrace") |
| 2 | frame content | `<macro name> at <site>` — this ticket also adds spans to macro budget errors |
| 3 | scope | one stack across macro applications **and** checker demands |
| 4 | truncation | outermost K, `… N more …`, innermost K |

1. **Order — outermost → innermost.** ✓ *Ruled by the user 2026-09-25: "like a stacktrace".*
   The outermost request comes first and the frame that overran last, which is also the
   direction the port's message already reads in. So the program below must name `keep`
   before `spin`.
2. **Frame content — a macro's name *and* its site.** ✓ *Ruled by the user 2026-09-25:*
   *"Name + site now."* Each frame reads `<macro name> at <site>`, so **this ticket also takes
   on the span half**: the port's *macro* budget errors carry no source position today while
   its *checker* ones do
   (`calling loop, in an evaluation while reading the type at <file>:1:57-1:64`), and a stack of
   bare names is only half of what the prototype prints. Spans on macro budget errors are the
   map's recorded fog item (elaborator errors carry no location) — this ticket now owns that
   half rather than leaving it implied.
3. **Scope — one stack across both kinds of request.** ✓ *Ruled by the user 2026-09-25.* Macro
   applications and checker demands are frames in the same chain, each labelled with what it
   was; the checker form's existing `in an evaluation while reading the type at …` wording
   becomes a frame in that chain rather than a message of its own. A mixed overrun — a checker
   demand that forces a macro application — is exactly the case the original ruling was aimed at.
4. **Truncation — both ends, with an elision.** ✓ *Ruled by the user 2026-09-25.* Show the
   outermost K frames, `… N more …`, then the innermost K frames: the head of the chain (who
   asked) and the tail (what overran) both survive. The ruled order makes that load-bearing —
   in outermost→innermost order the culprit is always last, so a plain prefix cap would drop it.
   K is the fork's to pick and to record.

## What to do

1. Build the chain in `Budget`/`MacroApplication` instead of keeping only the depth-0 name.
2. Prove it with the program above (the corrected one — `<diverging>` parses in neither
   implementation): the port should name **both** `keep` and `spin`, `keep`
   first, each with its site.
3. **Carry spans on macro budget errors** (sub-question 2's other half): a macro application
   must know its source position the way a checker demand already does.
4. Land the assertion in `BudgetTests.cs` that is blocked today (xUnit is 183 where the ruling's
   three observable behaviours wanted 184).

## Reading

- `dotnet/src/Fun.Compiler/Budget.cs:57` (`MacroApplication`), `BudgetTests.cs`
- the prototype's `Eval_budget` and `application.exceeded`
- [the observable budget cases](port-budget-observable-cases.md) — what landed, and why this
  one could not

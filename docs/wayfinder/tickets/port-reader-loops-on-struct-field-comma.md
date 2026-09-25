---
title: "Port: the reader loops forever on a comma in a struct field list"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-25
resolution: Closed 2026-09-25 - fixed by a fork (74f7a8c + 96fdb69, merged) and verified by the integrator: port 770 cases 0 failed and 185/185 xUnit, the four programs all terminate, and the guard covers module and block items as well as struct fields. One robustness gap the fork reported is filed as its own ticket.
assignee:
blocked_by:
---

# Port: the reader loops forever on a comma in a struct field list

> ## Resolution (2026-09-25) — closed
>
> Fixed by a fork (`74f7a8c` reader, `96fdb69` cases, merged) and verified by the integrator.
> **The root cause is exact**: `TakeStatement` splits at `;` *and* `,`, but `DropSeparators`
> strips only `;` — so a leading comma left the term list **unchanged** and `ReadContext`'s
> statement loop asked for another statement forever.
>
> **The fix is the invariant, not the case**: `Enforest.RequireAdvance(before, after)` refuses a
> step that hands back as many terms as it was given, naming the offending token's position —
> and it guards **all three statement walks**, `ReadContext` (struct fields, eagerly-read blocks,
> module items), `BlockDecls`, and `Expander`'s `Binding.Items` case (module and block items read
> lazily).
>
> **Verified by the integrator, both the four programs and the class claim**:
>
> | program | before | after |
> | --- | --- | --- |
> | `struct { f : I64 }` (control) | `VALUE 1` | `VALUE 1` |
> | `struct { f : I64; g : I64 }` (control) | `VALUE 1` | `VALUE 1` |
> | `struct { f : I64, g : I64 }` | **killed at 20 s** | `ELAB unexpected token in a definition context at <unknown>:2:22-2:23` |
> | `struct { f : I64, }` | **killed at 20 s** | same refusal, same position |
> | a comma in a **module** body | hung | `ELAB unexpected token in a definition context at <unknown>:1:24-1:25` |
> | a comma in a **block** | an error already | `ELAB empty block` (its own pre-existing error; it terminated before and after) |
>
> So the check is genuinely class-wide rather than a struct-field special case. Match arms are the
> one family left alone, and legitimately so: `SplitMatchBranches` is a separate loop that
> structurally always shrinks (`match (…) { s => s.f, }` answered `1` before and after).
>
> **Counts**: port `770 cases, 0 failed` (was 766) and `185/185` xUnit. The four cases are
> ordinary — two `.expect error`, two controls that must keep answering `1` — and
> `prototype-divergences.txt` is untouched. These are the first cases in the suite whose real
> content is *"must terminate"*.
>
> **Robustness gap the fork reported, filed rather than left in its report**: the conformance
> runner does not timebox elaboration, so a *future* non-advancing step would hang the suite
> instead of failing it — the fix removes the known loops, and only
> [a runner timeout](port-runner-does-not-timebox-elaboration.md) catches unknown ones.

**A hang on a grammar error.** The language spells struct fields with `;`; write `,` and the
reader never advances, so the compiler spins at 100% CPU with no diagnostic. Any typo of that
shape wedges the compiler.

## The smallest program

```fun
{ R = struct { f : I64, g : I64 }; 1 }
```

| what you write | what happens |
| --- | --- |
| `struct { f : I64 }` (one field) | `VALUE 1` |
| `struct { f : I64; g : I64 }` (the language's separator) | `VALUE 1` |
| `struct { f : I64, g : I64 }` (a comma) | **no output, forever** |
| `struct { f : I64, }` (a trailing comma) | **no output, forever** |

Measured by the integrator 2026-09-25, after the prototype was deleted, so there is no second
implementation to compare with — but the OCaml prototype **did not hang** on the comma shape: it
reported `Enforest_util.Error("unsupported pattern in Phase 7B match")`, i.e. it refused the
program. So this is a port defect the corpus never saw (no case writes a comma in a struct) and
the differential harness could never have seen (a hang is not a disagreement, it is no answer).

## What it does, with evidence rather than inference

- **Debugger**: with `DOTNET_PerfMapEnabled=1`, `dotnet-stack report -p <app pid>` (the *child*,
  not the `dotnet` muxer) gives the parked stack:

  ```text
  Enforest.DropSeparators ← Enforest.TakeStatement ← Enforest.ReadContext
  ← Enforest.ParseStructExpr ← Enforest.ParsePrimary ← Enforest.ParseExprPrec
  ← Enforest.ParseAll ← Enforest.ParseValueDeclStatement ← Enforest.DoStatement
  ← Enforest.ParseBlockHead ← Expander.Expand ← Driver.Elaborate
  ```

  So it is purely the **reader/enforester**, before elaboration: a statement that consumes
  *nothing* and is asked for again, forever.
- **Resource profile**: flat RSS (~72 MB, unchanged over 10 s) and CPU time rising linearly —
  a tight spin, no allocation, no growing structure.
- **The checker budget cannot catch it**: the budget is spent on unification, function calls and
  fixpoint unfolds only, and this loop produces none of them. Dropping `DefaultLimit` to 30 000
  changes nothing (verified), so the budget is not the guard to rely on here.
- Neither of the two plausible value-level candidates was the loop: a guard in `Nbe.ForceLoop`
  never fired, nor did guards in the machine's term-reduction loop. Both were reverted.

## What to do

1. **Make the reader advance or refuse.** The rule the port is missing is the same one
   `CLAUDE.md` records for the deleted prototype: *a function that consumes tokens and returns the
   remaining tokens must never hand back the same list*. `ReadContext`'s statement loop needs the
   cheap invariant check — if a statement step did not shorten the terms, that is a parse error,
   not a retry.
2. **A grammar error is a language error.** A comma there should produce the reader's ordinary
   "unexpected token" refusal naming the position, in the port's words. It must not hang and must
   not be an unported path.
3. **Guard the class, not the case.** A non-advancing step is a whole family of bugs (the same
   shape exists in any `while` over a token list). A single check in `ReadContext` covers
   `struct {…}`, blocks, modules and match arms alike.
4. **Tests** — the four programs in the table, as `elaborate/…` cases with `.expect error`:
   the comma and the trailing comma must be *errors* (a hang fails the suite, which is what they
   are for), and the semicolon and one-field forms are the controls that must keep answering `1`.
   This is the first case in the suite whose real content is "must terminate".

## Why it matters more now

The port is the only implementation. A hang gives the user no position and no message, and the
budget — the obvious place to look for a bound — structurally cannot see it. Anything that walks
a token list is the same hazard, so the check belongs where the walk happens rather than around
this one grammar production.

---
title: "A pattern synonym's nominal head is captured by the use site's scope"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# A pattern synonym's nominal head is captured by the use site's scope

**Confirmed in both implementations, 2026-09-25** — the prototype has this defect too, so it is a
language bug, not a port gap. It is the hazard
[the sealed-nominal head ticket](port-pattern-synonym-over-sealed-nominal-head.md) recorded as
unprobed when that ticket closed on prototype parity; the probe now exists and it fires.

## The program, and what each runner does

Declare a pattern synonym whose right-hand side is a type from a generative module, then use it
from a scope with **one more name in it**:

```fun
{ SymbolTable = fn(u : Unit) { module {
    table = ref("");
    pub type Symbol = Sym(String);
    pub intern = fn(s : String) { table <- s; Sym(deref(table)) } } };
  st1 = SymbolTable(());
  M = module { pub pattern S = st1.Symbol };
  f = fn(x : Type) { match (st1.Symbol : Type) { M.S => 42, _ => 0 } };
  f(I64) }
```

| program | OCaml | port |
| --- | --- | --- |
| the same `match` at **top level** (0 extra names) | `VALUE 42` | `VALUE 42` |
| inside `fn(x : Type) { … }` (1 extra) | `EVAL EvalError("field not found")` | `EVAL no member \`Symbol\`` |
| inside `fn(x : Type, y : Type) { … }` (2 extra) | `EVAL EvalError("field access on non-struct")` | `EVAL member access \`.Symbol\` on a non-module` |
| after `k = 7;` in a block (1 extra, a `let`) | `EVAL EvalError("field not found")` | `EVAL no member \`Symbol\`` |

The working case and the failing ones differ **only** in how many names are in scope where the
`match` sits. `42` should come out in all four.

## What happens, plainly

The synonym's right-hand side is a *term* — `st1.Symbol` — that the compiler keeps and runs again
at every use, and it runs it **in the environment of the use site**: the chain of bindings around
the `match`. Elaborated terms name bindings by position, not by name, so the binding `st1` the term
means is only correct when the use site happens to put it at the same position. Add a parameter or
a `let` in between, every position shifts by one, and `st1.Symbol` becomes "the variable `x`, then
`.Symbol`" — hence `no member \`Symbol\`` / `field access on non-struct`.

That is textbook variable capture, and it is why
[the sealed-nominal head ticket](port-pattern-synonym-over-sealed-nominal-head.md) originally
prescribed a **definition-site closure**: the head term should run where it was written, against
the environment it was written in, not against whatever is in scope at the `match`.

## What was tried, and why it did not land

The fork that closed that ticket implemented the definition-site closure first and reported it
fails: inside the module the elaborated head term is `Dot(Var 1, Symbol)` whose slot is a `VVar`
elaboration artifact, so under the recorded environment it stays a stuck `VNeutral` rather than
becoming a nominal. It then shipped **match-site re-evaluation** — which is prototype parity, and
which is what this ticket shows to be wrong. So the closure is the right direction and the earlier
attempt did not find its shape; that is the thing to solve, with the `VVar` artifact above as the
specific obstacle to get past rather than a reason to keep parity.

## What to do

1. **Give the head term a definition-site closure** — a captured environment (or a quoted term
   carrying one) that the matcher evaluates under, so the head resolves as written, whatever is in
   scope at the use site.
2. **Reproduce the three failing shapes above before and after**, and keep all four programs: the
   working one is the control that says the fix did not break the everyday case, and the three are
   the regression.
3. **Both implementations, or a divergence — ask before choosing.** The prototype fails all three
   exactly as the port does, so a fix in C# only would need the cases listed in
   `test/conformance/prototype-divergences.txt`; the alternative is the route taken for
   [an unused type parameter](port-generative-former-phantom-parameter.md), which the user ruled must land
   in both. This ticket does not assume which.
4. **The case cannot be committed before the fix** (porting convention 8 — the conformance count
   never drops). Test with the programs above once the fix is in; if the ruling is "C# only", the
   four cases are ordinary-versus-divergence per that ruling.

## Reading

- [the sealed-nominal head](port-pattern-synonym-over-sealed-nominal-head.md) — the closed ticket,
  its recorded deviation, and the four cases that pass today because they all sit at top level
- `dotnet/src/Fun.Compiler/Nbe.Generative.cs` — `MatchesNominalHead` / `SameInstance`, the
  comparison that evaluates the head term, under the match's environment as `nbe.ml:716` does
- `dotnet/src/Fun.Compiler/Elaborator.Patterns.cs`, `Core.Patterns.cs` (`NominalHead`, and what it
  carries: a term plus, today, no environment)
- `lib/backend/interp/nbe.ml:716` — the prototype's `same_instance mc env …`, the same defect

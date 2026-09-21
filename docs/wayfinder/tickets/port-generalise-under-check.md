---
title: "Port: generalizing the argument to an unknown-typed function under a check"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: generalizing the argument to an unknown-typed function under a check

Found by [the implicit application](port-implicit-application.md) fork while
strengthening its cases, **verified by the integrator in both runners**: an inline
polymorphic lambda and the same lambda bound to a name behave differently, and the two
implementations are wrong in *opposite* directions.

```fun
-- inline: implicit-lambda-argument-inline.fun  (expect 7)
{ h = fn(g) { g[I64](7) }; h(fn[A : Type](a : A) { a }) }

-- named: implicit-lambda-argument-named.fun  (expect 7)
{ ch = fn[A : Type](a : A) { a }; h = fn(g) { g[I64](7) }; h(ch) }
```

| program | prototype | port |
|---|---|---|
| inline | `7` | `type mismatch: cannot unify VAtomTy with VVar` |
| named | `UnifyError(CannotUnify(function type vs function type))` | `7` |

## Ruled (integrator, 2026-09-20): both must be accepted

**Reaffirmed and widened by the user (2026-09-20): all three of these are accepted**, and
none of them is an error —

```fun
{ h = fn(g) { g[I64](7) }; h(fn[A : Type](a : A) { a }) }              -- inline, → 7
{ ch = fn[A : Type](a : A) { a }; h = fn(g) { g[I64](7) }; h(ch) }    -- named,  → 7
{ h = fn(g) { g[I64]; 7 }; h(fn[A : Type](a : A) { a }) }             -- no result applied, → 7
```

— so the third is already correct in both runners, and each implementation must stop
rejecting its half of the first two.

The two programs differ only by let-inlining — the same term, one written inline and one
bound first. **Generalization must not depend on that.** This is the project's own
stated priority (`Consistency > Flexibility > Correctness`, root `README.md`) applied to
the one place the port has already decided it is allowed to be more correct than the
prototype: the prototype is not maintained after the port, so its rejection of the named
form is a **prototype defect** and the port's rejection of the inline form is a **real
gap**. Both get fixed in C#; neither implementation gets to keep its half.

So this ticket has three pieces of work:

1. **Fix the port's check path** so the inline polymorphic lambda generalizes — the
   inline program must answer `7`. This is in `Check`/generalization (`Elaborator.cs`,
   `Elaborator.Generalise.cs`), not in the `Implicits` code the implicit-application fork
   mirrored, which is why that fork correctly left it alone.
2. **Record the named form as a divergence** (convention 5): the shared case
   `implicit-lambda-argument-named` with `expect` `7`, listed in
   `test/conformance/prototype-divergences.txt` naming this ticket. The OCaml runner will
   report a listed case that *passes*, which is how the divergence is noticed if the
   prototype is ever fixed.
3. **Add both as shared cases** and run them through both runners as the acceptance
   condition — after the fix the inline one is an ordinary case and the named one is a
   divergence; before the fix the inline one fails, which is the point.

## The cause (confirmed in code, 2026-09-20)

Both failures are the *same* root, which is why the two implementations are wrong in
mirror image: **let-generalisation abstracts an undecided meta into a rigid variable
before the other side can solve it.**

`Elaborator.cs:553` generalises the value of a `let`, and `Elaborator.Generalise.cs`
does it by *solving* each unsolved meta of the type to a rigid variable:

```csharp
for (var i = 0; i < n; i++) ctx.Metas.Solve(unsolved[i], new Value.VVar(ctx.Width + n - 1 - i, []));
```

`h = fn(g) { g[I64](7) }` is a single-parameter lambda, so it is generalised: the implicit
domain that `g[I64]` created (`InferApImplicitUnknown`) is turned into a rigid variable.
At `h(fn[A : Type](a : A) { a })` the written `Type` is then checked against a **rigid
variable** instead of a solvable meta — hence `cannot unify VAtomTy with VVar`.

OCaml's half is the mirror: it generalises `ch`'s fully-determined type at its binding,
and unifying that with the inferred `[?] -> ?` gives `CannotUnify(function type vs
function type)`. The named case works in C# precisely because `ch`'s type has no
unsolved meta, so nothing is abstracted; the inline case works in OCaml because `h`'s
body is never let-generalised the same way.

**Not established:** which side should move. The candidates are (a) insert the callee's
implicit arguments *before* checking the argument, so the domain is a fresh meta rather
than the generalised rigid variable, or (b) keep generalisation from abstracting metas
that a call site still supplies. Decide by finding which is consistent with the
prototype's restriction (`ClosedUnder(body, 1)` — "only where the lambda names nothing
outside itself"), not by what makes this case pass.

`Generalise` is also one of the sites whose `ClosedUnder` traversal used to
"not ported yet" on an unknown binder form; the G2 work replaced that with the single
`Term.Map`, so its closedness check is now total.

## Also recorded

Cosmetic, from the same fork: the prototype prints `<lam>` where the port prints `VLam`
when a value is described. Not worth a ticket on its own; fix it if a case ever becomes
observable.

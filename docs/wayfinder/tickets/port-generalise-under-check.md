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

Be careful not to "fix" this by making the inline case *also* reject: the direction is
fixed by the ruling above. If reading the two paths shows the generalization rules differ
in more than this shape, report it rather than patching the one case — an eta/inlining
invariance that holds for one shape and not another is the same defect elsewhere.

## Also recorded

Cosmetic, from the same fork: the prototype prints `<lam>` where the port prints `VLam`
when a value is described. Not worth a ticket on its own; fix it if a case ever becomes
observable.

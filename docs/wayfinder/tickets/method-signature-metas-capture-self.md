---
title: A meta in a method's signature captures `self`, so calling the method fails
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-27
resolution: Implemented 2026-09-27 by a fork (fix cea0b3d, tests 7460131, merged 6b706b8). Root cause: MethodType/MethodBody (Elaborator.Structs.cs:112/131) elaborated written parameter and result types after BindAnonymous(self), so FreshMeta's EntryKinds spine included self; a call substitutes the receiver and Unify.Invert rejects the non-variable. Fix: Context.WithoutSelfInMetas(level) marks self's entry Defined, so InsertedMeta skips it, threaded through MethodType, Params and MethodBody - both VPi builders, so unify compares like with like. MethodBody's promise check now unifies body-first so a body's self-carrying metas solve instead of escaping. Cases method-300 (7), method-301 (40), method-302 (4). Conformance 779 cases 0 failed, xUnit 185/185. Three out-of-scope findings recorded below; the first is now its own ticket.
assignee:
blocked_by:
---

# A meta in a method's signature captures `self`, so calling the method fails

Found by the small-followups-2 run (2026-09-16); pre-existing, unrelated to
effect rows.

## Defect

A method's parameter types are elaborated with `self` already bound
(`Ctx.bind_anonymous` in `Elab_infer`'s `method_type` and `elaborate_method`), so
an **inserted meta** in a parameter's type carries `self` in its spine. At a call
the method's `VPi` is applied to a real struct value, that spine entry is no
longer a variable, and solving the meta is no longer a pattern problem.

```fun
{ S = struct { k : I64; pub method keep(r : Ref(I64)) : Ref(I64) { r } };
  s = S{ k = 2 }; x = ref(40); y = s.keep(x); 7 }
// UnifyError(VarNotInSpine(59))

{ S = struct { k : I64; pub method bump(r : Ref(I64)) ->{Mutate(r)} I64 { deref(r) } };
  s = S{ k = 2 }; x = ref(40); s.bump(x) }
// UnifyError(NonVariableInSpine)   — the spine's first entry is the record
```

`Ref(I64)` is `Ref(?h)(I64)`, so the hidden heap is the meta; any implicit
argument in a parameter type does the same. The declaration alone elaborates; only
the call fails. The same signature on a plain `fn` works, because there is no
`self` binder to substitute.

## Direction

The method's signature should not capture the self binder: elaborate parameter
types, the result and the row in a context where `self`'s entry is not part of a
meta's spine (e.g. create signature metas before binding `self`, or abstract them
over it), so a call substitutes only variables. Check `method_type` and
`elaborate_method` together — they build the same `VPi` twice and
`unify_method_types` compares them.

Regression tests: both programs above, plus a method whose parameter type is a
user type with an implicit argument.

## Closed 2026-09-27

Implemented by a fork (`general-purpose` on glm-5.3; fix `cea0b3d`, tests `7460131`, merged
`6b706b8`), and verified by the integrator: **conformance 779 cases, 0 failed**, xUnit **185/185**.

**Root cause.** `MethodType`/`MethodBody` (`src/Fun.Compiler/Elaborator.Structs.cs:112`, `:131`)
elaborated a written parameter or result type *after* `BindAnonymous(self)`, so `FreshMeta`'s
`EntryKinds` spine listed `self`. At a call the receiver is substituted in, `Unify.Invert`
(`Unify.cs:144`) finds a non-variable in the spine and refuses: `a meta's spine argument is not a
variable` — the error both programs in this ticket showed.

**Fix.** `Context.WithoutSelfInMetas(level)` marks `self`'s entry `Defined`, so `Nbe.InsertedMeta`
skips it; it is threaded through `MethodType`, `Params` and `MethodBody`. The promise check in
`ElaborateMethod` now unifies `type` first and `promised` second, so the body's self-carrying metas
solve against the promise rather than escaping — a local heap in
`pub method m() ~> Ref(I64) { ref(self.k) }` still works.

**Cases.** `values/method-300` (the ticket's first program, `7`), `method-301` (its second, `40`),
and `method-302` (`4`) for the third — with the rewording below.

**Three things this did not fix, each measured on `main` *and* on base `a2c0044` so the ticket does
not invent a regression:**

1. **A parameter's type meta captures the *earlier parameters*** — not just `self`. Moved to its own
ticket: [a parameter's type meta captures the earlier binders](parameter-type-metas-capture-earlier-parameters.md).
2. **The ticket's third program cannot be written as suggested.** A user type with an *unsupplied*
implicit is rejected in type position — `Ref(Pair)` where `Pair = fn[A, B] { struct { … } }` gives
`cannot unify VStruct with VU`, for a method and for a plain `fn` alike. `method-302` supplies the
implicit (`Ref(Pair[I64, Bool])`) instead, which exercises the same hidden heap. Where an implicit
*should* be inserted inside `Ref(…)` is a question this ticket does not answer; the failure is
pre-existing and identical in both shapes.
3. **A method on a struct inside a `fn[A : Type]` body still fails** (`Box[I64]{…}.get(x)`), and the
identical plain-`fn` shape fails on base the same way — the ambient `A` is substituted at
`Box[I64]`, the same non-variable-in-spine story one layer out. Pre-existing, untouched.

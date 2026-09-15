---
title: Domain model — effects
parent: ../fun-design-map.md
---

# Domain model — effects

Fourth pass of the model (pass one: the [elaborate ↔ evaluate
boundary](core-tt-domain-model.md); pass two: [surface and
enforestation](core-tt-domain-model-surface.md); the third — macro evaluation
semantics — is still pending). The model here is the set of decisions recorded in
the five effect tickets: [bare-arrow-is-pure](../tickets/bare-arrow-is-pure.md),
[checker-evaluation-budget](../tickets/checker-evaluation-budget.md),
[handlers-tunnel-callback-effects](../tickets/handlers-tunnel-callback-effects.md),
[refs-in-effect-rows](../tickets/refs-in-effect-rows.md),
[nominal-identity-applicative-by-purity](../tickets/nominal-identity-applicative-by-purity.md).
Vocabulary lives in the root [`CONTEXT.md`](../../../CONTEXT.md); this document
holds the invariants behind it, each marked **enforced by construction**,
**checked**, or **unchecked convention** — or, where the decision is not yet
built, **decided, not implemented**, naming the ticket that is the distance.

## The model in brief

A function's row names the effects it may perform when called. The empty row —
**pure** — is what a bare arrow means, and purity is load-bearing twice: it is
what lets the checker evaluate a call while type checking, and what makes a
nominal declared in the call applicative rather than generative. Handling is
lexical: a handler handles the effects its own scrutinee performs; a callback's
effects tunnel past it, because they belong to the callback's row. Mutation is
three heap effects, and internal mutation discharges at generalisation.

## Invariants

### E1 — an effect family is a nominal

**Status: enforced by construction.** An effect's identity is its declaration
plus the values of its parameters: `State(I64)` is one effect wherever written,
and `State(I64).get` is a different handler branch from `State(Bool).get`
(`find_effect_branch` compares the operation *and* the full instance by
conversion).

### E2 — a row is a set with an optional tail

**Status: enforced by construction.** Concrete effects are order-insensitive
and duplicate-free; a row tail flattens into the prefix
(`normalize_effect_row_value`). Closed rows (`can {IO}`), open rows (`can {IO |
r}`) and row metavariables all normalise to this shape before conversion.

### E3 — a bare arrow is pure

**Status: decided, not implemented — today's default is the opposite.** In the
model `A -> B` means `A -> B can {}`; `A -> B can _` infers the row (Koka's
`_e`), generalised at `let`, which is where effect polymorphism comes from;
`~>` is prelude sugar for the inferred form. Today an omitted row *is* the
inferred form — a fresh meta tail (`elab_type_expr.ml`) — so every unannotated
arrow is possibly effectful and the checker must assume the worst. The
unwritten case must be the safe one. Distance:
[bare-arrow-is-pure](../tickets/bare-arrow-is-pure.md).

### E4 — the checker evaluates pure closed terms under a budget

**Status: budget enforced; purity still by the wrong criterion.** No
termination check exists or is wanted; divergence is not an effect. Each checker
request to the evaluator spends from one budget, and exhaustion is an elaboration
error naming the source call, the demand and the form being checked
([checker-evaluation-budget](../tickets/checker-evaluation-budget.md),
[budget-error-names-no-source-call](../tickets/budget-error-names-no-source-call.md),
closed). The budget measures work — every call and every conversion or
unification step — and macro expansion spends from the same budget (M5; there is
no separate fuel). Recursive definitions unfold on open arguments under it; two
applications of the same *pure* fixpoint to convertible arguments compare without
unfolding (lazy delta, `VGlued`)
([recursive-definitions-stuck-on-open-arguments](../tickets/recursive-definitions-stuck-on-open-arguments.md),
closed). Running a program stays unbudgeted. Effects are computed in
the one inference pass, and a let is evaluated at check time only when its value
performs nothing. Distance: refs contribute no effect, so a ref-using value still
counts as pure ([refs-in-effect-rows](../tickets/refs-in-effect-rows.md));
and a fixpoint counts as pure only with a written `can {}`, since a bare arrow's
row is still open ([bare-arrow-is-pure](../tickets/bare-arrow-is-pure.md)).

### E5 — handling is lexical, not dynamic

**Status: decided, not implemented — handling is dynamic today.** A handler
handles the effects its own scrutinee performs. An effect raised by a function
passed *in* belongs to that function's row and tunnels past handlers in code
polymorphic over that row. Today an unhandled request is rewrapped into the
nearest enclosing handler (`handle_effect`'s `resume_with`), so a library's
internal handler catches a callback's raise that no type ever mentioned — the
repro in [handlers-tunnel-callback-effects](../tickets/handlers-tunnel-callback-effects.md)
answers `0` where the model answers `999`. The port must route operations by
evidence (the handler the row was bound to), not by a runtime search.
Distance: same ticket.

### E6 — a handler's effects may not escape its scope

**Status: decided, not implemented — no check exists.** A stored continuation
may be resumed after its branch returns (schedulers, async); resuming re-enters
the handler scope. What may not happen is a closure whose row *names* a handled
effect escaping the handler that handles it — the handler binds its effect like
a type variable, and the escape check is the one existentials and `runST`-style
brands need. No such check exists today: an escaping closure fails only at run
time with "unhandled effect". Distance: the same ticket's second half. A
program's residual row is checked empty at its entry
([unhandled-effects-pass-the-checker](../tickets/unhandled-effects-pass-the-checker.md),
closed), so an escaping closure called at the top is an elaboration error; one
that escapes and is never called at the top is not yet caught.

### E7 — continuations are one-shot

**Status: enforced by construction, at run time — which is the model's choice.**
A second `resume` of the same continuation is a run-time error
(`continuation already used`), and the model keeps it there; multi-shot is out
(it conflicts with refs and C frames). The companion rule — never captured
across an extern frame — has nothing to attach to yet: there is no extern
mechanism. It attaches when one exists.

### E8 — handlers are deep

**Status: enforced by construction.** Branch bodies and resumed continuations
run under the active handler loop (`handle_body` re-enters on every `Effect`),
so an effect performed inside a branch is handled by the same handler.
Generators stepped one value at a time and per-operation handler changes are
written with deep handlers plus state, not shallow handlers.

### E9 — `resume` is lexically scoped to the effect branch

**Status: enforced by construction.** Available inside nested lambdas of an
effect branch (threaded as `Ctx.resume_entry`), rejected everywhere else.
It is a syntax form, not a variable — nothing can capture it.

### E10 — mutation is three heap effects over a branded reference

**Status: decided, not implemented.** `Alloc(h)`, `Read(h)`, `Write(h)`;
surface `Ref(A)` abbreviates `Ref(h, A)`. Three rather than one merged `Mut(h)`
so read-only code earns the weaker row; parameterised rather than a constant
effect because discharge needs a parameter to compare — a constant effect
cannot tell `make` (refs stay inside, effect may drop) from `leak` (a live cell
escapes, effect must stay), and `Ref` is already the type's name in the one
bare-name namespace anyway. **Discharge is automatic**: a definition's heap
effects for a heap that does not occur in its type are dropped at
generalisation — `runST`'s condition, met by inference. Today `RefNew`,
`RefGet` and `RefSet` collect no effect of their own (`elab_effect_collect.ml`),
`compile_time_safe` stands in syntactically, and refs are invisible in types.
Distance: [refs-in-effect-rows](../tickets/refs-in-effect-rows.md).

### E11 — nominal identity is applicative by purity

**Status: decided, not implemented.** Same declaration + convertible free
variables = same type; generative only under a run-time effect, inferred from
the row, never declared. Forced by dependent types (the checker re-evaluates
`Set(I64, cmp).T` during conversion — a type minted per evaluation would not
equal itself) and by the `SymbolTable` abstraction case. Today a nominal ADT
declared under a binder does not evaluate at all (`mk(I64)(1)` → `unbound
nominal type: T`; a structural record under a binder does evaluate);
`nominal_id` is minted once per declaration at elaboration (`NominalId.fresh`), so neither
applicative nor generative semantics is implemented. A recursive record's identity (`fresh_record_id`, minted by its
`rec` binding) has the same limit. Distance:
[nominal-identity-applicative-by-purity](../tickets/nominal-identity-applicative-by-purity.md),
blocked on E10. [adts-as-let-bindings](../tickets/adts-as-let-bindings.md) is
blocked on this.

## Model decision vs today

Checked against main on 2026-09-15 (probes in the effects audit).

| model | implementation today |
|---|---|
| bare arrow is pure | omitted row = fresh meta tail — the opposite default ([ticket](../tickets/bare-arrow-is-pure.md)) |
| checker evaluates under a budget | enforced (work-measured, names the call); purity is the one-pass row, refs excepted |
| handling is lexical (tunnels) | dynamic — nearest enclosing handler catches ([ticket](../tickets/handlers-tunnel-callback-effects.md)) |
| handler-scope escape check | none; an effect unhandled at a program's entry is an elaboration error |
| one-shot continuations | run-time `used` check — matches the model |
| deep handlers, `resume` scoped | enforced, tested |
| rows set-like, normalized | enforced |
| effect identity = family + params | enforced (`runtime_value_equal` on id and params) |
| mutation = three heap effects, branded refs, discharge | refs contribute no effect; invisible in types ([ticket](../tickets/refs-in-effect-rows.md)) |
| nominal applicative by purity | nominal ADT under a binder does not evaluate ([ticket](../tickets/nominal-identity-applicative-by-purity.md)) |

## What the port's types should be named after

- **Effect family** and **Effect row** for the declaration and the annotation;
  **Residual row** for what survives a handler.
- **Heap** for the store region a mutation effect is parameterised by;
  **Reference** for the branded cell — `Ref(h, A)` under the surface `Ref(A)`.
  Not `region` (that is lifetime inference's word), not `Ref` as an effect.
- **Mutation effect** as the trio `Alloc`/`Read`/`Write` — not a merged `Mut`,
  not `can Ref`.
- **Discharge** for dropping a non-escaping heap's effects at generalisation.
  Not `mask` — Koka's written `mask` is a different thing.
- **Evaluation budget** measuring work — one budget shared with macro
  expansion (M5); *fuel* is its retired name.
- Routing evidence for lexical handling, not a handler stack: the port's
  evaluator holds *which handler a row was bound to*, never "the current one".

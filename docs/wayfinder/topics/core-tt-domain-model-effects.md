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

### E2 — a row is a set of effects with a set of tails

**Status: enforced by construction.** Concrete effects are order-insensitive
and duplicate-free; a row carries a *set* of row variables (`->{IO | e1, e2}`,
2026-09-16), so a result can unite several callbacks' rows. A tail solved to a
row is spliced into the prefix (`normalize_effect_row_value`), so a union
disappears as its variables are solved. Closed rows (`->{IO}`), open rows
(`->{IO | r}`) and row metavariables all normalise to this shape before
conversion. Unification cancels the tails both sides name and solves a single
remaining one; a union of two unsolved tails against a concrete row is a
mismatch rather than a guess.

### E3 — a bare arrow is pure

**Status: implemented (2026-09-15).** In the
model `A -> B` means `A ->{} B`; a row sits on its arrow (`A ->{IO} B`, open
`A ->{IO | e} B`), `A ->{_} B` infers it and is an error when nothing solves it,
and `~>` is effect polymorphism: a parameter's `~>` mints a row variable bound at
the signature, a result's collects them ([bare-arrow-is-pure](../tickets/bare-arrow-is-pure.md),
[effect-arrow-syntax](../tickets/effect-arrow-syntax.md), closed). Remaining
distance: a result uniting two row variables needs multi-tail rows, which E2
rules out. Methods follow the same rule: pure unless `->{E} T` declares a row ([methods-follow-the-arrow-rule](../tickets/methods-follow-the-arrow-rule.md), closed).

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
and (historically) a fixpoint counted as pure only with a written empty row, before a bare arrow
was pure ([bare-arrow-is-pure](../tickets/bare-arrow-is-pure.md)).

### E5 — handling is lexical, not dynamic

**Status: implemented (2026-09-15).** A handler handles the effects its own
scrutinee performs. An effect raised by a function passed *in* belongs to that
function's row and tunnels past handlers in code polymorphic over that row: a
call whose row has an open tail is wrapped in `Core.Tunnel`, and a request of an
effect family the row does not name, but a handler lexically enclosing the call
in the same function body handles, skips those handlers (`effect_request.hops`).
The repro in [handlers-tunnel-callback-effects](../tickets/handlers-tunnel-callback-effects.md)
answers `999`. Granularity is the effect family, not the instance. The port may
route by evidence instead; the observable rule is the same.

### E6 — a handler's effects may not escape its scope

**Status: implemented (2026-09-15), for the result type.** A stored continuation
may be resumed after its branch returns (schedulers, async); resuming re-enters
the handler scope. What may not happen is a closure whose row *names* a handled
effect escaping the handler that handles it — the handler binds its effect like
a type variable, and the escape check is the one existentials and `runST`-style
brands need. A match whose result type carries a function whose row names a
family the match handles is `HandledEffectEscapes`. Not checked: an escape
through an outer ref's type (assigning the closure to a ref declared outside the
handler).

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

**Status: implemented (2026-09-15), in the grilled shape.** One surface effect
`Mutate(r)` (naming a reference; its heap stays hidden) rather than three —
`Mutate(r)` = `{Read(r), Write(r)}` stays a compatible later split. Surface
`Ref(A)` takes its heap `h` implicitly. The model text below is the original
decision. Three rather than one merged `Mut(h)`
so read-only code earns the weaker row; parameterised rather than a constant
effect because discharge needs a parameter to compare — a constant effect
cannot tell `make` (refs stay inside, effect may drop) from `leak` (a live cell
escapes, effect must stay), and `Ref` is already the type's name in the one
bare-name namespace anyway. **Discharge is automatic**: a definition's heap
effects for a heap that does not occur in its type are dropped at
generalisation — `runST`'s condition, met by inference. Implemented at function boundaries: a
heap created inside a function, absent from its type and not aliased by an older
heap, is dropped; the program entry's runtime handler discharges the rest
([refs-in-effect-rows](../tickets/refs-in-effect-rows.md), closed).

### E11 — nominal identity is applicative by purity

**Status: implemented (2026-09-16).** Same declaration + convertible free
variables = same type; generative only under a run-time effect, inferred from
the row, never declared. Forced by dependent types (the checker re-evaluates
`Set(I64, cmp).T` during conversion) and by the `SymbolTable` abstraction case.

- A nominal (and a `rec` struct occurrence) captures what its enclosing module
  or function body names, compared by conversion.
- Every module has a private stamp slot its nominals capture: `()` at check
  time and for a pure module, a fresh cell at run time for a module whose
  evaluation performs something - so type-case separates evaluations.
- Such a module's declared nominals are generative: sealed at the binder
  (`st1.Symbol`), and a sealed type may not leave its binder's scope, its
  module's type, or an unnamed module's field access (`GenerativeTypeEscapes`).

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
  not a `Ref` row.
- **Discharge** for dropping a non-escaping heap's effects at generalisation.
  Not `mask` — Koka's written `mask` is a different thing.
- **Evaluation budget** measuring work — one budget shared with macro
  expansion (M5); *fuel* is its retired name.
- Routing evidence for lexical handling, not a handler stack: the port's
  evaluator holds *which handler a row was bound to*, never "the current one".

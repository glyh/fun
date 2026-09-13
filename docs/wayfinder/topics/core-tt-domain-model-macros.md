---
title: Domain model — macros
parent: ../fun-design-map.md
---

# Domain model — macros

Third pass of the model (pass one: the [elaborate ↔ evaluate
boundary](core-tt-domain-model.md); pass two: [surface and
enforestation](core-tt-domain-model-surface.md); pass four:
[effects](core-tt-domain-model-effects.md)). Asked for by
[domain-model-macro-hygiene](../tickets/domain-model-macro-hygiene.md).
Vocabulary lives in the root [`CONTEXT.md`](../../../CONTEXT.md); this document
holds the invariants behind it, each marked **enforced by construction**,
**checked**, **unchecked convention**, or — where the decision is not yet
built — **decided, not implemented**, naming the ticket that is the distance.

## The model in brief

A macro is a program, elaborated where it was defined and run where it is
called. It sees and builds syntax through **reflection ADTs** that decompose
syntax totally — nothing is opaque; what the implementation cannot yet
decompose is scaffolding, not a boundary. Destructuring into the ADTs and
reconstructing is the **round trip**, and it must be the identity: every
field — name, span, scope, and every payload — survives. One hygiene contract
governs every application, procedural or template. And expansion is guarded by
the same **evaluation budget** that guards every other evaluation the checker
performs: a macro application is a call.

## Invariants

### M1 — a syntax object is a value; the round trip is the identity

**Status: decided, not implemented — the round trip is lossy in both
directions.** In the model a macro does not receive "a `Syntax.t` with a
conversion"; it receives a value of a datatype, the way Klister and Racket
treat syntax. **Reflection is total** (decided this pass): every form is
decomposable, the ADTs cover the whole grammar, and the `StxExpr` escape
hatch is scaffolding until they do — not a deliberate boundary. Because a
macro can take syntax apart and put it back, destructuring followed by
reconstruction must be the identity on every field.

Today it is not, in either direction:

| field | wrap (`→ value`) | unwrap (`→ Syntax.t`) |
|---|---|---|
| id scope | **erased** — `("scope", VAtom (I64 0L))`, a dummy | **hardcoded empty** — the field is not even read |
| `Lam`/`Let` type annotations | preserved (`type_val`) | **discarded** — `type_ = None` always |
| `Ap` explicitness | preserved | **hardcoded `Explicit`** |
| `PatCon` path | **destroyed** — the id is built from the bare constructor string | rebuilt as `PatCon ([], …)` |
| span line/col | preserved | **hardcoded `None`** (bytes round-trip, positions do not) |

And four silent degradations where the model demands a loud error: a
non-record becomes an id named `?`; `value_to_bool` maps *any* value that is
not `True` to `False` (without even checking the nominal); `wrap_stx_decl`
skips non-`Let` bindings; a pattern argument list that fails to unwind is
filtered (`List.filter_map`) rather than rejected.

The scope-set row is the soundness slice already ticketed as
[procedural-macros-capture-use-site-variables](../tickets/procedural-macros-capture-use-site-variables.md);
the full inventory is the fidelity defect
[syntax-round-trip-is-lossy](../tickets/syntax-round-trip-is-lossy.md). Under
total reflection these are not two bugs but one distance: the ADTs do not yet
*be* the syntax.

### M2 — one hygiene contract governs every application

**Status: decided (pass two), not implemented.** Every macro or template
application mints an intro scope and a use-site scope; ids written in the
application resolve where the macro was *defined*; splices keep their
occurrence scopes; output is expanded in place. Pass two measured the three
paths against this contract — untyped procedural, type-aware procedural,
template — and every bold cell of that table is a defect ticket. Nothing in
this pass changes the contract; what this pass adds is where it is *enforced*
by the port: at the application site, once, not spread over three
implementations.

### M3 — a macro elaborates in its definition site's scope, nothing ambient

**Status: decided, not implemented.** The body is elaborated against the
context as of its definition — the same ordered interleaving every other
binding obeys. Today `Macro_driver` elaborates every macro body with the
prelude ambiently opened. Distance:
[macro-bodies-implicitly-open-the-prelude](../tickets/macro-bodies-implicitly-open-the-prelude.md).

### M4 — a macro's name exists from the start of its definition

**Status: enforced by construction.** A macro binding registers its name
provisionally *before* its value is expanded and compiled, so the name is
known during its own definition. A call to it during that window is a loud
error ("cannot be expanded during its own definition"), not a silent lookup
failure; a failed definition restores the macro-table snapshot; and recursion
*after* definition works — the macro's own output re-enters expansion, which
is exactly what the budget must guard. Vocabulary: **Provisional macro**.

### M5 — expansion is guarded by the evaluation budget, not a separate fuel

**Status: decided, not implemented.** One compile-time budget counts semantic
steps everywhere the checker evaluates — and a macro application is one of
them: it is a call. This pass decided the unification (over keeping the
classic Racket/Klister depth guard), for the same reason the effects pass
chose call-counting: the count is a property of the program, stable across
compiler versions, and it catches what a depth guard cannot — breadth blowup,
where each output spawns two sibling calls at bounded depth and the depth
guard never trips while the work is unbounded. Today a separate
application-*nesting* fuel exists (reserve/release around each application,
256), and it is exactly as leaky as that analysis predicts. Budget exhaustion
is an error value naming the call, raisable per evaluation — which also
retires the `failwith`s at the application sites (kind mismatch, non-syntax
result, missing callback): at the one boundary where diagnostics matter most,
the model makes them errors, not exceptions. Vocabulary: **Evaluation budget**
in [`CONTEXT.md`](../../../CONTEXT.md), extended to count macro applications;
**fuel** becomes its retired name.

### M6 — a type-aware call is deferred, and its output is expanded in place

**Status: enforced.** When a macro's
annotation names a type, the *call* is deferred to the elaborator: the
annotation was resolved semantically at the definition (against what was
elaborated before it), so the call must land where the expected type is known.
The arguments travel as syntax objects — transport, not opacity; under total
reflection there is no other way for them to travel. At the call site the
elaborator unifies the expected type with the annotation's constraint and
applies. The output, like every macro's output, **is expanded in place**, and
a result that is not syntax is an error naming the macro, never an unsolved
hole. Both live in one helper shared by the infer and check sites
([type-aware-macro-output-is-not-expanded](../tickets/type-aware-macro-output-is-not-expanded.md),
closed).

### M7 — template heads resolve by scope set

**Status: decided, not implemented — heads are string-keyed and newest-wins.**
Templates and operators are binders; their fixity and precedence are the
binder's **syntactic role**, resolved by scope set like every other meaning a
name carries. That pass one named this seam (I4c) and deferred it is recorded;
this pass places it inside M2's contract: heads are part of the one hygiene
contract, not a parse-time exception to it. A later binder *can* take the role
away. Distance: the operator table in `binding.ml` (string-keyed,
newest-wins).

### M8 — kind checking is positional, and its error is an error

**Status: enforced by construction; error is an exception.** A macro's kind
(`Expr`, `Decl`, …) is fixed by its annotation; a use is well-formed only
where the site's expansion position matches, and the check happens before the
macro runs. The model keeps the check but not its delivery: today a mismatch
`failwith`s. Under M5's decision it is an error value, like every other
expansion failure.

## What the modelling found beyond the invariants

- **`Expand_ctx.phase` is dead.** `type phase = Runtime | CompileTime`,
  constructed as `Runtime`, copied once, never read, `CompileTime` never
  constructed. The same class of dead code as pass one's `types` column —
  written by every constructor, read by nothing — and deleted for the same
  reason: a port would transliterate a distinction the compiler never makes.
- The macro body's compilation runs each application through a **fresh
  metas context** (`MetaContext.create` per `eval_and_apply`). Sound today
  because a macro application solves nothing its caller needs; worth stating
  as a property rather than leaving as an accident, since the port must
  decide it consciously.

## Model decision vs today

| model | implementation today |
|---|---|
| round trip is the identity | lossy both directions; four silent degradations |
| total reflection | five `Expr` forms + `Pat`/`DeclLet` subsets; rest opaque via `StxExpr` |
| one hygiene contract | three contracts, one tested (pass two's table) |
| definition-site scope, nothing ambient | prelude ambiently opened |
| provisional name, loud self-call error | enforced |
| one evaluation budget counting macro applications | separate nesting fuel, 256, `failwith`; breadth unguarded |
| type-aware output expanded in place | never expanded; failed unwrap silently mints a meta |
| template heads scope-keyed | string-keyed operator table, newest-wins |
| kind mismatch is an error value | `failwith` |

## What the port's types should be named after

- **Reflection** for the ADTs and **Round trip** for the
  destructuring/reconstructing move — never *wrap/unwrap* (that names the
  two lossy functions being replaced) and never *conversion* (there is
  nothing to convert to; syntax already is a value).
- **Provisional macro** for the name-before-body registration; never *stub*
  or *forward reference*.
- **Evaluation budget** — one budget, one unit, counting macro applications
  with everything else. *Fuel* is its retired name and should not survive
  into the port's vocabulary.
- The reflection ADTs named after the forms themselves (`RawVar`, `RawAp`, …)
  with one constructor per `Syntax.t` kind — a port that finds a kind with no
  constructor has found a hole in the scaffolding, not a design choice.

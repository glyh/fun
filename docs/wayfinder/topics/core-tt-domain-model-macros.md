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

**Status: enforced (2026-09-14).** Reflection is total: one constructor per
form. The round trip is pinned by a test over a varied program, before and
after expansion. The table below is the defect as it was found. The one remaining hole is a
pattern constructor head, a bare string in `Syntax.pat` with no scope set. In the model a macro does not receive "a `Syntax.t` with a
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

**Status: enforced for macros (2026-09-14); templates pending M9.** Every
macro application (untyped, type-aware, decl and operator) goes through
`Expand.application`, which is the contract below. Templates still mint their
own intro scope at enforestation, until they desugar to macros. Every macro or template
application mints an intro scope and a use-site scope; ids written in the
application resolve where the macro was *defined*; splices keep their
occurrence scopes; output is expanded in place. Pass two measured the three
paths against this contract — untyped procedural, type-aware procedural,
template — and every bold cell of that table is a defect ticket. Nothing in
this pass changes the contract; what this pass adds is where it is *enforced*
by the port: at the application site, once, not spread over three
implementations.

### M3 — a macro elaborates in its definition site's scope, nothing ambient

**Status: enforced (2026-09-14).** Of the definition site, a body compiled
during expansion can see only the unit opens around it: an import loads, a
local has no value yet. The body is elaborated inside exactly those opens. The body is elaborated against the
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

**Status: enforced (2026-09-14).** A macro application spends one call and
opens a request; its body runs with fresh metas under that request's budget,
and its output's expansion (and, for a type-aware call, elaboration) spends
from it too. The decision as taken: One compile-time budget counts semantic
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

**Status: enforced; its error is an `Expand_error` value (2026-09-14).** A macro's kind
(`Expr`, `Decl`, …) is fixed by its annotation; a use is well-formed only
where the site's expansion position matches, and the check happens before the
macro runs. The model keeps the check but not its delivery: today a mismatch
`failwith`s. Under M5's decision it is an error value, like every other
expansion failure.

### M9 — a template is sugar for a macro

**Status: decided (2026-09-14), not implemented.** Racket's `syntax-rules` is
a `syntax-case` macro; `fun`'s template is likewise a macro. The template keeps
one job, the **parse**: its patterns and fixity decide which tokens a use
consumes and what each hole captures. Everything after is a macro whose
parameters are the captures and whose body is the replacement as **quoted
syntax**. M2's one contract then holds because there is one path, not because
three implementations agree. Today a replacement is `Raw_syntax.t list`,
re-enforested at every use site (`Enforest_template.expand`) — the mechanism
behind [template-literals-resolve-at-use-site](../tickets/template-literals-resolve-at-use-site.md).

### M10 — quoted syntax is parsed where it is written

**Status: implemented for macros (2026-09-14); templates pending M9.**
`Quote_holes` finds and fills the holes on the reflection value. A macro builds syntax by
writing it: `quote(one($e))`. Strings build no hygienic syntax — gensym and
spelling-resolution (Common Lisp, Clojure) are rejected. Quoted ids carry the
definition site's scopes. The quote is **parsed at the definition**, so its
shape is fixed there as well as its names: no caller's operator or syntax form
reaches inside it. The cost, accepted: a template can no longer rely on
syntax its user declares (`$x ** 2` where only the caller declared `**` is an
error at the definition, not a silent dependency).

A **hole**, `$e`, splices a syntax object that keeps its own scopes. Its kind
is fixed by its position in the parse and checked at the splice. Hole kinds,
template capture kinds and macro kinds are one set — the reflection types
`Expr`, `Pattern`, `Decl`, `Id`. `binder` and `ident` collapse into `Id`:
binding or referring is the splice position, not the kind. Pattern position
gains a kind it lacks today.

### M11 — scope sets are opaque values; borrowing is construction

**Status: enforced (2026-09-14).** `Scopes` is an atom type with no literal
syntax and no primitives except `no_scopes`. `Id.scope` carries the
real scope set as an opaque `Scopes` value — no constructors, no inspection.
A macro holds one only by quoting (definition site) or receiving syntax (use
site), so it cannot forge a scope set matching an unrelated binder, and its
behaviour never depends on scope counter values. **Borrowed context** is not an
operation: it is constructing an `Id` with another id's `scope`. An `Id` built
from a name alone has an empty scope set and is unbound. Today the field is a
dummy `I64 0` (M1's scope row).

### M12 — no name is found by its spelling alone

**Status: enforced (2026-09-14) for bare names and path heads; traits and
nominals are located through the entry a head resolves to. Macro annotation
constraint names, being strings, remain by spelling until explicit macro type
binders.** Decided with the user: which members an
open supplies may be known only from its type, so expansion does not enumerate
them. A bare name inside an open's region that no binder takes, or that only a
binder outside the open takes, resolves to an **open choice**. The choice holds
the candidate opens, innermost first, then that binder. Elaboration picks the
first open that has the member. Opens are labelled (`unit:p` for an open of an
import, `open:n` otherwise), and the elaborator records each open's members
under its label. A template imported from a unit adds that unit's open as a
candidate for ids it introduced. Resolved names are always fresh, so a
generated name can never equal a written one's resolution.

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
| round trip is the identity | enforced |
| total reflection | enforced; pattern constructor heads carry no scope |
| one hygiene contract | three contracts, one tested (pass two's table) |
| definition-site scope, nothing ambient | enforced |
| provisional name, loud self-call error | enforced |
| one evaluation budget counting macro applications | enforced (2026-09-14) |
| type-aware output expanded in place | never expanded; failed unwrap silently mints a meta |
| template heads scope-keyed | string-keyed operator table, newest-wins |
| kind mismatch is an error value | enforced (2026-09-14) |
| template = parse + macro | separate path; replacement re-parsed at use site |
| quoted syntax, parsed at definition, typed holes | ids from strings only; hole kinds `expr/binder/ident/decl` |
| opaque `Scopes`; borrowing is construction | dummy `I64 0` |

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

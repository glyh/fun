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
after expansion. A pattern constructor head is a path whose head is an id
(`PatCon of path * pat list`, reflected `RawPatCon`), so it carries its scope
set too. The `Core.StxExpr` escape hatch still exists as scaffolding. The table
below is the defect as it was found (history). In the model a macro does not receive "a `Syntax.t` with a
conversion"; it receives a value of a datatype, the way Klister and Racket
treat syntax. **Reflection is total** (decided this pass): every form is
decomposable, the ADTs cover the whole grammar, and the `StxExpr` escape
hatch is scaffolding until they do — not a deliberate boundary. Because a
macro can take syntax apart and put it back, destructuring followed by
reconstruction must be the identity on every field.

Before 2026-09-14 it was not, in either direction:

| field | wrap (`→ value`) | unwrap (`→ Syntax.t`) |
|---|---|---|
| id scope | **erased** — `("scope", VAtom (I64 0L))`, a dummy | **hardcoded empty** — the field is not even read |
| `Lam`/`Let` type annotations | preserved (`type_val`) | **discarded** — `type_ = None` always |
| `Ap` explicitness | preserved | **hardcoded `Explicit`** |
| `PatCon` path | **destroyed** — the id is built from the bare constructor string | rebuilt as `PatCon ([], …)` |
| span line/col | preserved | **hardcoded `None`** (bytes round-trip, positions do not) |

And four silent degradations (fixed with it) where the model demands a loud error: a
non-record becomes an id named `?`; `value_to_bool` maps *any* value that is
not `True` to `False` (without even checking the nominal); `wrap_stx_decl`
skips non-`Let` bindings; a pattern argument list that fails to unwind is
filtered (`List.filter_map`) rather than rejected.

The scope-set row is the soundness slice already ticketed as
[procedural-macros-capture-use-site-variables](../tickets/procedural-macros-capture-use-site-variables.md);
the full inventory is the fidelity defect
[syntax-round-trip-is-lossy](../tickets/syntax-round-trip-is-lossy.md), both
closed. Under total reflection these were not two bugs but one distance: the
ADTs did not yet *be* the syntax.

### M2 — one hygiene contract governs every application

**Status: enforced (2026-09-14).** Every macro application (untyped,
type-aware, decl, operator, and a syntax form's use) goes through
`Expand.application`, which is the contract below; a typed macro the elaborator
applies (M6) takes it through its macro runtime's `application`. Every macro or template
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
binding obeys. (Before 2026-09-14 `Macro_driver` elaborated every body with the
prelude ambiently opened;
[macro-bodies-implicitly-open-the-prelude](../tickets/macro-bodies-implicitly-open-the-prelude.md),
closed.)

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
guard never trips while the work is unbounded. The separate nesting fuel is
deleted
([macro-fuel-is-the-evaluation-budget](../tickets/macro-fuel-is-the-evaluation-budget.md),
closed). Budget exhaustion is an error value naming the call — its source name,
the request that demanded it and the form being checked
([budget-error-names-no-source-call](../tickets/budget-error-names-no-source-call.md),
closed) — and expansion failures (kind mismatch, non-syntax result, missing
callback) are `Expand_error` values. Since 2026-09-15 the budget measures work:
every call and every conversion or unification step spends one unit
([recursive-definitions-stuck-on-open-arguments](../tickets/recursive-definitions-stuck-on-open-arguments.md),
closed). Vocabulary: **Evaluation budget**
in [`CONTEXT.md`](../../../CONTEXT.md), extended to count macro applications;
**fuel** becomes its retired name.

### M6 — a type-aware call is deferred, and its output is expanded in place

**Status: enforced (signatures, 2026-09-15).** A macro's type binders
(`[A, B]`, any number), `(x : Expr(T))` parameters and `: Expr(T)` output are
its **signature**, a pi type elaborated where the macro is defined
(`Syntax.macro_signature`); a name in it must resolve there and a promised `T`
must be a type. When the signature promises a type, the *call* is deferred to
the elaborator (`Elab_resolve.apply_typed_macro`): binders become metas, each
typed argument is elaborated once at its type, the result meets the expected
type, every binder must then be solved ("cannot infer A for `default`"), the
macro runs with them, and its output is checked at the promised type ("macro
`n` promises Expr(I64) …"). Where the output places a typed argument unchanged,
its elaboration is reused (`Syntax.Elaborated`), shifted past binders the output
adds. The arguments travel as syntax objects — transport, not opacity. The
output, like every macro's output, **is expanded in place**, and a result that
is not syntax is an error naming the macro
([macro-annotation-constraints-mean-nothing](../tickets/macro-annotation-constraints-mean-nothing.md),
[type-aware-macro-output-is-not-expanded](../tickets/type-aware-macro-output-is-not-expanded.md),
both closed). Distance: a deferred call inside syntax that effect collection
walks aborts it
([effect-collection-rejects-deferred-macro-calls](../tickets/effect-collection-rejects-deferred-macro-calls.md)).

### M7 — template heads resolve by scope set

**Status: enforced (2026-09-14) — roles resolve by scope set, never mix with
another binder of their name, and generated syntax is hygienic; the
expander-driven loop and unparsed block captures ride on M9
([resolution](../tickets/template-heads-resolve-by-scope-set.md#resolution-2026-09-14)).**
Revision by grilling: a role never mixes with another binder
of its name (an error at the binder, by scope set), rather than being taken away;
the expander drives the enforester form by form; bodies become brace groups
([ticket](../tickets/template-heads-resolve-by-scope-set.md),
[surface syntax](../tickets/surface-syntax-braces.md)). Syntax a template or
macro generates is hygienic like any binder it writes: a role named by an
intro-scoped id is invisible to user code, so generated callable syntax takes
its name from the use site (decision 6). The paragraph below is the original
decision.
Templates and operators are binders; their fixity and precedence are the
binder's **syntactic role**, resolved by scope set like every other meaning a
name carries. That pass one named this seam (I4c) and deferred it is recorded;
this pass places it inside M2's contract: heads are part of the one hygiene
contract, not a parse-time exception to it.

Since then (2026-09-15): an imported role binds in the region of the open or
binder that imported it, and every open — import opens and driver-run ones — is
checked
([role-visibility-gaps-after-m7](../tickets/role-visibility-gaps-after-m7.md),
closed). Precedence is no longer a number: a role joins a named **order group**,
itself a role binder resolved by scope set; the order is transitive,
associativity lives on the group, an undeclared relation is an error, and an
ungrouped role is weaker than every grouped one
([brackets-decide-grouping](../tickets/brackets-decide-grouping.md), decisions 3–4).
Distance: `<-`'s own non-associative group and dotted group references
(`Std.additive`) are decided, not implemented (same ticket).

### M8 — kind checking is positional, and its error is an error

**Status: enforced; its error is an `Expand_error` value (2026-09-14).** A macro's kind
(`Expr`, `Decl`, …) is fixed by its annotation; a use is well-formed only
where the site's expansion position matches, and the check happens before the
macro runs. A mismatch is `Expand_error.KindMismatch`; an argument of the wrong
parameter kind is `ArgumentKind`. Distance: an under-applied `Decl` macro is fed
a dummy `Unit` argument, and the argument count is checked only when a parameter
has a non-`Expr` kind
([decl-macro-fed-dummy-unit](../tickets/decl-macro-fed-dummy-unit.md)).

### M9 — a template is sugar for a macro

**Status: implemented (2026-09-15)
([ticket](../tickets/templates-desugar-to-macros.md), closed).**
A use is `Instantiate`, filled through `Expand.application`; the expander
drives the enforester form by form. Procedural macro parameters take the hole
kinds (`(n : Id)`, `(p : Pattern)`, `(b : Block)`, `(d : Decl)` — a brace group
of items, value `Syntax.Decls`), and a call's arguments are read as those kinds.
A capture's extent is structural: the trailing hole reads at its form's order, a
hole followed by `,` or `;` reads to it, any other hole is one term
([capture-extents-chosen-by-exceptions](../tickets/capture-extents-chosen-by-exceptions.md),
closed; [brackets-decide-grouping](../tickets/brackets-decide-grouping.md)). Racket's `syntax-rules` is
a `syntax-case` macro; `fun`'s template is likewise a macro. The template keeps
one job, the **parse**: its patterns and fixity decide which tokens a use
consumes and what each hole captures. Everything after is a macro whose
parameters are the captures and whose body is the replacement as **quoted
syntax**. M2's one contract then holds because there is one path, not because
three implementations agree. (Before run 2 a replacement was a `Raw_syntax.t
list` re-enforested at every use site — the mechanism behind
[template-literals-resolve-at-use-site](../tickets/template-literals-resolve-at-use-site.md),
closed.)

### M10 — quoted syntax is parsed where it is written

**Status: implemented (2026-09-14), templates included: a rule's replacement is
quoted syntax.**
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
gains a kind it lacks today. `Block` joins the set (2026-09-14, M7): a captured `{…}` group
stays unparsed until the output places it. Kinds are spelled as types —
`$(x : Decl)`, a bare `$x` being `Expr` — so a template capture is literally the
macro parameter it desugars to. `quote { … }` quotes declarations; nested holes
resolve to their nearest binder, with no quote levels
([ticket](../tickets/templates-desugar-to-macros.md)). An identifier token
spelled `$n` — a generated rule's head — is an `Id` hole too, filled as the token
spelling that id (2026-09-15).

### M11 — scope sets are opaque values; borrowing is construction

**Status: enforced (2026-09-14).** `Scopes` is an atom type with no literal
syntax and no primitives except `no_scopes`. `Id.scope` carries the
real scope set as an opaque `Scopes` value — no constructors, no inspection.
A macro holds one only by quoting (definition site) or receiving syntax (use
site), so it cannot forge a scope set matching an unrelated binder, and its
behaviour never depends on scope counter values. **Borrowed context** is not an
operation: it is constructing an `Id` with another id's `scope`. An `Id` built
from a name alone has an empty scope set and is unbound. Distance: a name that
contains `#` is trusted as already resolved, so `Syntax.new_id("x#3")` reaches a
binding by spelling, and an empty-scope id still falls back to the macro table
by spelling
([resolved-names-forgeable](../tickets/resolved-names-forgeable.md)).

### M12 — no name is found by its spelling alone

**Status: enforced (2026-09-14) for bare names and path heads; traits and
nominals are located through the entry a head resolves to; a macro
annotation's names are references in the macro's body, resolved by scope at
the definition (explicit macro type binders).** Decided with the user: which members an
open supplies may be known only from its type, so expansion does not enumerate
them. A bare name inside an open's region that no binder takes, or that only a
binder outside the open takes, resolves to an **open choice**. The choice holds
the candidate opens, innermost first, then that binder. Elaboration picks the
first open that has the member. Opens are labelled (`unit:p` for an open of an
import, `open:n` otherwise), and the elaborator records each open's members
under its label. A template imported from a unit adds that unit's open as a
candidate for ids it introduced. The model: resolved names are always fresh
(`name#n`, unwritable), so a generated name can never equal a written one's
resolution. Distance: types, constructors, traits, effects and module items
keep their written name as their resolved name, and the elaborator's macro table
is keyed by string
([declaration-binders-keep-written-names](../tickets/declaration-binders-keep-written-names.md));
forgeable resolved names ([resolved-names-forgeable](../tickets/resolved-names-forgeable.md)).

## What the modelling found beyond the invariants

- **`Expand_ctx.phase` was dead** and is deleted. `type phase = Runtime | CompileTime`
  was constructed as `Runtime`, never read. The same class of dead code as pass one's `types` column —
  a port would transliterate a distinction the compiler never makes.
- The macro body's compilation runs each application through a **fresh
  metas context** (`Nbe.apply_macro` creates one per application, sharing the
  budget). Sound today
  because a macro application solves nothing its caller needs; worth stating
  as a property rather than leaving as an accident, since the port must
  decide it consciously.

## Model decision vs today

Checked against main on 2026-09-15.

| model | implementation today |
|---|---|
| round trip is the identity | enforced |
| total reflection | enforced; `StxExpr` scaffolding remains |
| one hygiene contract | enforced: every application through `Expand.application` |
| definition-site scope, nothing ambient | enforced |
| provisional name, loud self-call error | enforced |
| one evaluation budget counting macro applications | enforced; measures work (calls and conversion steps) |
| typed macro: signature checked, output expanded in place | enforced; deferred call breaks effect collection ([ticket](../tickets/effect-collection-rejects-deferred-macro-calls.md)) |
| template heads scope-keyed | enforced; order groups replace numbers; `<-` group and dotted group refs pending ([ticket](../tickets/brackets-decide-grouping.md)) |
| kind mismatch is an error value | enforced; under-applied `Decl` macro gets a dummy argument ([ticket](../tickets/decl-macro-fed-dummy-unit.md)) |
| template = parse + macro | enforced; parameter kinds included |
| quoted syntax, parsed at definition, typed holes | enforced; token-position holes filled |
| opaque `Scopes`; borrowing is construction | enforced; `#` names forgeable ([ticket](../tickets/resolved-names-forgeable.md)) |
| no name found by spelling | declaration binders keep written names ([ticket](../tickets/declaration-binders-keep-written-names.md)) |

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

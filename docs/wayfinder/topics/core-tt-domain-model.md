---
title: Domain model — the elaborate ↔ evaluate boundary
parent: ../fun-design-map.md
---

# Domain model — the elaborate ↔ evaluate boundary

First pass of the model asked for by
[domain-model-core-tt](../tickets/domain-model-core-tt.md). Vocabulary lives in
the root [`CONTEXT.md`](../../../CONTEXT.md); this document holds the invariants
behind it, each marked **enforced by construction**, **checked**, or **unchecked
convention**.

Scope of this pass is the boundary itself. Surface syntax, enforestation, macro
hygiene, and effects are later passes.

## The central mistake this model corrects

The elaborator and the evaluator were described as holding "two views of one
thing". They do not. The evaluator holds **one column of** the elaborator's
context, and is never given the rest.

A context is one ordered sequence of entries seen through several columns:

| column | what it holds | who reads it |
|---|---|---|
| `env` | one value per entry | evaluation, unification |
| `bds` | bound-vs-defined per entry | meta creation |
| `lvl` | the width | quoting, name lookup, unification |
| `name_table` | name → (level, type) | name lookup |

No operation uses all of it. Evaluation takes `env` alone; quoting takes `lvl`
alone; meta creation takes `bds` alone. Naming the whole thing "context" and the
first column "environment" hid that the second contains the first, which is why
a port would have modelled them as peers.

### A fifth column existed and was dead

There was also a `types` column, one type per entry, written by every context
constructor. **Nothing ever read it.** Its only read mapped a substitution over
it to rebuild itself, next to three neighbours whose results *are* read. An
entry's type lives in `name_table`; the column was a redundant second copy.

Removed. A port that transliterated it would have carried a parallel list it had
to keep correct for no reason — the most expensive kind of dead code.

## Invariants

### I1 — every column of a context has length `lvl`

**Status: enforced by construction, with one historical escape.**

The four context constructors (`bind`, `bind_anonymous`, `define`,
`define_anonymous`) extend every column together. The escape is that a context is
a plain record, so a record update can extend one column and not another — which
is exactly what happened, see D1 below.

Half of this invariant *is* checked, late and narrowly: evaluating a meta walks
`env` and `bds` in step and raises `bd mask length mismatch` when they differ.
That check fires only if a meta is actually evaluated in the offending context.

### I2 — elaborator and evaluator widen a context identically per binding

**Status: enforced by construction.**

`Core.binding_slots` states what a binding contributes: an ordered list of slots,
one per entry, each carrying a name where there is one and where its payload
comes from — a term to evaluate, a value the term already holds, or a stand-in
each side fills in for itself. Width is the length of that list rather than
arithmetic repeated per site.

Both sides now consume it. The evaluator pushes exactly those slots, and its two
folds — one for modules, one for structs — collapsed into one parameterised by
the entry constructors, deleting a 55-line copy. The elaborator zips its own
payloads, a type and a value per slot, onto the same list, so a shape
disagreement fails while the context is being built instead of surfacing as a
wrong de Bruijn index.

The earlier ask — that the elaborator *derive* its extension from the width
function — was not achievable as stated: a count cannot produce named typed
entries. The slot list is what both sides can genuinely share, and the payload
stays each side's own.

Every binding kind goes through it, impls included: an impl's contribution is
now worked out without touching the context, and the evidence and optional name
that ride along with its entry are installed separately. Traits were never an
exception — their elaborator does not extend the context at all. With nothing
left computing a second opinion, both width checks and the drift error are
deleted.

`open` returns no width: its contribution is the public-entry count of a module
that must be evaluated first, so it is not recoverable from the term. Every
traversal that walks a binding list with a constant cutoff is wrong for the same
reason, which is why they now refuse lists they cannot account for.

### I3 — a dotted path denotes the last member of that name

**Status: enforced by construction.**

One helper, `Core.find_field_last`, is used by every field lookup in both
libraries. Previously the rule was written out seven times as a bare
first-match scan, and disagreed with `open` and with `do` bindings, both of
which take the last. That disagreement is what made a constructor sharing its
type's name unreachable through a path.

### I4 — names live in one namespace

**Status: unchecked convention, and deliberately so.**

Types, constructors, fields, macros, operators and effect operations all share
one namespace. A later binding shadows an earlier one. The consequence is
accepted rather than repaired: after `type T = T I64` the constructor shadows
the type, so `T` no longer works in type position.

This is a language decision, not a defect. Splitting the namespaces would
contradict *types are values*.

### I4b — bare names share one namespace; members are not bare names

**Status: enforced by construction, with one phase-shaped exception (I4c).**

I4 says names share one namespace. Finishing the ticket's fourth bullet — *which
names exist, and which namespace each lives in* — the answer is two-level, and
verified rather than assumed:

| kind | bare? | evidence |
|---|---|---|
| value, type, constructor, pattern synonym, effect family, module | yes | compete and shadow each other |
| record field | no | `do x = 1; type P = {x: I64}; x end` still yields the value |
| effect operation | no | bare `f` is unbound; only `E.f` resolves |
| trait method | no | bare `eq` is unbound; reached through the trait |
| module member | no | `C(1)` is unbound until `M.C` or an open |

So a **member** never competes with a **bare name**. Every container carries its
own member namespace, and a member is reached only through it. That is why I3
(last-wins path lookup) and I4 (single namespace, last-wins shadowing) are
separate rules rather than one: they govern different namespaces.

### I4c — a name's syntactic role is decided a phase earlier, and is not shadowed

**Status: known gap, documented in the source, deferred by ticket.**

The single-namespace rule does not survive the phase boundary. Whether a name is
an operator or a syntax form is settled by the expander, **string-keyed and
newest-wins**, because scope sets do not exist yet at enforestation. A later
binding of the same name therefore cannot take the role away:

```
do not = 5; not end          -- parse error: `not` is still a prefix operator
do if = 5; if end            -- parse error: `if` is still prelude syntax
do (+) = fn(a,b) -> 0; … end -- accepted: the parenthesised form rebinds the value
```

`lib/expand/binding.ml` states this in a comment and defers scope-set-keyed
operator resolution to the interleaving work. Recording it here because it is a
real seam in the model, not an implementation detail: a name has a **syntactic
role** as well as a context entry, and the two obey different resolution rules.

This is the seam into the next pass (macro expansion and hygiene). The
elaborator additionally carries both its own macro table and a mutable reference
to the expander's context, which is the same coupling seen from the other side.

### I4d — macros resolve on a different axis from every other name

**Status: decided and implemented.** The decision below is in the tree, with
tests for the qualified call, for a bare import no longer injecting macros, for
the `open` form, and for the import-order case that used to answer differently
depending on which unit was imported first. The measurements that follow record
the behaviour as it was.

Not merely "less strict". Measured, macros arrive by a form that delivers nothing
else, and do not arrive by the form that delivers everything else:

```
do M = import "m1"; answer(0) end        =>  1                    -- binding the import delivers it
do open (import "m1"); answer(0) end     =>  UnboundVariable      -- open does not
do M = import "m1"; M.answer(0) end      =>  UnboundVariable      -- no qualified form
```

`M` is never used in the first line: **binding an import injects the unit's
macros as a side effect.** So two units exporting the same macro name overwrite
each other with no error and no way to disambiguate:

```
do A = import "m1"; B = import "m2"; answer(0) end   =>  2
do B = import "m2"; A = import "m1"; answer(0) end   =>  1
```

Contrast with I4b, where every other kind splits cleanly into bare names and
members. Macros are neither: not reachable as members, and reachable bare only
through a form that is supposed to bind a module value.

**Why.** Expression-level macros are keyed by a freshly uniquified *resolved*
name, so the elaborator's flat `macro_table` is sound for them. Module-level
macros register with `~resolved_name:binding_name` — the **written** name — and
the driver copies each unit's entries into that one flat, unscoped table as the
import is elaborated. Hygiene holds within a unit and is absent between units.

#### Decision

**Macros become members, and otherwise obey the rules values already obey.**

1. `M.answer(0)` expands. The *call* form resolves; the value form `M.answer`
   stays an error, because a macro is not a runtime value.
2. Macros arrive bare through `open`, like any other public name.
3. Binding an import stops delivering macros as a side effect.

|  | today | after |
|---|---|---|
| `M = import "m1"; answer(0)` | 1 | unbound |
| `open (import "m1"); answer(0)` | unbound | 1 |
| `M = import "m1"; M.answer(0)` | unbound | 1 |
| `M = import "m1"; M.answer` | unbound | unbound |

This needs **no new naming mechanism** — unlike impls, where evidence is never
written at the use site and so required a compile-time handle. A macro call *is*
written at the use site, so qualification is available and disambiguation falls
out of it. Module-level macros may keep the written name as their key once that
key is scoped to a container instead of shared in one flat table.

It also closes a quiet violation of the I5 rule: today a bare `import` injects
names the importer never asked for.

**Cost.** Two existing tests encode today's behaviour. The imported-macro
expansion test uses the bare-import form and needs an `open` or a qualified call.
The "macro is not a runtime field" test should survive unchanged, since it
asserts the value form errors.

### I4e — the elaborator's expander handle is a capability, not a context

**Status: fixed.** The importer-side mutation was deleted (see
[base-context-shared-state](../tickets/base-context-shared-state.md)), so the
field no longer survives as a last-writer-wins latch, and the field itself is now
a `macro_runtime`: how to run a macro, and the expansion-depth budget to run it
under. The elaborator no longer holds a reference to the expander at all.

`Elab_ctx.Ctx.expand_ctx` reads as "the expander's context", i.e. a namespace. It is
read for exactly two things: `eval_and_apply`, which is how to run a macro, and
`with_macro_fuel`, an expansion-depth budget. Neither consults the expander's
binding table. It is a **capability handle**, not a namespace.

It is also a *latch*: assigned by the macro driver and again by each `Import`,
never restored, so the last writer wins for the rest of elaboration. That is
tolerable only because both readers want capabilities that are effectively the
same everywhere — which is an argument for giving them their own field rather
than reaching through a borrowed context.

Same shape as the mistake I1 corrects: a name promising a whole context while
delivering one projection.

### I5 — a term may only be transported if it is closed

**Status: decided and implemented.** A compilation unit is elaborated against
the base context (`Elab_ctx.Ctx.unit_base`), not the importer's. The double
import below now answers, and a unit no longer resolves a name the importer
happened to bind. What follows records the reasoning and the behaviour it
replaced.

Every term's indices are relative to a context — its **anchor**. The anchor is
implicit everywhere and recorded nowhere, which is fine for as long as a term
stays where it was built. Exactly one thing moves terms between contexts:
`Core_loader.runtime_elab_cache`, keyed by resolved path and holding a
`Core.term`.

That key is a claim: *a module's meaning depends only on its path*. Elaboration
makes the opposite true — an imported module is elaborated in the **importer's**
context, so any name it did not bind itself becomes a free index into that context.
Two claims, contradictory, and the types record neither.

Importing the same module twice is where they collide. The second import splices
a term anchored at one width into a context of another:

```
module m:  pub v = Some(1)          (* Some comes from the importer *)
do A = import "m"; B = import "m"; … end
  => EvalError "bd mask length mismatch"
```

Reproduced against the current tree. Note *what* it fails as: the mask/width
check from **I1**. A violation of I5 surfaces as a violation of I1, because
splicing a term anchored at width W into a context of width W′ is precisely how
the columns come to disagree. The two invariants are one invariant seen twice.

A closed module (`pub x = 21`) survives the same double import, which is the
control that isolates the rule.

**The predicate already exists.** `Elab_generalize` computes exactly this notion
under the name `closed_under`, as a local helper, to decide whether a lambda may
be generalised. The concept is implemented, unnamed, and not applied at the one
boundary where transport happens.

#### "Module" was two concepts, and only one is affected

The first draft of this section said *modules must be closed*, which reads as an
attack on first-class modules. It was a conflation:

- A **module** is `module … end` in expression position — a first-class value
  that **captures its enclosing context**, like a closure. Verified: it closes over
  an outer binding and over a lambda parameter, and survives being returned from
  a function. Its term is anchored, necessarily and correctly.
- A **compilation unit** is a `.fun` file reached by `import`. It has no
  enclosing context to capture. The importer's context is an artefact of how the
  elaborator threads its context, not something the unit asked for.

Only the compilation unit is in question. Nothing here constrains `module … end`.

#### Values are transportable; terms are not

The distinction that does the real work is **term vs value**, not module vs unit.
A value carries its environment — a closure holds its own, a module value holds
each member's. So moving a *value* between contexts is sound by construction, and
that is exactly why first-class modules work at all.

The import path does the other thing. `Elab_infer`'s `Import` case calls
`load_elaborated`, which hands back `(term, value, type)` — and the case returns
the **term**, discarding the value it just computed. A cached term spliced into a
new anchor is the unsound move. The value beside it in the same cache entry
would not have been.

`import "std"` is the same shape in miniature: it elaborates to a bare `Var`
index into the importer's context, which is why a module writing its own
`open (import "std")` is affected identically.

#### The rule, decided

A compilation unit's meaning depends only on **its own source plus what it
imports and opens**. It elaborates against a base context rather than the
importer's. One rule covers values, operators, macros and syntax — today only
syntax obeys it.

This resolves
[imported-module-elaboration-context](../tickets/imported-module-elaboration-context.md)
step 1. It is the loose end of
[module-level-open](../tickets/module-level-open-strict-imported-modules.md),
which made units strict for syntax and recorded the value leak as the remainder.

**How much a unit currently sees is worse than that ticket states.** It is not
only the prelude. A unit resolves *any* name the importer happened to bind:

```
importer:  do outer_val = 9; U = import "u"; … end
unit u:    pub v = outer_val          -- resolves today
```

So a unit's meaning depends on the importer's choice of local variable names.

#### The invariant is base-anchored, not closed

Under this rule a unit's term is still **not closed**: `import "std"` elaborates
to a `Var` into the base context, and stays that way. The condition that makes
transport sound is weaker and sufficient — every free index points into a base
context that every importer shares.

Recorded as a consequence: the base context's width is implicitly part of every
cached term, so changing what `init_ctx` binds shifts every index. That is safe
only because the cache lives in a loader created per run and is never persisted
across a prelude change. A port that persisted it would have to key on the base.

#### What the base context holds

The atom types, the primitives, and `stdlib` **bound as a name — not opened**.
Verified: `pub v = stdlib.Some(1)` already works in a unit with no open, because
`init_ctx` binds `stdlib`; bare `Some(1)` works only through the leak this rule
removes.

The distinction is the whole of it. Binding `stdlib` keeps the prelude reachable
without ceremony. Opening it would restore the blanket prelude open that
[module-level-open](../tickets/module-level-open-strict-imported-modules.md)
deliberately deleted, and would cut against demoting built-ins into the prelude
as a library at all.

## Defects the modelling found

### D1 — the parameterised-ADT branch broke I1

**Fixed.** Elaborating `type Box(A) = …` built its context by record update,
extending `env` and `lvl` but not `bds`. Every other site in the codebase
extends them together. Latent rather than live: it only surfaces as `bd mask
length mismatch` if a meta is created and later evaluated in that context, and no
program in the suite does. A port would have copied it silently.

## What the port's types should be named after

- `Context` for the ordered sequence, with `environment` as a named projection
  rather than a field that happens to be passed. Not `Scope`: that word is the
  hygiene one and has no synonym.
- `Binding` for a module member, since that is what a user writes. Context slots
  are `entry`; the expander's hygiene records are not bindings at all.
- `Slot` for what a binding contributes, as a list produced once and consumed by
  both sides, with width as its length. Impls and traits should join it, which
  the prototype leaves undone.
- `BaseAnchored` for the transport condition — every free index pointing into the
  base context every unit shares — rather than the closedness it is easily
  mistaken for.

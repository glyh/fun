---
title: Impl visibility — usability implications
parent: ../fun-design-map.md
---

# Impl visibility — usability implications

Investigation of one question: **what must a user write to get a trait impl into
consideration?** Raised while building the
[domain model](core-tt-domain-model.md); it is the practical face of the
"public/imported implementations do not yet participate in module visibility
rules" item left open in [traits](traits.md).

All behaviour below was measured against the current tree, not inferred.

## What is already decided

- [traits](traits.md): *implementations are resolved from lexical/imported
  scope*, and *multiple matching implementations in scope are an ambiguity
  error*. Impls are values in a scope, not a global table.
- [trait-module-stdlib](trait-module-stdlib.md): a public impl satisfies an
  external bound *only after the module is explicitly imported/opened*.

So scoped resolution is settled. What is **not** settled is whether `import`
alone suffices or `open` is required — the doc says "imported/opened" and the
code means `open`.

## The finding

**A type travels qualified. A trait bound on it does not.**

```
A = import "lib"
A.R                              -- fine
same(A.R, A.R)                   -- fails, where same : [T : Eq] -> …
open (import "lib"); same(R, R)  -- fine
```

You may name a module's type, pass it, store it, return it. The moment any trait
is involved — `==`, a bounded generic, later `derive` — you must `open` the
module. Qualified access is a second-class citizen precisely where traits begin.

Two supporting measurements:

- Opens do **not** re-export. Opening two libraries that each open the prelude
  does not double the prelude's impls. Transitive duplication is not a problem.
- Opening the **same** module twice *does* duplicate its impls and makes them
  ambiguous. Separately decided: open should be idempotent, deduped by impl
  identity.

## What the workaround costs

There is no selective open — `open M` brings every public name M exports, and
collisions shadow silently:

```
p exports: C, R, G, helper        q exports: C, S, T, helper
open p; open q; helper            -- resolves to q's, no warning
```

So "open the module to get its impl" means "accept that module's entire export
surface". Consequences for library design under today's rule:

- A library that defines a type **and** an impl for it forces every user to open
  it wholesale. Users cannot take the type and leave the names.
- That pressures libraries toward one-type-per-module, to keep the blast radius
  of a mandatory open small. This is a real design tax, paid by every library
  author, to work around a resolution rule.
- It gets worse with `derive`: generated impls live in the defining module, so
  deriving anything makes that module mandatory-open for its users.

## The options

### A — impls arrive only via `open` (today)

One rule with no exception: a unit sees what it imports and opens. Clean story
against first-class modules, since an impl is a value and `open` is what puts
values in scope.

Cost is the whole of the section above. It also makes the qualified style the
language otherwise supports unusable in the presence of traits.

### B — impls for a type travel with the type

Referring to `A.C` brings A's impls **for C** into consideration. Orphan impls —
`impl Eq(M.C)` declared in some third module N — still require opening N.

This is not the exception to the strict rule it first appears to be. The rule can
be restated as **you see what you name**, and `A.C` names A. Under that reading A
and B are the same rule with different notions of "name".

Mechanically it needs a nominal to reach its defining module. A nominal carries
an id, a name, parameters and constructors, but no back-pointer today, so this is
real work, not a flag.

The two-tier result — own-module impls automatic, orphan impls explicit — is
close to the coherence intuition from Rust and is teachable in one sentence.

### C — impls are global once loaded

Rejected. An impl inside a module is a value, and a module is first-class: it can
be built by a function and returned. There is no well-defined moment to register
a global impl for a module produced at runtime, and no clear meaning for one. The
usability appeal is real but the mechanism is not available to this language.

## What other languages do

Two axes, usually conflated. **Where an impl is found**, and **whether the
trait's own name must be in scope to use its methods**. Rust separates them —
impls are global, but the trait must be imported to call its methods. `fun`
currently answers both with `open`.

| language | where impls are found | modules first-class? |
|---|---|---|
| Haskell | global, one per (class, type), program-wide | no |
| Rust | global, orphan rule; trait must be in scope for method syntax | no |
| Swift | global, retroactive conformance allowed | no |
| Lean 4 | global on import, with `local`/`scoped instance` escapes | no |
| Scala 3 | lexical scope **plus the companion of the type** | yes |
| Agda | scope only, no type-directed search | yes-ish |

**Every language with global coherence has non-first-class modules.** That is not
a coincidence: a global registry needs a fixed set of modules known at link time.
It confirms on independent grounds the mechanism argument against option C.

**Scala is the closest precedent, and it is option B.** Scala has first-class
objects and resolves a given from lexical scope *plus the implicit scope*, which
includes the companion object of the type. "Impls travel with the type, orphans
need an explicit import" is the standard answer for a language whose modules are
values, not a novelty.

**Agda is the precedent for option A** — resolution from scope only. The honest
minimal rule, with the tax this document measures.

### Two costs worth naming

**Option B buys usability and spends legibility.** Scala's implicit scope is its
most criticised feature for debuggability: resolution consults places the reader
did not write down, and "where did this given come from?" is a standing
complaint. Against a stated priority of *Consistency > Flexibility*, that is the
real argument for A.

**Scoped resolution gives up coherence, which is correctness, not convenience.**
Haskell guarantees one canonical instance per (class, type) program-wide. That is
what makes it sound for a sorted map to be parameterised by an ordering: two maps
of the same type cannot have been built with different orderings, so merging them
is safe. Once impls are scoped, two values of the same type may carry different
impls and merging is unsound.

Nothing is broken today, because there is no such data structure. But every
future ordered or hashed collection inherits the hazard, and **the decision that
creates it is the already-settled one in [traits](traits.md)** — impls resolve
from scope — not the A-versus-B choice in front of us. Recorded on that ticket.

### The closest precedent argues for A, not B

[Modular implicits](https://www.cl.cam.ac.uk/~jdy22/papers/modular-implicits.pdf)
(White, Bour, Yallop) is the nearest relative to `fun`'s situation: OCaml has
first-class modules, and the design is type-directed implicit *module* arguments
elaborating into first-class functors — structurally the same as `fun`'s
"structural dictionary evidence, bound implicits".

They chose **lexical scope with explicit implicit declarations** — option A — and
did not choose the Scala companion-object route. Their §4.2–4.4 argue *canonicity
as a feature*: scoped implicits keep conflicting instances from meeting globally,
and they weigh the alternative explicitly rather than by omission.

That matters more than the Scala precedent, because they were solving this
problem *for a language with first-class modules*, which is the constraint that
rules out the Haskell/Rust answer here.

### What the global camp is actually buying

Rust's rationale is narrower than "it is convenient". Coherence exists to make
**transitive dependencies** work: lookup must give the same answer "no matter
when and where the lookup is performed", so that a crate depending on two crates
that both implement the same trait for the same type has a defined meaning. The
orphan rule — an impl is allowed only if the trait *or* the type is local — is
the discipline that makes "look in the trait's crate or the type's crate" a
*complete* search.

Worth noting: option B is essentially Rust's orphan rule minus the globality. The
orphan rule is what makes the two-place search exhaustive; without it, B is a
heuristic rather than a rule.

And the global camp's own critics are clear about the price. Global uniqueness is
[inherently non-modular](https://blog.ezyang.com/2014/07/type-classes-confluence-coherence-global-uniqueness/):
two components can become uncomposable because each defined the same instance
deep in its internals. The community workaround — newtype wrappers to force a
choice — is widely described as ad hoc. PureScript and Idris add **named
instances** so a specific instance can be passed explicitly.

## Recommendation

**Changed after reading the precedents: A, plus an explicit escape hatch.**

An earlier draft recommended B. The evidence went the other way. The design built
for first-class modules chose A deliberately, and B's two-place search is only
exhaustive when an orphan rule makes it so — which `fun` does not have and would
have to invent.

The mandatory-open tax measured above is real, but it is **not inherent to A**.
It is inherent to A *with no way to name an impl*. Under modular implicits you
pass the implicit module explicitly when scope does not give you what you want;
PureScript and Idris do the same with named instances. That escape hatch is what
makes A livable, and `fun` does not have it — `trait evidence is not a
user-facing value` ([traits](traits.md)) forecloses it by decision.

So the real question is not A versus B. It is: **does the decision that trait
evidence is never user-facing survive contact with scoped resolution?** Every
language that chose scoped resolution also gave users a way to name an instance.
`fun` has chosen scoped resolution and forbidden naming, which is the combination
none of the precedents use.

Recommend re-opening that sub-decision on
[trait deriving and protocols](../tickets/design-trait-library-deriving-and-protocols.md)
before extending the trait library.

## Revisiting "trait evidence is not a user-facing value"

### Why it was decided

[traits](traits.md) pairs it with *compile-time specialization and dictionary
erasure are optimizations, not initial semantics*. The point is not surface
tidiness: if a user can hold a dictionary as a value, the compiler can no longer
erase it. Keeping evidence non-user-facing keeps erasure on the table.

### What it actually costs

It forecloses the escape hatch every other scoped-resolution language provides.
That is what makes the mandatory-open tax unavoidable rather than merely default:
when scope does not give you the impl you want, there is nothing to say.

### How much machinery is missing — less than expected

Measured, not assumed:

- `same[I64](1, 1)` elaborates. Explicit application of an implicit argument
  already works.
- `same[I64][d](1, 1)` reaches unification and fails with *trait dictionary Eq vs
  module value*. So the evidence position is live and type-checked; a plain
  module is simply not a dictionary.
- `impl` is a declaration form and cannot be bound to a name
  (`e = impl Eq(C) = …` is rejected at parse).
- `Core.ImplBind` carries `(kind, term, type)` — **no name field**. Impls are
  structurally anonymous in the core.

So the gap is naming, not plumbing.

### The distinction that resolves it

**Nameable impls are not user-facing dictionaries.** These are separate:

| | evidence is a value | impls are nameable |
|---|---|---|
| user can hold a dictionary at runtime | yes | no |
| user can say *which* impl to use | yes | yes |
| dictionary erasure still possible | no | **yes** |

A named impl is a *compile-time handle*. The name resolves during elaboration;
the dictionary's representation stays entirely the compiler's business, so
erasure and specialization remain available. This is what PureScript and Idris
named instances are, and what passing an implicit module explicitly is in
modular implicits.

The original decision's intent — keep dictionaries out of user hands so they can
be erased — survives naming intact. The decision as *written* forbids more than
its intent requires.

### Decision

**A — impls arrive through `open` — with named impls as a prerequisite.**

1. **Keep** "trait evidence is not a user-facing value". It is protecting
   erasure, which is worth protecting.
2. **Add** named impls as a compile-time handle, usable only in evidence
   position:

   ```
   A = import "lib"
   same[A.C, A.eq_C](A.R, A.R)     -- name the impl, skip the open
   ```

   The name resolves during elaboration; the dictionary's representation stays
   the compiler's, so erasure and specialization remain available.

3. **Then A** for visibility, matching modular implicits.

**The ordering is the decision, not a caveat.** A without the escape hatch is the
mandatory-open tax with no way out — which is today's state. A with named impls
is what modular implicits, PureScript and Idris each settled on.

### What this needs

- A name on `Core.ImplBind`, which today carries `(kind, term, type)` and is
  structurally anonymous.
- A surface form for naming an impl; `impl` is currently a declaration that
  cannot be bound (`e = impl Eq(C) = …` is rejected at parse).
- Nothing on the application side: explicit implicit application already
  elaborates and type-checks. `same[I64](1, 1)` works, and supplying evidence
  reaches unification and fails only because a plain module is not a dictionary.

### What would reverse this

A's cost falls on library authors, who must keep export surfaces small so a
mandatory `open` does not collide. Named impls reduce that but do not remove it.
If authors are still forced toward one-type-per-module in practice, revisit B —
knowing it needs a type-to-module relation that is currently undefined for
modules produced at runtime (`(f(1)).C` is unbound today, while `(f(3)).v`
works).

## Defect found alongside

`UnknownTrait` is raised when the trait **is** known and no impl matches: the
no-evidence branch reuses the unknown-trait error. There is no missing-impl
error. Every failure above reported `UnknownTrait "Eq"` while `Eq` was in scope
the whole time, which cost real time during this investigation.

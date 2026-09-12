---
title: Trait library deriving and protocols
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by: []
---

# Trait library deriving and protocols

## Question

Decide the next library-level deriving/protocol operations for traits, using
type-case where possible rather than compiler magic.

## Context

- The [map](../fun-design-map.md) and [`docs/STATUS.md`](../../STATUS.md) record trait work
  as mostly complete; detail in [traits](../topics/traits.md) and
  [trait-module-stdlib](../topics/trait-module-stdlib.md).
- Remaining milestones include explicit deriving/fallback behaviour and more
  protocol-style operations (e.g. `Eq`, `Ord`, `Show`, `Hash`).
- Where possible these should be implemented as library-level macros or
  type-case generic programming, not additional compiler machinery.

## Open input from the impl-visibility investigation

[impl-visibility](../topics/impl-visibility.md) measured what a user must write
to get an impl into consideration. Two results belong to this ticket.

**Deriving inherits the mandatory-open tax.** A trait impl reaches a use site
only through `open`, and there is no selective open — `open M` brings M's whole
export surface, with silent shadowing on collision. Generated impls live in the
defining module, so `derive Eq` on a type makes that module mandatory-open for
every user who wants equality. Whichever way
[imported-module-elaboration-context](imported-module-elaboration-context.md)
settles impl reach, decide it before deriving generates impls at scale.

**Scoped resolution gives up coherence, and that is a correctness property.**
[traits](../topics/traits.md) already decided impls resolve from lexical/imported
scope rather than a global table, which is the right call here because modules
are first-class values (every language with global coherence — Haskell, Rust,
Swift, Lean — has non-first-class modules).

The consequence is that two values of the same type may carry different impls.
Haskell's guarantee of one canonical instance per (class, type) is what makes it
sound for a sorted map to be parameterised by an ordering: two such maps cannot
disagree, so merging them is safe. Under scoped resolution they can, and merging
is unsound.

Nothing is broken today — there is no such data structure. But **any ordered or
hashed collection added to the trait library inherits this hazard**, so the
protocol work should say what it does about it: forbid impl-parameterised
containers, carry the impl in the value and compare impl identity on merge, or
accept the unsoundness and document it.

## Sub-decision to re-open: is trait evidence never user-facing?

[traits](../topics/traits.md) decided *trait evidence is not a user-facing
value*. The cross-language survey in
[impl-visibility](../topics/impl-visibility.md) suggests that decision and scoped
resolution do not sit well together.

Every precedent pairs them the other way:

- **Global resolution** (Haskell, Rust, Swift, Lean) — no need to name an
  instance, because there is only ever one. Requires non-first-class modules.
- **Scoped resolution** (OCaml modular implicits, Scala, PureScript, Idris) —
  always with a way to name one: pass the implicit module explicitly, or a named
  instance.

`fun` has chosen scoped resolution *and* forbidden naming, which is the
combination none of them use. That is what makes the mandatory-open tax
unavoidable rather than merely the default: when scope does not give you the impl
you want, there is nothing else to say.

Decide this before the trait library grows, since deriving and protocol
operations will multiply the number of impls in flight.

**Decided.** Keep the rule — it protects dictionary erasure — but add **named
impls** as a compile-time handle usable only in evidence position:

```
A = import "lib"
same[A.C, A.eq_C](A.R, A.R)
```

A named impl is not a user-facing dictionary. The name resolves during
elaboration and the representation stays internal, so erasure survives; this is
what PureScript and Idris named instances are, and what explicitly passing an
implicit module is in modular implicits.

Needs a name on `Core.ImplBind` (today `(kind, term, type)`, structurally
anonymous) and a surface form for binding an impl. The application side already
works.

Impl visibility itself is settled as **A** — impls arrive through `open` — but
only *after* named impls land. A without the escape hatch is the mandatory-open
tax with no way out. See [impl-visibility](../topics/impl-visibility.md).

## Resolution

**The named-impl prerequisite is built; the deriving/protocol design itself
remains open.**

- `Core.ImplBind`, `ModuleImpl` and `StructImpl` carry a name. The surface form
  is `impl NAME : Trait(Args) = module … end`, with the name optional; an
  unnamed impl behaves exactly as before.
- A named impl is reachable as a member, so `same[A.C, A.eq_C](x, y)` works
  without opening `A`. It names the entry the impl already occupies rather than
  adding one, so the binding's width is unchanged.
- `open` is idempotent, deduplicated by impl identity. Opening the same module
  twice used to report `AmbiguousTraitImplementation`; two *different* impls for
  one trait and argument still do.
- There is a `MissingTraitImplementation` error. `UnknownTrait` is no longer
  raised when the trait is in scope and the impl is what is missing.
- Impl visibility A needed no code: impls arriving through `open` is what the
  compiler already did. What it needed was the escape hatch, which is now there.

**Known gap.** The decision says a named impl is usable *only* in evidence
position. Nothing enforces that yet: `M.eq_C` in a value position elaborates and
prints the dictionary. Erasure is still available - the name resolves during
elaboration and the representation stays internal - but the restriction is
intent, not a rule the compiler applies.

Still unresolved, and unchanged by this work: which deriving and protocol
operations to add, and what an ordered or hashed collection does about scoped
resolution giving up coherence.

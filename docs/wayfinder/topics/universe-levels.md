# Universes and level polymorphism

A fog-stage direction: replace the single `Type` with a universe hierarchy whose
levels are first-class expressions carrying arithmetic (`max(i, j)`, `i + 1`),
so meta-libraries and category-theoretic abstractions can be universe-polymorphic
instead of duplicated per tier.

## What it is

`Type` stops being one thing and becomes `Type(i)` for a level expression `i`,
with `Type(i) : Type(i + 1)` and cumulativity (a `Type(i)` is a `Type(j)` when
`i ≤ j`). Levels are values: a definition can be generic over `i` and instantiate
at any tier. This is the Agda / Lean 4 / Coq arrangement, and it is what lets one
`Category`, `Functor` or `Monad` definition serve small and large types alike.

## Where `quill` stands

`Type : Type` today, deliberately.

- `Core.U` is a **singleton term** and `VU` a singleton value
  (`src/Quill.Kernel/Core.cs`) — no level field, nothing to carry one.
- [dependent-types](dependent-types.md) says so in as many words: the model "is
  intentionally simple … can be replaced with a proper universe hierarchy
  later."

So this is not a gap that was overlooked; it is a chosen simplification whose
replacement was always foreseen. Nothing in the tree is blocked by it yet.

## What a hierarchy would touch

Every place a `Type` appears is candidate churn, and the churn is not localized:

- `Core.U` / `VU` gain a level; `Nbe` must evaluate and quote levels; `Unify`
  needs level unification (and level arithmetic is *not* structurally
  invertible — `max`, `+1`, variables — so a sound algorithm is real work, not a
  field addition).
- Elaboration inserts levels at every `Type` occurrence and must solve or
  default them; the checker budget applies to level solving too.
- Cumulativity is a new coercion rule through unification and subtyping, hitting
  the same code as the type-specialized equality decisions.
- `Type`-case on the open `Type` is a design pillar ("types are values, type-case
  on open `Type` is acceptable"). A hierarchy has to say what matching on
  `Type(i)` means and whether `i` is observable.
- Prelude nominals and `Compiler_names`'s `Type` reference move, and the
  dependent-typed prelude (`std/stage1.qll`, `stage2.qll`) is re-elaborated
  under the new rule.

## Why this is fog, not a ticket

`Type : Type` is not currently producing wrong results or blocking a use — it is
a soundness shortcut knowingly taken in an experimental language, and every
feature landed so far is expressible without tiers. The cost is pervasive and
the payoff is unmeasured: no prelude library in the tree has needed to be
polymorphic over universe level, because there is no category-theoretic
abstraction in the prelude yet.

Adding the hierarchy before something needs it buys a large mechanical rewrite
with no consumer, which is exactly the trade the design priority
(Consistency > Flexibility > Correctness) says not to make blind.

**Sharpens when** a library in `std/`, or a userland abstraction, is written
that must quantify over arbitrary types and cannot be duplicated per tier —
concretely, the first `Category`/`Functor`/`Monad`-shaped definition that has to
be spelled twice. The ticket then starts from that definition, not from the
general idea, and can decide the level arithmetic it actually needs
(`max`-only is a much smaller sound algorithm than full level unification).

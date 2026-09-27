---
title: A written bound on an impl head
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by: []
---

# A written bound on an impl head

Split off 2026-09-27 when
[a generic impl's head variable carries no bound](generic-impl-head-has-no-bound.md)
was implemented. That ticket settled *that* an impl's head variable takes evidence;
this one asks whether the source can **say so**.

## Where it stands

The shipped contract is **inference**: the evidence an impl demands for its own head
variable is the evidence its method bodies use. The impl is elaborated as a function of
those dictionaries and its type records them, so a named impl's type reads

```
{A : Type} -> {Eq(A)} -> Eq(List(A))
```

even though no source line writes `Eq(A)`. Ruling on 2026-09-27: keep the inference,
and decide the written form in its own ticket.

## What the inference costs, measured

Two impls with the same head and the same declared type, differing only in the method
body, and a program comparing lists of a type with no `Eq` of its own
(`rec Foo = enum { F }; open Foo;` then `Cons[Foo](F, Nil[Foo]) == Cons[Foo](F, Nil[Foo])`):

```fun
# body never mentions the element
pub impl probe : Eq(List(A)) = module { fn eq(xs, ys) { True } };
#   -> VALUE True        Foo's lack of Eq is irrelevant; the impl needs no dictionary

# body compares the elements
pub impl probe : Eq(List(A)) = module { fn eq(xs, ys) {
  match (xs) { Nil => True, Cons(h, t) => match (ys) { Nil => False, Cons(h2, t2) => Eq.eq(h, h2) } }
} };
#   -> ELAB missing implementation of `Eq`
```

So whether a type gets equality is decided by a line inside a method: an impl that
forgets to use its element is indistinguishable, in the declaration, from one that
cannot work without it — and it silently claims equality for element types that have
none.

## The proposed form, which does not exist yet

```fun
pub impl probe[A : Eq] : Eq(List(A)) = module { fn eq(xs, ys) { … } };
```

The requirement moves into the declaration, matching the precedents this language
otherwise follows (Rust's `impl<T: Eq>`, Haskell's instance context), and the body
stops deciding who can use the impl.

## Open questions

1. **Where does the binder go?** `impl name[A : Eq] : …` before the colon, or after the
   head (`impl name : Eq(List(A)) with A : Eq`)? The head is a type, so the binder could
   also be read as belonging to it (`impl name : [A : Eq] -> Eq(List(A))`, which is what
   the impl's *type* already is).
2. **Must the written set and the body's demands agree?** Three answers: the written set
   must be exactly what the body uses; it must be a superset (an unused dictionary is
   simply unused); or a written bound is the whole truth and the body may only use what
   is written. Each needs a rule for the mismatch and an error at the definition.
3. **Does writing a bound change applicability?** Under the superset reading, `impl
   probe[A : Eq] : Eq(List(A))` refuses `List(Foo)` even when its body would not have
   needed `Eq(Foo)` — which is the point, and also a behaviour change for any impl that
   already exists.
4. **What happens to `Vars`?** The implementation already threads an impl's own type
   variables and now its bounds (`TraitEvidence.Bounds`, `ModuleEntry.Impl.Bounds`); a
   written bound has to be reconciled with the inferred set rather than becoming a second
   source of truth.

## Not this ticket

Surface syntax generally. [The library surface](design-std-library-surface.md) §14 sent
new syntax to [the Stage 11 spec](specify-stage-11-macro-powered-language-features.md),
which is where the spelling belongs; this ticket is the *semantics* question that would
have to be answered first.

## Reading

- [A generic impl's head variable carries no bound](generic-impl-head-var-has-no-bound.md)
  — the fix, its root cause, and the measurement table
- `src/Fun.Compiler/Elaborator.Traits.cs` — `ImplBound`, the promotion of pending
  evidence into dictionary arguments, and the impl's `Pi` type
- `src/Fun.Compiler/Elaborator.Export.cs` — how an impl's `Vars`/`Bounds` travel through
  `export`, which a written bound would also have to survive
- [trait library deriving and protocols](design-trait-library-deriving-and-protocols.md)
  — `derive` is the case that would most want to state a bound it can prove

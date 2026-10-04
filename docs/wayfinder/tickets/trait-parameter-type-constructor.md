---
title: A trait parameter may be a type constructor
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-10-03
resolution: "Closed 2026-10-03: landed as `51d79c8`. Both halves: a trait parameter's type is a meta the sig solves, and the constructor is inferred by a first-order approximation shaped as Lean's `foApprox` (named flag, default off, bounded to the head's trailing captures, one enable-site). Gate: `dotnet build` 0 errors, conformance 965 cases / 0 failed, xUnit 210/210, and the unwritten form still refused with the flag at its default."
assignee:
blocked_by: []
---

# A trait parameter may be a type constructor

Split out 2026-10-03 while probing the last item of the
[universes and level polymorphism](../fun-design-map.md#fog) fog: the sharpening
definition named there — a `Functor`-shaped trait — turned out to be blocked by a
**kind**, not a universe tier. This ticket is that measured defect. Tiers stayed fog.

## What the probe measured

The topic's own sharpening move was a `Functor` trait. Written through the runner:

```fun
trait Functor(f) = sig {
  map : [A : Type, B : Type] -> (A -> B) -> f(A) -> f(B)
};
Functor
```

```
ELAB applying non-function while inferring the form at <unknown>:3:44-3:48
```

`f(A)` applies a non-function because `ElaborateTrait` typed **every** trait parameter
as `Type` (`Value.VU.Instance`), and `f` here is a type *constructor*
(`Type -> Type`), not a type. The same shape works everywhere else:

```fun
apply_fn = fn(f : Type -> Type, a : Type) : Type { f(a) };   # -> VALUE VNominal
apply_fn(List, I64)
```

So the gap is specific to trait parameters. Under `Type : Type` there is one universe,
which is why tiers had no consumer: nothing is "duplicated per tier" when there are no
tiers. Tiers would only buy soundness (Girard's paradox), a trade the
[universes topic](../topics/universe-levels.md) already records as deliberate.

## Landed

The parameter's type starts as a **meta the sig solves**, so no trait ever writes its
parameter's kind — `Eq(a)` still infers `a : Type`, and `Functor(f)` infers
`f : Type -> Type` from the use `f(A)`:

- `Elaborator.Traits.cs` — `ElaborateTrait`: the param type is `ctx.Eval(FreshMeta(ctx))`
  instead of `Value.VU.Instance`; the sig's uses solve it.
- `Core.Traits.cs` — `TraitDecl` carries `Value ParamType` (reference equality, so trait
  identity is unchanged).
- `Elaborator.Traits.cs` — `ImplDictType` checks the impl head's argument against the
  trait's own `ParamType` rather than forcing `Type`; `TraitMethod`'s argument binder and
  `InferBoundArrow`'s bound variable take `ParamType` too.

Measured working:

```fun
trait Functor(f) = sig { map : [A : Type, B : Type] -> (A -> B) -> f(A) -> f(B) };
impl list_functor : Functor(List) = module {
  fn map[A : Type, B : Type](g : A -> B, xs : List(A)) : List(B) { Std.Lists.map(g, xs) }
};
Functor.map[List](fn(x) { x + 1 }, Cons(1, Cons(2, Nil)))     # -> Cons(2, Cons(3, Nil))

fmap : [f : Functor] -> [A : Type] -> [B : Type] -> (A -> B) -> f(A) -> f(B) =
  fn[f : Type -> Type, A : Type, B : Type](g, xs) { Functor.map[f](g, xs) };
fmap[List](fn(x) { x + 1 }, Cons(1, Cons(2, Nil)))            # -> Cons(2, Cons(3, Nil))
```

The bound goes in the **type annotation** (`[f : Functor] ->`), the plain type in the
lambda (`fn[f : Type -> Type]`) — the existing `(==) : [A : Eq] -> … = fn[A : Type](…)`
idiom. Regression: conformance 965 / 0 failed, xUnit 210/210.

## Landed: the constructor is inferred, under a named approximation

Of the three ways out, **rule 2 was taken** — but shaped as Lean 4 does it, as a
*first-order approximation* that is named, bounded and configurable rather than a silent
fallback inside `Solve`. Lean ships exactly this as `foApprox`, a mode enabled
deliberately at the call sites that need it; the same flag name and the same default
off are used here.

- **`MetaContext.FoApprox`** (`src/Fun.Compiler/MetaContext.cs`) — a documented `bool`
  defaulting to **false**. `Matching` is the existing precedent for a mode flag set
  around a region.
- **`Unify.Approximate`** (`src/Fun.Compiler/Unify.cs`) — under the flag, a spine that is
  not a pattern is solved by approximation instead of being refused; with the flag off,
  `Invert`'s refusal stands byte for byte.
- **`Elaborator.Approx`** (`src/Fun.Compiler/Elaborator.Implicits.cs`) — turns the flag on
  around the explicit argument check in `InferAp` and `InferApWithPendingDicts`, and
  nowhere else. An argument checked against an arrow's domain is what determines the
  hidden type arguments before it.

Because the trigger fires only where `Invert` would have thrown, the approximation turns
a present failure into a success. It cannot change a program that passes today.

### The shape the probe settled

The rule is **not** "the spine equals the head's arguments". Instrumenting the failing
unification printed it outright — a spine of **one** entry against a head of **two**
captures:

```
NOMINAL n=1 caps=2 decl=enum#38 rhs=Nominal { Captures = [Atom { Value = () }, AtomTy { Ty = I64 }] }
  spine[0]=VAtomTy/AtomTy { Ty = I64 }
```

So the spine's entries are the head's **trailing** captures, and a head carrying more
than the spine has entries keeps its leading ones as written: `?F := λx. Head(c₁…cₖ, x)`.
An arity-equality rule would have declined every real case. A head whose *leading*
captures mention a variable is declined rather than guessed — it would escape the spine's
lambdas, exactly as `Rename` refuses one for a pattern.

### Measured

| form | result |
|---|---|
| `Functor.map(fn(x) { x + 1 }, Cons(1, Cons(2, Nil)))` | `Cons(2, Cons(3, Nil))` |
| `fmap(fn(x) { x + 1 }, Cons(1, Cons(2, Nil)))` | `Cons(2, Cons(3, Nil))` |
| both written forms | unchanged |
| `Eq.eq(1, 1)` | `True` |
| with `FoApprox` left at its default | the unifier refusal stands; the written form still works |

Conformance 965 / 0 failed, xUnit 210/210, `dotnet build` 0 errors. The default-off row
was demonstrated by temporarily disabling the enable-site, observing the failure, and
restoring — not asserted.

### What stays true

`F` is still not *determined* by `F(A)`. The approximation picks one of several solutions
the equation admits, which is why it is bounded to the shape above and off everywhere
else, and why the written `[F]` remains the way to override a guess you can see coming.

**State:** landed as `51d79c8`.

## Why it is a ticket and not a fog item

The fog item asked for the first abstraction that must quantify over arbitrary types.
This is that abstraction, measured: the quantification was a trait-parameter *kind*
defect, and the inference is a bounded approximation. Neither was a universe level.

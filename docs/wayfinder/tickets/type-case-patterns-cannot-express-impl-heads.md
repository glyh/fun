---
title: A type-case pattern cannot express what an impl head can
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-27
resolution: Closed as **superseded**, the day it was filed. Its measurements stand (kept below as the record) but its framing was wrong in two ways, both found in the grilling session that closed it. First, the refusals it called a bug were the *decided* rule working — `type-case-generic-programming.md:88` ("unresolved uppercase names stay concrete and reject instead of becoming binders", checked) and `Enforest.Match.cs:118` ("A capitalised name is a constructor, anything else a binder") — and half of what it filed as missing was already there: a lowercase argument binds (`Option(a)` → 10), a type function heads a pattern (`Seq(a)` → 1), a nominal alias names a shape (`X = Option(I64)` → 10). Second, the real gaps are not a type-case-local fix but one rule change plus two pattern forms: the case rule made strict in impl heads too, and `^` pinning a term — which together make aliases and enclosing binders reachable by convertibility. Both are [A pattern binder is lowercase; naming an existing term takes `^`](pattern-binders-are-lowercase-and-references-are-pinned.md), whose stage 1 takes the parse-shape probe this ticket filed (whether `Option(X)` reaches `TypeHead` as head+argument or as a bare head) and whose `Not in scope` section keeps this ticket's two deferrals (an or-pattern in a head; width-tolerant heads) with [pattern-headed impls](pattern-headed-impls.md).
assignee:
blocked_by:
---

# A type-case pattern cannot express what an impl head can

> **Closed 2026-09-27 as superseded** — see the resolution in the front-matter.
> The measured evidence below is the record and is cited by its successor; the
> "bug" reading and the four reproducers' framing do not stand.

Found 2026-09-27 while grilling
[pattern-headed impls](pattern-headed-impls.md) — the ticket whose whole idea is *"the
head is a pattern over types, matched by the mechanism the language already has"*. A
recon fork probed impl heads; the integrator re-ran the load-bearing probes and added
these. **The premise does not hold today: the two grammars are incomparable, and their
binding rules are opposite.** So pattern-headed-impls is only evaluable after this is
settled, and it is now blocked on it.

Every line below was run on the Debug runner:

```sh
dotnet build
dotnet test/Fun.Conformance/bin/Debug/net10.0/Fun.Conformance.dll --file /tmp/probe.fun
```

## Measured — the same form in the two positions

| form | as an impl head (expression position) | as a type-case pattern (`match (T) { … }`) |
|---|---|---|
| `Option(A)`, no outer `A` | ✅ `trait-generic-impl` cases it; `Size.size(Some(5))` → 3 | ❌ `ELAB unbound variable: A` |
| `Option(A)`, `A : Type` in scope | — (same, binds) | ❌ `ELAB a type-case head must name a type` |
| `Option(_)` | ✅ blanket at any depth (`impl Size(Option(_))` served `Option(I64)` **and** `Option(Bool)`) | ✅ → 1 |
| `Option(I64)` before `Option(_)` | ✅ precision picks the specific | ✅ arm order: 12 |
| `Option(Option(I64))` | ✅ | ✅ → 10 |
| `Option(X)`, `X = I64` in scope | ✅ | ❌ `ELAB a type-case head must name a type` |
| `Tuple(2, A, B)` | ✅ `trait-generic-impl-two-bounds` (→ 3) | ❌ ``ELAB `Tuple` is not a constructor in scope`` |
| `I64 -> A` | ✅ `trait-impls-incomparable` (an ambiguity error, cased) | ❌ `ELAB unconsumed terms after pattern` |
| `Seq(A)`, `Seq = fn(A : Type) { Option(A) }` | ✅ → 4 | ❌ `ELAB unbound variable: A` |
| `struct { y : p; _ }`, outer `p : Type` | ❌ `ELAB unsupported module item: _` | ✅ **`p` binds and shadows** the outer `p` (matched `P = struct { y : String; … }` → 1) |
| `struct { a : I64 }` head at a `struct { a : I64 }` use, no width | ❌ `ELAB missing implementation of \`Size\`` | ✅ enters the arm |
| bare name head `X`, `X = Option(I64)` | ✅ → 10 | ✅ → 10 |
| bare name head `P`, `P = struct { y : String; z : I64 }` | ✅ (nominal and structural types are interchangeable there) | ❌ ``ELAB `P` is not a constructor in scope`` |
| head is a variable: `fn[A : Type] … match (T) { A => … }` | — | ❌ ``ELAB `A` is not a constructor in scope`` |

Reproducers for the four that are `❌` only in type-case:

```fun
# 1. a free name in a nominal argument position does not bind
{ f : Type -> I64 = fn(T) { match (T) { Option(A) => 1, _ => 0 } }; f(Option(I64)) }
#   -> ELAB unbound variable: A

# 2. …and does not refer either, even when a name of that spelling is in scope
{ X = I64; f : Type -> I64 = fn(T) { match (T) { Option(X) => 1, _ => 0 } }; f(Option(I64)) }
#   -> ELAB a type-case head must name a type

# 3. an arrow type is not a pattern
{ f : Type -> I64 = fn(T) { match (T) { A -> B => 6, _ => 0 } }; f(I64 -> Bool) }
#   -> ELAB unconsumed terms after pattern

# 4. so is neither a nominal tuple former nor a type-function application
{ f : Type -> I64 = fn(T) { match (T) { Tuple(2, A, B) => 5, _ => 0 } }; f((I64, Char)) }
#   -> ELAB `Tuple` is not a constructor in scope
{ Seq = fn(A : Type) { Option(A) }; f : Type -> I64 = fn(T) { match (T) { Seq(A) => 7, _ => 0 } }; f(Option(I64)) }
#   -> ELAB unbound variable: A
```

## What the two grammars actually are

They are **incomparable**, not one-poorer:

- **type-case has** that heads do not: `|`, the struct rest-`_`, struct field-type
  positions that *bind* (`struct { y : p; _ }`), atom-type patterns, and nested nominal
  applications.
- **heads have** that type-case does not: plain type expressions — arrows, nominal
  tuple formers, type-function applications — and **free names that bind**.
- **The binding rules are opposite.** A head's free name binds (decided 2026-09-18,
  implemented `cb52e96`; that is what makes `impl Size(Option(A))` work). In a type
  pattern a free name in argument position binds nothing: it is refused unbound, and
  refused *again* as "must name a type" when something of that spelling is in scope.

The refusing check is `ElaborateNominalHeadPattern` → `TypeHead`
(`src/Fun.Compiler/Elaborator.Patterns.cs:379`, `:395-416`): a head is accepted only
when its type is `VU` **and** its value is a `Value.VNominal` (or a projection on a
generative module's sealed binder). Struct types are `VStruct`, so a struct type name is
not a head — which is why `struct { y : p; _ }` exists as a separate pattern form.

**Not settled:** the parse shape for a nominal application whose argument is a name —
whether `Option(X)` reaches `TypeHead` as head `Option` + argument `X`, or as a head
`Option(X)` with no arguments. That decides whether a fix is in the enforester or in
`TypeHead`. Pin it first; the four reproducers above are the probes.

## The questions a fix has to answer

1. **Does a free name in an argument position bind?** If yes, it shadows any outer
   binding of that spelling — `match (T) { Option(A) => … }` inside `fn[A : Type]`
   would stop meaning the outer `A`. No passing program relies on either reading today
   (both error), so this adds meaning rather than changing it — but it must be a
   decision, not a side effect.
2. **Do type expressions become patterns?** Arrows, `Tuple(2, A, B)` and type-function
   applications are what heads use most; admitting them means the type-pattern grammar
   is `pattern ∪ type-expression`, which is the whole reason this is not a small change.
   (`Unify operators into the scope-aware binding table` is precedent for the cheaper
   direction: delete a second mechanism rather than grow one.)
3. **What is the endpoint — one grammar, or two with a documented boundary?** Heads
   shrinking to type-case's grammar is not available: it regresses four cased forms
   (`Option(A)`, `Tuple(2, A, B)`, `I64 -> A`, `Seq(A)`). So the endpoint is either
   type-case growing to a superset, or a written asymmetry.

## Why this is filed rather than decided

[pattern-headed impls](pattern-headed-impls.md) proposed `List(Expr) → List(Pattern)` for
a head — "one ADT change, touched in six places". Measured, that change would make a head
express *less* than it does today unless this ticket lands first, and the ticket's own
cost estimate omits the grammar work and the binding-rule reconciliation. Fixing
type-case first is what makes that ticket evaluable at all.

## Not in scope here

- **Width heads** (`impl Size(struct { a : I64; _ })`) and the rest row belong with
  pattern-headed-impls, not here. Adjacent unknown found while probing, which belongs to
  that work: `x : struct { a : I64 } = struct { a = 1 }` is refused with
  `ELAB type mismatch: structs with different members`, so the failure of a structural
  record head may be about how a struct literal's members are typed rather than about
  width. Probe before designing on it.
- `# 1`–`# 4` above are **not** to be added as conformance cases yet: they assert the
  current refusal. When the grammar changes they become cases —
  `type-pattern-binds-nominal-argument`, `type-pattern-arrow`,
  `type-pattern-tuple-former`, `type-pattern-type-function-application` — and the gates
  to re-run are every `test/conformance/cases/**/*trait*` case, the type-case cases
  (`core-073`, `type-case-struct-field-type`, `pattern-synonym-over-struct-type-case-subposition`),
  and the ~44 files under `cases/` that mention `impl`.

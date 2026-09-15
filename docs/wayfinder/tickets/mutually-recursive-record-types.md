---
title: Mutually-recursive record type declarations
parent: ../fun-design-map.md
status: open
assignee:
blocked_by:
---

# Mutually-recursive record type declarations

## Question

Two (or more) record declarations referencing each other — `type A = { b : B }`
together with `type B = { a : A }` — so they elaborate as a group.

## Context

- Split out of
  [mutually-recursive-nominal-types.md](mutually-recursive-nominal-types.md)
  during grilling: that ticket chose **nominal-only** `and` chains. Records join no
  `and` chain yet, and a nominal payload referencing a record declared later
  (`type A = MkA(S)` with `S` a record below) still errors — this ticket is the
  deferred remainder.
- Harder than the nominal case: a record declaration compiles to a lambda value
  (structural `VStruct` over `VSelfType`). Mutual A↔B is a **value-level knot
  between two closures** — the nominal placeholder trick does not transfer because
  it leans on placeholder and finished type sharing one `NominalId`; a record value
  has no such identity.
- Touches the open struct-elaborator questions on the map (e.g. struct open over
  `con_fields`), so it is deliberately parked until those settle.
- Real user impact once hit: AST pairs where one side is a record, mixed
  nominal↔record forward references.

## Design questions (to grill)

- Knot mechanism for mutually recursive record-type values.
- Whether mixed nominal↔record chains stay excluded or open up.
- Interaction with the record self-reference rewrite (`rewrite_record_self_refs`).

## Research (2026-09-13)

Read-only research: code reading plus REPL probes. No source changes.

### Finding 1 — the premise is false: self-recursive records do not work either

The ticket assumes a single self-recursive record works, so that the only open
question is how to extend it to several. It works only for the base case.

```
do type Opt A = Som A | Non; type L = {meta: I64; next: Opt(L)}
   l = L{meta = 1; next = Non}; l.next end                     → Non(Self) : Opt(Self)
   … L{meta = 2; next = Som(l)} end                            → CannotUnify(Self vs struct type)
```

You can declare the record and build a value whose recursive field is empty.
You can never put a record into that field. The existing tests
(`test_elaborate.ml:468–479`) only elaborate the declaration and the `Non`
case, so they don't catch this.

Mechanism: `rewrite_record_self_refs` replaces `L(A)` inside the field types
with `SelfType`, which elaborates to `VSelfType args`
(`elab_infer.ml`, three copies: module binding, struct fold, `RecordTypeDef`).
The resulting struct type value contains `VSelfType` with **no binder**. No rule
ever unfolds `Self` back into the record type. The unifier only accepts
`VSelfType` against `VSelfType` (`unify.ml:480`), so `Self` escapes into user
types (`Opt(Self)`) and cannot be related to the struct it stands for.

### Finding 2 — `Self` has no identity, so it confuses records

`VSelfType` carries only the type arguments, not which declaration it refers
to. Two unrelated recursive records therefore have the same `Self`:

```
do type Opt A = Som A | Non
   type L = {meta: I64; next: Opt(L)}
   type K = {name: String; next: Opt(K)}
   l = L{meta = 1; next = Non}
   K{name = "k"; next = l.next} end                              → accepted
```

A `K` whose field is typed "optional `L`" type-checks. Finding 1 means no
non-empty value can currently exploit this, but fixing finding 1 alone would
turn it into a real soundness hole. The same `ctx.self_type` slot also serves
`Self` inside struct method bodies (`elab_infer.ml:559`, bound to the partial
struct type), so the word "Self" covers two different ideas.

### Finding 3 — why the nominal trick doesn't transfer, restated

Nominal chains close the knot through a **name**: placeholder and finished type
share one `NominalId`. Records are structural `struct` values
([records](../topics/records.md), and "struct = record/module/namespace" in
`CONTEXT.md`). A structural type has no name to close a knot through, and
findings 1–2 show that `VSelfType` is currently standing in for a name it
doesn't have.

### Prior art — how structural type systems tie recursive knots

| System | Records are | Recursion closes through |
|---|---|---|
| Go | structural underlying types | a **named** type declaration; `type L struct{ next *L }`. Two named types with the same shape stay distinct. |
| OCaml records / Haskell / Rust | nominal | the type name; mutual via `and` / module scope |
| Lean / Coq / Agda | nominal (a structure is a one-constructor inductive) | the inductive's name; mutual blocks. Structural recursive types would need coinductive conversion checking, which these systems avoid. |
| Dhall | structural | **nowhere**; recursive types are forbidden and encoded via Böhm–Berarducci |
| OCaml objects / polymorphic variants, TypeScript | structural | **equirecursive** `μ`, compared coinductively (Amadio–Cardelli 1993). TypeScript additionally caps nesting depth heuristically. |
| OCaml `module rec`, SML recursive modules | structural modules | explicit signatures for every member, checked assume-first (Crary–Harper–Puri 1999). Abstract types lead to the "double vision" problem (Dreyer 2007). |

The recursive-module row is the closest analogue, because a `fun` struct *is* a
module.

### The design space, mapped onto `fun`

1. **Identity at the knot (Go-style iso-recursion).** A recursive record
   declaration mints an identity. `VSelfType` becomes `{ id; args }` and is
   unfolded on demand: field access, construction and unification against a
   struct look up the declaration by `id`. Non-recursive records stay purely
   structural. Mutual chains reuse the nominal three-phase knot: register every
   `id`, elaborate, finish. Fixes findings 1 and 2 with one mechanism. Cost:
   two recursive records with the same shape are distinct types, so structural
   typing stops at recursion. That is a consistency hit, but one Go has shipped
   with for a long time.
2. **Nominalize records outright.** Simplest, and shares everything with
   nominal ADTs. It breaks "struct = record/module/namespace", the design's top
   priority. Not recommended.
3. **Equirecursive structural types.** Keeps full structural typing. It
   requires a coinductive unifier and conversion in NbE, where termination of
   type equality stops being structural. This is the highest-risk option in a
   dependent core and not proportionate to the need.
4. **Forbid recursive records (Dhall-style).** Delete the half-working feature
   and tell users to recurse through a nominal ADT (`type L = MkL({…})`).
   Honest and zero-cost, but it gives up the user impact this ticket was filed
   for.

**Recommendation:** option 1. It is the only option that fixes the existing
defects and the mutual case with one mechanism, and it keeps records structural
everywhere except at the knot. Open questions for grilling:

- Does a recursive record's identity show up in type equality with a
  structurally identical non-recursive struct (`L` vs `struct meta: I64; next:
  Opt(L) end`)? Go says no.
- Should method-body `Self` get a different word, so that record `Self` can own
  the identity?
- Whether option 4 should ship as a stopgap until option 1 lands, since today's
  feature accepts programs it can't run and confuses unrelated records.

Defects spun out:
[recursive-records-cannot-hold-a-record](recursive-records-cannot-hold-a-record.md) (finding 1),
[self-type-has-no-identity](self-type-has-no-identity.md) (finding 2).

## Resolution

**Grilled 2026-09-15: option 1, identity at the knot (Go-style).** A recursive
record declaration mints an identity; `Self` carries it (`{ id; args }`) and is
unfolded on demand at field access, construction and unification against a
struct. Non-recursive records stay structural. Two recursive records with the
same shape are different types:

```fun
type Numbers = struct { head : I64; tail : Option(Numbers) }
type Scores  = struct { head : I64; tail : Option(Scores) }
s : Scores = Scores{ head = 99, tail = None }
n : Numbers = s   // error: Scores is not Numbers
```

Iso- and equi-recursive μ-types were considered and rejected. Mutual chains
reuse the nominal three-phase knot (register every id, elaborate, finish).
Still open: whether method-body `Self` gets its own word (fog item "`Self`
names two things").

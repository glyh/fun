---
title: "Review: are the 20 recorded divergences desired, or should the port match the prototype?"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:grilling
status: open
assignee: glyh
blocked_by:
---

# Review the recorded divergences

`test/conformance/prototype-divergences.txt` has **20 case lines**. Each line asserts
something specific: *"the `.expect` is the language's behaviour, and the OCaml prototype
is wrong about it"*. This ticket is that assertion put in front of you, once, so a wrong
claim cannot sit in the file unexamined.

**What each verdict does:**

- **desired** — nothing to do. The line stays; C# keeps the behaviour.
- **reject** — C# changes to match the prototype, the line is **deleted** from
  `prototype-divergences.txt`, and the case's `.expect` is rewritten to the prototype's
  answer. The OCaml runner then requires the case to pass instead of fail.

**Ten lines are already decided by you** and need no review — listed at the end so the
file adds up. **Ten need a call.** Two of them are two directions of one question, so
there are really **five** decisions.

## The five decisions (my read in the middle column)

### 1. Impl resolution: by argument type, or the innermost impl? — 3 lines

`trait-op-takes-innermost-impl` (still open: its grilled generic-impls half is
unimplemented). The lines say C# resolves `Size.size(x)` by **x's type** where the
prototype takes the innermost `impl` in scope, and that the prototype can even fail to
tie-break two equally near impls.

Cases: `values/trait-op-resolves-by-argument` (1) · `values/trait-impl-per-argument` (2)
· `elaborate/trait-op-nearness-no-tiebreak` (error)

**My read: desired.** Resolution by argument type is the only rule that keeps
`Size.size(5)` and `Size.size('c')` from depending on declaration order; and an
untiebreakable pair being an *error* rather than a silent pick is the honest half.

### 2. Does opening a handle on a unit deliver the names its forms introduce? — 1 line

`imports/unit-handle-open-form-member` (3): unit `w` does `open V; pub x = three_of`,
where `three_of` is a syntax form `v` introduces. Case expects `3`.

**My read: desired.** Without it, a unit cannot re-export a syntax form at all, and this
line and `unit-handle-open-not-a-unit-open`'s own ticket wording contradict each other —
the ticket is titled as if the answer were *no*, but its case expects *yes*. Worth a
decision partly to fix that confusion.

### 3. A duplicated member name: first or last? — 1 line

`values/signature-check-takes-last-member` (1): `sig { x : I64 }` checked against
`module { pub x = 'a'; pub x = 1 }`.

**My read: desired** — last wins, because every other member lookup in the language
takes the last match (`open`, `do` bindings, dotted paths). First-wins would be the one
place shadowing runs the other way.

### 4. Does a pattern-synonym argument bind by name or position? — **decided, listed for completeness**

Already ruled 2026-09-16 (*by name*) — see the last section. This is the single most
surprising line in the file if you read only the ticket titles, since
`pattern-synonym-arguments-bind-by-position` names the *defect*: `Flip(a, b) = Pt(b, a)`
used as `Flip(first, second)` binds `first` to `a`, i.e. to `Pt`'s **second** slot, so the
case expects `20`. The port does this; the prototype substitutes by position and answers
`10`.

### 5. Type-case head arity from the template — 1 line

`values/type-case-former-parameter` (2): `Opt(I64)` and `Opt(x)` on
`Opt = fn(A : Type) { enum … }`.

**My read: desired.** The former already declares its arity; requiring the head to be
written bare is a limitation with no purpose. (Decided by that ticket, so read it as
confirmation.)

## Prototype-only bugs — 4 lines, confirm in one word

No design content; the prototype is simply wrong and C# is right. Say **desired** unless
you want C# to reproduce the defect.

| case | prototype | C# |
|---|---|---|
| `elaborate/elab-049` | `(fn(T : Type, x : T) { x }) : Type -> I64 -> I64` accepted, written parameter type ignored | error |
| `elaborate/lambda-param-type-mismatch` | `(fn(x : Char) { x } : I64 -> I64)(1)` accepted | error |
| `values/rec-record-field-of-own-type` | can't use `rec L = struct { …; next : L }` | `1` |
| `elaborate/meta-solution-dependent-spine` | `VarNotInSpine` solving a meta to a term mentioning its own binders | `ok` |

## Already decided by you — no review needed (10 lines)

| ticket | lines | your ruling |
|---|---|---|
| `bare-constructor-pattern-resolves-by-name` | 3 | 2026-09-16: a bare head resolves through a binder or an open, never by the scrutinee's type |
| `pattern-synonym-arguments-bind-by-position` | 2 | 2026-09-16: arguments bind **by parameter name** |
| `pattern-synonym-not-a-block-declaration` | 1 | 2026-09-17: a block may declare one |
| `check-against-implicit-type-inserts-first` | 1 | 2026-09-16: accept it |
| `panic-with-unknown-message-fails-checking` | 1 | 2026-09-16: stays unevaluated |
| `deref-of-unknown-type-is-not-a-reference` | 1 | 2026-09-16: infer that it is a reference |
| `method-cannot-infer-row-with-poly-arrow` | 1 | 2026-09-17: allow it |

## Machinery, for reference

The file is a two-sided contract. The **OCaml** runner *requires* a listed case to fail and
prints `passes, but prototype-divergences.txt lists it: remove it from the list` if one
starts passing, so a fixed prototype cannot be forgotten. The **C#** runner ignores the
file entirely and compares each case to its `.expect`. So: a line in this file is never
how a C# bug hides.

There is also a pending divergence not yet in the file — the named-vs-inline polymorphic
lambda, ruled 2026-09-20 in
[port-generalise-under-check](port-generalise-under-check.md) — which will add a 21st line
when that fork lands.

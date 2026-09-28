---
title: A pattern binder is lowercase; naming an existing term takes `^`
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-28
resolution: Closed 2026-09-28 — both stages and every follow-up landed. Stage 1 (`665b8e3`) added the pin `^`, arrows, universes and tuple type patterns **additively**: 876 → 888 cases with *no* existing expected value changed, which was the proof it was designed around. Stage 2 (`997b57c`) made case decide a pattern name's role — lowercase binds, uppercase refers, an unresolved uppercase is an error — with `trait Eq(a)` and `impl Size(Option(a))` as the enforced spellings, migrating 41 `.fun` files, `std/lib.fun` and the live docs (892 cases). The follow-ups (`f705101`) then wired the unreachable-arm check as an error, restricted `a -> b` to an explicit and non-dependent Pi, and added `[a] -> b` for an implicit Pi with its binder nameable, so `[k] -> k -> k` matches `[k : Type] -> k -> k` (903 cases). The merge had to lowercase one line of stage 1's own new case, which the two parallel forks could not see. Residues recorded rather than chased: `[a]` binds the Pi's domain as `Type` only (nothing needs a non-`Type` domain yet); the pin matcher now matches when the scrutinee *is* the pinned rigid variable, which is what lets a binder's reference resolve; and a dependency check on an arrow applies the codomain closure to a *variable*, never a meta. The one form held by the user — an or-pattern in a head — is ruled on [an impl head is a pattern over types](pattern-headed-impls.md) and unbuilt until a call site appears.
assignee:
blocked_by: []
---

# A pattern binder is lowercase; naming an existing term takes `^`

Decided 2026-09-27 in a grilling session, from probing the two implementations'
pattern grammars against each other. Supersedes
[type-case patterns cannot express what an impl head can](type-case-patterns-cannot-express-impl-heads.md)
(closed the same day — its "bug" framing was wrong, and its remaining work is
stages 1–2 below). Retires the "Cost accepted" paragraph of
[trait-op-takes-innermost-impl](trait-op-takes-innermost-impl.md).

## The rule

1. **Case decides a name's role, in pattern positions only.** A bare lowercase
   name **binds**; a bare uppercase name **refers**; an uppercase name that
   resolves to nothing is an **error**. Scope does not enter into it. This
   already governs value patterns (`Enforest.Match.cs:118`: *"A capitalised name
   is a constructor, anything else a binder"*) and type-case arms; impl heads
   are the place it did not apply.
2. **A trait declaration takes an explicit lowercase parameter**:
   `trait Eq(a) = sig { eq : a -> a -> Bool }`, enforced at the declaration.
3. **Function and type-former binders stay uppercase** (`fn[A : Type]`,
   `rec Option = fn(A : Type) { … }`). They *declare*; a pattern *references*
   them, and that reference is what makes an enclosing-binder arm writable.
4. **`^name` names an existing term** — a pin. It is case-independent, it is
   **surface syntax only** (a resolved uppercase reference and a pin are the same
   internal thing: a term in a pattern position, tested by convertibility), and
   it is *required* where the case rule would otherwise bind (a lowercase name)
   or read a constructor (an uppercase *value* binding). `^` on a name that
   already resolves is redundant but accepted.
5. **Enforcement is the elaborator's**, at the point a name would bind — the
   reader cannot know, since `Option(Z)` is a binder only when `Z` is not in
   scope, and that is the judgement being removed. Hard error; no diagnostic
   channel needed.
6. **A pin covers what a literal covers**: a pin to a compile-time-known atom
   contributes that value to exhaustiveness (the existing `CorePattern.Atom` →
   `CompileSwitch` route); otherwise it covers nothing.
7. **A pinned head ranks as the value it denotes** in impl resolution's rule 2 —
   `impl Size(Option(^T))` with `T = I64` *is* `impl Size(Option(I64))`, and
   beats `impl Size(Option(a))`. It falls out of `Instance(p, q)`'s existing
   conversion check (`Elaborator.Traits.cs:385`); no new concept.
8. **Unreachable arms** get a check built on structural subsumption (an earlier
   binder/`_` at a position, or-patterns split; no value reasoning). Hard error
   now; a **warning** once Stage 12 ships a non-fatal channel.
9. **Two new pattern forms plus two new shape keys, and tuple routing** — the
   additive half, stage 1 below.

## Evidence (all measured on the Debug runner, `dotnet build` clean)

The case rule is *already* in force for arms, and already covers pattern
synonyms; what is missing is impl heads and the two directions a name cannot
express.

| probe | result |
|---|---|
| `match (T) { Option(a) => 1, _ => 0 }` at `Option(I64)` | ✅ 10 — a lowercase binder in a type pattern |
| `match (T) { List(a) => match (a) { I64 => 1, _ => 2 }, _ => 0 }` | ✅ 12 — nested and usable |
| `Seq = fn(A : Type) { Option(A) }; match (T) { Seq(a) => 1, Option(b) => 2, _ => 0 }` | ✅ 1 — a type *function* already heads a pattern (`TypeHead` reads the nominal off `Force(Seq(?))`, `Elaborator.Patterns.cs:395-416`) |
| `match (T) { Seq(I64) => 1, _ => 0 }` | ✅ 10 — a closed application |
| `X = Option(I64); match (T) { X => 1, _ => 0 }` | ✅ 10 — a *nominal* alias names a shape |
| `match (T) { struct { y : p; _ } => … }` at `P = struct { y : String; z : I64 }` | ✅ `p` binds and shadows, rest `_` gives width |
| `match (T) { Option(A) => 1, _ => 0 }` at `Option(I64)` | ❗ `unbound variable: A` |
| `fn[A : Type](T) … match (T) { Option(A) => … }` | ❗ `a type-case head must name a type` |
| `Seq = fn(A : Type) { Option(A) }; fn[A : Type](T) … match (T) { Seq(A) => … }` | ❗ `a type-case head must name a type` — an enclosing binder is unreachable from a pattern |
| `X = I64; match (T) { Option(X) => … }` / `{ X => … }` / `{ struct { y : X; _ } => … }` | ❗ `a type-case head must name a type` / `` `X` is not a constructor in scope `` — a lowercase *atom* alias is unreachable |
| `match (T) { I64 -> Bool => … }` | ❗ `unconsumed terms after pattern` |
| `match (T) { Tuple(2, a, b) => … }` / `{ (a, b) => … }` | ❗ `` `Tuple` is not a constructor in scope `` / `cannot unify VU with VProdTy` (a parenthesized pattern parses as a **value** product) |
| `match (T) { Type => … }` | ❗ `` `Type` is not a constructor in scope `` (a universe is `Value.VU`, `Core.cs:145`; `AtomTy` is `{ I64, Unit, Char, String, Scopes, Absurd }`) |
| `pattern Two(A, B) = (A, B)` / `pattern Two(A, b) = (1, b)` | ❗ `unbound variable: A` / `a pattern synonym's parameter `A` is not bound by its pattern` — **the rule already covers synonyms** |
| impl head `Option(Z)` with `Z` unbound | ✅ binds a fresh variable — any case, silently |
| impl head `Option(z)` with `z = I64` in scope | ✅ resolves to the **alias** (so `Size.size(Some(True))` is `missing implementation`) |
| `Some(True)` in an argument position | ✅ resolves as a **constructor** — the case rule fires inside arguments |
| `x = 5; match (v) { x => 1, _ => 0 }` at 5 and 9 | ✅ 11 — lowercase **shadows**, so an outer value cannot be matched today |
| `match (n) { _ => 1, 5 => 2 }` | ✅ 1 — **no unreachable-arm check exists today** |
| `impl Size(_)` + `impl Size(I64)`, use at `I64` | ✅ 2 — impls are precision-ordered, so a blanket never kills a later head |

Check sites: `Enforest.Match.cs:118` (the case rule), `Elaborator.Patterns.cs:379`
and `:395-416` (`TypeHead` returns non-null only when the head's value is a
`VNominal`), `Core.Patterns.cs:61` (`NeedsDirectMatch`), `Elaborator.Match.cs:71`
(any such pattern swaps the whole match to `DecisionTree.Sequential`),
`MatchCompile.cs:255` (`KeyOf`: only `AtomType` and `NominalHead` are keys).

## Stage 1 — the additive forms (fork 1)

No existing program changes meaning; **the suite must be green with unchanged
expected values**, and that is the proof of additivity. Two new `Syntax.Pattern`
forms and `CorePattern` forms, two new `TypeKey`s, one elaboration rule, and the
diagnostics:

1. **Pin.** `Pattern.RawPatPin`/`PatPin` (following the existing `RawPat*`
   convention) → `CorePattern.Pin(Term)`. Test = convertibility against the
   scrutinee, via the `Sequential` path; `NeedsDirectMatch() => true`, contagious
   through `Prod`/`Or`/`Con`/`Record` exactly as `StructType` is. Coverage per
   rule 6. Works in value patterns (`Some(^x)` — a capability the language does
   not have today), type patterns (`Option(^A)`, `Option(^z)`) and impl heads.
   A pinned test against a not-yet-known scrutinee must park — the `FMatch` stuck
   path exists (`Nbe.StuckMatch.cs`).
2. **Arrow.** `A -> B` as a pattern form → `TypeKey.Pi`. Note the head/arm
   asymmetry it closes: `impl Conv(I64 -> A)` is cased today
   (`trait-impls-incomparable`) while `match (T) { I64 -> Bool => … }` is a parse
   error.
3. **Universe.** `Type` as a pattern form → its own key (not an `AtomTy` case: a
   universe is `Value.VU`).
4. **Tuple routing.** When the scrutinee's type is `Type`, a parenthesized
   pattern is a **tuple type pattern**, not a value product — today `(a, b)`
   reaches the elaborator as `CorePattern.Prod` and fails `cannot unify VU with
   VProdTy`. Likewise let a pattern head name the tuple former (`Tuple(2, a, b)`
   is cased as an *impl head*, `trait-generic-impl-two-bounds`).
5. **Unreachable arms** (rule 8): a new structural-subsumption check, hard error.
   A later arm is an error when an earlier arm covers it syntactically — the same
   head with a binder/`_` at a position, `_` wholesale, or-patterns split — with no
   value reasoning. Keyable pins are checked by the tree for free (a pin to a known
   atom is a literal pattern); unkeyable ones are ordered trial and unchecked —
   document that asymmetry where the check lives. **This belongs to stage 1, not
   stage 2**: it is pattern semantics, and it shares `MatchCompile.cs` with the new
   keys, which is where it lives.
6. **Diagnostics** (see the three in the table): `a type-case head must name a
   type` fires when the *argument* is at fault and the head did name a type —
   name the offending term and position; `… is not a constructor in scope` is the
   wrong word at the type level (the rule is `TypeHead`'s `VNominal` requirement)
   — say type/nominal. Several become unreachable once 1–4 land, which is the
   check on the wording. Pin each with an **xUnit** assertion: conformance
   `.expect` files hold only a value or the literal `error` and cannot pin text,
   while `BudgetTests`/`LoaderTests` already assert message substrings.
7. The **reflection ripple** (`CLAUDE.md`'s six steps: prelude ADT + builders +
   wrap/unwrap + the syntax-nominals registry + **every** construction site +
   rule templates). The round trip must stay the identity, so a macro can now
   emit `^a`, `A -> B`, `Type`, and a tuple type pattern.
8. **Reader**: `^` is currently `unexpected character` and is not in
   `OperatorChars` (`+-*/%=!<>@~&|`, `Reader.cs:28-29`). It becomes a **dedicated
   prefix token**, matching the project's existing decision that structural
   punctuation keeps dedicated tokens rather than joining the operator set.

## Stage 1 — landed 2026-09-27 (`8c7b029`, `e275530`, `21a1f98`; merged as `pi-agent-8f63245a-a2d0-431`)

Suite `876 cases, 0 failed` → `888 cases, 0 failed`; xUnit `188` → `202`; **no existing
expected value changed** — the additivity proof this stage was designed around. Working:
`match (v) { Some(^x) => … }` against an outer value (impossible before); `Option(A)` with
an enclosing `A` and `Option(^A)` both as references; `Option(^z)` against a lowercase
alias; `I64 -> Bool`; `Type`; `(a, b)` and `Tuple(2, a, b)`. Diagnostics reworded to name
the offending term: `` `C` is not a type or nominal in scope ``, `` `Some` must name a type
in a type-case ``, `unexpected terms after the pattern: Bool`.

**Three things stage 1 opened, none of them silent:**

1. **The unreachable-arm check is implemented but not wired — decided 2026-09-27: wire it
   FULL (user).** Coverage by a variable or wildcard counts, so the check reports the two
   shapes it was written for, and the two *deliberate, cased* behaviours it contradicts are
   the accepted cost:
   - `values/core-133` — `match (1) { _ => 0, 1 => 1 }`, commented "match first branch wins" —
     its expect `0` becomes `error`. **Stage 12 flips it back**: this error becomes the
     warning the ticket already asks for, so the idiom becomes legal again, with a warning.
   - `values/stuck-match-pruned-arm` — "a stuck match whose tree prunes a shadowed arm still
     reads back: it waits, arm 0 wins" — its *subject* disappears, because the pruning it
     exercises becomes illegal. The stuck readback needs another home (a legal stuck match
     with no pruned arm), unless another case already covers it.
2. **A dependent Pi's codomain read at the domain — measured, then restricted (decided
   2026-09-27, user: non-dependent arrows only, "for now").** What the arrow pattern did:
   `Nbe.Match.cs` resolves a `VPi` occurrence as `c.Index == 0 ? pi.Domain :
   ApplyClosure(…, pi.Domain)`, so `a -> b` bound `a` to the Pi's domain and `b` to the
   codomain **instantiated at that domain** — one instance of the family, handed over as if
   it were the result type. Measured: `I64 -> Bool` → `(I64, Bool)`; `I64 -> I64 -> Bool` →
   `(I64, I64 -> Bool)`; `((k : Type) -> List(k)) -> Bool` → matches (a dependent *domain*,
   constant codomain); `(k : Type) -> List(k)` → `(Type, List(Type))`; `[k : Type] -> k -> k`
   → `(Type, Type -> Type)`; `[k : Type] -> Bool` → `(Type, Bool)`. The middle pair is the
   trap.

   **Decision: match only a non-dependent codomain.** Implementation: apply the codomain
   closure to a fresh **variable** (not a meta — no solving, nothing to restore) and
   occurs-check the result; depends on it ⇒ the arm does not match (falls through to a later
   arm, and there is no error, because the scrutinee is a runtime value).

   | scrutinee | before | after |
   |---|---|---|
   | `I64 -> Bool` | ✅ `(I64, Bool)` | ✅ same |
   | `I64 -> I64 -> Bool` | ✅ `(I64, I64 -> Bool)` | ✅ same |
   | `((k : Type) -> List(k)) -> Bool` | ✅ matches | ✅ same |
   | `[k : Type] -> Bool` (implicit, constant codomain) | ✅ `(Type, Bool)` | ✅ same — the restriction is about *dependence*, not implicitness |
   | `(k : Type) -> List(k)` | ✅ `(Type, List(Type))` | ❌ no match |
   | `[k : Type] -> k -> k` | ✅ `(Type, Type -> Type)` | ❌ no match |

   So `a -> b` now means *a plain function type*: an arrow whose result does not vary with
   its argument.

   **Two more decisions, same day (user).**

   - **`a -> b` matches an *explicit* Pi only.** A polymorphic type is an implicit Pi, and the
     explicit arrow pattern stops matching it: `[k : Type] -> Bool` matched before
     (`(Type, Bool)` measured) and does not after. Each pattern form names one Pi kind; none
     matches both.
   - **A new pattern form `[a] -> b` matches an implicit Pi and names its binder.** The shape
     that motivated it:
     ```fun
     match (T) { [k] -> k -> k => … }    # against [k : Type] -> k -> k
     ```
     `k` is the Pi's own binder and the `k` inside the codomain pattern **refers** to it —
     which is what makes a *dependent* pattern expressible, and why the explicit form's
     restriction costs no coverage: what the explicit form cannot name, this one can.
     Mechanism: bind the Pi's binder to a fresh **rigid variable**, then match the codomain
     pattern with it in scope, the pattern's mention of the name being the `Pin` node stage 1
     already built. The codomain is matched *as a pattern* — not read at the domain.
     - **The local rule this needs:** a name the pattern itself binds (an explicit binder
       position) is a **reference** in the rest of that pattern; the case rule decides only
       otherwise-unbound names. Without it, `[k] -> k -> k`'s second `k` would bind a second
       variable.
   - **Unmeasured:** arrows carrying effect annotations (`->{E}`) — which side of the
     explicit/implicit split they fall on is a probe, not a guess.

   Stage 1's own case (`pattern-arrow-type-case.fun`) uses only explicit, non-dependent
   arrows (`I64 -> Bool`, `I64 -> I64`, expect 3), so neither restriction is covered by it:
   both need new cases — the implicit form against a polymorphic type, and the explicit form
   *failing* to match one — plus the `[k] -> k -> k` dependently-matched case.

   A term-indexed family was already unreachable as a scrutinee (`(n : I64) -> List(n)` is
   `cannot unify VAtomTy(I64) with VU`); `[a] -> b` is what makes the type-indexed dependent
   case expressible.
3. **It is four forms and three keys, not two and two.** `Pin`, `Arrow`, `Universe`,
   `TupleType` in `Syntax.Pattern`/`CorePattern`, and `TypeKey.Pi` + `TypeKey.U` +
   `TypeKey.Tuple(arity)` — the last because the plain `Prod` route throws at runtime for a
   non-tuple scrutinee. Factual correction to the prose above.

## The follow-ups landed 2026-09-27 (`f705101`, merged as `pi-agent-14e3c216-arrow-implicit`)

Base `997b57c`; on the merged tree `898` → **`903` cases, 0 failed**, xUnit `205` → `206`.
**Exactly one expected value changed** — `values/core-133.expect` `0` → `error`, the cost rule 8
accepted — and every other `.expect` in that diff is a *new* file.
`values/stuck-match-pruned-arm` was rewritten to stay a *stuck* match without the now-illegal
shadowed arm (subject kept, `None => I64` added, deliberately not a byte duplicate of
`stuck-match-sub-occurrence`) rather than deleted.

- **Rule 8 is wired**: coverage by a variable or wildcard now reports — an error until
  Stage 12's channel turns it into the warning the ruling asked for.
- **`a -> b` is explicit-only**: `[k : Type] -> Bool` no longer matches; it falls to a later arm.
- **`a -> b` is non-dependent**: the codomain closure is applied to a fresh variable and
  occurs-checked, so `(k : Type) -> List(k)` falls through instead of binding
  `b := List(Type)`. `I64 -> Bool`, `I64 -> I64 -> Bool` and
  `((k : Type) -> List(k)) -> Bool` still match, and an effect-annotated arrow
  (`I64 ->{E} Bool`) matches with `b := Bool` — pinned by a case.
- **`[a] -> b` exists** for an implicit Pi and names its binder: `[k] -> k -> k` matches
  `[k : Type] -> k -> k`, the codomain matched *as a pattern* (the binder bound to a fresh
  rigid variable, mentioned through `Pin`). It paid the reflection ripple — prelude ADT,
  registry, wrap/unwrap, every construction site, rule templates — and a macro round trip is
  cased.

**Two judgement calls it reported, recorded rather than buried:**

1. **`[a]` binds the Pi's domain as `Type` only** — the form writes no domain constraint, so
   the elaboration had to choose one. Consequence: an implicit Pi whose binder is not over
   `Type` (`[n : I64] -> …`) will not match `[a] -> b`. Nothing needs it today; if something
   does, the choice to revisit is a wildcard domain.
2. **The pin matcher now matches when the scrutinee *is* the pinned rigid variable**, where it
   previously parked on any `VVar`. That is what lets the binder's reference resolve, and it
   is sound because two distinct rigid variables cannot share a level.

## Stage 2 — the case rule, the trait parameter, the migration (fork 2)

One atomic change: the rule and its call sites cannot land apart, or `main` has
spellings that now mean something else.

1. The error for a binder that is not lowercase, at the elaborator's
   binder-introduction sites: `InferImplBinding` / `ImplDictType` for an impl
   head (`Elaborator.Traits.cs:275`, `:80`) and the trait declaration's parameter
   (`ElaborateTrait`, `Elaborator.Traits.cs:41`). The message names the term and
   the fix (`A` in `impl Size(Option(A))` is a reference; write `a` to bind).
2. **Keywords** resolve before that check and are never binders or pins: `Self`
   is uppercase and *refers*, so `pub impl Eq(Self)` (`core-152.fun`) stays legal
   with no exemption — the rule is merely ordered after keyword resolution. `^Self`
   is an error (a keyword is not a term).
3. **Pattern synonyms need no code**: `pattern Two(A, B)` already fails
   (`unbound variable: A`) and an unused uppercase parameter fails with its own
   message. Add one conformance case pinning the rule (the repo did the same for
   grouping rules in `pin-grouping-rules-with-cases`).
4. **The migration**, classified by the rule itself:

   | spelling | under the rule | action |
   |---|---|---|
   | `impl Size(A)` (blanket, `trait-impl-precision.fun`) | unresolvable reference | → `impl Size(a)` |
   | `impl Size(Option(A))` ×12, `impl Size(I64 -> A)` ×2, `impl Size(B -> Bool)` ×2, `impl Size(Tuple(2, A, B))` ×1 | a free uppercase name binds today | → lowercase |
   | `impl Eq(C)` with `pub type C = R` above (`elab-194.fun`), `eq_T : impl Eq(T)` (`elab-087.fun`) | already in scope → a reference | unchanged |
   | `pub impl Eq(Self)` (`core-152.fun`) | keyword | unchanged |
   | 44 trait declarations `trait Size(A) = sig { size : A -> I64 }` | parameter must be lowercase | → `Size(a)` / `a -> I64` |
   | `std/lib.fun:32` `pub trait Eq(A) = sig { eq : A -> A -> Bool }` | same | → `Eq(a)` / `a -> a -> Bool` (`std` has **no** generic impls) |
   | `test/Fun.Tests/TraitTests.cs` head strings | same | → lowercase |
   | live docs: `CLAUDE.md`, `CONTEXT.md`, `docs/STATUS.md`, `docs/wayfinder/topics/**` | 21 occurrences | → lowercase |
   | `docs/wayfinder/tickets/**` (including this one), `macro-system/` | dated records | untouched — a ticket keeps the spelling it was written with, the same way closed tickets keep the old `do … end` syntax |

   A lowercase *type* binding referenced from a pattern is **pinned, not
   renamed** — `impl Size(Option(^z))` with `z = I64`. The sweep found no such
   spelling in the repo (every in-scope head reference is uppercase), so this is
   a rule statement rather than a migration item.
5. Cases: one per migrated spelling class, plus the new refusals
   (`trait-parameter-must-be-lowercase`, `impl-head-binder-must-be-lowercase`,
   `impl-head-uppercase-is-a-reference`, `unreachable-arm-subsumed`), and an
   xUnit assertion for each new message.

## Stage 2 — landed 2026-09-27 (`abbc3fb`, `4aad218`, `131c027`; merged as `pi-agent-7e5b728e-5842-45f`)

Suite `888 cases, 0 failed` / xUnit `202` on the stage-1 base → **`892, 0 failed` / `204`**.
The migration-only commit was green *under the old rule* (876/188), so the rename is
behaviour-free and the rule is what changes meaning.

The rule lives in `RequireLowercase`, called from `ElaborateTrait` and `BindHeadNames` — the
impl head's binder-introduction site — and it checks only names that would *bind*: a resolved
uppercase name stays a reference (`impl Eq(C)`, `impl Eq(T)` unchanged) and keywords never
reach it (`pub impl Eq(Self)` still resolves). Rejections, measured: `trait Size(A)` →
`` a trait parameter `A` must be lowercase; write `a` to bind ``; `impl Size(A)` and
`impl Size(Option(A))` → ``` `A` in an impl head is a reference, not a binder; an impl head
binder must be lowercase; write `a` to bind ```; `pattern Two(A, B)` → `unbound variable: A`
(already the rule, no code).

Migrated: 41 `.fun`/`.unit-lib.fun` files, `std/lib.fun:32`, the `TraitTests.cs` /
`ReflectionTests.cs` strings, doc comments in `Elaborator.Traits.cs` / `Enforest.Traits.cs`,
and live docs (`STATUS.md`, `topics/traits.md`, `topics/trait-module-stdlib.md`); tickets and
`macro-system/` untouched. Added 4 case pairs and 2 xUnit tests.

**One question it raised, resolved by the integrator:** item 1 above names one impl-head
message while item 5 names two refusals. They are the two *spellings* of that one message —
bare `impl Size(A)` and nested `impl Size(Option(A))` — kept under those two case names.
Override if they were meant to read differently.

**One fix the merge needed, because the two stages ran in parallel:** stage 1's
`pattern-pin-impl-head.fun` was written before the rule existed and spelled
`trait Size(A) = sig { size : A -> I64 }`; the merge lowercased it. The two other uppercase
`trait Size(A)` occurrences in the tree are the *negative* cases that assert the error
(`trait-parameter-must-be-lowercase.fun` and its xUnit sibling) and must stay as they are.

## What this settles, and the boundary

- The rule governs **pattern positions only**. A type *expression* — an
  annotation, a `sig` field, a return type — resolves every name by scope
  whatever its case, so `fn(x : Option(z))` with `z = I64` stays legal and needs
  no pin.
- References and pins are one internal thing, so the ticket that proposed
  `List(Expr) → List(Pattern)` for a head is answered without it: a head keeps
  accepting type expressions, and what a name in one *means* is what changed.
- Cross-reference: `macro-type-binders-should-be-explicit`'s "**No case rule**"
  bullet is scoped to **macro annotations**, a type position in a signature. A
  head is a *matched* position, the same family as a value pattern and a
  type-case arm, which is why the two conventions coexist rather than
  contradict. That ticket's functional benefits (meaning no longer depends on
  unrelated code; a typo errors) are both delivered here.

## Not in scope

- **An or-pattern in a head** (`impl Size(Option(a) | List(a))`) — decided
  2026-09-27 as *one declaration with or-pattern rules* (every alternative binds
  the same names), **not** an abbreviation for N declarations; the ruling and its
  reasoning are recorded on [pattern-headed impls](pattern-headed-impls.md), which
  is where it is built.
- **Width-tolerant heads** (`impl Size(struct { a : I64; _ })`), likewise — now
  cheap, since `StructType` is already a `NeedsDirectMatch` pattern and
  `Sequential` exists.

## Gates

Every stage: `dotnet build`, `dotnet test test/Fun.Tests`, and
`dotnet run --project test/Fun.Conformance` — **the case count must not drop and
no expected value may change in stage 1**. Stage 2 also re-runs the type-case
cases (`core-073`, `type-case-struct-field-type`,
`pattern-synonym-over-struct-type-case-subposition`) and every case whose file
name contains `trait`.

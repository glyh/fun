---
title: Port core_tt to .NET (C#)
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
  - domain-model-core-tt.md
  - domain-model-surface-enforestation.md
  - imported-module-elaboration-context.md
  - env-width-contract-is-unnamed.md
  - unify-primitive-declaration.md
  - constructor-lookup-matches-type-name.md
  - core-traversals-ignore-binding-list-depth.md
  - struct-open-does-not-scope-over-con-fields.md
  - templates-desugar-to-macros.md
  - bare-arrow-is-pure.md
  - refs-in-effect-rows.md
  - nominal-identity-applicative-by-purity.md
  - handlers-tunnel-callback-effects.md
  - self-type-has-no-identity.md
  - recursive-records-cannot-hold-a-record.md
  - mutually-recursive-record-types.md
  - recursive-definitions-stuck-on-open-arguments.md
  - pattern-head-accepts-type-formers.md
  - macro-annotation-constraints-mean-nothing.md
---

# Port `core_tt` to .NET (C#)

## Handover (2026-09-17) — start here

**State (updated 2026-09-18).** C#: **677 of 690** conformance cases pass, **168/168**
xUnit; `dune test` and `dune test test/conformance` green. Stage 2 is the prelude and
every stage-2 stopgap is gone. What remains is triaged in
[port-stage2-residue](port-stage2-residue.md).

*Previous pause (2026-09-17).* `main` was `1453609`+, C# **393 of 689**, **172/172**
xUnit. OCaml: 689 cases, 0 failed, **19 known
prototype divergences** (`test/conformance/prototype-divergences.txt`). History of
each step is in the waves below and in each `port-*.md` ticket's Resolution.

**Verify first:**
```
cd dotnet && dotnet build -v q --nologo && timeout 300 dotnet test test/Fun.Tests --nologo -v q \
  && timeout 300 dotnet run --project test/Fun.Conformance --no-build | tail -1
cd .. && dune test && dune test test/conformance
```

**How the work runs** (user preferences, also in memory):
- The domain model and `CONTEXT.md` are the spec; the OCaml prototype is supporting
  material and is not maintained after the port. A prototype defect: reproduce it,
  ticket it, fix it in C# only, add the shared case with the correct `.expect`, list
  it in `prototype-divergences.txt`.
- Work fans out to fork subagents in worktrees (`fork-and-integrate` skill), **at most
  two implementation forks at once**; the parent integrates (merges, runs both
  suites, tickets follow-ups, asks the user). Every fork follows
  [Porting conventions](#porting-conventions-2026-09-16).
- The user decides semantics: ask one question at a time, with a concrete example
  and a recommendation, and record the ruling on disk before a fork reads it.

**What blocks the remaining 296 cases** (first blocker, 2026-09-17): prelude
operators (199, "the infix operator …"), stage-2 syntax forms such as `type` (95,
"prelude syntax roles"), two stage-2 names, and one C# bug (below).

**Next, in order:**
1. ~~**Prelude stage 2**~~ **Done 2026-09-18** ([port-prelude-stage2](port-prelude-stage2.md),
   merged from `port/prelude-stage2`). `stdlib` and `import "std"` are stage 2, which
   imports stage 1 as `"std/stage1"` and re-exports it; every stage-2 stopgap deleted.
   393 → **611/689**; the "the infix operator …" (199) and "prelude syntax roles" (95)
   groups are gone. xUnit 172 → 168 (four cases pinned to the deleted stopgap; two
   added). Recorded there: reflection anchors on stage 1, and a loader seeds its macro
   metas from its prelude stage at construction.
2. ~~**C# bug, `imports/core-301`:** a syntax form's template `module $b` (a Block
   hole after `module`) fails with the *language* error "module is written module
   { … }".~~ **Done 2026-09-18** (`268c355`): `Enforest.ParseModuleExpr` now takes the
   prototype's eager-hole branch (`parse_module_expr`, `enforest.ml:385`) before it
   requires a brace group. 392 → **393/689**, xUnit 172/172. (The 2026-09-17 handover's
   "393 before" was off by one; the merge base actually measured 392.)
3. **Triage — done, see [port-stage2-residue](port-stage2-residue.md).** 78 failures in
   six causes. 68 of them are one family (`export`, as the stage-2 `type` macro uses
   it) and are **blocked on one undecided rule**: may a declaration syntax form used
   inside a block write an `export`? Ask the user before forking that family.
   `macros/core-308` (expected 1010, got 110) was the only silently wrong answer and is
   **fixed** (`b8bf5bb`): it took 19 more cases with it, closing the residue's item 2.
   611 → 630/689. Then the `export` family (residue items 1 and 3, `ac86a56`) →
   **677 of 690**, **13 failures left**, each a singleton or near-singleton. **All six
   causes are now closed** (2026-09-20, `fbea929`, `4fdab26`): C# **688 of 691**, and
   the 3 that remain are E11's — they moved to
   [E11 nominal identity](port-nominal-identity.md).
4. **Parity, not a green suite** — [reach feature parity with the
   prototype](port-parity-plan.md) is the recipe for what remains, in order: finish
   [E11 nominal identity](port-nominal-identity.md) (the one place the port is *less*
   correct than the prototype), the audit of every
   [unported path](port-unported-path-audit.md) (62 sites found, sorted into 17 real
   gaps, 13 wrong-kind refusals, 17 unreachable, 9 undecided — a throw is only a
   failure when a case reaches it), and closing the recorded divergences (done
   2026-09-20: 13 of the 14 closed).

**Open for the user (not blocking):**
- Generic impls (`impl Size(Option(A))`): **decided 2026-09-18** — a free name in an
  impl head binds, on pattern grounds; ambiguity still fails. Written up on
  [trait-op-takes-innermost-impl](trait-op-takes-innermost-impl.md) "Grilled
  (2026-09-18)"; ready to implement, not yet forked. The rest of the idea (synonym
  heads, or-patterns, blanket `_`, `DeclImpl` args as patterns) is split off to
  [pattern-headed-impls](pattern-headed-impls.md) and is not blocking.
- Unverified deviation on [prelude stage 1](port-prelude-stage1.md): generalising
  several metas at once.

**Known stopgaps (`ponytail:` in code):** a typed macro argument is elaborated twice;
matching a nominal head evaluates its head with a nested `Eval`; the run-time module
stamp for generative modules is not built (type-case heads on generative nominals,
generative formers and rec groups are "not ported yet").

**Blockers (2026-09-16): none open.** Every ticket in `blocked_by` is closed; E11
(nominal identity) was the last.

## Question

Keep the `core_tt` language model, reimplement the compiler and runtime on .NET,
target the CLR (GC/JIT/tooling for free) rather than a custom VM, preserving room
for effects via CPS/trampolining. Promoted from the design map's fog list now
that it has a blocking set.

Open within this ticket: **F# or C#**. F# is far closer to the prototype —
discriminated unions, exhaustive matching, immutability by default — so the
elaborator and NbE transliterate with the pattern-match structure intact, and
the compiler still catches a missing case when a `Core.term` variant is added.
C# gets the mainstream tooling and ecosystem. A split (F# core, C# tooling/host)
is a third option. Decide before writing, not during.

## Why it is blocked rather than started

The prototype's load-bearing invariants are not in its code — they are in the
author's head, and a port carries code. See
[domain model for core_tt](domain-model-core-tt.md) for the argument and the
evidence: four separate defects, each one a rule with no name in the source.
Porting first means re-deriving each invariant in a language where the mistakes
are harder to find, and inheriting the defects looking deliberate.

The blockers are therefore of two kinds:

**Model** — [domain model for `core_tt`](domain-model-core-tt.md). The port's
specification. Types in the new implementation should be named after its
vocabulary.

**Defects that would be ported faithfully:**

- [imported modules elaborate in the importer's context](imported-module-elaboration-context.md)
  — a crash today; also the cache design the port would copy
- [env-width contract is unnamed](env-width-contract-is-unnamed.md) — the single
  most likely thing to be silently mis-transcribed
- [one declaration per primitive](unify-primitive-declaration.md) — (closed
  2026-09-15) one table; overflow semantics written down (checked I64)
- [constructor lookup matches the type name](constructor-lookup-matches-type-name.md)
- [core traversals ignore binder depth](core-traversals-ignore-binding-list-depth.md)
  — the env-width invariant broken in a third place
- [struct open does not scope over `con_fields`](struct-open-does-not-scope-over-con-fields.md)
  — settle the rule before the struct elaborator is written a second time

**Blockers added 2026-09-14** — the language is still moving, and a port carries
the design as it stands the day it starts. These change meaning, not code shape,
so each is cheaper to settle in the prototype than to re-port:

- **Type-system semantics** (decided, unimplemented) — they change what a type
  means for every program, through every elaborator and unifier path:
  [a bare arrow is pure](bare-arrow-is-pure.md),
  [refs in effect rows](refs-in-effect-rows.md),
  [nominal identity by purity](nominal-identity-applicative-by-purity.md),
  [handlers tunnel callback effects](handlers-tunnel-callback-effects.md) (closed 2026-09-15).
- **The known soundness hole and the knot it depends on** —
  [`Self` has no identity](self-type-has-no-identity.md) (unrelated recursive
  records unify), [recursive records cannot hold a record](recursive-records-cannot-hold-a-record.md),
  [mutually recursive record types](mutually-recursive-record-types.md).
- **Open semantic questions** (grilling) —
  [recursive definitions stuck on open arguments](recursive-definitions-stuck-on-open-arguments.md),
  [pattern heads accept type formers](pattern-head-accepts-type-formers.md),
  [what a macro annotation constraint means](macro-annotation-constraints-mean-nothing.md),
  alongside the existing struct-open blocker.
- **The macro model's last distance** —
  [templates desugar to macros](templates-desugar-to-macros.md) ("Left":
  procedural macro parameter kinds, token-position quote holes).

**Readiness is also a stability signal, not only a closed list:** start when a
few implementation runs in a row land without reopening a decision. The M7/M9
run revised two decisions mid-implementation (generated syntax is hygienic;
quotes parse at definition).

**Decide at the start of the port, not before:**

- **The evaluator does not recurse on the native stack per object-level call.**
  In OCaml 5 that is O(N²) through stack scanning
  ([deep non-tail recursion](deep-non-tail-recursion-is-superlinear.md)); on the
  CLR's default 1 MB stack it is a crash. CPS/trampolining for effects already
  points there.
- **F# or C#** (above) — C# has been the working assumption in conversation;
  confirm it deliberately against F#'s near-transliteration of the elaborator
  and NbE.

Explicitly **not** blocking: diagnostics polish and error spans (deliberately
deferred to post-rewrite), enforester combinator work, the IR-layer-count
question (worth resolving *during* the port design), prototype performance
hotspots ([type-case refinement](type-case-refinement-walks-whole-context.md)),
the [role visibility gaps](role-visibility-gaps-after-m7.md) and
[capture extents by exceptions](capture-extents-chosen-by-exceptions.md) —
implementation defects the port redesigns rather than transliterates.

## Related

- [formalized core semantics](../topics/formalized-semantics.md) — a Lean/Coq
  spec as an AI-checked structural-correspondence reference across the port.
  Depends on the same vocabulary the domain-model ticket produces.
- [too many IR layers](../fun-design-map.md#fog) — `Syntax.t` and `Surface.t`
  look near-isomorphic; whether one collapses is a port-design question.

## Resolution

_Unresolved._

## Decided (2026-09-16): C#

The port is written in **C#** (user decision), not F#. Variants become sealed
record hierarchies matched with `switch` patterns; keep the domain-model names
(the port's types are named after the domain model docs). Still decided at port
start: the evaluator does not recurse on the native stack per object-level call.

## Decided (2026-09-16): the evaluator is an explicit machine

The C# evaluator is a loop over a heap-allocated stack of frames (a CEK-style
machine), not native recursion per object-level call and not trampolined CPS.
Depth is bounded by memory (and the budget), never by the 1 MB CLR stack; a
captured continuation is a slice of the frame stack, which is how effect handlers
(deep, one-shot, tunneling hop counts) are implemented. The elaborator may still
recurse natively over syntax (its depth is program-text depth, not run-time depth)
— revisit only if deeply nested source hits it.

## Decided (2026-09-16): conformance suite plus C# unit tests

- **Shared conformance suite** for language behaviour: each test is a `.fun`
  program plus an expected result (`.expect`). Both implementations have a small
  runner; the port is complete when C# passes every file OCaml passes.
  **Built 2026-09-16** at `test/conformance/cases/<area>/` (repo convention is
  `test/`, not `tests/`): 590 cases in `values`, `macros`, `imports` and
  `elaborate`, extracted from the Alcotest binaries. `<name>.expect` holds a
  value, a constructor name, `ok` (elaborates) or `error` (fails anywhere) —
  error wording is deliberately not pinned, since it is implementation-specific.
  Extra units are `<name>.unit-<unit>.fun`. Run with `dune test test/conformance`;
  format and conventions in `test/conformance/cases/README.md`. The OCaml runner
  is `test/conformance/run_conformance.ml` (~120 lines) — the C# port needs the
  same walk-and-compare.
  **The suite is the single source for language behaviour** (2026-09-16): the
  Alcotest copies are deleted (`test_core` 443 → 172 cases, `test_elaborate`
  500 → 269; 601 conformance cases unchanged). Each deleted case was matched to
  its twin by program text, or — for programs built by concatenation helpers — by
  provenance label plus fragment containment. What the port must therefore also
  cover, and what stays OCaml-only, is the internals list below.
- **C# unit tests (xUnit)** for implementation internals that are not observable
  as a program's result — enforester/syntax shapes, reflection round trips, NbE
  and unifier internals, budget accounting — mirroring the OCaml internal tests.

## Started (2026-09-16)

The user gave the word; the port is under way in `dotnet/`, in the same repo as
the prototype so the conformance suite stays one copy.

**Layout — three projects** (`dotnet/Fun.slnx`), not seven and not one:

```
src/Fun.Kernel/     Atom, ScopeSet, SourceSpan, TokenTree, Syntax, Core
src/Fun.Expand/     reader, enforester, expander     -> Kernel only
src/Fun.Compiler/   elaborator, unify, match, NbE, loader
src/Fun.Cli/
test/Fun.Tests/         xUnit, internals
test/Fun.Conformance/   walks ../../test/conformance/cases
```

The split enforces the one edge that is load-bearing: `Fun.Expand` cannot
reference the elaborator, exactly as `core_tt_expand` depends on only
`core_tt_kernel` and `core_tt_syntax`. `match`, `interp` and `loader` are leaves
off the kernel that only `typecheck` consumes, so they are folders, not projects.

**Decided (2026-09-16): the expander's callbacks become one injected
capability.** `Expand_ctx`'s `elaborate`, `eval_and_apply` and
`load_macros`/`load_syntax` are `mutable … option` fields installed after
construction, and a missing one is the runtime error `MissingCallback` - an
expander that silently compiles no macro and leaves every macro call unexpanded
(`core_loader.ml:26` comments on exactly that). The *recursion* they carry is
real and stays: expanding a `MacroDef` must compile and evaluate the macro body
before the next form is read, because that form may call it. What goes is the
optionality. The port declares one `IMacroRuntime` in `Fun.Expand`, implemented
in `Fun.Compiler`, taken non-nullable by the expander's constructor, so
`MissingCallback` is unrepresentable. The genuinely runtime-free pass
(`Parse_expand.syntax_exports`, which only reads a unit's exported roles) becomes
its own entry point rather than an expander with nulls in it.

**Decided (2026-09-16): porting order is a vertical slice.** Reader through
evaluator for a handful of conformance cases first, accepting rework as each
layer fills in, rather than finishing `Fun.Kernel` bottom-up with nothing
running until the end. The reason is the ticket's own risk list: the env-width
contract and the scope-set invariants are what gets silently mis-transcribed,
and only a running pipeline catches that early.

**Decided (2026-09-16): the reader is hand-written.** Researched against the
alternatives: ANTLR4 is the only real generator for .NET and needs a JDK at
build time; Pidgin, Superpower and Sprache are runtime combinators (slower than
hand-written, not faster); Hime is effectively unmaintained. Generated is not
faster for this token set - a `switch` on the first character beats ATN
simulation - and two thirds of `raw_syntax.ml` is the delimiter-group builder,
which no generator supplies. `#|…|#` also nests, which no regex can express. The
scanner uses `System.Buffers.SearchValues<char>` for its character classes, and
it never grows a rule: operators lex uniformly and the enforester assigns their
meaning.

**Slice 1 (done, 2026-09-16)** targeted the conformance cases that need no
prelude. The whole pipeline runs: reader, enforester (blocks read a statement at
a time), scope-set expander, bidirectional elaborator with metavariables and
readback, and the frame-stack evaluator. **10 of 601 cases pass**, each checked to
pass for a genuine reason; 35 xUnit cases.

Honesty rules the runner depends on, so a missing form never passes a case that
expects `error`:

- Unported forms raise `NotImplementedException`, which the runner counts as a
  failure; `FunException` is reserved for real language errors.
- A program read as an expression is elaborated inside `open (import "std")`, as
  the prototype's entry point does. A name the base context lacks there may be a
  `std` member, so it is "not ported", not "unbound". Checking the base before
  `std` gives the prototype's answer only while `std` rebinds no base-context
  name - true today (the base names it spells are `Syntax` members).
- While `std`'s syntax roles are unported, no enforest error is known to be
  genuine (the enforester reads every statement against roles first), so the
  Driver reports enforest errors as not ported. Reader errors stay real.

**Decided (2026-09-16): the domain model is the specification; the OCaml
prototype is supporting material.** A prototype defect gets a ticket and is fixed
**in the C# port only**; the prototype keeps it. The shared conformance suite
states the specified behaviour, so a case the prototype gets wrong keeps the
correct `.expect` and is listed in `test/conformance/prototype-divergences.txt`
with its ticket: the OCaml runner expects it to fail and reports it once it
passes; the .NET runner ignores the list.

Defects found while porting, each reproduced in the prototype before filing:

- [Solving a meta applied to a spine fails on a dependent right-hand side](meta-solution-renaming-not-lifted-under-binders.md)
  - `rename` never lifts its renaming under a binder. The port uses a partial
  renaming lifted at every binder.
- [Checking a lambda ignores its written parameter type](lambda-check-ignores-written-parameter-type.md)
  - `fn(x : Char) { x }` checks against `I64 -> I64`. The port unifies the written
  type with the expected domain; `elaborate/elab-049` diverges.

## Porting conventions (2026-09-16)

Every slice and every fork follows these. They are the port's working rules; the
reasons are in the decisions above.

1. **Spec.** The domain model (`docs/wayfinder/topics/core-tt-domain-model*.md`)
   and the glossary (`CONTEXT.md`) are definitive; the OCaml prototype is
   supporting material. Name types and members after the glossary (`Context`,
   `Environment`, `Width`, `Entry`, `Locate`, `Binding`, `Binder`, `Slot`, `Meta`),
   never after the prototype's abbreviations.
2. **Honest runner.** An unported form or path raises `NotImplementedException`
   ("not ported yet: …"); `FunException` is only for a genuine language error.
   A missing feature must never make a case expecting `error` pass. When a case
   starts passing, check it passes for the real reason.
3. **Machine.** The evaluator never recurses on the native stack per object-level
   call: a new term that needs a sub-evaluation gets a `Kont` frame. Readback,
   unification and the elaborator may recurse over structure.
4. **Slots (I2).** A binding contributes entries only through
   `BindingTerm.Slots()`, which both the elaborator and the evaluator consume.
5. **Prototype defects.** Reproduce in OCaml (a temporary conformance case), file
   a ticket, fix in C# only, add the shared case with the *correct* `.expect`, and
   list it in `test/conformance/prototype-divergences.txt`.
6. **Tests.** A source-to-result test is a conformance case, never xUnit; xUnit is
   for internals (shapes, unifier, machine, kernel).
7. **Layout for parallel work.** A feature's code goes in its own partial files
   (`Elaborator.<Feature>.cs`, `Nbe.<Feature>.cs`, `Syntax.<Feature>.cs`,
   `Core.<Feature>.cs`, `Enforest.<Feature>.cs`, `Expander.<Feature>.cs`); shared
   dispatch switches get one case line that calls into them.
8. **Green steps.** Commit in green steps: `dotnet build`, `dotnet test`, the
   conformance count never drops, and `dune test` stays green (OCaml code is not
   edited by the port).
9. **Undecided semantics.** Stop and report with a concrete example; never guess.
10. **Shared docs.** A fork does not edit `docs/STATUS.md` or this ticket; it
    reports, and the integrator records.

## Decided (2026-09-16): the prelude's source lives in `dotnet/std/`

The prelude (`std`) is `.fun` source the OCaml prototype holds as string literals
in `lib/semantic/typecheck/elab_prelude.ml`. The port keeps it as real files,
`dotnet/std/stage1.fun` and `dotnet/std/stage2.fun`, copied verbatim (user
decision); the OCaml is left as it is. The OCaml prototype is **not maintained
once the port is done**, so the two copies are not kept in step long-term.

## Wave 1 (2026-09-16) — merged

All five merged the same day: imports, implicit parameters, structs/records/
signatures, recursive definitions, match and enums. C# conformance 21/604 →
89/618; xUnit 44 → 73; OCaml 618 cases, 0 failed, 3 known divergences. Each ticket
records what landed and its follow-ups. Open for the user: how a bare constructor
pattern resolves ([match and enums](port-match-and-enums.md)).


Forked in parallel from `b894c12`+1, one worktree each:
[structs, records, signatures](port-structs-records-signatures.md),
[match and enums](port-match-and-enums.md),
[implicit parameters](port-implicit-parameters.md),
[recursive definitions](port-recursive-definitions.md),
[imports](port-imports.md). Held: effects (need `match`), refs (need effect
rows), macros and syntax roles (need the prelude), recursive enums (need both
match-enums and recursive definitions).

## Wave 2 (2026-09-16) — merged

Export, traits, recursive types, patterns (synonyms included), syntactic roles and
effects merged. C# 91/621 → 206/650; xUnit 73 → 90; OCaml 650 cases, 0 failed,
9 known divergences. Each ticket records its merge and what remains.


Forked in parallel from main, one worktree each:
[recursive types](port-recursive-types.md), [patterns](port-patterns.md),
[effects](port-effects.md), [syntactic roles](port-syntax-roles.md),
[traits](port-traits.md), [export](port-export.md). Held for wave 3: the prelude
(needs all six), procedural macros (need the prelude's `Syntax` module), refs
(need effect rows). Effects adds rows to arrows as an optional, pure-by-default
member so no other fork's arrow construction changes.

## Wave 3 (2026-09-16 – 2026-09-17) — merged, paused

**At most two implementation forks run at once** (user, 2026-09-16); the rest
queue. Running: [primitives](port-primitives.md) and
[the macro runtime interface](port-macro-runtime-interface.md), both prerequisites
of the prelude. Primitives merged (`5f16665`); the macro runtime interface merged
(`107a91c`); refs merged (`4fd4b6a`); [effects follow-ups](port-effects-followups.md)
and [follow-up verification](port-followup-verification.md) merged. [Prelude stage 1](port-prelude-stage1.md)
and the verification rulings merged. A method's `~>` row, impl resolution by argument and procedural macros merged.
[Unit interleaving and operator macros](port-unit-interleaving.md) merged
(`stage2.fun` compiles). **Paused by the user** after this wave. Next: prelude stage 2
(bind `stage2.fun` as the prelude). The deferred cleanups (the `Syntax.AddScope` / `Syntax.Map` duplicate)
wait for a quiet moment with no fork editing `Syntax.cs`.

## Readiness (2026-09-16)

**The prototype is ready to be ported.** Recorded here so a later session can
check the claim rather than re-derive it.

| Gate | State |
|---|---|
| Blockers (19 listed in `blocked_by`) | all closed |
| Model | four domain-model passes written, "today" sections refreshed 2026-09-15 |
| Conformance suite | `test/conformance`, 601 cases, the single source of truth for language behaviour (the Alcotest duplicates were deleted 2026-09-16) |
| Internal tests | 613 Alcotest cases (10 + 162 + 172 + 269) covering shapes, reflection, budget, driver, exact error constructors |
| Stability signal | the 2026-09-16 rounds landed without reopening a decision; every fork that met an undecided rule stopped and asked |
| Port decisions | C#; evaluator an explicit frame-stack machine; conformance suite + xUnit internals tests |

**How to verify:** `dune test` (all Alcotest binaries) and `dune test
test/conformance` (601 cases, 0 failed) on `main`.

**Known, deliberately not blocking:**
- `method-signature-metas-capture-self.md` — an inserted meta in a method's
  parameter type captures `self` (live defect, small).
- Performance research: deep non-tail recursion, type-case refinement.
- Parked design: general set literals, deriving/protocols, private types.
- Diagnostics polish and error spans — deferred to post-port by decision.

**Not to be started without the user's word** (see the section above).

## Port environment decisions (2026-09-16)

- **Latest .NET** (whatever is current when the port starts), latest C# language
  version.
- **Use the framework's immutable collections** (`System.Collections.Immutable`)
  wherever they fit; hand-roll a persistent structure only where a measured hot
  path (scope sets, contexts) demands it, and say so in the code.
- **Test framework: xUnit** (grilled 2026-09-16 against NUnit, MSTest, TUnit):
  biggest ecosystem and tooling in modern .NET, and it carries only the internals
  tests since the 601 conformance cases run from their own walker. Async tests
  (`async Task`) are supported if the loader or a language server later needs them;
  the elaborator and the frame-stack evaluator stay synchronous.
- **Nullable reference types: on, with nullable warnings as errors**
  (`<Nullable>enable</Nullable>` plus the CS86xx warnings escalated). OCaml's
  `option` maps to `T?` and the compiler enforces the checks the OCaml `match`
  enforces today. Considered and rejected: an explicit `Option<T>` type (most
  faithful, but verbose and unidiomatic C#); nullable off (every dropped `None`
  becomes a runtime `NullReferenceException`). `default!` and `!` are escape
  hatches to avoid — if one is unavoidable, comment why.

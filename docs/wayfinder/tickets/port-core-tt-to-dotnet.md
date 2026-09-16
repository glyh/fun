---
title: Port core_tt to .NET (F#/C#)
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
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

# Port `core_tt` to .NET (F#/C#)

**Blockers (2026-09-16): none open.** Every ticket in `blocked_by` is closed; E11
(nominal identity) was the last. What remains before starting is the stability
signal below and the two decisions taken at port start.

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

**Slice 1 (in progress)** targets the three conformance cases that need no
prelude: `values/core-001` (`42`), `core-002` (`(fn(x) { x })(7)`) and
`core-004` (`{ x : I64 = 5; x }`). Landed: the scaffold, `SourceSpan`,
`ScopeSet`, `Atom`/`AtomTy`, `TokenTree` and the reader, with 15 xUnit cases.
The conformance runner walks the same 601 files the OCaml runner does and
reports 601 failures - that number reaching 0 is the port.

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

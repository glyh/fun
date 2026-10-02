# Types and semantics — what a program's meaning is checked to be

Ideas about how a language decides that a program means what the programmer wrote: inference
architecture (Hindley–Milner, bidirectional, local, principal types, error- and cost-driven
design), polymorphism (parametric, ad-hoc, rows, higher-rank, type classes/traits/dictionaries),
subtyping and its alternatives (nominal vs structural, algebraic subtyping, refinements,
gradual, contracts), data modelling (sums vs products, records, null vs option, mutability as a
type-level property), equality and identity, and semantics (laziness, purity, divergence, how
much to specify). Out of boundary: effect systems and handlers, which live in
`effects-and-handlers.md` — effect rows appear here only as the type-level device that serves
them; and memory/ownership as a runtime mechanism, which lives in `runtime-and-memory.md` —
linearity, affine types and lifetimes appear here as *type-system ideas* (entry 28).

## How this was gathered

The corpus is 2538 threads from r/ProgrammingLanguages and r/Compilers collected through
Reddit's own JSON endpoints, dated 2009-01 to 2026-09. The types slice holds 260 threads sorted
by comment-count × 2 + score × 0.5 + body length; beyond it I ran keyword searches over
`corpus.jsonl` for the topics the slice under-serves (laziness, coherence, value restriction,
uniqueness, graded/ordered types, set-theoretic types). **Comment coverage**: comment trees were
fetched for the 20 richest threads of this axis, yielding 520 comments (26 per thread), and the
entries below fold in what those trees contained. The fetch is shallow by construction — as the
slice's own preamble records, a reply with `limit=100` still returns only ~30 top-level
comments, the remainder sitting in unretrieved `more` placeholders (0 to 13 recorded per thread,
against fetched trees listed at 46 to 98 comments) — so a retrieved tree is its top slice, not
the debate in full. Of the bodyless link posts, "Why You Need Subtyping" (71) was among the
fetched and its argument is recovered from comments below; "HM vs Bidirectional" (94),
"Designing inference for high quality type errors" (72), "Traits are a Local Maximum" (63) and
"Don Syme on type classes" (60) were not, so for those I can cite a score but not an argument.
39 of the 260 slice threads are link posts with no body at all, and those are
disproportionately the highest-scoring ones. And this is what Reddit upvoted, not a survey of
the field: popular ≠ correct, several high scores are self-promotion for hobby languages whose
design claims are unverified, and those are cited below only as evidence that an idea is being
tried.

## The ideas

### Bidirectional elaboration as the default checking architecture

**What it is.** Two mutually recursive modes: `check` verifies a term against an expected type,
`infer` synthesizes one. Every form is assigned to whichever mode needs less guessing —
lambdas are checked, applications infer — and the switch points are the design. Annotations
become the places where inference is *seeded* rather than places it is *required*.

**Buys.** A local, single-pass checker with no global constraint solve, precise error positions
("this lambda body does not match the annotated result"), and a natural home for implicit
argument insertion and for elaboration that produces a core term as a side effect.

**Costs.** You give up principal types and inference completeness: what gets inferred depends on
which mode the form landed in, so a term can fail to check that a more global algorithm would
accept. The switch points have to be chosen and then defended. The comment tree of the
full-inference thread turns the annotation side into a positive: annotations are "a conversation
between you and the compiler", and with full inference there is "no longer a conversation, just
the compiler trying to figure out if there's any way to assign the types" — while an OCaml
practitioner counters that full inference type-checks "a fraction of a second per file" and
simply does not cause slowdowns (top comments on "Thoughts about static, but fully inferred,
typing", https://www.reddit.com/r/ProgrammingLanguages/comments/14t96qf/thoughts_about_static_but_fully_inferred_typing/).

**Maturity.** `shipped` — Agda, Idris, Lean, Elm and Rust-family checkers all sit on some
version of it.

**Tried by.** Agda, Idris, Lean, Elm; the tutorial ecosystem has grown large enough that people
write tutorials complaining there are already enough.

**Source.** How to Choose Between Hindley-Milner and Bidirectional Typing, score 94, 24
comments, 2026-02 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1r5ldxp/how_to_choose_between_hindleymilner_and/
· The appeal of bidirectional typechecking, score 78, 47 comments, 2022-06 —
https://www.reddit.com/r/ProgrammingLanguages/comments/v3z7r8/the_appeal_of_bidirectional_typechecking/
· I wrote a bidirectional type inference tutorial using Rust because there aren't enough
resources explaining it, score 99, 13 comments, 2025-12 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1pzjqjb/i_wrote_a_bidirectional_type_inference_tutorial/

**Bearing on `fun`.** Already has it, decided: `Elaborator` runs `infer` and `check` over
`Syntax.t` with higher-order metas and Miller-pattern unification
(`docs/wayfinder/topics/dependent-types.md`). The interesting open residue is not the mode
split but where the switch points sit — tickets `check-against-implicit-type-inserts-first`,
`lambda-check-ignores-written-parameter-type` and `implicit-lambda-rigid-or-instantiate` are all
about a single switch point being wrong.

### Local type inference: push the expectation down, pull the synthesis up

**What it is.** No global constraint generation. A `let` with an annotation sends the type down
into the initialiser; a `let` without one pulls a type up from the initialiser's head. Rust,
OCaml and C# all work this way, and the interesting cases are the ones where neither direction
has an answer:

```rust
let vec: Vec<i32> = Vec::new();   // pushed down
let num = computeNum();           // pulled up
let num: i16 = 10 + 12;           // pushed down, both literals become i16
```

**Buys.** Linear-time, predictable checking with a trivially explainable failure ("add an
annotation here"), and no constraint solver to maintain.

**Costs.** Annotation burden moves to exactly the generic-heavy code people write most, and
inference becomes direction-dependent: a refactor that changes which side is known can break a
program that used to check.

**Maturity.** `shipped` — OCaml, Rust, Swift, C#, Kotlin.

**Tried by.** OCaml (local inference over function bodies), Rust, Swift, C#.

**Source.** How to implement local type inference?, score 16, 26 comments, 2024-11 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1gkjkz4/how_to_implement_local_type_inference/
· Type Inference in Rust and C++, score 55, 18 comments, 2025-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1i6j043/type_inference_in_rust_and_c/

**Bearing on `fun`.** Has it differently: the push half is check mode against an expected type,
the pull half is inference plus implicit insertion, and unsolved metas at a `let` are
generalised rather than rejected — `port-generalise-under-check` is merged. `fun` goes further
than local inference in one direction (metas may be solved by *evaluating* the term, within the
evaluation budget) and no further in the other.

### Principal types as an invariant — and the places it dies

**What it is.** Every well-typed term has one *most general* type, and any other type is an
instance of it; inference computes that one and users may omit annotations entirely. The
"beyond HM but keep this" school treats principality as the property worth defending and stacks
extensions that preserve it: Abelian-unifying types (units of measure), qualified types,
scoped-label extensible records, row-polymorphic effects.

**Buys.** Refactoring safety with no annotations, and a compiler answer ("here is *the* type")
that is never arbitrary.

**Costs.** Principality is the first casualty of subtyping, overloading and refinement: with a
subtype relation there is no unique most-general type, and with user predicates the "principal"
type can be one nobody would write. One resource-post author reports a richer-inference
"pathological" failure mode of his own — weird code becomes well-typed, or simple code gets
weird types — and notes Dolan himself admits algebraic subtyping's principal types can be too
forgiving. The full-inference thread adds the ceiling from the other direction: the author of
CubiML states there is "a fundamental limitation in the power of the type system that can be
fully inferred" — compilers can always do more static analysis "if they're allowed to request
more help from the programmer", and no Rust-like language with full type inference exists — and
an OCaml practitioner adds that inference without annotations makes the compiler "start from the
assumption that there are no mistakes", so it catches inconsistency but not mistakes (top
comments on "Thoughts about static, but fully inferred, typing",
https://www.reddit.com/r/ProgrammingLanguages/comments/14t96qf/thoughts_about_static_but_fully_inferred_typing/).

**Maturity.** `contested`. The evidence that principality is achievable and deployable is on
the keep-it side: the whole ML family ships it. The evidence that it survives the extensions
people actually want is on the other side: subtype inference exists as implementations (CubiML,
PolySubML, 1SubML) but none is deployed at scale, and the corpus's own resource post hedges
that it is "95% sure" the pieces compose.

**Tried by.** OCaml, Haskell, Elm, Standard ML (shipped); CubiML, PolySubML, 1SubML (research
implementations).

**Source.** Beyond Hindley-Milner (but Keeping Principal Types), score 86, 25 comments,
2020-08 —
https://www.reddit.com/r/ProgrammingLanguages/comments/ijij9o/beyond_hindleymilner_but_keeping_principal_types/
· Subtype Inference by Example Part 1: Introducing CubiML, score 31, 26 comments, 2020-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/hl6bq0/subtype_inference_by_example_part_1_introducing/
· PolySubML: A simple ML-like language with subtyping, polymorphism, higher rank types, and
global type inference, score 55, 27 comments, 2025-02 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1ijmxbc/polysubml_a_simple_mllike_language_with_subtyping/
· Thoughts about static, but fully inferred, typing, score 24, 46 comments, 2023-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/14t96qf/thoughts_about_static_but_fully_inferred_typing/

**Bearing on `fun`.** Not claimed, not ticketed: `fun` has no subtyping relation, so its only
equality is NbE convertibility, and principality has nothing to fight with. What stands in the
slot is meta solving — Miller-pattern spines, generalisation of unsolved metas at a `let` — and
no wayfinder ticket asserts that the resulting types are principal. If one ever did, type-case
on the open `Type` and `Type : Type` are the two things it would have to argue past.

### Designing inference backwards from its errors and its compile time

**What it is.** Treat the quality of a reported failure, and the wall-clock cost of checking, as
requirements of the algorithm rather than as polish applied afterwards. Concretely: prefer the
algorithm whose failure names the *written* thing that is wrong; measure what the algorithm
costs on realistic code and pick accordingly.

**Buys.** Type errors are the language's main user interface; an algorithm chosen for them
produces "expected `I64`, found `Bool` at the annotation" instead of a trail of internal
constraints. Cost-driven choice keeps a whole class of "the compiler is slow on my generics"
bugs from existing at all.

**Costs.** Both requirements pull against expressiveness: the diagnostics-friendly algorithm is
usually the less powerful one, and the cheap one gives up inference. There is no settled answer
— the corpus contains an active research survey asking constraint-based-inference authors how
to do it, a thread on displaying errors under *global* inference (a hard case), and a separate
thread arguing inference itself makes Swift slow.

**Maturity.** `contested`. The side with better evidence: local/bidirectional algorithms win on
both measured axes (predictable cost, localisable blame), and the thread explicitly blaming
Swift's slowness on inference is about a global algorithm — but global-inference languages
(OCaml, Haskell) have shipped for decades, so the disagreement is about degree, not viability.

**Tried by.** Swift, OCaml, GHC (all ship some compromise); the survey itself is a research
instrument with participants recruited on that subreddit.

**Source.** Designing type inference for high quality type errors, score 72, 12 comments,
2025-02 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1ipiams/designing_type_inference_for_high_quality_type/
· Help us improve type error messages for constraint-based type inference by taking this
15–25min research survey, score 44, 22 comments, 2023-03 —
https://www.reddit.com/r/ProgrammingLanguages/comments/11ymq2f/help_us_improve_type_error_messages_for/
· Strategies for displaying type errors with global type inference, score 27, 7 comments,
2020-08 —
https://www.reddit.com/r/ProgrammingLanguages/comments/i2hfti/strategies_for_displaying_type_errors_with_global/
· The Swift compiler is slow due to how types are inferred, score 69, 27 comments, 2024-06 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1dewmbu/the_swift_compiler_is_slow_due_to_how_types_are/

**Bearing on `fun`.** Open and named: the map's **Diagnostics polish boundary** fog item is
exactly this trade, and it was re-measured 2026-10-01 — `Budget._site` already records the form
and `Budget.Where()` prints it, so the cheap improvement is one line whose real cost is nine
exact-`Message` xUnit assertions. The ticket `budget-error-names-no-source-call` is the
error-message half; the cost half is the **evaluation budget** the checker shares with the
evaluator.

### Generalisation at `let`, and the value restriction that patches it

**What it is.** A `let`-bound definition with unsolved type variables becomes polymorphic —
that is what makes `id x = x` usable at every type. In the presence of mutation that is
unsound, so the value restriction (or an effect-aware variant) says: only *values* generalise.

```ocaml
let r = ref []    (* value restriction: stays (unit list ref), not 'a list ref *)
```

**Buys.** Let-polymorphism, the single feature that makes ML-style code annotation-free in
practice, plus a one-line rule that repairs the type-and-effect hole.

**Costs.** The restriction produces exactly the annoying failures where a definition is
perfectly reasonable but stays monomorphic until someone eta-expands it; and which syntactic
class counts as a value is an arbitrary-looking rule users must learn. Inference quality then
depends on syntactic shape.

**Maturity.** `shipped` — OCaml's value restriction, Rust/C# equivalents, and a steady trickle
of papers arguing for a better rule.

**Tried by.** OCaml, SML, Java, C#, Rust.

**Source.** Let should not be generalised during type inference, score 63, 14 comments,
2020-12 —
https://www.reddit.com/r/ProgrammingLanguages/comments/k4gkxc/let_should_not_be_generalised_during_type/
· Subtype Inference by Example Part 11: The Value Restriction and Polymorphic Recursion, score
26, 3 comments, 2020-09 —
https://www.reddit.com/r/ProgrammingLanguages/comments/ivu7oh/subtype_inference_by_example_part_11_the_value/
· Value Restriction and Generalization in Imperative Language, score 13, 6 comments, 2025-12 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1pfpbks/value_restriction_and_generalization_in/

**Bearing on `fun`.** Has it differently, and better placed: generalisation does not need a
syntactic value restriction because soundness is carried by the **effect row**, not by shape. A
definition whose row names `Alloc(h)` for a heap `h` that does not occur in its type is
**discharged** to a pure signature (`Discharge`), which is `runST`'s condition met by inference
instead of a wrapper. The generalisation-under-check path is merged
(`port-generalise-under-check`); the residual risk sits in tickets that mention vars and bounds
being dropped across re-evaluation.

### Row polymorphism for records — and the recursion it chases

**What it is.** A record type is a set of labels plus a *row variable* standing for "whatever
else is there", so a function can require a field without requiring the absence of others:

```text
get_name : { name : String | r } -> String
```

Unification extends or matches rows instead of comparing closed field lists.

**Buys.** Polymorphic field access and type-safe builders without wrapping, and — the reason
people keep reaching for it — extensible variants come out of the same unification change for
free.

**Costs.** It is a unification change, so it touches the algorithm everywhere; recursive types
interact badly (a `has-field` constraint on a recursive record makes naive unification chase
its own tail, as one poster discovered mid-implementation); and row variables leak into error
messages as noise.

**Maturity.** `shipped` in Elm, PureScript, Koka's effect rows, and OCaml's polymorphic
variants; the recursive-types corner is `research`.

**Tried by.** Elm, PureScript, Koka, OCaml, TypeScript (structurally, without the row
variable).

**Source.** Row Polymorphism without the Jargon, score 38, 35 comments, 2020-04 —
https://www.reddit.com/r/ProgrammingLanguages/comments/g2lm11/row_polymorphism_without_the_jargon/
· Type Safe Builders with Row Polymorphism, score 23, 12 comments, 2019-04 —
https://www.reddit.com/r/ProgrammingLanguages/comments/bhz8vg/type_safe_builders_with_row_polymorphism/
· Type inference for recursive types in row polymorphism, score 42, 9 comments, 2023-02 —
https://www.reddit.com/r/ProgrammingLanguages/comments/115c1ya/type_inference_for_recursive_types_in_row/
· Adding row polymorphism to Damas-Hindley-Milner, score 48, 5 comments, 2024-10 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1gab4p6/adding_row_polymorphism_to_damashindleymilner/

**Bearing on `fun`.** Has it half-way on purpose: `fun`'s rows are real and row-polymorphic
where they serve effects (`can {IO | r}`, `can _`), but record types are *declared* structural
`struct`s with named fields, not open rows — width only appears where a pattern opts in with
`struct { a : p; _ }` (`pattern-headed-impls`). The recursion trap is live here too:
`recursive-record-field-of-own-type-rejected` and `recursive-records-cannot-hold-a-record` are
the two tickets the same corner produced.

### Higher-rank polymorphism through bidirectional checking

**What it is.** Quantifiers to the left of an arrow — a function that *takes* a polymorphic
argument — inferred where HM would give up and checked where an annotation exists. The standard
construction is Dunfield–Krishnaswami: check against `∀`, instantiate when checking, and let a
*unification* variable stand for a type when neither side is known.

**Buys.** Encoding pass-through functions, church-encoded data and evaluator boundaries without
a wrapper combinator; and it is the same machinery that handles implicit arguments and
`forall`-in-data.

**Costs.** Inference is no longer complete: un-annotated higher-rank code frequently fails, so
annotations move inward to argument positions. It also opens the door to programs whose
termination the checker cannot decide — one thread asks precisely how a checker avoids looping
on `foo(x) = foo([x])` when inferring its return type.

**Maturity.** `shipped` — GHC's `RankNTypes`, and a family of readable implementations exists
precisely because people keep porting the paper.

**Tried by.** GHC, Agda, Lean, F#, and every language that adopted the Dunfield–Krishnaswami
algorithm.

**Source.** Bidirectional typing with unification for higher-rank polymorphism, score 36, 10
comments, 2025-05 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1ky385w/bidirectional_typing_with_unification_for/
· Readable Rust implementation of "Complete and Easy Bidirectional Typechecking for Higher-Rank
Polymorphism", score 34, 6 comments, 2019-04 —
https://www.reddit.com/r/ProgrammingLanguages/comments/b8w70f/readable_rust_implementation_of_complete_and_easy/
· How do type checkers deal with functions like this?, score 58, 29 comments, 2021-10 —
https://www.reddit.com/r/ProgrammingLanguages/comments/q6jyx6/how_do_type_checkers_deal_with_functions_like_this/

**Bearing on `fun`.** Already has it, past where HM languages stop: a `Pi` is a `Pi`, implicit
Pi domains are inserted in both modes, and rank is not a special case because the core is
dependently typed. The divergence question above is answered by the **evaluation budget** —
a call the checker spends budget on fails loudly with the call's name instead of looping.

### Ad-hoc polymorphism: contested worth, not contested mechanism

**What it is.** One name, many implementations, chosen by type — type classes, traits,
protocols, overloads, multimethods. The mechanism is settled; whether it pays is not. The
sharpest statement in the corpus lists six costs: compile-time blow-up, LSP lag, worse
documentation discoverability, cryptic errors, weakened HM inference (annotations appear), and
the mental overhead of remembering which implementation ran.

**Buys.** Generic code that can behave differently per type without a wrapper, and a library
vocabulary (`Eq`, `Ord`, `Show`) that reads the same everywhere.

**Costs.** The list above, plus instance resolution becoming a second, implicit program that
readers cannot see. The counter-cost of *omitting* it is verbosity: without ad-hoc polymorphism
every operation needs its own name or its own module argument — and the Futhark maintainer's
counter-example to the module escape hatch is that an ML-style module system is "good for
polymorphism-in-the-large, but very awkward for smaller functions". The comment tree contests two
of the six costs directly: HM inference is *already* worst-case exponential (and those cases
"essentially never occur in real code"), and Haskell-98-scale type classes were fast enough on
1990s hardware (Hugs), so the blow-up is attributed to the extensions rather than to ad-hoc
polymorphism; the documentation claim is answered with usage counts — the post's claim of
rarely using `+` ("I don't think I've used it at all in my compiler codebase") drew replies
counting ~1100 uses in the C
implementation of Lua, ~2800 in SQLite3 and ~750 in one self-hosted compiler (comments on
"Ad-hoc polymorphism is not worth it",
https://www.reddit.com/r/ProgrammingLanguages/comments/1hg4r9v/adhoc_polymorphism_is_not_worth_it/).

**Maturity.** `contested`, and the comment round sharpened both sides. The "not worth it" side
still has Swift/Haskell compile-time anecdotes and a 61-comment thread, plus a commenter's
reframing that ad-hoc polymorphism is a *spectrum* — undecidable instances, overlapping
instances, multi-parameter classes and functional dependencies are separate bets, and
single-parameter Haskell-98 classes are "perhaps the best balance". The "worth it" side has
Rust, Swift, Haskell, Scala, Java (adding it now), F#'s *rejection* of type classes written up by
its designer as a deliberate cost-benefit call, and a point-by-point rebuttal of the cost list
from the maintainer of Futhark, which deliberately has no ad-hoc polymorphism. On this corpus's
evidence the compile-time charge is the weakest of the six (rebutted, and unrebutted in turn);
the documentation-discoverability and "which impl ran?" charges remain unresolved, and the
thread's original author was answered into conceding he had been "implicitly referring to
multiple dispatches AHP" rather than type classes.

**Tried by.** Haskell, Rust, Swift, Scala, Kotlin, Java (integrating), and F# (declined);
Futhark on the other side — no ad-hoc polymorphism, ML modules instead, with the awkwardness
its maintainer reports above.

**Source.** Ad-hoc polymorphism is not worth it, score 56, 61 comments, 2024-12 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1hg4r9v/adhoc_polymorphism_is_not_worth_it/
· Traits are a Local Maxima, score 63, 13 comments, 2024-11 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1guatuo/traits_are_a_local_maxima/
· Don Syme explains the downsides of type classes and the technical and philosophical reasons
for not implementing them in F#, score 60, 14 comments, 2021-09 —
https://www.reddit.com/r/ProgrammingLanguages/comments/placo6/don_syme_explains_the_downsides_of_type_classes/
· Generics vs Traits - What are the differences and similarities?, score 30, 22 comments,
2021-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/oq9sql/generics_vs_traits_what_are_the_differences_and/
· Why use a struct plus traits instead of objects?, score 35, 70 comments, 2021-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/oltkwt/why_use_a_struct_plus_traits_instead_of_objects/

**Bearing on `fun`.** Has it, decided, with the costs priced in: nominal `trait`/`impl`,
structural dictionary evidence passed hidden, bounds written `[A : Eq + Jsonable]`. The
diagnostic cost of ad-hoc polymorphism lands on the same **evaluation budget** as everything
else the checker runs, and the "which impl ran?" cost is answered by *naming* the resolution
rule: most-precise-impl-wins, lexical nearness never breaks a tie.

### Evidence as hidden dictionaries vs search-time resolution

**What it is.** Two ways to make ad-hoc polymorphism run. **Dictionary passing**: a bound
`[A : Eq]` elaborates to a hidden parameter holding the operations, and calls project from it —
uniform, inspectable, erasable. **Search-time resolution**: at each call the compiler looks the
instance up in an index (tries, tables, a global registry) and inlines whichever it finds — no
runtime parameter, but lookup cost and a resolution rule to specify.

```text
same : [A : Eq] -> A -> A -> Bool
-- dictionary:  same = fn[A](eq_dict, x, y) -> eq_dict.eq(x, y)
```

**Buys.** Dictionaries make generic code a plain function again (specialisation and erasure are
then optimizations), and they make evidence nameable. Search makes call sites look free and
moves the cost to compile time.

**Costs.** Dictionaries cost a hidden argument and a dependent-closure allocation at the call;
search costs an index, a resolution rule, and error messages that must explain a failed lookup
through a chain of candidates.

**Maturity.** `shipped` — GHC's dictionary passing and Swift's witnesses are the dictionary
side; the trie-index idea in the corpus is `research` at best (a hobby implementation, evidence
of interest, not of viability).

**Tried by.** GHC, Swift, Scala 3, Lean, Rust, Java (its integration plan is dictionary-shaped).

**Source.** Blazingly Fast™ Type Class Resolution with Tries, score 67, 15 comments, 2024-02 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1auxy31/blazingly_fast_type_class_resolution_with_tries/
· How Java plans to integrate "type classes" for language extension, score 72, 39 comments,
2025-08 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1mwj302/how_java_plans_to_integrate_type_classes_for/
· How does one determine what instances a polymorphic function needs?, score 29, 13 comments,
2023-04 —
https://www.reddit.com/r/ProgrammingLanguages/comments/12jfq6s/how_does_one_determine_what_instances_a/

**Bearing on `fun`.** Chose the dictionary side explicitly: trait evidence is *not* a
user-facing value, dictionaries are passed hidden, and specialisation/erasure are deferred as
optimizations (`docs/wayfinder/topics/traits.md`). Resolution is scoped search over in-scope
impls with the most-precise one winning, and while the use's argument types are still unsolved
metas the choice *waits* rather than guessing. `design-trait-library-deriving-and-protocols`
and `impl-head-written-bound` are the open edges.

### Modules as the other spelling of type classes

**What it is.** The thesis that ML modules (functors, signatures, opaque ascription) and type
classes are the same abstraction seen twice, so a language needs one. A module with a type
parameter *is* an instance; a constrained function *is* a functor argument; associated types
come free from the signature.

```text
form Sort A { def sort :: [A] -> [A] }
impl Quicksort A :: Sort A = { def sort = ... }
```

**Buys.** One concept instead of two: instance packaging, abstraction and namespacing all use
the same struct, and you get associated types, constants and nested modules without a second
mechanism.

**Costs.** Functors are explicit and verbose where type classes are implicit; unifying them
usually means either making modules implicit (resolution problem) or making type classes
explicit (ergonomics problem). The corpus's own asker stops at the hard part: how a function
becomes generic over a *form* while still looking constraint-shaped. The comment round supplies
a practitioner's yardstick: the maintainer of Futhark, whose language does all its
polymorphism-in-the-large with an ML-style module system (a module implementing the module
type `field`, a `mk_linalg` parameterised module), reports it is "very awkward for smaller
functions" and hopes to steal OCaml's modular implicits when they are done (comment on
"Ad-hoc polymorphism is not worth it",
https://www.reddit.com/r/ProgrammingLanguages/comments/1hg4r9v/adhoc_polymorphism_is_not_worth_it/).

**Maturity.** `research` — modular implicits (OCaml) and the mtc paper are the nearest
implementations; no production language has unified the two. Demand is not what is missing: a
commenter reports OCaml's modular-implicits proposal is so popular it has to be excluded from
community surveys to get variation ("don't write this in the box"), and is unimplemented only
because it is hard, effects take priority, and the developers insist on doing it properly
(comment on "Ad-hoc polymorphism is not worth it", permalink above).

**Tried by.** OCaml modular implicits, Scala (partial), Haskell's `newtype deriving` route; the
thread's own "forms" exist only in the post.

**Source.** Unifying typeclasses and modules, score 40, 31 comments, 2020-10 —
https://www.reddit.com/r/ProgrammingLanguages/comments/j7ypnq/unifying_typeclasses_and_modules/
· Are OCaml modules basically compile-time records?, score 18, 13 comments, 2024-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1e6d88x/are_ocaml_modules_basically_compiletime_records/
· Modules: Overcoming Stockholm and Dunning-Kruger, score 84, 44 comments, 2022-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/vqx19e/modules_overcoming_stockholm_and_duningkruger/

**Bearing on `fun`.** Already lives there: one `struct` is record, module and namespace;
modules are first-class values; impls arrive through `open` (OCaml's modular implicits, not
Rust's global registry); a struct's fields and its bindings are one binding list. The gap is
naming — `impl-visibility` records that scoped resolution with *no way to name an impl* is the
one combination no precedent uses, and it plans `same[A.C, A.eq_C]` as the compile-time handle.

### Types as values: generics vs type-level metaprogramming

**What it is.** Two routes to "a function over types". Generics: a `forall` the checker
instantiates and possibly erases. Metaprogramming: a function *returning a type*, evaluated at
compile time, whose output is a fresh type — Zig `comptime`, TypeScript computed types, C++
templates.

**Buys.** Type-level functions are total functions you already know how to write, with no new
typing rule; conditional types, mapped types and derivation all fall out. The corpus's
questioner notes the "principled" feel of generics but no type-theoretical wall between them.

**Costs.** Metaprogrammed types are generative (each call a new nominal), so they resist
nominal identity and caching; they are hard to analyse statically, so error messages and
tooling must show generated structure; and inference cannot look inside a computation it has
not run.

**Maturity.** `shipped` — Zig, TypeScript, C++ templates, Rust `const fn` returning types.

**Tried by.** Zig, TypeScript, C++, Rust, D; and TypeScript's type system has been driven far
enough to run an assembly interpreter inside it.

**Source.** Is there a type-theoretical difference between generics and compile-time
metaprogramming?, score 44, 33 comments, 2023-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/14xmlma/is_there_a_typetheoretical_difference_between/
· Assembly interpreter inside of TypeScript's type system, score 118, 15 comments, 2022-11 —
https://www.reddit.com/r/ProgrammingLanguages/comments/yww51r/assembly_interpreter_inside_of_typescripts_type/

**Bearing on `fun`.** Took the "types are values" side from the start: `Type : Type`, `type` is
a prelude macro rather than a reserved form, **one grammar for types** means annotations are
read with the expression grammar so a user type operator like `~>` works everywhere, and
type-case on the open `Type` is accepted as a design pillar. What `fun` does *not* take from
the metaprogramming side is generativity by accident — a nominal declared under a run-time
effect is generative deliberately, decided by purity in the effect row.

### Nominal vs structural type identity

**What it is.** Two types are the same when their *names* agree (nominal: Rust, Haskell,
Java) or when their *shapes* agree (structural: TypeScript, Go's fields-by-name, OCaml object
types). The choice also decides who may add a case: structural identity makes extension
trivial and invariance hard; nominal identity makes extension deliberate and lets you hang
extra semantics on structurally identical shapes.

**Buys.** Nominal: marker semantics, safe evolution of a shape, and error messages that name a
concept the programmer wrote. Structural: interoperability and no boilerplate declarations —
the whole reason TypeScript could be imposed on an existing language.

**Costs.** Nominal demands a declaration per concept and produces nominal-vs-structural
convertibility errors that look pedantic. Structural produces accidental conformance (two
unrelated concepts that happen to have the same shape become interchangeable) and, as one
TypeScript practitioner in the corpus puts it, a lack of invariance that "really got in the
way".

**Maturity.** `contested` — every large language picks one and the argument never resolves; the
corpus has a dedicated "Structural and/or nominal?" thread arguing *against* adding structural
types, a 66-comment structural-typing thread arguing for, and a 73-comment "Why You Need
Subtyping" thread on the structural side. The comment trees of the first two were not fetched;
the subtyping thread's comments supply both positions in miniature — a set-based camp (ArkType's
`type("number > 0").extends("number")`, redundant information normalized away as a matter of
taste) against a declared-subtype camp that would allow unions and intersections only between
subtypes of the same ML type, because `Nullable<T>` collapsing to `T` lets a user probe an
abstract type's representation (comments on "Why You Need Subtyping", permalink in Source).

**Tried by.** Nominal: Rust, Haskell, Java, Swift. Structural: TypeScript, Go, OCaml
(records/objects), Dart.

**Source.** Structural and/or nominal?, score 39, 28 comments, 2021-03 —
https://www.reddit.com/r/ProgrammingLanguages/comments/lzyjma/structural_andor_nominal/
· Structural typing, score 25, 66 comments, 2019-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/ai1kmw/structural_typing/
· Why You Need Subtyping, score 71, 73 comments, 2025-03 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1jk1zmd/why_you_need_subtyping/

**Bearing on `fun`.** Decided, split by role: **nominal** identity for declared types and
effect families (same declaration + convertible free variables; applicative by purity, or
generative under a run-time effect), **structural** identity for `struct`s and modules. Two
record types with the same fields are the same type; two enums declared in two places are not.
The open work is `design-private-type-visibility-model` (what a nominal's own module may see)
and `public-members-are-unique`.

### Algebraic subtyping: principality with a subtype relation

**What it is.** Dolan's MLsub line: types form a lattice, `A ∩ B` and `A ∪ B` appear in
*types* rather than only in type checking, functions are contravariant in their domain, and
inference still produces one principal type. Subsumption is pushed into the type language so
unification stays a lattice operation.

```text
(A1 → B1) ∩ (A2 → B2)  ≡  (A1 ∪ A2) → (B1 ∩ B2)
```

**Buys.** Overloads, subtyping and principal inference in one package; simpler code that picks
the narrower type without annotations; and a route for HM languages to gain subtyping without
giving up principality.

**Costs.** The identity above is exactly where it breaks: a thread in the corpus shows that
*user-provided subtype contracts* (a `nonzero Real` attribute with several ascribed function
types) violate the equivalence even with no function overloads, so the feature cannot coexist
with predicate-carrying types. Separately, the principal types it produces can be
unreadable/"too forgiving". Checking cost is the third worry — untagged unions and intersections
are reported as a quadratic-or-exponential corner. The comment tree of "Why You Need Subtyping"
names an escape hatch I could not check: the co-author of a *structural refinement types* paper
combines Freeman–Pfenning's declared refinements (all refinements of ML types declared upfront)
with algebraic subtyping, non-coercively — refinements say at compile time that certain elements
cannot be present and "should not leave any trace at runtime" — and reports that full inference
plus polymorphic variants plus equi-recursive types infers "surprisingly precise" refinements
automatically (doubling a Peano number infers an even-number return type). Whether that
composition survives the contract counter-example above is settled neither by the thread nor by
this pass — the paper was not read (no network) (comment on "Why You Need Subtyping",
https://www.reddit.com/r/ProgrammingLanguages/comments/1jk1zmd/why_you_need_subtyping/).

**Maturity.** `research` — implemented in MLsub, Crust, Sugar[C], 1SubML and PolySubML, none
deployed.

**Tried by.** MLsub, Crust, Sugar, Ante, 1SubML, PolySubML.

**Source.** The Simple Essence of Algebraic Subtyping: Principal Type Inference with Subtyping
Made Easy, score 38, 15 comments, 2020-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/hpi54o/the_simple_essence_of_algebraic_subtyping/
· Notes on Implementing Algebraic Subtyping, score 36, 30 comments, 2024-05 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1cky61t/notes_on_implementing_algebraic_subtyping/
· It's not just "function overloads" which break Dolan-style algebraic subtyping.
User-provided subtype contracts also seem incompatible, score 43, 15 comments, 2026-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1qcz4g8/its_not_just_function_overloads_which_break/
· 1SubML - structural subtyping, unified module and value language, polynomial time type
checking and more, score 54, 22 comments, 2026-04 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1sal879/1subml_structural_subtyping_unified_module_and/
· Why You Need Subtyping, score 71, 73 comments, 2025-03 (bodyless link post; comment evidence
only) — https://www.reddit.com/r/ProgrammingLanguages/comments/1jk1zmd/why_you_need_subtyping/

**Bearing on `fun`.** Genuinely new to the project — no ticket in `wayfinder/` proposes a
subtype relation, and the design map's decided list assumes convertibility is the only
equality. The rival approach the corpus also carries is worth recording as the contrast: keep
HM and admit a *bounded* set of subtyping rules (a mutable reference is a subtype of a shared
one, `⊥ <: T`) rather than a
lattice — see "Is there some easy extension to Hindley Milner for a constrained set of
subtyping relationships?", score 32, 31 comments, 2025-05 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1kqqt9w/is_there_some_easy_extension_to_hindley_milner/
— which is roughly how `fun`'s heap brands behave *without* a subtyping rule: `Ref(h, A)` and
`Ref(h', A)` are simply different types, related by nothing.

### Gradual typing, and the boundary-check tax

**What it is.** A type `dynamic` (or an unannotated position) that is *checked at run time*
whenever it meets a static type, so typed and untyped code interoperate in one program. The
interesting variants are optional typing (annotations ignored at run time: Python, TypeScript)
versus sound gradual typing (annotations enforced with inserted checks: Racket, Guara/Banana).

**Buys.** Migration: existing untyped code and new typed code coexist, and each annotation
buys a guarantee incrementally.

**Costs.** Two, both measured. The overhead of boundary checks in sound gradual systems was
concluded "not tolerable" by the paper the corpus's thread is asking about; and the
alternative — optional typing — is unsound, so the annotations document intent without
enforcing it. On top of that, "types bolted onto an existing language" produce clumsy syntax
and complicated, unsound type systems, which is the fate the corpus's 124-comment thread asks
how to avoid.

**Maturity.** `contested`. Which side has the better evidence: the *unsound-but-cheap* side has
TypeScript/Python deployed everywhere; the *sound* side has the "not tolerable" overhead result,
the counter-observation in the same thread that C#/Dart `dynamic` is sound and fast enough in
practice, and — from a commenter who spent ten years on Dart as it was laboriously migrated to
fully static typing — the verdict that optional typing was "really the lowest common
denominator: the verbosity and complexity of a statically typed language, and the performance
and safety of a dynamically typed one", with almost all users *much* happier once the system
became sound (comment on "Future of high-level
languages",
https://www.reddit.com/r/ProgrammingLanguages/comments/12x46f5/future_of_highlevel_languages/).
Also live in that tree: a top comment argues gradual typing's merit is mostly retrofitting
existing languages and that freshly designed languages will not be based on it, answered
in-thread by a language designer who is doing exactly that. No deployment evidence exists for a
language that is both implicit-`Any` and sound.

**Tried by.** TypeScript, Python (mypy/pyright), Ruby (Sorbet), Racket, Dart, C#, Skiff
(hobby).

**Source.** Is sound gradual typing alive and well?, score 34, 24 comments, 2025-03 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1jatlyq/is_sound_gradual_typing_alive_and_well/
· Will a dynamically typed language eventually need optional static typing?, score 80, 124
comments, 2020-09 —
https://www.reddit.com/r/ProgrammingLanguages/comments/ipvyx2/will_a_dynamically_typed_language_eventually_need/
· Is gradual-typing bad for new languages?, score 33, 39 comments, 2024-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/193ssc2/is_gradualtyping_bad_for_new_languages/
· The Behavior of Gradual Types: A User Study, score 11, 24 comments, 2018-12 —
https://www.reddit.com/r/ProgrammingLanguages/comments/a5fi1x/the_behavior_of_gradual_types_a_user_study/
· Experiment: performance costs of dynamic safety checks, score 30, 25 comments, 2022-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/w1dpib/experiment_performance_costs_of_dynamic_safety/
· Future of high-level languages, score 73, 76 comments, 2023-05 (comment evidence for the
Dart migration) —
https://www.reddit.com/r/ProgrammingLanguages/comments/12x46f5/future_of_highlevel_languages/

**Bearing on `fun`.** Not proposed anywhere, and the nearest thing is the opposite trade:
`fun` has no `dynamic`, so nothing is checked at run time that was not decided at elaboration —
except that the *checker itself* runs code under the evaluation budget. If a gradual escape
hatch were ever wanted (`panic` already returns any `T`), the honest shape here is an
`Absurd`-returning hole with a residual row, not a `dynamic` type. Treat as new and unwanted
until a ticket says otherwise.

### Refinement types, predicates-as-types, and contracts

**What it is.** Narrow a type with a predicate — `x : { a : Int | a > 0 }` — so the type checker
carries facts. Three intensities: **contracts** (assert at the boundary, check at run time),
**refinement types** (decide the predicate by SMT/abstract interpretation at check time), and
**dependent types** (the predicate is an ordinary proposition and the checker proves it).
Predicate-as-type is the middle one with the proof obligation explicit.

**Buys.** `divide(x: Real, q: nonzero Real)` rejects the division at the call; array indices,
units, and non-empty collections become checkable facts rather than conventions.

**Costs.** The corpus's own poster names two: restricting a type to `M` loses information about
the *output* of an operation that was really `M -> M -> S` (pre/postcondition systems exist to
patch this); and unification of two refinements is not obvious — does `{x : Int, x > 0}` unified
with `{x : Int, x % 2 == 0}` give the conjunction? Always? Plus undecidability (a real
objection, though the poster notes plenty of shipped type systems are already undecidable) and
the solver as a build dependency. Practitioner evidence from two comment trees sharpens the bill
further: a user of the Dafny stack reports that Dafny → Boogie → Z3 layers mean a rejection says
*that* something is wrong but often cannot say *what*, so "after a few days you develop a gut
feeling" for what the solver will accept — the dependent-types thread's own commenter agrees,
wishing SMT solvers gave better errors — and another commenter states flatly that
predicates-as-types "basically makes type inference impossible, and even static type checking
will not always be possible" (comments on "How impractical/inefficient will "predicates as
type" be?" — permalink in Source — and on "Dependent types and usability?",
https://www.reddit.com/r/ProgrammingLanguages/comments/hb6rn4/dependent_types_and_usability/).

**Maturity.** `research` for inference-backed refinements (Liquid Types, F*, Dafny, Ante);
`shipped` for the contract half (Eiffel, Ada/SPARK, D). One tag: `research`, because the idea
as stated is the *inferred* version. The comments pull the contract half toward the type half:
a working Dafny program in the predicates thread (`requires isEven(x)`, `ensures isOdd(y)`) is
reported as "3 verified, 0 errors", with the rejoinder that "weaseling around Rice's theorem is
what programming languages researchers do all day" — while Common Lisp's `satisfies` is
reported as the opposite pole, a declaration that is "essentially just a stand-in for a runtime
type check" with none of the compile-time benefit.

**Tried by.** Liquid Haskell, Dafny, F*, SPARK/Ada, Eiffel, Common Lisp (`satisfies`,
runtime-only), Ante (hobby, refinement types as its headline feature); Freeman–Pfenning's
declared-refinement proposal is named in comments as the original (not read in this pass).

**Source.** How impractical/inefficient will "predicates as type" be?, score 43, 68 comments,
2023-08 —
https://www.reddit.com/r/ProgrammingLanguages/comments/166er7n/how_impracticalinefficient_will_predicates_as/
· Any papers/ideas/suggestions/pointers on adding refinement types to a PL with Hindley-Miller
like type system?, score 16, 20 comments, 2024-06 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1d6nogn/any_papersideassuggestionspointers_on_adding/
· Languages with optional SMT Solver to allow for additional reliability?, score 23, 16
comments, 2026-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1v538oo/languages_with_optional_smt_solver_to_allow_for/
· Why isn't design by contract more common?, score 81, 57 comments, 2021-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/l238o5/why_isnt_design_by_contract_more_common/
· Ante: A safe, easy, low-level functional language for exploring refinement types, lifetime
inference, and other fun features, score 75, 8 comments, 2022-06 —
https://www.reddit.com/r/ProgrammingLanguages/comments/vkhfhm/ante_a_safe_easy_lowlevel_functional_language_for/

**Bearing on `fun`.** The *expressive* half is already the native register: the core is
dependently typed, so `Vec T n -> (k : I64) -> Lt(k, n) -> T` is an ordinary `Pi` chain and
`Absurd` names the uninhabited case. What is missing is the *automation* half — no SMT, no
implicit proof search; proofs are supplied by the programmer or by the checker evaluating a
term within the **evaluation budget**. `wayfinder` has no ticket asking for an external solver;
the closest fog is `formalized-semantics`.

### Exhaustiveness pushed into value constraints

**What it is.** Take the pattern-matching exhaustiveness checker and let it reason about *values*
rather than only constructors: integer ranges (Rust already reports that `101` is uncovered
between `-2147483648..=100` and `102..=`), singleton/negation types, and eventually arbitrary
predicates — so `x: i32 for x < 10` passed to a function requiring `x < 20` is checked.

**Buys.** One mechanism (the decision-tree coverage check) covering a class of bugs that would
otherwise need assertions, and exhaustiveness becomes the pressure that forces predicates into
the type system in the first place.

**Costs.** Coverage over an infinite domain needs a decision procedure, and the poster already
spots the hard part: the checker cannot know `x < 10 ⟹ x < 20` unless that is stated as an
invariant. Every invariant you do *not* have turns into a false negative, and false negatives
in an exhaustiveness checker are worse than none because programmers trust them.

**Maturity.** `speculative`. Range and literal exhaustiveness ship; constraint-driven
exhaustiveness over user predicates has no shipping implementation the corpus names.

**Tried by.** Rust (integer ranges, niche cases), Swift (enum exhaustiveness); nobody has
shipped predicate-level exhaustiveness.

**Source.** How do languages like rust determine exhaustive patterns? Can this be extended to
include compile time constraints?, score 69, 9 comments, 2022-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/w1uco8/how_do_languages_like_rust_determine_exhaustive/
· Algorithms for typechecking untagged unions, intersections, and dependent types?, score 36,
11 comments, 2023-06 —
https://www.reddit.com/r/ProgrammingLanguages/comments/14783jp/algorithms_for_typechecking_untagged_unions/
· Anybody Know a Dynamic Language With Exhaustive Case Checking / Pattern Matching, score 29,
38 comments, 2020-02 —
https://www.reddit.com/r/ProgrammingLanguages/comments/fan61g/anybody_know_a_dynamic_language_with_exhaustive/

**Bearing on `fun`.** Half here already: matches compile to decision trees with
exhaustiveness checking, finite nominal domains are checked precisely, and the bottom type is
`Absurd` with `all_atoms` returning an empty domain. The other half — the guard that decides a
literal range — is deliberately conservative over the open `Type` ("the type universe is
infinite, matches need a fallback"), and `type-case-refinement-walks-whole-context` is the
ticket that decides how far a branch may narrow the context (its `d`/`e` ruling is still owed
by the user).

### The sums-vs-products asymmetry

**What it is.** Languages make "this *and* that" easy (tuples, records, structs) and "this *or*
that" hard, even though the sum is the half that makes exhaustive reasoning possible. Two
diagnoses are in the corpus, and the comments argue the post's is secondary: the post says
subtype polymorphism and virtual dispatch absorbed the sum's job in the OO era, while the top
comments say the deeper cause is C's memory model — a product is byte concatenation, a sum
needs a tag plus worst-case padding, "too complex for C to do directly" — and that unions
existed all along in Modula/Ada/Pascal/C/Algol; what actually came and went, per a long comment
in the thread, was the *checked discriminant* (coupling field access to setting the
tag), which depends on whether the designer has seen the feature and thinks ubiquitous checking
is worth paying for (top comments on the same thread, permalink in Source).

**Buys.** A sum type plus a match gives exhaustiveness, makes impossible states unrepresentable,
and turns "handle every case" into a compiler-checked obligation instead of a code review.

**Costs.** Representation (tag + payload, or a niche optimisation that must be *invented* per
type), and the fact that adding a constructor then breaks every non-defaulting match — which is
the point, but is also why teams resist them. A commenter in the companion thread makes the
asymmetry exact: with N operations over M types you must write N×M pieces of code either way;
sum types make adding an operation cheap and adding a variant expensive, OO subtyping makes the
reverse cheap, and in both styles the compiler checks the coverage (comment on "Could you
explain why sum types are so good?", permalink in Source).

**Maturity.** `shipped` — Rust, Swift, Kotlin, F#, Scala, OCaml, Haskell.

**Tried by.** Every language with `enum` + pattern matching; and a stream of hobby languages
re-deriving structural sums from scratch (one poster concluded their whole language was
"structural sums + structural products, nothing new but the syntax").

**Source.** Why are product types so common while sum types are so rare?, score 97, 115
comments, 2021-04 —
https://www.reddit.com/r/ProgrammingLanguages/comments/minw5w/why_are_product_types_so_common_while_sum_types/
· Could you explain why sum types are so good?, score 45, 68 comments, 2023-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/10jewgp/could_you_explain_why_sum_types_are_so_good/
· Generalization of Sum-Types, Pattern Matching & Niche Optimization, score 22, 14 comments,
2026-05 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1t4vycj/generalization_of_sumtypes_pattern_matching_niche/
· Algebraic Shape Composition in a tiny functional language, score 15, 21 comments, 2026-06 —
https://www.reddit.com/r/Compilers/comments/1ujv6kn/algebraic_shape_composition_in_a_tiny_functional/
(hobby/LLM-assisted project: evidence of interest only)

**Bearing on `fun`.** Both halves are first-class: `Constructor`s build a nominal and live in
the same namespace as everything else (a constructor sharing its type's name shadows that
type), `Tuple(n, T1, …, Tn)` is the built-in flat product, and a `struct` is the record-shaped
product. The open design work is representation-adjacent — `enum-captures-from-payload-values`
— not whether sums exist.

### Null vs option: nesting is the whole argument

**What it is.** Two designs for "absent": a `null` inhabiting every reference type (Java,
C, Python's `None`), or a proper sum `Option<T>` with a `None` case. The corpus's Zig thread
shows the third shape — a *typed* optional `?T` that nests, so `??u32` distinguishes "no entry"
from "entry holding null" — and reports that the usual objection to optionals (verbosity) and
the usual objection to nullable types (no nesting) are both addressed by it.

**Buys.** A sum-typed absence makes the compiler force a check, nests correctly, and composes
with generic code; a typed optional additionally avoids wrapping every value in `Some`.

**Costs.** Optionals are verbose at construction (`Some` everywhere) unless the language
special-cases them; a nullable scheme needs a blanket "non-null" discipline to be useful at
all; and Zig's scheme costs one corner (`@TypeOf(null)` cannot be an optional type) plus a cast
in the three-level case. The comments add the failure a *nullable-as-union* form cannot
recover: `fn find<T>(...) -> Nullable<T>` over a `List<Nullable<Int>>` collapses to
`Int | null`, so "found a matching item but it was null" and "no item matched" become the same
value; and because `Nullable<T>` may *be* `T`, a user can probe an abstract type's
representation — the reason one commenter would accept no union-based null at all. Two
well-argued comments also disagree on whether that collapse is a bug: the set-based position
calls `string | null | null` → `string | null` "objectively sound" and says to reach for a
discriminable value when detail is needed; the explicit position answers "I want to be the one
to decide when information is irrelevant and can be dropped—not for the programming language to
default to reducing a vector to its magnitude" (comments on "Why You Need Subtyping",
https://www.reddit.com/r/ProgrammingLanguages/comments/1jk1zmd/why_you_need_subtyping/).

**Maturity.** `shipped` — Rust/Swift/Kotlin/OCaml `Option`, Zig `?T`, Java's annotated
nullness. The set-collapse question underneath nullable unions is `contested` in this round's
comments (Costs above): sound-by-normalization against explicit-by-default.

**Tried by.** Rust, Swift, Kotlin, OCaml, Haskell, Zig, Java (`@Nullable`), C# (`Nullable<T>`).

**Source.** What's up with Zig's Optionals?, score 28, 28 comments, 2024-03 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1bn2i58/whats_up_with_zigs_optionals/
· Static "Optional" type in an otherwise dynamic language?, score 26, 33 comments, 2023-08 —
https://www.reddit.com/r/ProgrammingLanguages/comments/15jn1gv/static_optional_type_in_an_otherwise_dynamic/
· CCore basics. Part 3: nullable types, pattern matching and control flow, score 6, 1 comment,
2016-10 —
https://www.reddit.com/r/ProgrammingLanguages/comments/59xk4k/ccore_basics_part_3_nullable_types_pattern/
· Why You Need Subtyping, score 71, 73 comments, 2025-03 (nullable-collapse argument, comment
evidence) — https://www.reddit.com/r/ProgrammingLanguages/comments/1jk1zmd/why_you_need_subtyping/

**Bearing on `fun`.** No null anywhere; absence is the prelude nominal `Option`, matched like
any other enum, and `Absurd` covers the never-happens case rather than a null that always can.
Two live edges: `std-eq-for-list-and-option` is parked on the two-list recursion crash
(`recursive-match-on-two-lists-cores`), and `port-enum-captures-from-payload-values` is the
constructor-payload work.

### Structural records: identity, order, width

**What it is.** A record type is identified by its fields — name and type — so field order is
either irrelevant (ML: `{a: real, b: string}` ≡ `{b: string, a: real}`) or load-bearing (Go:
both name *and* sequence, so `{a, b int}` ≠ `{b, a int}`), and width is either exact or
tolerant of extra fields. Three independent knobs that languages usually bundle silently.

**Buys.** Structural identity means no declarations, straightforward interoperability, and
records usable as both data and namespace; name-keyed identity makes field order free to
change.

**Costs.** Name-keyed identity loses the information Go gets for free (field order as part of
layout/FFI meaning); width-subtyping or width-patterns make the "what does this function need"
question open-ended; and structural identity means accidental conformance between two
unrelated records with the same fields.

**Maturity.** `shipped` — OCaml, TypeScript, Go, Elm, Haskell `RecordWildCards`-style rows.

**Tried by.** OCaml, Go, TypeScript, Elm, PureScript, Rust (structural for field access,
nominal for the type).

**Source.** How do product and record types work in your language?, score 29, 51 comments,
2023-05 —
https://www.reddit.com/r/ProgrammingLanguages/comments/13dya1e/how_do_product_and_record_types_work_in_your/
· Record type inference for dummies, score 54, 16 comments, 2026-06 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1udg7pa/record_type_inference_for_dummies/

**Bearing on `fun`.** Decided: a `struct` is a record type and a namespace in one construct;
construction checks every required field and rejects unknown ones; field order at construction
is not significant; a complete record pattern lists every field and a partial one uses trailing
`_` (`docs/wayfinder/topics/records.md`). The identity twist is recursion: `rec Numbers = struct
{ … }` mints a record *identity*, two same-shape recursive records stay distinct, and plain
records stay structural. Open tickets: `env-width-contract-is-unnamed`,
`struct-open-width-depends-on-value`, `design-private-type-visibility-model`.

### Maps with statically known keys

**What it is.** Index a product by a compile-time-known key set, so `myData[axis]` compiles to
an exhaustive match and the key type itself is the schema:

```text
enum Axis { X, Y, Z }
struct MyData { [Axis]: Data }     -- myData[axis] becomes a checked match
```

**Buys.** Kills the hand-written key→field mapping (and its typos, which the corpus's poster
calls "a common occurrence"), gives exhaustiveness when a new key is added, and gets
TypeScript's mapped-type ergonomics without adopting structural subtyping wholesale.

**Costs.** Changing the enum changes the struct's layout and alignment — an FFI hazard the poster
flags — and it is a step toward structural typing, which is a can of worms the same poster
explicitly wants to avoid. Indexing also hides a match behind a bracket, so a branch can become
invisible to the reader.

**Maturity.** `shipped` in TypeScript as mapped types / literal-union indexing; the
*enum-indexed struct with exhaustive-match lowering* form is `speculative`.

**Tried by.** TypeScript (`{ [K in Axis]: Data }`); nobody else in the corpus.

**Source.** Idea for maps with statically known keys, score 18, 22 comments, 2024-08 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1eo5oo4/idea_for_maps_with_statically_known_keys/

**Bearing on `fun`.** Genuinely new, and cheap enough to be plausible: **record type reflection**
is complete (`struct … end` type patterns over constructor fields) and type-case can match on
nominal heads, so a struct carrying an enum-indexed group could elaborate to ordinary fields
plus an ordinary match — no new type-system machinery, one new construction form. The FFI
layout objection does not apply (there is no FFI yet), but the "one construct, many roles"
priority says the *indexing* should be a macro/template, not a second record grammar.

### Dependent types: the usability tax, and what it buys back

**What it is.** Types may mention values, so `Vec T n` carries its length and
`access : Vec T n -> (k : Nat) -> (k < n) -> T` makes out-of-bounds unrepresentable. The
corpus's question is not whether that works — it does — but why no dependently typed language
is in "general use" the way F# or OCaml are, and at what cost.

**Buys.** Correctness per annotation: indexed vectors, protocol conformance, invariants that
survive refactoring, and a single language for program and proof.

**Costs.** Three, all visible in the corpus. (1) *Ergonomics*: annotations get heavy and the
"how many use cases beyond vectors?" question goes unanswered in a 75-comment thread. (2)
*Representation*: the thread asking why dependent vectors cannot compile to O(1) indexing
points at the real issue — a proof-carrying index is erased, but nothing forces the *container*
to be a flat array, and Nat-as-unary is only fast because Agda/Idris special-case it to a GMP
integer. (3) *Elaboration cost*: type-level computation must run at check time, which is where
checking gets slow and where totality questions start. (4) *Abstraction boundaries*, from the
thread's comments: a client's proof depends on the *definitional* equality of the
implementation, so flipping the clause order of `_+_` flips which client lemmas typecheck, and
the breakage is "not just sitting there in the source for you to add or remove `public` — it's
woven through your function definitions"; and a commenter adds that the cons "still apply,
sadly" even if you never use the expressive types — "you'll pay for what you don't use". The
same tree records the defence: you do not have to write proofs everywhere, "so long as there is
a good enough ecosystem" for the simpler types (comments on the same thread, permalink in
Source).

**Maturity.** `contested`. Which side has the better evidence: the pro side has Idris, Agda,
Lean, F* shipping real tools; the con side has the observed fact that none is widely used for
application code, and no corpus thread refutes it.

**Tried by.** Agda, Idris, Lean, F*, Coq, Twelf; and a long tail of hobby dependent languages
(Cicada, Par, Juvix).

**Source.** Dependent types and usability?, score 64, 75 comments, 2020-06 —
https://www.reddit.com/r/ProgrammingLanguages/comments/hb6rn4/dependent_types_and_usability/
· Is there a fundamental reason why dependently typed Vectors can't compile to O(1) access
vectors?, score 74, 20 comments, 2020-10 —
https://www.reddit.com/r/ProgrammingLanguages/comments/j7gjbd/is_there_a_fundamental_reason_why_dependently/
· With dependently typed lists, what is the difference to a tuple?, score 31, 36 comments,
2022-09 —
https://www.reddit.com/r/ProgrammingLanguages/comments/x7gxic/with_dependently_typed_lists_what_is_the/
· Dependent types do's and don'ts, score 21, 16 comments, 2023-06 —
https://www.reddit.com/r/ProgrammingLanguages/comments/14czkbu/dependent_types_dos_and_donts/
· Dependent Type Systems, score 61, 24 comments, 2020-11 —
https://www.reddit.com/r/ProgrammingLanguages/comments/jtwyxu/dependent_type_systems/

**Bearing on `fun`.** This is `fun`'s own position, so the entry is the project's risk
register. The tax is already being paid in tickets: `checker-evaluation-budget` (type-level
computation runs under one budget shared with the evaluator),
`type-case-refinement-walks-whole-context` (a refinement case measured at 37.8 s before the fix
brought it to 1.02 s), `deep-non-tail-recursion-is-superlinear` (research-only, resolved in the
port's favour), and `Type : Type` — a soundness shortcut deliberately taken, with
`universe-levels` as the fog item that sharpens when a `Category`/`Functor` must be written once
instead of per tier.

### Immutability as a type-level property vs a property of a reference

**What it is.** Does "immutable" change the *type* (`Immutable<Array<T>>` / a `readonly`
class) or a *binding* (`const`, `final`, Rust's shared reference vs its mutable reference)?
The two are not
interchangeable: type-level immutability applies to the value everywhere, reference-level
immutability applies to one use and expires with it.

**Buys.** Type-level: a guarantee that travels with the data, impossible to undo at a call
site. Reference-level: no second type per type, no conversion cost when nesting, and functions
can demand "read-only *here*" — which the corpus's practitioner notes is what UI/snapshot code
actually wants, since the value may change later, just not while this function runs.

**Costs.** Type-level immutability forces a conversion whenever you must share a mutable value
and produces the nested-mutation problem (mutable inside immutable: which type is it?).
Reference-level needs a borrow discipline — path/aliasing rules — or it is only a
convention, and the corpus threads are candid that mutability-as-a-convention does not survive
contact with a real codebase.

**Maturity.** `contested`. Which side has the better evidence: reference-level is what every
shipped systems language chose (C++ `const`, Rust `&`, Java `final`), and PHP's readonly
classes are the type-level counterexample being debated — but the "readonly class" approach
still has to answer the nesting question, and no corpus thread claims it does.

**Tried by.** C++ (`const`), Rust (shared vs mutable references), Java (`final`), PHP 8.2
(readonly classes),
JavaScript (`Object.freeze`), Kotlin (`val`).

**Source.** Does/should/can immutability vs mutability introduce a new type? or is it just a
feature?, score 49, 54 comments, 2020-08 —
https://www.reddit.com/r/ProgrammingLanguages/comments/i91p9e/doesshouldcan_immutability_vs_mutability/
· What are the advantages/disadvantages of immutability as a property of a type vs. immutability
as a property of an object/reference/parameter?, score 66, 42 comments, 2023-05 —
https://www.reddit.com/r/ProgrammingLanguages/comments/13vozxh/what_are_the_advantagesdisadvantages_of/
· Ownership vs full immutability, score 72, 49 comments, 2022-05 —
https://www.reddit.com/r/ProgrammingLanguages/comments/uxtcme/ownership_vs_full_immutability/

**Bearing on `fun`.** Answered by splitting the question in two, which is exactly what the
rejected merged mutation-effect name was hiding: mutation is an **effect** (`Alloc(h)` / `Read(h)` /
`Write(h)`, so read-only code earns a weaker row) and reference-ness is a **type** (`Ref(h, A)`
branded by its heap). Immutability therefore never changes the *data's* type — a value is a
value — it changes which row an arrow carries, and `Discharge` is what lets a locally mutable
implementation present a pure signature. `refs-in-effect-rows` and
`nominal-identity-applicative-by-purity` are the decided tickets.

### Type-specialized equality — and when a type should have none

**What it is.** Equality is not one operator but a family dispatched by type: primitives get a
monomorphic primitive, records and nominals get a derived or hand-written instance, and a type
that should not be comparable simply has no instance. The stronger version in the corpus argues
a domain type *should not* get `==` at all: two bank accounts with the same id and different
balances make "equal" a question the type cannot answer, so equality should be a named
predicate (`sameId`, `structurallyEqual`) and the type should be barred from `Eq` — hence from
`Set` and `Map` keys.

**Buys.** Correct behaviour per type (no reference-vs-structural surprises), the freedom to
*withhold* equality as a design statement, and the freedom to make equality a trait rather
than a reserved word.

**Costs.** Withholding equality breaks generic containers that need *some* key (the answer is
"key on `id`", which surprises people); derived structural equality on a deep structure costs
stack/time and can loop on cycles; and dispatch-by-type means equality is no longer a single
primitive an optimizer can see through.

**Maturity.** `shipped` — Haskell's `Eq`, Rust's `derive(PartialEq)`, C++ overloads, OCaml's
structural `=`. The *withhold* variant is argued, not built.

**Tried by.** Haskell, Rust, OCaml, C++, Swift; nobody in the corpus ships a language that
refuses `Eq` by default for user types.

**Source.** Do we even need equality?, score 43, 59 comments, 2022-05 —
https://www.reddit.com/r/ProgrammingLanguages/comments/uzq8xw/do_we_even_need_equality/
· Which languages have equality, hashing, and ordering on recursive trees?, score 18, 32
comments, 2023-11 —
https://www.reddit.com/r/ProgrammingLanguages/comments/17tw4nc/which_languages_have_equality_hashing_and/

**Bearing on `fun`.** Already has it, by name: `type-specialized-equality.md` — user-level `==`
is `(==) : [A : Eq] -> A -> A -> Bool`, the implementation is a type-head match over `Type`
with primitive heads `I64`, `Bool`, `Char`, `Unit`, `String`, and nominal/record equality is
deliberately *not* automatic: an explicit `Eq` impl is required — which is the corpus's
"withhold" position, already implemented. Because `Type` is open, the type-head match needs a
fallback branch; and `std-eq-for-list-and-option` is parked on
`recursive-match-on-two-lists-cores`.

### Coherence: local most-precise resolution vs one global instance

**What it is.** *Coherence* = the same question gets the same answer everywhere: for a given
trait and type there is one instance program-wide (Haskell), or the search is made complete by
an orphan rule (Rust: only the trait's or the type's crate may define it). The alternative is
scoped resolution: instances are found where you wrote them, so two components can carry
different instances for the same type.

**Buys.** Global coherence buys *transitive dependencies*: a crate depending on two crates that
both impl `Ord(T)` has a defined meaning, and data structures parameterised by an ordering can
be merged soundly. Scoped resolution buys local reasoning — you can see where an instance came
from — and lets conflicting instances never meet.

**Costs.** Global coherence needs non-first-class modules (a fixed link-time registry) and
produces the newtype-wrapper workaround, widely described as ad hoc. Scoped resolution gives up
soundness for any structure that stores a dictionary in its data: two `Set T` values may have
been built with different `Ord T`, and merging them is unsound.

**Maturity.** `contested`. Which side has the better evidence: Haskell/Rust/Swift/Lean/Scala
ship global or quasi-global coherence, and Scala's *implicit scope* is the standard answer for
a language whose modules are values — so the "global works, but only without first-class
modules" claim has strong evidence on both halves.

**Tried by.** Haskell (global), Rust (global + orphan rule), Scala 3 (lexical + companion
scope), Lean (global on import with `local` escapes), OCaml modular implicits (scope only),
Agda (scope only), PureScript/Idris (named instances).

**Source.** Modules: Overcoming Stockholm and Dunning-Kruger, score 84, 44 comments, 2022-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/vqx19e/modules_overcoming_stockholm_and_duningkruger/
(lists coherence among the module-system constraints a design must navigate) · How Java plans to
integrate "type classes" for language extension, score 72, 39 comments, 2025-08 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1mwj302/how_java_plans_to_integrate_type_classes_for/

**Bearing on `fun`.** Rejected, with the reason recorded: **global coherence is unavailable
when modules are values** — an impl inside a module is a value, a module can be built by a
function and returned, and there is no well-defined moment to register a global impl
(`docs/wayfinder/topics/impl-visibility.md`, option C). What `fun` chose instead is
most-precise-impl-wins with lexical nearness never breaking a tie (`traits.md`), impls arriving
through `open`, and a planned named-impl handle. The residual hazard is named and parked: scoped
resolution makes ordered/hashed collections unsafe to merge, and `impl-visibility` says that
decision belongs to `traits`' already-settled "impls resolve from scope".

### Divergence: a side effect, or an implementation detail?

**What it is.** What does non-termination *mean* in the language? Four shipped answers, laid
out cleanly in one corpus post: C++ says an infinite loop with no side effects is undefined
behaviour and may be optimised away; Rust says non-termination is a side effect and may not
be; Koka makes divergence an algebraic effect; Haskell's laziness makes dropping an unused
divergent computation always legal.

**Buys.** Treating divergence as an effect lets optimisers delete dead loops and lets the type
system talk about termination; treating it as an implementation detail keeps recursion
expressible with no termination checker; treating it as undefined gives the optimiser maximum
freedom at the cost of surprise.

**Costs.** Undefined-behaviour divergence produces real bugs (the corpus poster's own
motivating fear: an optimiser deleting a loop whose result appeared unused). Making it an
effect means every recursive function carries it. Requiring a termination proof means no more
unrestricted recursion — the poster explicitly wants to avoid that.

**Maturity.** `contested`. Which side has the better evidence: Rust's position is the one with
production evidence *against* C++'s (the poster's scenario is a documented class of bug), but
C++'s optimisation argument is real and Haskell's lazy answer is the most defensible of the
four for a lazy language. No consensus thread exists in this corpus.

**Tried by.** C++ (UB), Rust (effect-like), Koka (explicit effect), Haskell (lazily moot).

**Source.** Infinite loops: a side-effect, or an implementation detail?, score 78, 45 comments,
2022-03 —
https://www.reddit.com/r/ProgrammingLanguages/comments/tdlff4/infinite_loops_a_sideeffect_or_an_implementation/

**Bearing on `fun`.** Recorded, unambiguously, in `CONTEXT.md`: **"Termination is never checked;
divergence is not an effect."** What `fun` has instead is the **evaluation budget** — how many
semantic steps the *checker* may spend evaluating while type checking; exceeding it is a
compile error naming the call; running a program spends none. The retired recursion guard is
the counter-example: a limit on how many recursive calls the checker would allow was replaced by
the budget precisely so divergence could not be modelled as a resource the program consumes.

### Strict by default, laziness as a targeted tool

**What it is.** Evaluate arguments when called (OCaml, Rust, most ML) or on demand (Haskell),
or — the corpus's more interesting proposal — stay strict and let the *type* mark where
laziness is wanted, with `force`/`lazy` inserted automatically:

```text
let lazy all-lazy' l = match ...        -- auto-thunked body
let lazy a && b = if a then b           -- short-circuit falls out of the type
```

**Buys.** Strict evaluation is predictable (cost is visible in the source) and composes with
tail calls; a laziness-marked arrow gives back short-circuiting and producer/consumer fusion
*without* a second copy of every combinator (`all-lazy`, `map-lazy`, `zip-lazy`).

**Costs.** Laziness is contagious: one lazy value forces lazy variants of everything that
touches it (the poster's complaint), and thunks conflict with tail-call elimination — a thunk
in tail position must remember where to write its memo, which the same thread identifies as the
exact place stack savings leak away. Full laziness also makes cost impossible to read off the
source.

**Maturity.** `contested`. Which side has the better evidence: strictness is what every
non-Haskell production functional language ships, and the corpus's own n-queens example shows
lazy lists can be *both* too much and not enough laziness; the laziness camp's evidence is
Haskell's 30+ years. The sub-question (TCO vs call-by-need) is `research` — the thread ends
without a resolution.

**Tried by.** OCaml, Rust, Elixir, F# (strict); Haskell, Agda (lazy); Elm, PureScript (strict);
Sophie (call-by-need, hobby).

**Source.** Strict and lazy without littering lazy everywhere., score 16, 8 comments, 2021-03 —
https://www.reddit.com/r/ProgrammingLanguages/comments/m26toc/strict_and_lazy_without_littering_lazy_everywhere/
· Apparent conflict between TCO and call-by-need?, score 18, 21 comments, 2023-11 —
https://www.reddit.com/r/ProgrammingLanguages/comments/17oifwx/apparent_conflict_between_tco_and_callbyneed/
· Is the abstraction of lazy-functional-purity doomed to leak?, score 15, 19 comments, 2023-06 —
https://www.reddit.com/r/ProgrammingLanguages/comments/146noct/is_the_abstraction_of_lazyfunctionalpurity_doomed/
· Sophie: A call-by-need strong-inferred-type language named for French mathematician Sophie
Germain, score 32, 11 comments, 2023-03 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1279h73/sophie_a_callbyneed_stronginferredtype_language/

**Bearing on `fun`.** Strict, decided: the evaluator never recurses on the native stack per
object-level call (a term needing sub-evaluation gets a `Kont` frame), and the only
non-strictness is a checker-side optimisation — a *pure* call is deferred (`VGlued`) inside
checker requests only, never inside a macro application, because purity is knowable from the
row. Short-circuiting is already delivered without a laziness type: `&&`/`||` are prelude
`pub infix` templates that expand to `match` over `Bool`.

### Purity as an inferred property rather than a monad

**What it is.** Mark or infer which functions are pure, and get optimisation, parallelism and
reasoning from that bit alone. The corpus's version is *inference*: (1) a pure function cannot
call an impure one, (2) it cannot modify globals or its parameters, (3) it may modify locals.
The lighter version is a keyword (`fn` vs `proc` in Nim) rather than a monadic type.

**Buys.** Purity becomes visible to the compiler without `IO` appearing in every signature;
parallel map/filter and memoisation become legal; and — the corpus's key observation — you
stop paying monad tax for code that never needed effects.

**Costs.** A keyword gives you a bit but not *which* effect, as the poster concedes ("you don't
get to see *what* impurities a function has"). Inference gives you the bit only if the effect
set of primitives is closed — every primitive must be classified, and third-party code cannot
be inferred.
And purity-by-marking is a convention unless the type system enforces it.

**Maturity.** `shipped` — Nim's `func`, Rust's `const fn`, Koka/Eff/OCaml 5's effect rows, and
Haskell's whole type system.

**Tried by.** Nim, Rust, Koka, Eff, OCaml 5, Haskell; the inference variant is what Rust's
`const`-qualification and effect systems approximate.

**Source.** Alternative to monads for enforcing purity?, score 39, 45 comments, 2021-02 —
https://www.reddit.com/r/ProgrammingLanguages/comments/lozq0h/alternative_to_monads_for_enforcing_purity/
· Built-in Purity Inference, score 40, 41 comments, 2020-05 —
https://www.reddit.com/r/ProgrammingLanguages/comments/gb93td/builtin_purity_inference/

**Bearing on `fun`.** Has the full version, decided: a bare arrow *is* pure (`A -> B` is
`A -> B can {}`), `can _` infers a row, purity is read off the row and never declared, and it
is load-bearing twice — it lets the checker *evaluate* a call while checking (within the budget)
and it decides whether a nominal declared in a call is applicative
(`nominal-identity-applicative-by-purity`). The effect machinery itself belongs to
`effects-and-handlers.md`; what belongs here is only that purity is an *inferred type-level
property*, which the keyword/inference debate in the corpus is the weaker half of.

### Linearity, affine types and lifetimes as type-system ideas

**What it is.** Give a type a *use count*: linear = used exactly once, affine = at most once,
ordered = exactly once and in introduction order. From affine you get Rust's whole vocabulary —
ownership, borrowing, moves, `Drop` — because "consumed" can be expressed as a type change and
Rust's mutable reference as a use count.

```text
File opened    -- affine: must be consumed or auto-closed
close(f)       -- consumes f; using it afterwards is a type error
```

**Buys.** Resources that cannot be leaked or double-closed, state machines that cannot fork,
aliasing rules the compiler checks, and in-place mutation with no garbage collector. A
defender in the borrow-checking thread reframes the whole family as a bonus: borrow checking
"is just a way of statically verifying time-based invariants", so it rules out logic bugs and
would make sense "for even GCed languages"; and GC's counter-price is rarely counted — to stop
mutation invalidating references, GC restricts *memory layout* (arrays hold only references)
rather than restricting references (comments on "Alternatives to borrow checking?", permalink
in Source).

**Costs.** The corpus's own threads: linearity (vs affine) forces *every* value to be
destroyed explicitly and makes generics over linear types infect their parameters
(`Option<linear T>` becomes linear), which the poster argues makes the whole type system
"much more complicated, while I don't see clear benefits". On top of that, propagation
verbosity — ownership passed to and back from callees — is the standing complaint against the
whole family. The borrow-checking thread adds two sharper ones: memory management leaking into
*function types* — one commenter counts C++'s parameter-form zoo and says Rust compounds it with
ownership and mutability (`Foo`, `&Foo`, `&mut Foo`, `Rc<RefCell<Box<...>>>`), answered in-thread
that most functions just take `&Foo` and ownership-taking is rare — and conservatism, the
checker forbidding aliasing that would in fact be safe, a price one practitioner says made him
walk away from Rust three times (comments on "Alternatives to borrow checking?", permalink in
Source).

**Maturity.** `shipped` — Rust (affine), Clean (uniqueness), Q# (linear), ATS, and session-type
libraries in Rust. The affine-vs-linear question is `contested` inside it.

**Tried by.** Rust, Clean, Q#, ATS, Idris (quantitative), Vale, Par, `par` (session types in
Rust); OCaml is reported in-thread as adding uniqueness types (in progress).

**Source.** Benefits of linear types over affine types?, score 51, 30 comments, 2024-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1e1o07f/benefits_of_linear_types_over_affine_types/
· What are the tradeoffs of Rust having affine types instead of linear types?, score 43, 22
comments, 2023-03 —
https://www.reddit.com/r/ProgrammingLanguages/comments/11gn93x/what_are_the_tradeoffs_of_rust_having_affine/
· Alternatives to borrow checking?, score 81, 63 comments, 2022-10 —
https://www.reddit.com/r/ProgrammingLanguages/comments/yd1g1s/alternatives_to_borrow_checking/
· "Am I the only one still wondering what is the deal with linear types?" by Jon Sterling, score
69, 15 comments, 2026-02 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1r3fcxw/am_i_the_only_one_still_wondering_what_is_the/
· What Vale Taught Me About Linear Types, Borrowing, and Memory Safety, score 58, 15 comments,
2023-06 —
https://www.reddit.com/r/ProgrammingLanguages/comments/13xkz1o/what_vale_taught_me_about_linear_types_borrowing/

**Bearing on `fun`.** Genuinely new as a *type-system* idea — no ticket proposes linearity,
affine types or lifetimes, and the closest thing `fun` has is the heap brand, which is a
different device: `Ref(h, A)` is branded by its heap and `Discharge` drops a heap that cannot
escape, giving runST-style leak-freedom with no use counts at all. The implementation side of
ownership (arenas, borrow checking, memory) is `runtime-and-memory.md`, not here.

### Ordered types: drop all three structural rules

**What it is.** Weakening (may ignore), Contraction (may duplicate) and Exchange (may reorder)
are the structural rules that make ordinary contexts sets. Drop them and every variable must be
used exactly once, in the order introduced — an *ordered* type system. The payoff the corpus
quotes: linear types reason about the heap; ordered types reason about the *stack*, because
deallocation order is now part of typing.

**Buys.** Stack-safety and deallocation order as a typing judgment rather than a
convention; a type system precise enough to say "this must be freed last"; and a natural home
for session/protocol ordering.

**Costs.** Everything becomes order-sensitive: reordering arguments or commuting independent
calls needs an explicit structural rule, ordinary code stops type-checking without a careful
sequencing discipline, and inference over ordered contexts is a research problem. The corpus's
asker found exactly one source (CMU lecture notes) and is asking whether anything more exists
— that is itself evidence of how thin the applied literature is.

**Maturity.** `research` — papers and lecture notes with small implemented systems, no
production language.

**Tried by.** Nobody has shipped this; research implementations attached to ordered/quantitative
type-system papers.

**Source.** Any info on ordered type systems?, score 39, 24 comments, 2026-09 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1whkoxo/any_info_on_ordered_type_systems/
· Unifying uniqueness and substructural (linear, affine) typing, score 59, 6 comments, 2023-05 —
https://www.reddit.com/r/ProgrammingLanguages/comments/13i4nm0/unifying_uniqueness_and_substructural_linear/
· A Friendly Tour of Substructural, Uniqueness, Ownership, and Capabilities Types, score 29, 7
comments, 2026-05 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1tpezv2/a_friendly_tour_of_substructural_uniqueness/

**Bearing on `fun`.** New, and adjacent-but-not-the-same: `fun`'s `Context` *is* an ordered
sequence where position is meaning — but that is a property of the meta-language's context, not
a typing rule about object-level variables. Nothing in `fun` restricts how or when a term uses
its variables; adding that would need a new judgement and would collide with the evaluator's
`Kont`-frame discipline, which is about native-stack safety rather than object-level ordering.
Treat as unproposed until a ticket wants stack-safety as a *type*.

### Decide the property before building the type system

**What it is.** The methodology entry, because the corpus asks for it explicitly: before
implementing a type-system idea, name the property you will test it for. The thread's own case
is instructive — a poster with `Type : Type` and unrestricted recursion, extending the same
treatment to product and record types, gets as far as asking "what property(s) should I be
looking for?" and does not know. Companion question: what does it even mean for a type system
to be "algebraic" (sums, products *and* exponentials, not just sums)? The companion thread's
comments show why that question comes first: the cardinality reading (`Bool -> Char` has as many
values as `Char * Char`) is answered with a worked exchange in which nontermination breaks it —
with bottoms, normal-order `Bool -> Char` has six values against five pairs, and equivalence
only returns if functions are required strict (comments on "What does it mean to have an
\"algebraic\" type system?", permalink in Source). So "algebraic" already presupposes an answer
to this axis's divergence question. A commenter's anti-feature list is the same instinct
inverted — name what you will reject: "anything that breaks parametricity", "anything that
breaks equational reasoning", "any form of ambiguity that is arbitrarily resolved by the
compiler rather than forcing the user to clarify their intentions" (comment on "What are some
anti features in a language?",
https://www.reddit.com/r/ProgrammingLanguages/comments/npn3cd/what_are_some_anti_features_in_a_language/).

**Buys.** Catches a bad idea before a rewrite: the questions worth asking are known —
canonicity, coherence, decidability of checking, principality, subject reduction, conservativity
— and each one has a standard counterexample you can try first.

**Costs.** Knowing the property names does not tell you whether your design preserves it
without a proof or an implementation; and hobby projects overwhelmingly implement first and
ask afterwards (the corpus is full of post-hoc "is my language a fraud?" threads).

**Maturity.** `speculative` as a *practice in this corpus*: the ask is well posed, and no
thread in it reports actually deciding a design by the property rather than by feel.

**Tried by.** Nobody in the corpus; the real practice lives in the literature (proofs of
progress/preservation) rather than in these threads.

**Source.** How to evaluate type system ideas, score 38, 11 comments, 2020-05 —
https://www.reddit.com/r/ProgrammingLanguages/comments/gmqzlp/how_to_evaluate_type_system_ideas/
· What does it mean to have an "algebraic" type system?, score 98, 64 comments, 2023-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/10ewz92/what_does_it_mean_to_have_an_algebraic_type_system/

**Bearing on `fun`.** This is the project's own method, already written down: `docs/STATUS.md`
is authoritative over prose, every design decision has a topic doc that states its rule, and the
fog list is exactly "properties we have not named yet". The two fog items that would answer
this entry's question for `fun` are **formalized core semantics** (write `Core.term` /
`Core.value` / `eval` as a Lean 4 spec — the property becomes checkable) and
**universe levels** (`Type : Type` is the known property being knowingly traded away).

## Threads worth reading in full

- **Beyond Hindley-Milner (but Keeping Principal Types)** (86) — the best single map of which HM
  extensions preserve principality, with the papers named.
- **Infinite loops: a side-effect, or an implementation detail?** (78) — four shipped answers to
  one question, laid out by someone who needs to choose.
- **Why are product types so common while sum types are so rare?** (97, 115 comments) — the
  sums-vs-products asymmetry argued from the OO era's history.
- **Ad-hoc polymorphism is not worth it** (56) — the most concrete cost list for traits/type
  classes anyone in the corpus wrote; its comment tree carries a point-by-point rebuttal, which
  is the other half of the entry. Read with the Java and F# threads beside it.
- **Why You Need Subtyping** (71, link post, no body) — 26 retrieved comments carry the whole
  argument: nullable-collapse against union-based null, declared-subtypes-only, and refinement
  subtyping with a paper named.
- **It's not just "function overloads" which break Dolan-style algebraic subtyping** (43) — a
  working counter-example to a claimed identity, worked in code.
- **How impractical/inefficient will "predicates as type" be?** (43) — the information-loss
  problem with refinement types stated better than most papers' intros.
- **Dependent types and usability?** (64) — the question `fun` exists inside; no answer
  consensus, which is the finding.
- **Strict and lazy without littering lazy everywhere** (16, but the body is the point) — a
  real attempt at targeted laziness, with an n-queens example and a self-correction.
- **Why GADTs aren't the default?** (51) and **Compelling use cases for GADTs?** (13) — the
  sum-type-with-refinement question from both directions; not covered as an entry above.
- **Any info on ordered type systems?** (39) — a good example of how thin the applied literature
  is outside papers.

## Gaps and disagreements

**Coverage improved, but it is still shallow.** Comment trees were fetched for the 20 richest
threads of this axis — 520 comments, 26 per thread — so the disagreement under the biggest
threads is readable now where the fetch reached: ad-hoc polymorphism, sums, refinements and
contracts, subtyping, dependent types and borrow checking all carry both sides in their comment
trees. But each fetch returns only ~30 top-level comments regardless of `limit=100`; the
remainder sits in unretrieved `more` placeholders or simply past what the fetch returned, and
a thread listed
at 115 comments contributes its top 26. The threads most likely to contain what is still missing
are link posts whose trees were *not* fetched: "Traits are a Local Maximum" (63), "HM vs
Bidirectional" (94) and "Designing type inference for high quality type errors" (72) — title,
score, and nothing to read. "Why You Need Subtyping" (71) was recovered this round; its comments
are cited throughout below.

**What the corpus visibly disagrees about.** (1) Whether ad-hoc polymorphism pays — six cost
claims on one side, four large production languages plus Java adopting it on the other. (2)
Whether gradual typing should be sound or cheap — a paper's "not tolerable" overhead against
TypeScript's deployment. (3) Global coherence vs scoped instances — settled per language, never
settled between them, and `fun` has already ruled on the grounds that its modules are values.
(4) Divergence's meaning — four shipped answers, no thread reconciling them. (5) Null: the
corpus keeps re-asking the same question (three separate threads, 2016, 2023, 2024) without
converging.

**What I could not verify.** I did not read any paper or implementation cited inside a thread —
Dolan's thesis, Dunfield–Krishnaswami, the mtc paper, "Is Sound Gradual Typing Dead?", CMU's
ordered-types notes — so every claim about what they prove is *second-hand through a Reddit
post*. Maturity tags are my judgement against the shipped/research/speculative/contested
rubric, not a measurement. Hobby projects (Clape, Skiff, Sophie, Ante, Passerine, CCore) are
cited as "an idea is being tried", never as "this works"; at least one of them is explicitly
LLM-assisted.

**What would settle the open ones.** Algebraic subtyping: 1SubML's polynomial-time claim
checked against a real library. Gradual typing: a language that is both implicit-`Any` and
sound, deployed. Coherence with first-class modules: Scala's companion-object rule or OCaml's
modular implicits, implemented and used. Dependent-type usability: not decidable from threads —
it needs a prelude written by someone other than the language's author. And the effect-row /
handler material deliberately excluded here belongs to `effects-and-handlers.md`.

## Dissent and corrections

Corrections the comment pass made to claims the post-body pass had written down:

- **Sums vs products.** The entry said OO-era subtype polymorphism absorbed the sum's job. The
  top comments on the same thread argue the deeper cause is C's memory model (products are byte
  concatenation; sums need a tag plus worst-case padding), and graydon2 records that unions
  existed in Modula/Ada/Pascal/C all along — what came and went was the *checked discriminant*.
  The entry now carries both diagnoses instead of the post's alone.
- **Ad-hoc polymorphism.** The post's "exponentially increases compile-time" cost is contested
  in its own comment tree (HM inference is already worst-case exponential; Haskell-98-scale type
  classes ran fast on 1990s hardware), and its author's claim of rarely using `+` was answered
  with counts (~1100 in C Lua, ~2800 in SQLite3, ~750 in one compiler). Both the claim and the
  rebuttal now sit in the entry; the compile-time charge is the weakest of the six on this
  evidence.
- **Gradual typing.** A commenter who spent ten years on Dart as it was migrated off optional
  typing reports it was "really the lowest common denominator" and that almost all users were
  happier once the system became sound — practitioner experience for one side; it is now in the
  entry, which had only post bodies.
- **Principal types.** The `**Costs.**` field flagged as missing on "Principal types as an
  invariant" does not reproduce — it was present; it has been strengthened instead, with the
  full-inference thread's counter-comments (CubiML's author on the ceiling of inference; an
  OCaml practitioner on inference assuming no mistakes).

Live disagreements recorded with both sides named: set-collapsing vs explicit absence in
nullable types; annotations-as-conversation vs full inference; borrow-checker rigidity vs its
in-thread defence; ad-hoc polymorphism's worth. Not resolved with the material available:
whether subtyping is *necessary* (the dedicated structural/nominal threads were not fetched —
only "Why You Need Subtyping"'s tree was read, which is one side plus its rebuttals), and
whether error quality or inference completeness should win (its thread is a bodyless link post
whose tree was not fetched). Nothing here was verified against a paper or an implementation —
Dolan, Dunfield–Krishnaswami, Freeman–Pfenning, the structural-refinement-types paper, "Is
Sound Gradual Typing Dead?" are all second-hand through Reddit — and the deeper `more`
placeholders were not expanded, which this pass had no network to do.

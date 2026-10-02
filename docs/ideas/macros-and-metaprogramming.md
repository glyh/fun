# Macros and metaprogramming — code that writes code

This axis covers code that writes code from inside the language: macro systems (hygiene
models, pattern form vs arbitrary code, quotation and syntax objects), when expansion runs
(before, after, or interleaved with type checking), staging and compile-time evaluation
(`comptime`, partial evaluation, reflection over declarations), the alternatives people
prefer instead of macros (external source generation, compiler plugins, attributes,
generics, deriving) and the costs each of those carries, and the tooling side: diagnostics
and IDE support for macro-heavy code. Out of boundary: how macro syntax itself is read —
that is `syntax-and-parsing.md`; hygiene or staging forcing a notation requirement is the
one exception.

## How this was gathered

The corpus is 2538 threads from r/ProgrammingLanguages and r/Compilers (2009-01 to
2026-09, mostly 2017+), collected via Reddit's JSON endpoints; this axis's slice holds 51
threads sorted by comment volume × score × body length, plus corpus-wide greps over
`hygien`, `comptime`, `codegen`, `metaprogram`, `reflect`, `derive`, `staging`, `quote`,
`plugin`, and friends. Comment trees were then fetched for the 19 richest threads of this
axis, yielding 474 comments. Each fetch returns only ~30 top-level comments regardless of
`limit=100`, with the remainder left unretrieved in `more` placeholders (96 of the 98
comments in one fetched tree, for instance), so every entry below reads the top of each
tree, not the whole of it; two comment fragments from other slices also survive (cited
where used). The corpus is
what Reddit upvoted, not a survey of the field: several high-scoring threads are
self-promotion for hobby languages (Passerine, Nomsu, Capy, easyjs, PreC, basil, poof),
usable as evidence that an idea is being tried, never that it works.

## The ideas

### A macro call is written like a function call

**What it is.** One invocation syntax serves macros and functions; nothing in the text
says which one a name is. The binding's kind — value or macro — decides during expansion.
The alternative, taken by Rust and Julia, marks macro uses distinctly (`m!(…)`, `@m`).

**Buys.** One application grammar, one precedence table, one namespace: a macro can carry
a fixity without a sigil, and a reader or tool needs exactly one rule instead of a
macro/function syntax split.

**Costs.** The source no longer announces that a rewrite will happen, so tooling must
resolve the binding kind before it can explain the code, and a misspelled macro name
fails differently from a misspelled function name — the marked syntax buys an honest
warning at the price of a second call form. The comment tree turns this into one
variable: u/XDracam argues the distinction matters in proportion to how poor the tooling
is (names carried `b_hasValue`-style hints when nobody could jump to definition), u/dnabre
reports Rust deliberately broke from C's uniform look because systems programmers want
callers that affect control flow marked, and u/TheUnlocked answers that hygiene already
makes the kind irrelevant. u/matthieum names the two differences that survive hygiene: a
macro can return early from its caller and can introduce named items into the caller's
context.

**Maturity.** contested — both forms ship (Lisp and Nim uniform, Rust and Julia marked),
and this thread's comments argue which is right, with tooling as the deciding variable;
the uniform side only wins if expansion UX carries the weight, which is fun's side.

**Tried by.** The Lisp family ("in most Lisps it's impossible for the caller to
distinguish" — the thread's own framing); Rust and Julia take the marked side; Nim shares
one syntax, one type checker and one representation between macros and functions, which
is how its author implements language features without compiler changes (comment
u/SultanOfSodomy).

**Source.** Should calling a macro look different than calling a function? — score 43, 45
comments, 2023-10 —
https://www.reddit.com/r/ProgrammingLanguages/comments/17dveoo/should_calling_a_macro_look_different_than/
· the marked side spelled out (Rust `macro!(…)`, Julia `@macro`) — Annotating literal
code (as opposed to macros) — score 20, 8 comments, 2025-02 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1itzcn1/annotating_literal_code_as_opposed_to_macros/
· comments on the first thread: u/LobYonder, u/XtremeGoose, u/XDracam, u/dnabre,
u/matthieum, u/TheUnlocked, u/SultanOfSodomy (on that permalink).

**Bearing on `fun`.** Already has it: `@` was removed, macros are invoked as `f(args)`,
and macros live in the scope-aware binding table with a Value/Macro kind tag —
`unify-macro-call-syntax-with-functions`, closed and implemented (commit `0441c9a`).

### Hygiene by sets of scopes, not by fresh names

**What it is.** Identifiers carry sets of scopes; an occurrence resolves to the binder
whose scope set is the largest subset of the occurrence's, and ambiguity is a loud error.
Nothing is renamed — introduction of a scope (one per application, on written syntax and
on received syntax) is what distinguishes names, so freshness is structural.

**Buys.** Capture behaviour becomes decidable by comparing sets instead of by counter
bookkeeping, and a definition-site reference works because the quote carries the
definition scopes rather than a name that happened not to collide.

**Costs.** Resolution is subset search over sets, and errors speak of sets — harder to
debug than rename-based hygiene. Writing a first expander without the model is visibly
painful in the corpus: the Scheme implementer reaches for a fresh name per expansion and
cannot make it resolve.

**Maturity.** shipped.

**Tried by.** Racket's scope-set model [general knowledge, not from corpus]; Passerine
ships a hygienic macro system (hobby language — interest, not viability); fun implements
sets of scopes; Dylan ran Scheme's hygienic-macro research over an Algol syntax (comment
u/nostrademons, cited below).

**Source.** What extensions to scheme are required to have hygienic macros? — score 5, 3
comments, 2020-04 —
https://www.reddit.com/r/ProgrammingLanguages/comments/fyqhpw/what_extensions_to_scheme_are_required_to_have/
· Ideas on how to break hygiene? — score 6, 13 comments, 2020-06 —
https://www.reddit.com/r/ProgrammingLanguages/comments/gwo212/ideas_on_how_to_break_hygiene/
· PLDI 2021: Hygienic Macro Technology — score 7, 1 comment, 2022-02 —
https://www.reddit.com/r/ProgrammingLanguages/comments/std6j7/pldi_2021_hygienic_macro_technology/
· named in comments — Rethinking macros. How should a modern macro system look like? —
score 30, 22 comments, 2024-10 —
https://www.reddit.com/r/Compilers/comments/1fybxt9/rethinking_macros_how_should_a_modern_macro/

**Bearing on `fun`.** Already has it: sets-of-scopes resolution (largest subset wins,
ambiguity loud), one hygiene contract at `Expand.application` minting an intro scope and a
use-site scope per application, and the string-built/generated-symbol id rejected outright
(`core-tt-domain-model-macros.md` M2/M10).

### The one deliberate hygiene break: borrow an id's scope set

**What it is.** Instead of a marker that flips a name to use-site resolution (`'baz`), an
`implicit` parameter in the signature, or an `!`-suffixed always-unhygienic macro, the
break is construction: build an identifier whose scope set is copied from another syntax
object the macro already holds.

**Buys.** One name-resolution rule stays intact — the escape is a value operation, visible
in the macro body — and since a scope set has no literal syntax and no primitives, a
macro cannot forge a match for an unrelated binder.

**Costs.** The break is invisible at the use site: nothing in the call says which names
leak, and the macro must hold the id it borrows from (typically a parameter), so it
cannot reach for an ambient name the way the unhygienic variants allow.

**Maturity.** research — implemented in fun's port; the marker/implicit variants are
argued in one thread, not built.

**Tried by.** fun (`Borrowed context`); nobody else in this corpus has shipped this
shape.

**Source.** Ideas on how to break hygiene? — score 6, 13 comments, 2020-06 —
https://www.reddit.com/r/ProgrammingLanguages/comments/gwo212/ideas_on_how_to_break_hygiene/

**Bearing on `fun`.** Already has it, named *Borrowed context* — "the one deliberate way
to break hygiene" (CONTEXT.md). The spelling-based variants are rejected:
`resolved-names-forgeable` (closed) and M10's ruling that strings build no hygienic
syntax — the generated-symbol and spelling-resolution routes (Common Lisp, Clojure) are
rejected by name.

### Quotation is parsed and checked where it is written

**What it is.** `quote(…)` is read at the macro's definition site, so its shape and its
names are fixed there; holes `$e` fill positionally with their kind fixed by the parse
(`Expr`, `Pattern`, `Decl`, `Id`, `Block`), and splicing a value of another kind is an
error at the splice. The stricter variant in the corpus typechecks under quotation, so
`'[Expr | (plus one ,[e1],)]` is a type error when `e1` is wrong.

**Buys.** No caller's operator or syntax form can reach inside the quote, a rule cannot
silently depend on syntax its user declared, and hole mismatches are caught without
running the macro.

**Costs.** The price fun records explicitly: a template can no longer rely on syntax its
user declares — `$x ** 2` where only the caller declared `**` is an error at the
definition rather than a silent dependency — and the macro's author must know the parse.

**Maturity.** research (fun's kinded holes enforced; Unseemly's typed quotation is a
prototype admitting "a bunch of missing features").

**Tried by.** fun; Unseemly; Scala 3, where staying inside the quote API makes the
compiler check generated code (comment u/RiceBroad4552, source below).

**Source.** Unseemly: a typed macro language — score 67, 15 comments, 2020-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/eq26iu/unseemly_a_typed_macro_language/
· typed quotation named in comments — Implementing "comptime" in existing dynamic
languages — score 32, 39 comments, 2025-10 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1og5av5/implementing_comptime_in_existing_dynamic/

**Bearing on `fun`.** Already has it: `Quoted syntax` parses at the definition site
(M10, implemented), holes are reflection types checked at the splice, and the round trip
through reflection must be the identity (M1).

### Expansion interleaved with elaboration, one binding at a time

**What it is.** There is no full expand pass followed by a checking pass: each top-level
binding is expanded against everything elaborated before it, in source order, through one
deterministic queue. A macro whose annotation names a type is deferred to the elaborator —
its arguments travel as syntax objects, its binders become metas, and its output is
expanded in place like any other macro's.

**Buys.** Type-aware macros resolve their annotations against real prior declarations;
generated declarations re-enter expansion in order; the compiler is one ordered queue
rather than two passes that must agree about what a name means.

**Costs.** The handshake is the hard part — split unresolved/resolved annotations,
per-binding advancement, provisional registration with rollback — and a deferred call
sitting inside syntax that effect collection walks aborts that walk (recorded distance:
`effect-collection-rejects-deferred-macro-calls`). The comment tree adds the price this
class of design pays somewhere: expansion needs something that runs macro code, so either
an interpreter sits beside the compiler or the expander must invoke `eval` (comment
u/Public_Grade_2145 on Static Metaprogramming, a Missed Opportunity?), and a macro system
that needs types before they exist has to wait for them — a Julia maintainers discussion
is recalled for exactly that gap (comment u/vanderZwan on basil: A Lightweight
metaprogramming language).

**Maturity.** shipped for the interleaving itself — u/LPTK's comment: Scala 3 macros
"expand during type checking/elaboration, so they can use and influence types as they
proceed"; fun's per-binding queue with the deferred-annotation handshake remains research.

**Tried by.** fun (Stages 1–9 plus the type-aware handshake, implemented); Scala 3
(expansion during type checking — comment u/LPTK); Nim passes already-typed expressions
to macros (comment u/ipe369); Klister's blocked-task interleaving is the corpus-visible
candidate list entry; Turnstile embeds type checking in macro definitions.

**Source.** Resources on statically typed hygenic macros? — score 12, 12 comments,
2023-08 —
https://www.reddit.com/r/ProgrammingLanguages/comments/15fs9pu/resources_on_statically_typed_hygenic_macros/
· Type Systems as Macros — score 7, 0 comments, 2017-04 —
https://www.reddit.com/r/Compilers/comments/673yck/type_systems_as_macros/
· comments: u/LPTK (What are good examples of macro systems in non-S-expressions
languages?, https://www.reddit.com/r/ProgrammingLanguages/comments/1elrpbz/), u/ipe369 and
u/vanderZwan (basil, https://www.reddit.com/r/ProgrammingLanguages/comments/jfqdxg/),
u/Public_Grade_2145 (Static Metaprogramming,
https://www.reddit.com/r/ProgrammingLanguages/comments/1m022pe/).

**Bearing on `fun`.** Already has it: expansion interleaved with elaboration per binding
(Klister minus suspended expansions), type-aware macros deferred to the elaborator with
explicit type binders (`macro-type-binders-should-be-explicit`, closed), and
`type-aware-macro-output-is-not-expanded` closed with output expanded in place.

### Check the macro once, at its definition

**What it is.** Give a macro a signature — type binders, parameter types, a promised
output type — elaborated where the macro is defined; uses are checked against the
signature. Unseemly's stronger claim: if the invocation typechecks, the expansion cannot
contain type errors, so macros feel exactly like built-ins.

**Buys.** A macro mistake becomes an ordinary error at the definition, and generated code
stops being the place where type errors surface far from their cause — the prerequisite
for an ecosystem of macro-defined languages sharing libraries.

**Costs.** The promise must be enforced somewhere: trust it and the guarantee rests on the
macro's own checking; re-check the output and you pay per use. Unseemly's prototype says
the guarantee is incomplete modulo one unimplemented feature and bugs, and the language is
"hard to program in".

**Maturity.** research.

**Tried by.** Unseemly (prototype); fun; Scala 3's quote API, which enforces type safety,
hygiene and stage consistency of generated code as long as the author stays inside it
(comment u/RiceBroad4552, source below).

**Source.** Unseemly: a typed macro language — score 67, 15 comments, 2020-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/eq26iu/unseemly_a_typed_macro_language/
· Resources on statically typed hygenic macros? — score 12, 12 comments, 2023-08 —
https://www.reddit.com/r/ProgrammingLanguages/comments/15fs9pu/resources_on_statically_typed_hygenic_macros/
· Scala 3's layered facilities quoted in comments — Implementing "comptime" in existing
dynamic languages — score 32, 39 comments, 2025-10 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1og5av5/implementing_comptime_in_existing_dynamic/

**Bearing on `fun`.** Has it differently: the macro annotation is a pi type elaborated
where the macro is defined (`macro m[A](x) : Expr(T)`, explicit type binders), the call is
deferred and its binders must all solve ("cannot infer A for `default`"), and the output
is *checked* at the promised type rather than trusted (M6).

### Comptime: ordinary code evaluated as a compilation step

**What it is.** A `comptime { … }` block is normal code the compiler runs, with the result
embedded — compile-time evaluation as partial evaluation (the Futamura framing the thread
opens with). The pragmatic implementation is direct evaluation plus serialization: the
compile-time run is one program and runtime continues it. Capy JITs the block as its own
function and embeds the resulting bytes in the data segment.

**Buys.** No second macro language: loops, arrays and arithmetic at compile time under the
host's own semantics, and results baked in (`log10 = ln(x) / comptime { ln(10) }` in
Capy; C3's compile-time `$foreach` iterating string literals).

**Costs.** Compilation acquires a second execution semantics and a tooling workflow — on
dynamic hosts the thread calls it a cultural shift whose hard part is "convincing people",
and the comments confirm that first-hand: GabrielDosReis, who did `constexpr` for C++,
writes that the real hurdles were "barely technical" and the resistance was cultural
(comment on that thread, citing his SAC 2010 paper). The corpus's own counter:
"C++'s templates and Zig's comptime are no replacement for parametric polymorphism" —
now argued inside the thread below.

**Maturity.** shipped.

**Tried by.** Zig (its `std.builtin.Type` / `@Type` quoted verbatim in the source-gen
thread), Capy (hobby), C3 (`$foreach`); Common Lisp (`eval-when`, named by
GabrielDosReis, and compiler macros that rewrite calls for the optimizer — comment
u/Soupeeee on Macros good? bad? or necessary?); D, whose CTFE engine began life as a
constant folder (comment u/alphaglosined); Raku, whose `BEGIN` phasers run at compile
time without new syntax (comment u/alatennaub).

**Source.** Implementing "comptime" in existing dynamic languages — score 32, 39 comments,
2025-10 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1og5av5/implementing_comptime_in_existing_dynamic/
· Capy, a compiled programming language with Arbitrary Compile-Time Evaluation — score 88,
42 comments, 2023-09 —
https://www.reddit.com/r/ProgrammingLanguages/comments/16cs6js/capy_a_compiled_programming_language_with/
· References for the theory behind Zig's comptime? — score 51, 20 comments, 2022-05 —
https://www.reddit.com/r/ProgrammingLanguages/comments/uz8ffq/references_for_the_theory_behind_zigs_comptime/
· counter: Unpopular Opinions? — score 158, 417 comments, 2020-10 —
https://www.reddit.com/r/ProgrammingLanguages/comments/jd30p7/unpopular_opinions/
· C3 0.6.6 Released — score 50, 19 comments, 2025-01 (compile-time `$foreach`) —
https://www.reddit.com/r/ProgrammingLanguages/comments/1i2nqy8/c3_066_released/
· comments on the first thread: u/GabrielDosReis (the `constexpr` author), u/alphaglosined,
u/alatennaub; on Unpopular Opinions?: u/Soupeeee (Common Lisp compiler macros).

**Bearing on `fun`.** Genuinely new: no `comptime` construct is on the map. `fun`'s
compile-time work is elaboration and NbE under the one evaluation budget shared with the
checker, and macros are syntax → syntax. A comptime block with build side effects would
land in the fog item "the library-vs-compiler-machinery boundary".

### Decide what compile-time code may not do

**What it is.** The sandbox policy for compile-time evaluation: forbid IO by default and
grant it through explicit capabilities (a dependency fetch is permission to reach a
host); treat divergence as a timeout producing a compile error rather than banning loops.
The same thread supplies the counter-argument: dependency management *is* IO at compile
time, and the C++ committee wants the whole breadth of the language available at compile
time.

**Buys.** Compilation stays bounded and reproducible without giving up the breadths that
make comptime worth having, and a compile-time failure is an error value rather than a
hang.

**Costs.** Capabilities are another policy surface to specify and audit; timeouts make a
program's acceptance depend on how fast the machine is. The comment tree argues both
sides with reasons rather than instincts: u/MattiDragon wants compilation to be a pure
function of source and compiler options — no clock, no system configuration, no network;
u/evincarofautumn gives the mechanism, that nothing immediately goes wrong with arbitrary
IO but it wrecks reproducible builds, caching, portable compilation and cross-compilation.
Against: npm, cargo and make already fetch over the network, so banning IO inside comptime
while the build system performs it is inconsistent (u/Norphesius); Rust proc macros can do
IO while Zig's comptime cannot, and Jai's `#eval` just runs the string (u/drewftg); F#
type providers and C# analyzers already do network IO during compilation (u/useerup). The
one concrete middle design offered is Zig's `build.zig`: a pure function mutates a build
graph and the compiler performs the IO (u/not-my-walrus).

**Maturity.** contested — one thread now holds both sides; the pure-by-default side has
the reproducibility mechanism, the permissive side the build-system precedent.

**Tried by.** Nobody named in the corpus ships the capability list; fun rejects
termination checking outright.

**Source.** What would you leave out of comptime? — score 21, 42 comments, 2026-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1qbq1n9/what_would_you_leave_out_of_comptime/
· Build processes centered around comptime. — score 3, 13 comments, 2025-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1hsa6j0/build_processes_centered_around_comptime/
· comments on the first thread: u/MattiDragon, u/not-my-walrus, u/Norphesius, u/drewftg,
u/useerup, u/matthieum (offline and reproducible builds).

**Bearing on `fun`.** Rejected on the termination half: no nesting limit and no
termination checking — expansion spends from the one evaluation budget, and exhaustion is
an error value naming the call (`macro-fuel-is-the-evaluation-budget`, closed; the budget
also catches breadth blowup a nesting guard never trips). The IO half is not on the map;
if it ever lands, the build-graph split above is the shape it would have to take.

### Parametricity or comptime: what a signature is allowed to promise

**What it is.** The charge — a link post titled Noel Welsh: Parametricity, or Comptime is
Bonkers — is that compile-time type inspection destroys parametricity, the reading that
`id<T>(x: T) -> T` can only return `x`. The comments answer it three ways. Idris2
quantities: mark the inspected parameter with multiplicity 0 and it cannot be pattern
matched on, so the type stays inspectable and still parametric (`id : {0 T : Type} ->
T -> T`, u/Syrak). A signature annotation: `comptime T` says what a typeclass constraint
says — specialization may happen here (u/tbagrel1). And the dismissal: parametricity is
already broken by impurity and divergence, a signature never determined its implementation
anyway (every formula has more than one proof), so the real fault is single responsibility,
not comptime (u/Red-Krow) — his example being `serialize : T -> JSON`, easy with comptime
and possible with typeclasses only if the language derives the instance.

**Buys.** Both sides buy the same thing, honestly stated: a place in the signature where
the reader is told not to expect parametricity, plus one serializer written for every type
instead of one per type.

**Costs.** u/SwingOutStateMachine's: comptime "combines both macros and traits into one
concept, which is difficult to disentangle and reason about", where `unsafe`-style
fragments at least mark where the dragons are. The quantity route costs a type language
that tracks how each variable is used.

**Maturity.** contested — one thread, argued on both sides with mechanisms; the quantity
compromise itself ships.

**Tried by.** Idris2 (quantities, shipped); Haskell's typeclasses as the counter-example;
Zig as the target of the critique; nobody in the corpus resolves it.

**Source.** Noel Welsh: Parametricity, or Comptime is Bonkers — score 54, 34 comments,
2026-03 (link post, no body) —
https://www.reddit.com/r/ProgrammingLanguages/comments/1rrgyl3/noel_welsh_parametricity_or_comptime_is_bonkers/
(comments: u/klekpl, u/Syrak, u/tbagrel1, u/Red-Krow, u/SwingOutStateMachine,
u/marshaharsha, u/oldretard).

**Bearing on `fun`.** Decided the other way, not fog: types are values and type-case over
open `Type` is acceptable, so fun never promises this flavour of parametricity — the
corpus's parametricity defence is already answered by ruling. Erasure quantities are not
on the map (genuinely new if ever wanted).

### Source generation outside the language instead of in-language metaprogramming

**What it is.** A generator program — written in any host, run natively, rerun only when
inputs change — emits source files that the compiler then compiles. Reflection becomes
text templates instead of syntax manipulation, and the generator can emit anything: a 3D
exporter writing a hardcoded array into a C file, for instance.

**Buys.** Per the thread: generators are debugged with ordinary tools and debuggers, the
output is inspectable text, they can be run manually outside every compilation, and they
do not slow the compiler the way an interpreted metaprogramming VM does (the thread's
charge against Zig's compile-time evaluation).

**Costs.** A separate build step, output that lives outside the type system and outside
hygiene, and drift between generator and call sites; the opposing side of the thread adds
that procedural metaprograms read as "mechanical" either way. The comment tree argues it
properly: the top comment calls the post's preference "a failing of the language, not a
universal truth" (u/The_Northern_Light); u/apocalyps3_me0w answers that generated output
with no IDE support is easier to get wrong, plus another build step; u/Smallpaul asks how
three or four transformations are supposed to compose. The speed charge does not survive
its own thread: u/pauseless notes generators pay the same rerun cost ("the codegen cost is
the same, no?"), and u/DokOktavo supplies the mechanism — Zig's comptime is interpreted and
its own tracking issue aims at roughly CPython-script speed, so the complaint is about
interpretation, not about in-language metaprogramming. u/needleful splits the verdict: for
compile-time reflection there is no reason to run half the compiler a second time in a
script; for pure syntax rewriting the external generator wins. Still no measurement on
either side.

**Maturity.** contested — both sides now argued inside one thread, with the speed claim
corrected (see Dissent and corrections).

**Tried by.** C/C++ hobbyists (the thread's author); F# type providers and C# source
generators as the in-toolchain middle ground; a corpus commenter reports generating a lot
of code for a real project and wanting it "more integrated with the main language".

**Source.** Unpopular Opinion: Source generation is far superior to in-language
metaprogramming — score 98, 97 comments, 2026-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1q17xo0/unpopular_opinion_source_generation_is_far/
· What's your ideal language feature? — score 55, 152 comments, 2018-11 (comment: emit
rewritten files as build artifacts, make generation testable) —
https://www.reddit.com/r/ProgrammingLanguages/comments/9um9nw/whats_your_ideal_language_feature/
· comments on the first thread: u/The_Northern_Light, u/apocalyps3_me0w, u/Smallpaul,
u/pauseless, u/DokOktavo, u/needleful, u/poralexc (Zig's build system has source
generation steps; "if you're using comptime for everything in Zig you're doing it wrong").

**Bearing on `fun`.** Not on the map: fun has no external generator and no second IR to
emit (a `Surface.t` IR was deleted on purpose). A tool consuming expansion output would be
a consumer of the fog item "first-class compiler API for tools/LSP/REPL".

### Compiler plugins hooked into the phases

**What it is.** The compiler exposes hooks — on parse, on analysis, on codegen — that a
(natively compiled) plugin implements; a plugin registers `#name` symbol handlers, may
install extra compilation steps between existing ones, and receives a context over
compiler state. Parsing of the payload can be delegated wholesale: input source text,
output a node.

**Buys.** New syntax and DSLs without changing the language; extension code runs natively
so expansion costs nothing; per-domain error messages come from the extension itself; the
approach ports onto an existing language unchanged.

**Costs.** The plugin sees compiler internals, so every internal change breaks it — the
thread itself offers "expose less state, for compatibility, at the cost of flexibility" —
and tooling must know which extensions are installed before it can read the code. The
phase hooks re-import the ordering problem that interleaved expansion already solved.
The thread's top comment also corrects the premise: every static language already has a
codegen step — Rust procedural macros, Go code generation, Zig comptime, C++ constexpr and
templates, Java annotation processors, C# source generators (u/PuzzleheadedPop567) — so
what is claimed as new is type ownership, which the author's own reply narrows to types
projected on first reference, incremental rather than "ocean boiling" (u/manifoldjava).

**Maturity.** research — the full hook set is argued, not built; the analysis-hook subset
is real in one project.

**Tried by.** Manifold (a Java compiler plugin: JSON/YAML/GraphQL/SQL as native types,
language extensions); the author previously built the `@name` hook surface into a C99
compiler (self-reported).

**Source.** A rare approach to metaprogramming — score 0, 8 comments, 2026-05 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1tkj4sd/a_rare_approach_to_metaprogramming/
· Static Metaprogramming, a Missed Opportunity? — score 74, 63 comments, 2025-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1m022pe/static_metaprogramming_a_missed_opportunity/
· comments on both: u/PuzzleheadedPop567, u/manifoldjava (the author), u/AlexReinkingYale
(Liquid Haskell and Gallina as statically typed languages without Turing-complete
compile-time evaluation).

**Bearing on `fun`.** Rejected as a seam: expansion's only handle on elaboration is the
fixed `IMacroRuntime` adapter, `Fun.Expand` cannot reference `Fun.Compiler`, and the
expander handle is a capability, not a context (`expander-handle-is-a-capability-not-a-context`).
A wider compiler surface is the fog item "first-class compiler API"
(`topics/first-class-elaborator-api.md`), which sharpens when the first real downstream
consumer appears.

### Compiler control: a dialectless core with the meaning in userland

**What it is.** Split the language in two: a "dialectless form" that is only directives and
raw machinery, where nothing of the language is defined in userland, and the real language,
which userland metaprogramming defines on top — in the thread's own example its language
has no null literal and no boolean literals, both are compile-time constants written in
userland. The neighbouring proposal in the same thread: make the intermediate
representations and compiler passes explicit and give users control over them
(u/Uncaffeinated), who admits he does not know an ergonomic way to do it.

**Buys.** Semantics become library decisions the user can change without compiler changes,
and the design gets a complexity test it applies to itself: if a feature would cause a
complexity explosion in the compiler, look hard for the simpler feature that covers 80%
of it (u/PL_Design's stated rule), with extra features hideable behind libraries when users
do not want them.

**Costs.** Everything the dialect author carries: the dialectless form is bare directives,
tooling must know the dialect before it can read the code, and the thread offers no
ergonomic answer to exposing passes. The evidence is one hobby language, self-reported.

**Maturity.** speculative — argued in one thread, implemented only in its author's own
language (self-reported, unreleased).

**Tried by.** Nobody has shipped this as stated; u/PegasusAndAcorn points at Rust and Lisp
dialects as the deep-primitives-plus-library shape, u/zachgk at his own `choice` mechanism.

**Source.** Metaprogramming vs. compiler control. — score 55, 29 comments, 2021-03 —
https://www.reddit.com/r/ProgrammingLanguages/comments/mf15cy/metaprogramming_vs_compiler_control/
(comments: u/PL_Design, u/Uncaffeinated, u/PegasusAndAcorn, u/raiph, u/zachgk).

**Bearing on `fun`.** Open, and deliberately narrow: userland meaning comes from Stage 11
demotion into prelude macros, not from exposed passes — expansion's only handle on
delaboration stays the fixed `IMacroRuntime`, and the expander handle is a capability, not
a context. The pressure this idea creates lands on the fog item "first-class compiler API
for tools/LSP/REPL" (`topics/first-class-elaborator-api.md`).

### Syntax as a value: reflect the whole grammar to macros

**What it is.** Macros inspect and build syntax through reflected types with one
constructor per form, covering the whole grammar — nothing opaque. Destructuring and
rebuilding is a round trip that must be the identity: name, span, scope and every payload
field survive, so a macro that merely reflects and rebuilds changes nothing.

**Buys.** A macro is an ordinary program over data instead of a string or node API;
anything the reflection cannot decompose is a hole you can find by testing the round trip
rather than a boundary you must design around; tooling can ask the compiler for the
program's structure instead of re-parsing it.

**Costs.** The reflection types become a public contract — every new form is a breaking
change for macros — and total reflection leaves no escape hatch: partial decompositions
must be loud errors. fun found what that costs when the round trip was first pinned: five
fields silently lost in either direction plus four silent degradations. The comment tree
says the same from the outside: introspection and rebuilding syntax by parts is "the
section you need to solve first", and where introspection is not first class — Rust —
macro authors end up hijacking a compiler pass (comment u/mamcx, top comment on What are
good examples of macro systems in non-S-expressions languages?,
https://www.reddit.com/r/ProgrammingLanguages/comments/1elrpbz/).

**Maturity.** research.

**Tried by.** fun (enforced, pinned by a test over a varied program before and after
expansion); Racket's syntax objects are the precedent [general knowledge, not from
corpus]; Klister appears in the corpus only as a candidate list entry.

**Source.** Why don't most programming languages expose their AST (via api or other
means)? — score 52, 29 comments, 2024-03 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1blldxz/why_dont_most_programming_languages_expose_their/
· Why have an AST? — score 58, 33 comments, 2022-06 —
https://www.reddit.com/r/ProgrammingLanguages/comments/vgekk2/why_have_an_ast/

**Bearing on `fun`.** Already has it: Reflection is total and the round trip is the
identity (M1, enforced); what still rides as an undecomposed `Core.StxExpr` is scaffolding,
not a boundary. Exposing the same machinery to out-of-process tools stays fog
(first-class compiler API).

### One macro path: pattern rules are sugar for a macro

**What it is.** A pattern→replacement rule declared by `syntax` or `pub infix` is not a
second mechanism: its rules are data on a syntactic role, a use is filled through the same
application as any other macro, and the template keeps exactly one job — the parse
(which tokens a use consumes, what each hole captures). Everything after is a macro whose
parameters are the captures and whose body is quoted syntax.

**Buys.** One hygiene contract holds because there is one path, not because several
implementations agree; the pattern form inherits the power of a macro written as code,
and the code form inherits the pattern parser, for free on both sides.

**Costs.** The template author must understand both jobs (parse and macro), and capture
extents are structural — a trailing hole reads to its form's order, a hole followed by `,`
or `;` reads to it — a rule users must be taught rather than guessed.

**Maturity.** research for this shape; macro-by-example is the shipped ancestor.

**Tried by.** fun; Unseemly ships "Macro By Example" (n-ary forms without boilerplate
loops); the pattern-form/arbitrary-code split itself is the corpus's Racket-family
standard. The non-S-expression thread's commenters name the shipped answers — Nim's
macro/template/tree-rewrite/pragma spectrum (u/MegaIng), Rhombus, whose class system is
macros (u/AlarmingMassOfBears), Crystal, Elixir, Dylan — with u/lookmeat's counter that
Rust's procedural macros are "clunky and very hard to use" and behave "more like compiler
plugins rather than macros".

**Source.** Unseemly: a typed macro language — score 67, 15 comments, 2020-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/eq26iu/unseemly_a_typed_macro_language/
· What are good examples of macro systems in non-S-expressions languages? — score 44, 38
comments, 2024-08 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1elrpbz/what_are_good_examples_of_macro_systems_in/
· Rethinking macros. How should a modern macro system look like? — score 30, 22 comments,
2024-10 —
https://www.reddit.com/r/Compilers/comments/1fybxt9/rethinking_macros_how_should_a_modern_macro/

**Bearing on `fun`.** Already has it: a template is sugar for a macro (M9, implemented,
`templates-desugar-to-macros` closed) — the template keeps the parse, the hole kinds are
the parameter types of the macro it expands to, and one hygiene contract covers both.

### Demote core constructs into library macros

**What it is.** Features the compiler builds itself become prelude macros over machinery
that already exists: `Bool` becomes a nominal type with `False | True` and `if` becomes a
`match` form, so the compiler shrinks while the macro system is forced to be good enough
to carry the language's own core.

**Buys.** One construct doing several roles; every demoted feature doubles as a worked
example users can read; compiler special cases (`Core.If`, `FIf`) disappear into
library code that tests cover the same way as any other library.

**Costs.** Bootstrap — anything used before the prelude loads cannot be demoted — and the
quality of the macro path (diagnostics, performance) becomes the language's quality. The
corpus's standing objection to macro-carried features is that they become "a whole
sublanguage to maintain, hard to code for the compiler's dev, hard to use for the final
user". The comments argue both halves: u/matthieum grants that macros cover gaps but
denies that makes the language badly designed — a feature starts at a negative score to
account for the burden it creates, and a well-thought macro system unblocks users now —
while u/realbigteeny answers the demotion case directly: if a switch is a good macro use
case, why not add switch, "now everyone will implement their own incompatible switch"
(comment on Rethinking macros, https://www.reddit.com/r/Compilers/comments/1fybxt9/).

**Maturity.** contested — now argued inside one thread rather than inferred from opposing
posts.

**Tried by.** fun (increment 1: `Bool` + `if`, 778 tests green); Unseemly implements `if`,
function definitions and pipes as macros.

**Source.** Macros good? bad? or necessary? — score 54, 96 comments, 2025-08 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1n41akt/macros_good_bad_or_necessary/
· Unseemly: a typed macro language — score 67, 15 comments, 2020-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/eq26iu/unseemly_a_typed_macro_language/
· Macros in 22 languages — score 57, 26 comments, 2023-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/10dfzhn/macros_in_22_languages/

**Bearing on `fun`.** Open ticket: `specify-stage-11-macro-powered-language-features`
(direction decided — demote into the library; increment 1 done). Further increments are
that ticket's to sequence, not new proposals.

### The macro system as a dumping ground (the anti-pattern)

**What it is.** When a design does not want to decide something, it becomes a macro:
`self`, imports, inheritance, validation, serialization — until the annotation mechanism
is a kitchen sink where macros, a native escape hatch and dependency declarations all
live, and the two things that cannot be macroed (bootstrapping and network access) become
special cases outside the language.

**Buys.** Nothing, in the end — it buys shipping today. This entry is the warning half of
the macros debate: the same mechanism that demotes core constructs thoughtfully demotes
undecided features thoughtlessly.

**Costs.** As observed: syntax grows heterogeneous, non-macroable concerns become
off-language keywords "that are not even in the language", and the macro system inherits
every deferred design decision.

**Maturity.** contested — a self-reported failure in one thread against the deliberate
demotion above; the corpus contains no measured case on either side.

**Tried by.** The poster's own unnamed hobby language (evidence of interest, not
viability).

**Source.** My macro design is doing too many things. — score 11, 8 comments, 2026-06 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1u1fmlt/my_macro_design_is_doing_too_many_things/
· the debate frame: Macros good? bad? or necessary? — score 54, 96 comments, 2025-08 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1n41akt/macros_good_bad_or_necessary/
· Are myths about the power of LISP exaggerated? — score 91, 100 comments, 2023-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/158iyza/are_myths_about_the_power_of_lisp_exaggerated/

**Bearing on `fun`.** The mirror image of Stage 11: demotion happens one increment at a
time under tests, and `macro-owns-its-output` (open, grilling) exists precisely so a
declaration macro's output is a decided rule rather than an accident — "decide this before
a second declaration macro exists".

### Expansion diagnostics: errors that name both sites

**What it is.** An error raised during expansion carries the macro's definition site and
the application's site, plus the chain of expansions that reached it. The one concrete
model in this corpus is the C preprocessor's diagnostics: `note: expanded from macro
'Concat3'`, `note: expanded from here`, printed once per frame down to the failing
expansion.

**Buys.** A failing macro reads as an error instead of a crash, and a reader can follow
what a macro did without running an expansion dump by hand. A comment puts the whole
design space on one axis (u/sciolizer): from most powerful to least — text rewriting, read
macros, brace-constrained procedural macros, unhygienic tree rewriting, hygienic tree
rewriting, reflection, no macros — which is also least predictable to most predictable,
with "how difficult is it to implement smart IDE features" as the objective test at each
level. That test is a fair statement of what Stage 12 owes.

**Costs.** Every diagnostic needs two locations and a trace, and an evaluation failure
inside a macro body must be attributed back to the application — work fun has already
paid (`macro-body-eval-errors-lack-site`, `budget-error-names-no-source-call`,
`expansion-errors-reach-the-user-raw`, all closed) with more to specify. The practitioner
cost is in the corpus too: in Haskell and Rust macros slow compilation and the LSP's
refresh, and generated code is hard to debug because "the bug may not be in small
examples" (comment u/omega1612 on Macros good? bad? or necessary?).

**Maturity.** shipped for preprocessors; unspecified in this corpus for hygienic systems.

**Tried by.** GCC/Clang `cpp` (the thread quotes the note chain verbatim).

**Source.** clang cpp vs apple clang cpp — score 0, 8 comments, 2020-07 (the expansion
notes are the usable content) —
https://www.reddit.com/r/Compilers/comments/i1i3qy/clang_cpp_vs_apple_clang_cpp/
· IDEs and Macros — score 58, 7 comments, 2021-11 (link post, title only) —
https://www.reddit.com/r/ProgrammingLanguages/comments/qz3725/ides_and_macros/
· comments on Macros good? bad? or necessary?: u/sciolizer, u/omega1612.

**Bearing on `fun`.** Open ticket: `specify-stage-12-macro-diagnostics-and-expansion-ux`
(open, unblocked 2026-09-26 — no rewrite is coming to make the effort disposable). This
entry is the corpus's evidence for what that spec owes: definition site + application site
+ expansion chain on every expansion failure.

### Make the compile-time information a binding sees explicit

**What it is.** Classify what a binding consumes at compile time — a type, a constant
value, the code producing a value, or foreign non-code such as an SQL/HTML template — in
order of power, and require it to appear in the signature, so macro-like power is visible
in the text. The post's corollary: a language using this approach should ban shadowing,
because an introduced identifier that shadows an outer one defeats inference from outer
information.

**Buys.** The difference between a function and a macro is readable at the call; tooling
can tell what is computed when without executing anything.

**Costs.** The corollary is expensive — banning shadowing breaks ordinary code — and the
four categories keep collapsing in real designs (a type is a value; a constant has a
type), so the taxonomy fights the language it is bolted onto. The comments reduce it to one
question — where does the signature say specialization may happen? u/tbagrel1 argues a
`comptime T` annotation and a `MyClass t =>` constraint are the same signal, both warning
that parametricity cannot be expected there; u/PL_Design shows a working hobby design
where the macro's own signature carries the answer — an operator macro whose promised
result type is whatever the expansion's type evaluates to.

**Maturity.** speculative — argued in one post, no implementation named.

**Tried by.** Nobody has shipped this as stated.

**Source.** Annotating literal code (as opposed to macros) — score 20, 8 comments,
2025-02 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1itzcn1/annotating_literal_code_as_opposed_to_macros/
· comments: u/tbagrel1 (Noel Welsh: Parametricity, or Comptime is Bonkers,
https://www.reddit.com/r/ProgrammingLanguages/comments/1rrgyl3/), u/PL_Design
(Metaprogramming vs. compiler control.,
https://www.reddit.com/r/ProgrammingLanguages/comments/mf15cy/).

**Bearing on `fun`.** Has it differently: fun took the explicit-binder slice of this idea
— `macro m[A](x) : Expr(T)` with names in the annotation only referring
(`macro-type-binders-should-be-explicit`, closed) — and rejected the corollary implicitly:
sets-of-scopes resolve capture without forbidding shadowing.

### Attributes as the metaprogramming interface

**What it is.** Metaprogramming hangs on marked positions instead of call syntax: an
attribute (`#[…]`, `#meta_call`, `@name`) names a processor, its payload is parsed as
ordinary code or as data, and the processor runs at a fixed phase — on a declaration, an
expression, a block. C#'s source generators and F#'s type providers are the same instinct
bolted onto a type checker.

**Buys.** Tooling gets an anchor (the marked region is visibly not-ordinary code), and
attributes can attach to declarations where no call can be written; generation driven by
annotated declarations keeps IDE features alive.

**Costs.** Two syntaxes to learn instead of one, and the payload must still be parsed
like code while behaving differently — the post's point that this limits what tooling may
assume inside the region and constrains the syntax a macro may bring.

**Maturity.** shipped.

**Tried by.** C# source generators, F# type providers, Java via Manifold (all named in the
corpus); Rust `#[derive]` visible in corpus threads; easyjs's `@macro` (hobby).

**Source.** Annotating literal code (as opposed to macros) — score 20, 8 comments,
2025-02 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1itzcn1/annotating_literal_code_as_opposed_to_macros/
· Static Metaprogramming, a Missed Opportunity? — score 74, 63 comments, 2025-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1m022pe/static_metaprogramming_a_missed_opportunity/
· A rare approach to metaprogramming — score 0, 8 comments, 2026-05 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1tkj4sd/a_rare_approach_to_metaprogramming/

**Bearing on `fun`.** Has it differently: a macro is called like a function; the `: …`
after its parameters is a macro annotation fixing its kind and promised type — not an
attribute — and the position is checked before the macro runs (M8, kind mismatch is an
error value). `macro-annotation-constraints-mean-nothing` (closed) records that an
annotation means exactly its kind and type, nothing more.

### Compile-time reflection over types: iterate types like values

**What it is.** At compile time you walk a type's structure — fields, their types, names,
defaults — with the same loops and questions you ask of values, and build code from what
you find. Zig's version is quoted verbatim in the corpus: a `Vec(comptime T: type)` built
by filling a `[_]std.builtin.Type.StructField` array and calling `@Type(.{.Struct = …})`.

**Buys.** Serializers, generic vector types, formatters and schema bindings written once
over structure instead of once per type; the alternative in the same thread — an external
generator emitting `struct {name} …` templates — is what this replaces.

**Costs.** The mechanical version is verbose and compiler-bound (the thread calling C++
template metaprogramming "intolerably painful" and "practically impossible" is the
complaint), and publishing reflection types makes the compiler's representation of a type
a public contract.

**Maturity.** shipped.

**Tried by.** Zig (`std.builtin.Type`, quoted); C++ template metaprogramming and poof
(iterating C++ types like values); Capy lists type reflection as its next milestone.

**Source.** Unpopular Opinion: Source generation is far superior to in-language
metaprogramming — score 98, 97 comments, 2026-01 (quotes Zig's `@Type` route as the
thing it is arguing against) —
https://www.reddit.com/r/ProgrammingLanguages/comments/1q17xo0/unpopular_opinion_source_generation_is_far/
· A compile-time metaprogramming language targeting C & C++ — score 36, 6 comments,
2025-11 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1othlkv/a_compiletime_metaprogramming_language_targeting/
· Type reflection study material — score 34, 3 comments, 2021-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/krcg3j/type_reflection_study_material/

**Bearing on `fun`.** Already has it, stronger: types are values, type-case over open
`Type` is acceptable (decided, complete), record type reflection is complete, and the
map's own idea store (`macro-use-case-shortlist`) builds `derive` on exactly this pair.

### Deriving as a library macro, not a compiler attribute

**What it is.** `derive` for a trait is an ordinary macro: given a trait and a type, it
walks the type's structure and generates the impl — no `#[derive]` in the compiler, no
per-trait code generation in the elaborator. The polytypic-programming framing: equality,
comparison, serialization and custom deriving are all recursive functions over the shape
of a type, and every existing approach obfuscates that to work around a language
limitation.

**Buys.** New derived traits without compiler changes; the derived code is visible,
testable macro output; one mechanism for the whole family instead of four.

**Costs.** The macro author must handle every shape the structure walk can produce, and
errors inside derived code land in expansion — Stage 12 territory. The polytypic thread's
own complaint about the syntax-building route applies: constructing fragments by hand is
"cumbersome… you need to consider details that aren't conceptually relevant".
The comments hold the prior question open: u/lngns says `derive` falls out of generic
structure without macros at all (GHC.Generics, Scrap Your Boilerplate), and
u/eliminate1337 answers that Haskell still resorts to Template Haskell — the compiler-side
attribute's demand is not yet evidence for the library-side route.

**Maturity.** research — the compiler-side attribute form is shipped and proves demand;
the library-side form is argued in the corpus and designed, not built, in fun.

**Tried by.** Rust `#[derive]` (compiler-side, shown in a corpus thread as the boilerplate
it removes); fun's shortlist flagship #1 is design only.

**Source.** What might polytypic (datatype-generic) programming look like if it was built
in to a language? — score 44, 41 comments, 2025-10 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1ny6sw3/what_might_polytypic_datatypegeneric_programming/
· What features have you seen in a PL that helped encourage code re-use? — score 65, 55
comments, 2023-03 —
https://www.reddit.com/r/ProgrammingLanguages/comments/123sn1i/what_features_have_you_seen_in_a_pl_that_helped/
· Nuts or genius? "Modules are classes/objects" — score 40, 23 comments, 2021-06 —
https://www.reddit.com/r/ProgrammingLanguages/comments/nxumma/nuts_or_genius_modules_are_classesobjects/

**Bearing on `fun`.** Open, and already mapped: `macro-use-case-shortlist` names deriving
as its flagship, tying Stage 11 to `design-trait-library-deriving-and-protocols` (open);
`reflect-match-in-expr-macro-adt` (closed) is what enables a prelude-macro `derive`.

### DSLs as macro libraries that inherit the host's hygiene and tooling

**What it is.** An embedded language is written with the host's macro system — its syntax
is templates and its name resolution is the host's hygiene — rather than as a string
inside a call or a separate little compiler. Unseemly's target: real SQL or regexes
inline, "not inside strings", with a shared type system so macro-defined languages can
share libraries.

**Buys.** The DSL gets the host's scoping, modules and expansion machinery for free, and
two DSLs compose because both produce the same syntax objects — no per-DSL scope
implementation, which is where hand-rolled DSL embedders in this corpus struggle.

**Costs.** A type error inside the DSL surfaces through expansion unless a type-aware
deferral carries it to the elaborator, and the DSL's grammar must fit the host's
enforestation — a DSL that wants a token-level novelty needs reader-level power the macro
layer does not grant. A comment draws that line explicitly: macros "describe new
cbinations of existing classes of syntax"; a macro may reserve a new keyword or statement
grammar but "won't define a new way to form tokens" (u/R-O-B-I-N, comment on Rethinking
macros), and the MiniLang author — whose macros expand during the main parsing phase —
agrees they possess no parser ability.

**Maturity.** research.

**Tried by.** Unseemly (prototype, inline SQL/regex macros); Manifold (the plugin route
to the same destination).

**Source.** Unseemly: a typed macro language — score 67, 15 comments, 2020-01 —
https://www.reddit.com/r/ProgrammingLanguages/comments/eq26iu/unseemly_a_typed_macro_language/
· Static Metaprogramming, a Missed Opportunity? — score 74, 63 comments, 2025-07 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1m022pe/static_metaprogramming_a_missed_opportunity/
· What are good examples of macro systems in non-S-expressions languages? — score 44, 38
comments, 2024-08 —
https://www.reddit.com/r/ProgrammingLanguages/comments/1elrpbz/what_are_good_examples_of_macro_systems_in/

**Bearing on `fun`.** Open infrastructure: Stage 11 is the umbrella this presumes, kind
tagging (M8) is what lets a DSL declare whether it expands to an expression, a
declaration, a pattern or a block, and Stage 12 decides whether its errors read like the
host's.

## Threads worth reading in full

- **Macros good? bad? or necessary?** (54, 96 comments, 2025-08) — the axis's framing
  question, asked by someone who watched a podcast and wants the actual objections.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1n41akt/macros_good_bad_or_necessary/
- **Unpopular Opinion: Source generation is far superior** (98, 97 comments, 2026-01) —
  the best-argued case *against* in-language metaprogramming, with the Zig `@Type`
  example in full.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1q17xo0/unpopular_opinion_source_generation_is_far/
- **Unseemly: a typed macro language** (67, 15 comments, 2020-01) — one person's complete
  answer to typed macros: quotation typechecked, macro-by-example, no type errors in
  generated code, plus links to the working examples.
  https://www.reddit.com/r/ProgrammingLanguages/comments/eq26iu/unseemly_a_typed_macro_language/
- **What would you leave out of comptime?** (21, 42 comments, 2026-01) — the compile-time
  sandbox question stated cleanly: divergence, IO, capabilities, dependency management.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1qbq1n9/what_would_you_leave_out_of_comptime/
- **Ideas on how to break hygiene?** (6, 13 comments, 2020-06) — four concrete designs for
  deliberate capture, the exact question fun answers with `Borrowed context`.
  https://www.reddit.com/r/ProgrammingLanguages/comments/gwo212/ideas_on_how_to_break_hygiene/
- **Static Metaprogramming, a Missed Opportunity?** (74, 63 comments, 2025-07) — the
  compiler-plugin/type-provider route, and the "guess-based development" charge against
  dynamic metaprogramming.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1m022pe/static_metaprogramming_a_missed_opportunity/
- **Resources on statically typed hygenic macros?** (12, 12 comments, 2023-08) — the
  candidate list (Racket, Hackett, Klister, Typer) and the question fun's Stage 10 answers.
  https://www.reddit.com/r/ProgrammingLanguages/comments/15fs9pu/resources_on_statically_typed_hygenic_macros/
- **My macro design is doing too many things** (11, 8 comments, 2026-06) — the failure
  mode in the author's own words: annotations as a kitchen sink, two things that cannot be
  macroed.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1u1fmlt/my_macro_design_is_doing_too_many_things/
- **Should calling a macro look different than calling a function?** (43, 45 comments,
  2023-10) — the invocation-syntax question on both sides.
  https://www.reddit.com/r/ProgrammingLanguages/comments/17dveoo/should_calling_a_macro_look_different_than/
- **What might polytypic (datatype-generic) programming look like?** (44, 41 comments,
  2025-10) — deriving/polytypicism as the bridge between generics and macros, with every
  existing approach criticized by name.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1ny6sw3/what_might_polytypic_datatypegeneric_programming/
- **Noel Welsh: Parametricity, or Comptime is Bonkers** (54, 34 comments, 2026-03) — the
  best-argued comment thread on the axis: Idris2 quantities, `comptime` annotations vs
  typeclasses, and the case that parametricity was already broken.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1rrgyl3/noel_welsh_parametricity_or_comptime_is_bonkers/

## Gaps and disagreements

- **Comment coverage: 19 threads, 474 comments, the top of each tree only.** Comment trees
  were fetched for the 19 richest threads of this axis; each fetch returns only ~30
  top-level comments regardless of `limit=100`, with the rest unretrieved in `more`
  placeholders (one fetched tree records 96 of 98 unretrieved). Six of the 19 are off-axis
  (Odin's release thread, the Lua-everything-is-a-tree question, "26 programming languages
  in 25 days", 30 years of HPC, function inlining, the AI-slop bootstrap thread) and
  yielded almost nothing here. Entries whose `contested` tag now rests on an argued thread
  rather than on opposing posts: macro call syntax, the comptime sandbox, source generation
  vs in-language metaprogramming, macros carrying language features, parametricity vs
  comptime.
- **Live disagreements this corpus did not settle:** whether compile-time code may do IO
  (argued in one thread — the pure side has the reproducibility mechanism, the permissive
  side the build-system precedent, no vote taken); source generation vs in-language
  metaprogramming (argued, and the speed charge corrected, but nobody measures); whether
  macros should carry language features at all (u/matthieum vs u/realbigteeny, both in
  "Macros good? bad? necessary?"); whether `derive` needs macros at all (u/lngns vs
  u/eliminate1337).
- **Thin coverage, re-checked against the comments:** macro documentation and how to teach
  macros — the comments failed to fill this gap; the nearest hits are a Template Haskell
  aside ("the docs can leave a bit to be desired") and a reviewer asking MiniLang's docs to
  show idiomatic use, neither of which is about teaching macros. Idempotence and
  round-tripping of a syntax representation — the comments failed to fill this gap too;
  fun's M1/M10 rulings still have no corpus counterpart, the closest being u/mamcx naming
  introspection and rebuilding as "the section you need to solve first". Enforestation-style
  interleaving by name is still absent from the corpus, but the comments do supply
  interleaved expansion: Scala 3 "expands during type checking/elaboration" (u/LPTK) and
  R6RS implicit phasing (u/Public_Grade_2145). Staged-metaprogramming theory is no longer
  link-only: the 20 comments on "References for the theory behind Zig's comptime?" name
  multi-stage programming, partial evaluation and the Futamura projections, two-level type
  theory, and AndrasKovacs's definition of staged compilation as a static guarantee that no
  stage-n+1 constructions survive — but nobody in the corpus reads those papers for us.
  Expansion UX stays thin (one cpp implementation thread, one link post), though
  u/omega1612's and u/sciolizer's comments now give it a practitioner cost and a spectrum.
- **To decide anything here you would need to read beyond the corpus:** Flatt's
  sets-of-scopes paper and Klister's interleaving commentary (both already in
  `docs/wayfinder/macro-system/`), the Turnstile paper behind the "Type Systems as Macros"
  post, and Racket's own macro documentation [general knowledge, not from corpus] for the
  pattern-form/arbitrary-code split this corpus mentions but never explains.
- **Hobby-project caveat, repeated:** Passerine, Nomsu, Capy, easyjs, PreC, basil, poof,
  Unseemly and the `#meta` hook design are single-author projects at or before 1.0. They
  show ideas being tried; none of them shows an idea surviving a decade of users.

## Dissent and corrections

- **Corrected in this pass:** the header's claim of zero comment coverage; the staged-theory
  gap (the References thread now carries 20 comments naming the literature); and the
  source-generation entry's premise — the speed charge against in-language metaprogramming
  does not survive its own thread, since generators pay the same rerun cost and Zig's
  comptime is interpreted with a tracking issue aiming at CPython-script speed. The
  inferred contest over call syntax is now an argued one, both sides inside a single
  thread.
- **Corrections to cited threads, from their own comments:** the "Macros in 22 languages"
  Scheme example is a Common Lisp macro in Scheme clothing, and `defmacro` is not in R7RS
  (u/Zambito1, u/skyb0rg).
- **Dissent recorded rather than resolved:** u/matthieum against "macros are a symptom of
  bad language design"; u/Norphesius against the comptime-IO ban; u/lngns against needing
  macros for `derive`; u/The_Northern_Light against the source-gen post's preference being
  anything more than "a failing of the language".
- **Not resolvable with the material available:** every compiler-speed claim on both sides
  of the generation debate (no measurement in 474 comments); whether interleaved expansion
  improves diagnostics; whether any claim here survives contact with a decade of users.

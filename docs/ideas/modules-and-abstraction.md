# Modules and abstraction — how programs are partitioned and where the boundaries go

This axis covers partitioning and boundary-drawing: module systems (file-based vs declared,
namespaces, `open`, re-export, qualified paths, cyclic dependencies), visibility and sealing
(private/public, opaque and abstract types, signatures), the abstraction mechanisms head to head
(structs+traits vs classes vs objects vs interfaces vs protocols), compilation units (what a unit
is, separate and incremental compilation, whether a module is a value), package/build concerns
only insofar as they constrain language design, and interop/FFI as a design constraint. It does
**not** cover typeclass or generic *inference* — where an instance lives and how it is imported
appears here only as a module-system decision, and each such entry says so.

## How this was gathered

Primary source: `/tmp/reddit/slices/modules-posts.md`, 87 threads from r/ProgrammingLanguages and
r/Compilers sorted by comment-volume × score × body length, plus keyword greps over the full
2538-thread `corpus.jsonl` (functor, sealing, separate compilation, name mangling, incremental,
coherence, dispatch, FFI, …). Second pass: `/tmp/reddit/slices/modules-comments.md` — comment
trees fetched for the **20 richest threads of this axis**, **520 comments** in all, each cited
below as `(comment on "<title>")` against that title's permalink in the entry's Source line. The
fetch is capped in a way `limit` does not fix: each `/comments/<id>.json?limit=100` reply returns
only ~30 top-level comments regardless of the value of `limit`, so every thread contributes
exactly 26 and the rest sit in `more` placeholders that were never expanded — 147 across the set.
That is the limitation it is: **the other 67 of the slice's 87 threads have no tree**, and where
a body is a question with no fetched answers the entry says so; nothing argued only in
unretrieved comments is claimed here. The corpus is what Reddit upvoted, not a survey of the
field: several high-scoring entries are self-promotion for hobby languages (Flint, Seal, Kal,
Flow-Wing) and are usable only as evidence that an idea is being tried. Scores and comment
counts are the corpus's own; dates are the harvest timestamps.

## The ideas

### Logical module names instead of file locations

**What it is.** The import names a module in the program's own namespace (`import "myGame.physics"`,
`std.json`), not a path on disk; where source bytes live is a resolver/build concern the programmer
does not recite. The thread's sharpest formulation: "Files are an implementation detail, I should
not care where source is stored on the filesystem to use it."

**Buys.** Moving a file stops breaking every reference; no `../../../file` spellings; no
`import … as` renames that let one concept be called three things; the editor suggests members
from the module, not from the folder.

**Costs.** The language owes a resolver: a module-to-location map, a root, rules about relative
paths (the thread concedes packages mirroring folders is fine — "please make importing work like
C#"). Without that map, decoupling names from files just moves the same bookkeeping into an
invisible configuration file — and a commenter sharpens that into the exact move C# makes: "The
job of telling the language where the code named 'whatever' is just moved out of the .java/.cs
language source files and into the project-file language source files. Move the path to
something, and you now have to update a bunch of .xml files instead of a bunch of .Java files"
(comment on "I hate file-based import / module systems.").

**Maturity.** Contested, and the fetched replies show it is a real standoff rather than a
question nobody answered. The thread's top comment (+198) defends file = module outright — "I
love the simplicity and concreteness of knowing that everything is just a file" — and the next
(+78) rebuts OP point by point from million-line codebases: "I can't help but think you've never
worked on large-scale projects", closing with "Explicit imports are a lifesaver, there."
Against files, a reply reports two 100K+ LOC codebases where the language *without* explicit file
imports won out over Kotlin's "wall of imports" (comment on "I hate file-based import / module
systems."). From the file-correspondence side, an SML user supplies the sharpest experience in
the corpus: scanning the MLton Basis library, "separating module names and file names makes
determining their definitions impossible, especially when there are many definitions of the same
structure/signature which are constantly extended with open/include" (comment on "Modules:
Overcoming Stockholm and Duning-Kruger"). No verdict: 26 of the thread's 97 tree
comments were retrieved and 38 more were left in `more` placeholders. The axis's summary thread
still records the opposite norm — "Often, package/subpackage/module maps to the filesystem. But
some authors strongly oppose this."

**Tried by.** C# and Java (path-independent-ish namespaces), JavaScript import maps; opposed by
C++/Rust practice.

**Source.** "I hate file-based import / module systems.", score 28, 134 comments, 2025-02,
https://www.reddit.com/r/ProgrammingLanguages/comments/1in4fm0/i_hate_filebased_import_module_systems/
· counterweight: "r/ProgrammingLanguages on Import Mechanisms", score 80, 30 comments, 2023-04,
https://www.reddit.com/r/ProgrammingLanguages/comments/1340z3r/rprogramminglanguages_on_import_mechanisms/
· "Advice on designing module system?", score 31, 38 comments, 2019-02,
https://www.reddit.com/r/ProgrammingLanguages/comments/apfnsl/advice_on_designing_module_system/
· (comments above) "Modules: Overcoming Stockholm and Duning-Kruger", score 84, 44 comments,
2022-07,
https://www.reddit.com/r/ProgrammingLanguages/comments/vqx19e/modules_overcoming_stockholm_and_duningkruger/

**Bearing on `quill`.** Has it differently, by decision: a **compilation unit** *is* a `.qll` file
reached by `import` — but a unit is explicitly not a module, and `CONTEXT.md` names the
conflation as the error ("conflating it with a module is what made 'modules must be closed' sound
like it would break first-class modules"). Unit identity is the file; module identity is the
value.

### Modules as first-class values that capture their context

**What it is.** A module is an ordinary value: it can be passed, returned, bound, built by a
function. It is a closure over the context it was written in, so two calls that build "the same"
module capture different contexts. Type-level contents ride inside the value.

**Buys.** One mechanism for namespaces, parameterised libraries and runtime-selected
configuration; `quill`'s own glossary states the payoff — "moving a *value* is always sound,
because a value carries its environment with it — which is why first-class modules work and why
importing one does not."

**Costs.** A global registry of impls becomes impossible: there is no well-defined moment to
register an impl for a module produced at run time. Also the type theory's sharp edge: the
thread's source notes "paradoxes lie in the wings if you are not careful" (1ML's problem).

**Maturity.** Shipped — OCaml first-class modules, Scala objects.

**Tried by.** OCaml's first-class modules and Scala objects `[general knowledge, not from
corpus]` — the corpus shows OCaml's module system and 1ML but does not itself use the term
"first-class modules"; `quill`.

**Source.** "Nuts or genius? \"Modules are classes/objects\"", score 40, 23 comments, 2021-06,
https://www.reddit.com/r/ProgrammingLanguages/comments/nxumma/nuts_or_genius_modules_are_classesobjects/
· "Are OCaml modules basically compile-time records?", score 18, 13 comments, 2024-07,
https://www.reddit.com/r/ProgrammingLanguages/comments/1e6d88x/are_ocaml_modules_basically_compiletime_records/

**Bearing on `quill`.** Already has it (named): **modules are first-class values** that capture
their context like a closure, accessed by a dotted path that resolves to the *last* member of
that name. The recorded consequence: **global coherence for impls is rejected** — unavailable
when modules are values.

### One construct for record, module and namespace

**What it is.** Instead of three related-but-separate mechanisms (a record type, a module
namespace, a package grouping), one construct carries constructor fields *and* bindings, so
record types, namespaces and parameterised components are all written the same way. The corpus
thread asks the question explicitly: OCaml modules "seem similar to normal values and types" —
signatures ≈ struct types, implementations ≈ values, functors ≈ functions — "would there be
benefits in simplicity, universality, or expressive power if we tried to unify these two
concepts?"

**Buys.** One grammar, one member syntax, one Reflection for macros; no rule for deciding
whether a given `X` is "really" a record or a namespace.

**Costs.** Types living *inside* the construct push toward dependent typing (the thread's own
warning: allowing structs to expose associated types "would probably end up with a form of
dependent typing"), and the type-level and value-level halves must be told apart — a record type
and a module signature are *not* interchangeable.

**Maturity.** Research — argued in the thread with Zig `comptime` and Scala inner classes offered
as adjacent precedents (the thread's analogy, not a demonstration); the full unification is built
in `quill`, which is itself experimental.

**Tried by.** `quill`; nobody else in this corpus ships one construct for all three. The closest
reported practice is Zig, described in a comment as having "only namespacing is structs … and all
files are implicitly structs" — that is, the file, the namespace and the type are already one
thing there (comment on "r/ProgrammingLanguages on Import Mechanisms").

**Source.** "Are OCaml modules basically compile-time records?", score 18, 13 comments, 2024-07,
https://www.reddit.com/r/ProgrammingLanguages/comments/1e6d88x/are_ocaml_modules_basically_compiletime_records/
· (comment above) "r/ProgrammingLanguages on Import Mechanisms", score 80, 30 comments, 2023-04,
https://www.reddit.com/r/ProgrammingLanguages/comments/1340z3r/rprogramminglanguages_on_import_mechanisms/

**Bearing on `quill`.** Already has it (named): one `struct` serves as **record, module and
namespace**. The line `quill` keeps anyway: a module is never a type — a signature is its own value
(`sig { … }`), and checking a function that takes a module against a signature raises
`NotASignature` rather than unifying a record type with a namespace.

### Signatures as telescopes with path-dependent members

**What it is.** A module signature is a telescope: a later member's type may mention an earlier
one through the described module, so `sig { T : Type; empty : T; size : T -> I64 }` gives
`s.empty : s.T`. For a *parameter* the earlier member stays abstract; for an *argument* it is the
argument's own member. An impl a signature requires must be *named* (`eq_T : impl Eq(T)`).

**Buys.** The abstraction a parameterised component needs — write `count(s : Stack)` once and
each caller's `T` stays its own; two parameters' members are distinct (`s1.T` and `s2.T` do not
collide), which is what makes separate abstractions composable.

**Costs.** Types become path-dependent: equality of signatures is decided under a fresh module
rather than by structural comparison, and every consumer must learn that `s.T` is a *member*, not
a name. The corpus thread flags the same pressure from the other direction — associated types in
a struct type are "a significant difference from normal structs". And a path has to *stay* a path:
a comment notes that in Scala "to use an object as a module it needs to be assigned to a variable
or the field of a stable object, so that it has a stable path, referred to in types … function
applications are not considered stable … hence the lack of applicativity" — the price of
path-dependent members is that not every expression can be the thing being described (comment on
"Modules: Overcoming Stockholm and Duning-Kruger").

**Maturity.** Shipped — OCaml signatures, Scala path-dependent types (the corpus names the Scala
parallel).

**Tried by.** OCaml, Scala, `quill`.

**Source.** "Are OCaml modules basically compile-time records?", score 18, 13 comments, 2024-07,
https://www.reddit.com/r/ProgrammingLanguages/comments/1e6d88x/are_ocaml_modules_basically_compiletime_records/
· "Definitive text on \"module system(s)\"?", score 30, 42 comments, 2023-08,
https://www.reddit.com/r/ProgrammingLanguages/comments/15wdiqh/definitive_text_on_module_systems/
· (comment above) "Modules: Overcoming Stockholm and Duning-Kruger", score 84, 44 comments,
2022-07,
https://www.reddit.com/r/ProgrammingLanguages/comments/vqx19e/modules_overcoming_stockholm_and_duningkruger/

**Bearing on `quill`.** Already has it (named): signatures are telescopes; `s.T` reads the member
through the module, abstract for a parameter and concrete for an argument; a `sig`'s required
impls are named, and an anonymous `impl` in a `sig` is a parse error.

### Graydon's ten constraints as a checklist before choosing features

**What it is.** A design checklist, not a mechanism: before building a module system, fix answers
to *generativity, opacity, stratification, coherence, subtyping, higher-order-ness,
first-class-ness, separate compilation, extensibility, recursion*. The thread's contribution is
paraphrasing each one and asking which are worth a commercial language's complexity.

**Buys.** Turns "I want a module system" into ten answerable questions; each is independent, so
an answer can be deferred *consciously* instead of by accident (the thread's own reading: types
may already carry the subtyping/generics capability modules would add).

**Costs.** It is a glossary written by one reader of one blog post — the source itself notes
Graydon "provided no glossary nor bibliography", so the paraphrases drift (its "coherence"
reading is software-coupling, not instance-uniqueness). The fetched comments repair three of the
ten and leave the repair checkable: a reader who knows the ML literature glosses *generativity*
as applicative-vs-generative functors (`module M1 = F(M); module M2 = F(M)` compiles only if
applicative), *opacity* as opaque-vs-transparent ascription, and *stratification* as a separate
module language versus a term language — but that comment's own *coherence* gloss is cut off
mid-sentence in the slice, and a second reader has to fill coherence in from the other end
(quoted under *Where impls live*, below). A checklist with no cost model invites implementing
everything.

**Maturity.** Speculative — argued, not built; no artifact ships the checklist.

**Tried by.** Nobody has shipped this as a method; `quill`'s decisions happen to answer most rows.
The thread's real output is a reading list rather than a method: Pierce's modules slides and
*TAPL*, Dreyer and Rossberg's MixML, Leroy's "A Modular Module System" and the 1994 POPL paper
"Abstract Types, Manifest Types, and Separate Compilation", Mark Jones's parameterised
signatures and Mark Shields's work — all named in comments, none cited by the post itself
(comments on "Modules: Overcoming Stockholm and Duning-Kruger").

**Source.** "Modules: Overcoming Stockholm and Duning-Kruger", score 84, 44 comments, 2022-07,
https://www.reddit.com/r/ProgrammingLanguages/comments/vqx19e/modules_overcoming_stockholm_and_duningkruger/
· the 1ML/"better module system" wish appears in "What's your ideal language feature?", score 55,
152 comments, 2018-11,
https://www.reddit.com/r/ProgrammingLanguages/comments/9um9nw/whats_your_ideal_language_feature/

**Bearing on `quill`.** Use it as an audit: `quill` has ruled first-class-ness (yes, modules are
values), separate compilation (units base-anchored and deliberately *not* first-class), recursion
(an import cycle is an error), generativity (a nominal declared under a run-time effect is
generative; applicative under purity), and opacity — which is **open** on
`design-private-type-visibility-model`.

### Selective open and symmetric selective export

**What it is.** `open M.{a, b}` brings exactly the named members into scope; `export M.{a, b}`
re-exports exactly them. One selection list, both directions, carrying every member kind — a
value, an enum's constructor, a named impl, a syntactic role, a macro.

**Buys.** Ends the mandatory-open tax: a module that defines a type *and* its impl no longer
forces every user to swallow its whole export list — take the impl, leave the rest. The
corpus's standing advice is the same from the other side: "Globular imports are *usually*
frowned upon. List which names you import from where."

**Costs.** The selection must carry *every* member kind or the form is useless exactly where it
matters (its ruling records this as the bulk of the work); naming an impl does not bring its
trait's name along, so `open M.{i64_size}` leaves `Size` unbound and the call site must name or
qualify it too; and a list is one more place to keep in sync when a module's members change. The
*export* direction has its own cost, named precisely by a comment: a C++ `using` in a header is
"like a public open import — whatever you open import in the header file, all your users would
have them open imported … Just imagine that one day you remove a `using std::string`
declaration from a header file, this change would propagate to all your users" (comment on "Why
are some language communities fine with unqualified imports and some are not?"). An open that a
unit re-exports is a dependency edge it does not control.

**Maturity.** Shipped — JavaScript named imports, `quill`.

**Tried by.** JavaScript/TypeScript, `quill` (closed 2026-09-28, `ab95ccb` + `71bd812`).

**Source.** "r/ProgrammingLanguages on Import Mechanisms", score 80, 30 comments, 2023-04,
https://www.reddit.com/r/ProgrammingLanguages/comments/1340z3r/rprogramminglanguages_on_import_mechanisms/
· "Why are some language communities fine with unqualified imports and some are not?", score 73,
50 comments, 2025-05,
https://www.reddit.com/r/ProgrammingLanguages/comments/1kyaaf2/why_are_some_language_communities_fine_with/

**Bearing on `quill`.** Already has it (named): `open M.{a, b}` and `export M.{a, b}` are
**one shared form, symmetric in both directions** (`Syntax.Open`/`Binding.Open` gained `Names`,
matching `DeclExport`);
only what is named arrives, unknown names are an error in the export form's shape, opens stay
idempotent and deduped by impl identity.

### Unqualified-by-default vs qualified-by-default is a cultural, not technical, choice

**What it is.** The observation that communities split: C++ treats `using namespace std` as a
bug; C# treats `using System.Collections.Generics` as normal; JavaScript offers neither glob nor
nothing — qualified (`import * as foo`) or explicitly listed (`import {bar}`). The design question
is what the *default* should be and what the compiler must do when two imports collide.

**Buys.** Naming the default settles a huge fraction of error-message quality: if ambiguity is a
loud compile error, unqualified imports are safe; if it silently picks one (last-wins), they are
not. Python-style "glob for the DSL-shaped stdlib, qualify for the rest" is a defensible middle.

**Costs.** Unqualified imports make reading a file insufficient to know where a name came from —
the C++ complaint; qualified-first makes every use site carry qualifiers, which the thread notes
IDE completion papers over anyway. Two sharper costs came out of the comments: in C++ an
overloaded function brought into scope is "eligibile for overload resolution", so unqualified
imports widen the hairiest part of the language; and import is not the same act in every
language — "Import in JavaScript is a runtime action. Import in C# is just registering a name in
a dictionary at compile time for subsequent compiler name resolution" — which is why the same
syntax can feel safe in one language and reckless in another (comments on the same thread).

**Maturity.** Contested — no community consensus — but the fetched replies replace "no
consensus" with mechanisms, which is a better finding than the body alone gave. A member of the
C# design team answers the thread directly: the difference is not syntax but discipline — ".net
libraries therefore had very specific standards for how you did naming … naming collisions are
much rarer", plus reflection-based tooling that makes a qualified name free to type. A Dart
designer reports a measurement instead of an opinion: "something like 90% of imports didn't use
a prefix or explicitly list the imported names", and proposes the real split is static-vs-dynamic
— in a statically typed language many references are type annotations, where "dart.async.Future"
is "a tax for many users". And one reply contests the thread's own premise: the Python community
is *not* fine with `import *` ("you can't see which identifiers being 'taken' or overwritten");
the working distinction is per-name readability — `json.dumps` reads better than `dumps`,
`datetime.timedelta` adds nothing over `timedelta`. All comments on "Why are some language
communities fine with unqualified imports and some are not?"; 26 of 50 retrieved.

**Tried by.** C# and Python (unqualified-friendly), C++ and JavaScript (qualification/listing).

**Source.** "Why are some language communities fine with unqualified imports and some are not?",
score 73, 50 comments, 2025-05,
https://www.reddit.com/r/ProgrammingLanguages/comments/1kyaaf2/why_are_some_language_communities_fine_with/
· "r/ProgrammingLanguages on Import Mechanisms", score 80, 30 comments, 2023-04,
https://www.reddit.com/r/ProgrammingLanguages/comments/1340z3r/rprogramminglanguages_on_import_mechanisms/

**Bearing on `quill`.** Has it differently, deliberately: bare names never arrive by a glob — an
**open** is a binding whose open-choice resolution is *deferred to elaboration* (scope sets pick
the candidate opens; the elaborator takes the first open that has the member), a dotted path is
plain **member** access carrying no scopes, and the **strict phase rule** gives an imported unit
no prelude syntax — it must write `open (import "std")` itself.

### Sealing: private types that leak, abstract outside

**What it is.** The OCaml/SML model: a type declared private inside a module may appear in the
module's public bindings, so values of it cross the boundary — but outside the module it is
abstract: no constructor, no pattern, no inspection. A `sig` may also declare a type with *no*
right-hand side (`type Handle`), sealing it opaque even where the implementation is concrete.
Two mutually exclusive modes: inline `pub` (public types concrete) or a `sig` (body private by
default).

**Buys.** Representation freedom: change the implementation without breaking importers, while
still letting them store and pass the value. Constructors stay unreachable without a separate
name table — the visibility gate alone is the check.

**Costs.** A sealed type is unusable for anything requiring its structure outside the module
unless re-exported concretely; the two-mode rule means adopting a `sig` rewrites every inline
`pub`; and every sealed type costs pattern-matching ergonomics at the boundary. The refinement
syntax itself has a reported tax: OCaml's `with` is named alongside the `module`-keyword and
parentheses rules as what "decreases the ergonomic rating of the language" (comment on
"Modules: Overcoming Stockholm and Duning-Kruger") — the one place in this corpus where
`with`-refinement appears at all.

**Maturity.** Shipped — OCaml and SML have done this for decades `[general knowledge, not from
corpus]`. The body-only pass was right that no *thread* argues the trade; the comment pass is not
silent on it. A reader in the Stockholm thread pins the row to a mechanism: "This is about opaque
ascription (aka sealing) vs transparent ascription. Opaque ascription hides type equalities,
including for type aliases (not just 'data types')" — and adds a checkable claim about who is
missing it: Haskell, Rust and Swift "only allow data types to be abstract", i.e. they seal
`data`/`newtype`/`struct` but not a type alias (comment on "Modules: Overcoming Stockholm and
Duning-Kruger"). A second comment in the same thread states what sealing buys: "Opaque module
abstraction are like newtypes, and functorization/inclusion are both ways to build up
hierarchies." From the other direction, a comment on the inheritance thread argues opacity is
what *replaces* the access-modifier apparatus: interfaces plus "opaque existential types" are
said to save you "from having to define a bunch of weird, ad-hoc access modifier rules in your
language that only hinder extensibility … and that can be circumvented via reflection anyway"
(comment on "What do you dislike about class inheritance?"). Still unargued anywhere in the
retrieved corpus: nobody weighs constructors-hidden against representation-freedom *at a module
boundary*, and nobody defends the two-mode inline-`pub`/`sig` rule.

**Tried by.** OCaml, SML `[general knowledge, not from corpus]`; `quill` has the design written in
`topics/private-type-visibility.md` and the decision *not* taken.

**Source.** "Modules: Overcoming Stockholm and Duning-Kruger" (opacity as a named constraint),
score 84, 44 comments, 2022-07,
https://www.reddit.com/r/ProgrammingLanguages/comments/vqx19e/modules_overcoming_stockholm_and_duningkruger/
· "Definitive text on \"module system(s)\"?", score 30, 42 comments, 2023-08,
https://www.reddit.com/r/ProgrammingLanguages/comments/15wdiqh/definitive_text_on_module_systems/
· (comment above) "What do you dislike about class inheritance?", score 58, 97 comments, 2020-06,
https://www.reddit.com/r/ProgrammingLanguages/comments/he2wmh/what_do_you_dislike_about_class_inheritance/

**Bearing on `quill`.** Open/undecided — `docs/wayfinder/tickets/design-private-type-visibility-model.md`
is open with `Resolution: _Unresolved._`; its topic doc
(`topics/private-type-visibility.md`) already sketches the OCaml/SML model with
`open_module_value` as the single access-control gate, but the map keeps it in the grilling queue,
not the decisions list.

### Privacy vs testability: test-only access to privates

**What it is.** The observation that strict visibility breaks testing: to test code you want to
"access symbols that are otherwise private to a class or module", "override the value of a global
constant", "treat a concrete type as an interface with alternative test-only implementations" —
none of which a sealed module offers. The thread catalogs the workarounds (conditional
compilation, macro-built affordances) and their costs.

**Buys.** Naming the tension early prevents a language from pricing it in silently: either the
visibility model has an escape hatch, or library authors sprinkle test affordances through
production code.

**Costs.** Every escape hatch weakens the seal it bypasses — a test-only door is a door; and the
thread's own evidence is that macro/flag workarounds "present difficulty for static analysis
tools" and clutter non-test code.

**Maturity.** Contested, and the fetched tree now carries both sides. Against a language-level
door: "I'm heavily against supporting poor design on a language level … If you make it convenient
to test poor code, then you will just get poor code", with DHH's "test-induced design damage"
named as the standing cost of treating testing as a privileged language activity. Against *that*
answer: "You seem to be making a somewhat circular argument; a lot of what you call good design
is only good *only* because it facilitates unit testing" — plus the observation that declaring an
interface your one non-test type implements is the "say it twice if you really mean it"
anti-pattern. The constructive side offers a mechanism rather than a workaround: "make tests a
special construct … Easy to make it inaccessible from production code. Then within that scope,
some of the restrictions can be lifted, like overriding non-virtual methods, or accessing
private fields" — the author says his pet language is not ready to show it works. All comments on
"Why don't more languages have first-class testing features?"; 26 of 52 retrieved.

**Tried by.** Languages with `friend`/`internal`/reflection hacks (C++, Java, Python); the
annotation doorway the thread names (`@VisibleForTest`) and Mockito as the shipped workarounds;
nobody in this corpus ships a principled white-box door.

**Source.** "Why don't more languages have first-class testing features?", score 42, 53 comments,
2021-09,
https://www.reddit.com/r/ProgrammingLanguages/comments/pxytj7/why_dont_more_languages_have_firstclass_testing/

**Bearing on `quill`.** Genuinely new to the project — nothing in the map or tickets proposes test
affordances; `quill`'s conformance suite drives whole programs against `.expect` pairs and never
needs to reach inside a module, which is *why* the question has not come up. It will come up when
a library wants white-box tests.

### Structural interfaces defined where they are consumed (a giraffe is not a fish)

**What it is.** Inheritance groups by descent, so it can only carve clades — if `Fish` is an
abstract class, giraffes are fish. Interfaces ("grades") carve by *capability* instead, and the
structural version lets the *consumer* define the interface: in Go any type with `qux() int`
already satisfies `quxer`, so a third-party object can be mocked by naming only the methods you
need. The thread's own language goes further: a "de facto" interface takes any `T` with `foo :
T -> int` *so long as `foo` is defined in the same module as `T`* — a locality rule standing in
for coherence.

**Buys.** Small purpose-built interfaces at use sites; no orphan problem for library authors — you
never need the type's owner to declare your capability; retrofitting existing types works
("the third-party object *already* satisfies the interface").

**Costs.** The thread states three against pure duck typing, and structural satisfaction keeps
two of them: module namespaces clutter with qualifying functions; reading a call no longer tells
you what `foo` is or where it came from; nothing guarantees `foo`'s contract. And the
same-module locality rule is a coherence rule by another name — drop it and two `foo`s exist.
The fetched replies add the cost from both seats. The *reader* pays: dispatching on the
argument's type without any interface does work, but "The poor human trying to read the code, on
the other hand, has more of a puzzle finding out where the heck this `foo` thing came from"
(comment on "Inheritance and interfaces: why a giraffe is not a fish"). The *type author* pays:
"you can accidentally implement an interface you didn't mean to, and then accidentally call it",
worked as `Closeable`/`Close` — a `Window` becomes closeable "just because it happens to have a
close method" — which is why the same comment would permit adding an interface to a foreign type
"only locally in the module, and only explicitly" (comments on the same thread).

**Maturity.** Shipped — Go interfaces; the "de facto" variant is research (Pipefish, the thread's
author's hobby language). The post's *framing* is contested by its own fetched comments: the
biology analogy is charged with being backwards (grades *exclude* descendants, so interfaces are
"more like polyphyletic groups", which "can pop up at any branch"), the taxonomy reading is
rejected outright ("inheritance … is not supposed to be a perfect description of the universe,
but a method for code reuse and polymorphism"), and a reply that checked the Simula toll-bridge
story against the 1968 source document finds even it "was still a made up/contrived problem" —
the historic example the post leans on does not carry it.

**Tried by.** Go; Pipefish (hobby — data point about interest, not viability); `quill` takes the
other side.

**Source.** "Inheritance and interfaces: why a giraffe is not a fish", score 38, 58 comments,
2026-03,
https://www.reddit.com/r/ProgrammingLanguages/comments/1rnm7xi/inheritance_and_interfaces_why_a_giraffe_is_not_a/
· "What do you dislike about class inheritance?", score 58, 97 comments, 2020-06,
https://www.reddit.com/r/ProgrammingLanguages/comments/he2wmh/what_do_you_dislike_about_class_inheritance/

**Bearing on `quill`.** Has it differently: `quill`'s traits are **nominal** — a `trait`/`impl`
declaration pair — carried by **structural dictionary evidence** with most-precise-impl-wins.
Satisfaction is never inferred at the use site; an impl for a type is written somewhere and then
must be brought into scope (see *where impls live*, below).

### Composition and delegated inheritance instead of class inheritance

**What it is.** Treat inheritance as a bundle of separable capabilities — six, per the source's
blog series — and re-provide the wanted ones (polymorphic interfaces, inversion of control,
delegation, subtyping) without subclassing: composition first, delegation as an explicit
mechanism, abstract classes retired.

**Buys.** No fragile base class, no inherited data layout ("encapsulation of data … large memory
buildup that you have no control over"), no inability to make an existing type implement an
interface — the two complaints the 97-comment thread leads with.

**Costs.** Each capability you drop must be rebuilt by hand or by the type system: delegation
needs its own form, subtyping needs its own relation, and boilerplate reappears at every
composition site. The corpus does not show anyone measuring whether six re-implementations beat
one inheritance.

**Maturity.** Contested — Go is the shipped answer, and the corpus argues for it, but the
fetched comments refuse the premise that inheritance itself is the fault. One dates both the
complaint and its rebuttal to the same conference: academics had already "identified loads of
problems with inheritance by the mid 80s … (yoyo, diamonds)" and "composition over inheritance"
was heard at ECOOP 89, yet "none of these faults were with the concept. It was recognized that
inheritance had its place; the mistake was to apply it in the wrong way." Another locates the
fault in language shape instead: in CLOS "you only subclass if you want to inherit a class's
methods", and the C++/C#/Java mistake is conflating generic function declarations with classes —
which makes composition-vs-inheritance the wrong axis (comments on "What do you dislike about
class inheritance?", 26 of 96 retrieved). Neither position is rebutted in the retrieved set.

**Tried by.** Go; `quill` never grew inheritance. Prior art the comments name that the bodies did
not: *A compositional model for software reuse* (1989) and BETA (design on paper 1975, working
compiler 1983, shipped 1985) are offered as the real origin of composition-over-inheritance
(comment on "The Flint Programming Language").

**Source.** "Re-imagining inheritance", score 37, 21 comments, 2019-07,
https://www.reddit.com/r/ProgrammingLanguages/comments/cg7ko0/reimagining_inheritance/
· "What do you dislike about class inheritance?", score 58, 97 comments, 2020-06,
https://www.reddit.com/r/ProgrammingLanguages/comments/he2wmh/what_do_you_dislike_about_class_inheritance/
· "Abstraction, Perfect Abstraction, Enough Abstraction, Just Enough Abstraction ...", score 40,
28 comments, 2022-03,
https://www.reddit.com/r/ProgrammingLanguages/comments/t6gsi1/abstraction_perfect_abstraction_enough/
· (comment above) "The Flint Programming Language", score 71, 69 comments, 2026-07,
https://www.reddit.com/r/ProgrammingLanguages/comments/1usjaxf/the_flint_programming_language/

**Bearing on `quill`.** Already has it (named): no inheritance exists — structs carry fields and
bindings, traits carry **impls**, and `Self` is *the struct being defined only*, which is the
rule that stops a self-type from becoming a base class. Delegation maps to ordinary module
members and `impl`s.

### Modules as objects: a cloneable, state-holding class

**What it is.** Collapse module and class: if a module is cloneable, can hold state, and can
list/call its functions, it behaves like a class whose "instances" are cloned modules — the
thread proposes `mod Vec do var nums:Int … let nums = Vec.new()` with the module itself as
prototype. The Smalltalk-vs-Self thread reaches the same place from the other direction:
Smalltalk is "a prototype-oriented language, but … some objects have a 'class'-ness", and in
prototype languages "people usually just end up inventing classes".

**Buys.** One concept for namespace, factory and per-instance state; the state-is-just-a-binding
story makes module parameterisation obvious.

**Costs.** Type identity of per-instance members becomes unclear (which clone's `T`?), mutable
module state makes "the module" no longer a single value, and the thread itself asks "how
counter-intuitive could be collapse both things" — its 23 comments are not among the trees
fetched, so its own discussion stays unread.

**Maturity.** Contested — two threads speculate and neither demonstrates, but the fetched
Stockholm comments supply the strongest argument *for* the collapse from the opposite side:
"once you make modules first class, it starts to look a lot like objects", and "if you look at
advanced first-class module systems like 1ML, the similarity to advanced object systems is
striking … it makes a lot of sense to try to unify objects with modules to avoid the duplication
of a whole bunch of functionality" — with the cheaper cousin also on the record, "OOP without
mutation seems to be a close fit for the module system" (comments on "Modules: Overcoming
Stockholm and Duning-Kruger"). Lua users in the other thread report inventing local inheritance
via metatables rather than a global class system.

**Tried by.** Nobody named in the corpus ships it; Smalltalk/JS class systems are cited as
adjacent, not as this design.

**Source.** "Nuts or genius? \"Modules are classes/objects\"", score 40, 23 comments, 2021-06,
https://www.reddit.com/r/ProgrammingLanguages/comments/nxumma/nuts_or_genius_modules_are_classesobjects/
· "I used to categorize Smalltalk vs. Self strongly as class-based vs. prototype-based in my
mind…", score 46, 17 comments, 2020-10,
https://www.reddit.com/r/ProgrammingLanguages/comments/jdv2s8/i_used_to_categorize_smalltalk_vs_self_strongly/
· (comments above) "Modules: Overcoming Stockholm and Duning-Kruger", score 84, 44 comments,
2022-07,
https://www.reddit.com/r/ProgrammingLanguages/comments/vqx19e/modules_overcoming_stockholm_and_duningkruger/

**Bearing on `quill`.** Has it differently, at a precise seam: a **module** is a first-class value
but has *no* methods, `self` or `Self`; a record **struct** has methods and `Self` — the two are
kept apart on purpose (`struct-as-module.md`: "Modules cannot define methods and cannot refer to
`self` or `Self`"), which is this thread's collapse refused.

### One resolution mechanism per name kind — no single global symbol table

**What it is.** Namespaces, overloads, methods, macros and forward references each need their own
rules, so a compiler grows separate tables and passes rather than one hashed global table. The
Namespace Games thread lists five distinct rulesets a real language accumulates (nestable module
paths with aliases; forward-resolving globals needing two scans; lexical names that hook and
unhook at scope exit; order-dependent linear method search; hygienic macro names that must not
collide) and reports refactoring away from the single-table assumption.

**Buys.** Each rule stays explainable — "you wrote `xy.z`, so I looked for `z` in `xy`" (the
80/20 thread's stated goal); order-dependent method lookup and lexical shadowing stop fighting
over one structure.

**Costs.** Several tables must be kept in lockstep across passes — the third thread's exact
problem: the codegen pass cannot reuse the semantic pass's table because the scopes have exited.

**Maturity.** Shipped — implicitly, in every production compiler. The three original
name-resolution threads in Source remain unanswered questions whose trees were never fetched
(scores 2/3/10), but
the axis's *fetched* namespace thread answers the type-vs-value half of the same problem, and its
answers split the way you would expect: Common Lisp keeps separate namespaces for functions,
variables, types, classes and packages plus an escape hatch, because "how often my code includes
scopes which use both the term `list` and the function `list`"; Java splits fields from methods
and "most programmers never notice this, which suggests it works well"; C gives structs, unions
and enums their own tag namespace (`struct foo {…}; foo foo;` is legal); Haskell shares one
namespace between types and constructors and pays for it — "Lots of newbie Haskell questions on
StackOverflow stem from misunderstandings due to the shared namespace." And the case *against*
splitting lands exactly on `quill`'s own premise: "If you ever want to have dependent types … the
distinction between types and terms fades away. In that case, it doesn't make sense to have two
separate namespaces, since types are values" (comments on "Separating the type and value
namespaces?", 26 of 39 retrieved).

**Tried by.** Cone (hobby), Java (the third thread's `org.pack.SomeClass.someMethod()` question),
Common Lisp, C, Haskell, `quill`.

**Source.** "Namespace Games II, the Wreckoning", score 3, 13 comments, 2018-03,
https://www.reddit.com/r/ProgrammingLanguages/comments/822vaa/namespace_games_ii_the_wreckoning/
· "Problems with name resolution", score 2, 12 comments, 2018-04,
https://www.reddit.com/r/Compilers/comments/89xfm4/problems_with_name_resolution/
· "Function vs. method namespaces", score 10, 27 comments, 2018-02,
https://www.reddit.com/r/ProgrammingLanguages/comments/7vj0i2/function_vs_method_namespaces/
· "Separating the type and value namespaces?", score 40, 38 comments, 2021-03,
https://www.reddit.com/r/ProgrammingLanguages/comments/m0pki1/separating_the_type_and_value_namespaces/

**Bearing on `quill`.** Has it differently, by a two-phase split rather than several tables:
**expansion resolves** (scope sets against the **binder table**, which has no order and no
width) and **elaboration locates** (the **Context**, where position is meaning). No name is ever
found by its spelling alone, and the binder table's one namespace carries values, macros and
syntactic roles with a written no-mixing rule.

### Import elaborates the unit once and merges symbols — not textual inclusion

**What it is.** `import` loads the target as its own compilation: parse, check, and merge its
*interface* into the importer's namespace — versus C's `#include`, which splices text. The
corpus's asking threads reach for inclusion first ("Execute the contents of the file and bring
symbols in", "similar to an `#include` statement") and one answer explains the merge problem:
whether names collide, and if overloading exists, whether types must be compared to accept the
merge. Module init order is the second face — a JS thread documents hoisting and separate realms
making module-vs-script globals behave differently than anyone expects.

**Buys.** Compile-once means an imported unit is type-checked once and reused; symbol merging
needs an explicit rule (last-wins, error-on-clash) instead of textual shadowing accidents.

**Costs.** Initialization order and hoisting become semantics, not side effects — the JS thread's
confusion is the cost paid by every compile-once module system; and "merge" needs an interface
extract distinct from the implementation.

**Maturity.** Shipped — Go packages, Java, C++20 modules.

**Tried by.** Go, Java, C++20; `quill`.

**Source.** "How import works?", score 9, 14 comments, 2022-07,
https://www.reddit.com/r/Compilers/comments/vx9g1w/how_import_works/
· "How to approach a module system?", score 14, 8 comments, 2020-11,
https://www.reddit.com/r/Compilers/comments/jr0nnd/how_to_approach_a_module_system/
· "Whats the deal with the Global Environment in JavaScript module code and script code.", score
6, 3 comments, 2024-11,
https://www.reddit.com/r/Compilers/comments/1gljmf9/whats_the_deal_with_the_global_environment_in/
*(none of these three was among the 20 threads fetched for comment trees — their bodies are the
evidence)*

**Bearing on `quill`.** Already has it (named): a unit **elaborates once against the base
context** (atom types, primitives, `stdlib` bound as a name), so its meaning never depends on
what the importer had in scope; a cached term must be **base-anchored**, and moving a *value* is
sound precisely because a value carries its environment — the rule that separates importing a
unit from importing a module.

### Cyclic module dependencies

**What it is.** Let modules import each other in a cycle, instead of demanding a DAG. The C
thread's claim is that C-family module systems all reject cycles, and that cycles are what would
let mutually recursive data structures live in *different* modules instead of one file with
forward declarations — its enabler being that C definition names can be extracted without a
symbol table. The general framing is Graydon's "recursion" row: cycles are fine only when
*weakened*, the way recursive functions and types already are.

**Buys.** Kills forward-declaration scaffolding; lets a program's natural clusters (types ↔
operations) be separate units instead of one forced file.

**Costs.** Name extraction without a symbol table is a C-grammar trick, not a general method;
weakened cycles need a knot-tying semantics (who is elaborated first, and what is visible while
it is), and the corpus shows no language that has settled this.

**Maturity.** Research — one thread, a grammar observation, no implementation reported. Its 3
comments were not among the trees fetched, so nothing was concluded there either.

**Tried by.** The thread's own C experiment; nobody else named.

**Source.** "Adding cyclic modules to the C programming language", score 14, 3 comments, 2026-07,
https://www.reddit.com/r/ProgrammingLanguages/comments/1v7jfsg/adding_cyclic_modules_to_the_c_programming/
· "Modules: Overcoming Stockholm and Duning-Kruger" (the recursion/weakening framing), score 84,
44 comments, 2022-07,
https://www.reddit.com/r/ProgrammingLanguages/comments/vqx19e/modules_overcoming_stockholm_and_duningkruger/

**Bearing on `quill`.** Decided, the other way: **import cycles are an error**, reported by the
syntax load that reaches them first (STATUS; the circular-syntax tests use real import cycles).
Recursion lives at the *binding* level — `rec` groups and `type … and …` chains tie their own
knot — so a cycle is solved by moving a declaration down a level, not by letting units loop.

### Compilation unit ≠ module: units are base-anchored and not first-class

**What it is.** Separate the thing you *compile and cache* from the thing you *write with*. A
unit is the file-level artifact — reached by `import`, closed under a fixed base context, safe
to transport as a term; a module is an ordinary value with a captured context. The alternative
(the corpus's question: "should it be just another 'value' or a separate entity in the language?")
conflates them.

**Buys.** A unit's term can be cached and reused by every importer, because all importers share
one base — which is the whole justification for separate compilation; and "modules must be
closed" stops sounding like a threat to first-class modules, because units, not modules, are
what get closed.

**Costs.** Two concepts where users expect one: `import` gives you a unit, not a module
expression, so you cannot import *part* of a file, pass a file as an argument, or build a unit
at run time; the file boundary decides what is separately compiled, so a too-fine or too-coarse
file layout is a compile-time decision the language has opinions about ("in D and Rust, a module
is no larger than a source file, and that creates some interesting stitching challenges").

**Maturity.** Shipped — Java/Go compilation units; `quill`'s split is the explicit version.

**Tried by.** Java, Go, Rust (units = files), `quill`.

**Source.** "Modules: Overcoming Stockholm and Duning-Kruger", score 84, 44 comments, 2022-07,
https://www.reddit.com/r/ProgrammingLanguages/comments/vqx19e/modules_overcoming_stockholm_and_duningkruger/
· "What's the 80/20 of import & module handling?", score 22, 30 comments, 2026-02,
https://www.reddit.com/r/ProgrammingLanguages/comments/1r6fhq8/whats_the_8020_of_import_module_handling/
· "Definitive text on \"module system(s)\"?", score 30, 42 comments, 2023-08,
https://www.reddit.com/r/ProgrammingLanguages/comments/15wdiqh/definitive_text_on_module_systems/

**Bearing on `quill`.** Already has it (named), and `CONTEXT.md` gives the reason in one line: a
**compilation unit** "is not a module expression and not first-class: it has no context to
capture, so it is the one thing that can sensibly be required to be base-anchored."

### Incremental compilation: unit boundaries decide rebuild cost

**What it is.** Design the language so a change to one unit recompiles only what must change —
stable unit boundaries, a query/pure-function compilation model, and a cache keyed on something
smaller than "the file and everything it transitively includes". Lattner's thesis is quoted in
the corpus as the pre-history: with everything postponed to link time, "any change to a single
source file requires almost complete recompilation of the program."

**Buys.** Fast rebuilds and responsive tooling — one thread's wishlist makes it the standard
exercise textbooks skip ("How do do incremental compilation"), alongside linking and LSP.

**Costs.** It constrains the *language*, not just the build: caching a unit's result requires
that result to depend only on declared inputs, which fights implicit ambient context (a macro
macro's ambient context, an import side effect) and pushes toward content rather than path as
the key.

**Maturity.** Shipped — Zig's incremental compilation (a talk in the corpus), salsa-style query
compilers.

**Tried by.** Zig, Rust-analyzer/salsa, incremental .NET/Roslyn-style compilers.

**Source.** "Inside Zig's Incremental Compilation | mlugg", score 46, 16 comments, 2026-07,
https://www.reddit.com/r/Compilers/comments/1v951kq/inside_zigs_incremental_compilation_mlugg/
*(link thread — its body is empty in the corpus; the title and speaker are the evidence)* ·
"What is the history of incremental compilation?", score 23, 12 comments, 2020-07,
https://www.reddit.com/r/Compilers/comments/hqhmg4/what_is_the_history_of_incremental_compilation/
· "What I wish compiler books would cover", score 146, 36 comments, 2020-04,
https://www.reddit.com/r/ProgrammingLanguages/comments/gavu8z/what_i_wish_compiler_books_would_cover/

**Bearing on `quill`.** Fog, and named: **content-addressed codebase** is the quill-side fog item —
today the `Loader` caches per-process and keyed by file path, and the interleaved driver makes a
cache key a *(definition, context)* pair rather than one hash. Incremental builds are not a
decided feature anywhere in the map.

### Content-addressed definitions instead of path-keyed files

**What it is.** Identify a definition by the hash of its syntax, keep names as separately
resolved metadata, and cache expansion/elaboration results under that hash permanently and across
runs — the source of Unison's "no builds". The corpus's import survey states both the appeal and
the objection in two lines: "Unison and ScrapTalk use a content-addressable networked repository,
which is cute until log4j happens." A comment on the package thread reports a small real
implementation of exactly that objection being handled: Polaris takes "HTTPS imports (with a
content hash to make sure you're not opening up quite as many security holes)" and packs
libraries into self-contained archives so relative imports still resolve (comment on "Package
management and distribution of your language").

**Buys.** Rename and move become metadata edits, not rebuilds; identical definitions compile once
forever; refactoring stops invalidating caches (which is also why it sits next to the incremental
entry above).

**Costs.** Naming detaches from identity, so error messages and tooling must re-materialise names;
supply-chain hashing has the corpus's objection — a content hash is a trust root that can be
poisoned; and hygiene must be normalised before hashing, or two alpha-equivalent programs hash
differently.

**Maturity.** Shipped — Unison, in production use.

**Tried by.** Unison; `quill` has the fog item with two named obstacles.

**Source.** "r/ProgrammingLanguages on Import Mechanisms", score 80, 30 comments, 2023-04,
https://www.reddit.com/r/ProgrammingLanguages/comments/1340z3r/rprogramminglanguages_on_import_mechanisms/
· (comment above) "Package management and distribution of your language", score 46, 41 comments,
2023-02,
https://www.reddit.com/r/ProgrammingLanguages/comments/10s6rj7/package_management_and_distribution_of_your/

**Bearing on `quill`.** Fog item (content-addressed codebase) with the two obstacles already
written down: `quill`'s scope sets are per-run integer sets, so a hash needs a scope-normal form;
and the interleaved driver keys work on *(definition, context)*, not a term alone. The map also
records the order — the `std` restructure and the bootstrap↔compiler interface come first and
would have to be hash-shaped.

### Package management as a language-design constraint (versioned interfaces)

**What it is.** Decide early that distribution is a language concern: dependency resolution,
versioning of *interfaces* (not just implementations), a package layout that maps to the module
system, and a story for libraries that must keep working — "Should packages be delineated by
filesystem locations as in Python and Java or … Ruby where classes can be extended anywhere?"
and "Do you think package management should be tied to module system?"

**Buys.** Answering it with the module system avoids two incompatible notions of "package"
(language vs build); the corpus's operational advice is that this is *configuration management*
and it gets harder with versioned deps; Cargo and `go get` are named as the models people ask to
imitate.

**Costs.** SemVer is contested in the corpus itself — "SemVer sounds good, but people \*\*\*\* it
up periodically. Sometimes on purpose." Versioning pressure leaks back into the language:
interface changes must be detectable, which wants stable, inspectable interfaces — and pinning,
the corpus notes, is its own hazard ("people pinning old 'known working' versions and sticking
to those"). The fetched comments sharpen this from rhetoric into two design forks. First, the
language could enforce it: "Languages should *also* enforce semver-based backwards compatibility
of exports … for every library written in that language", with an explicit choice between going
"all the way to ABI" or "API only since we specialize the binary at runtime" — versioning
becomes a compiler feature, and how far it reaches decides what a backend owes. Second, whether
SemVer is even the right clock: "The difficult part is getting everybody's evolving interfaces
to synchronize. Adding annualized 'editions' atop semvers would surface roughly the right fights
at roughly the right time" — answered in the same thread by "I do think that semantic versioning
('semver') has addressed this." All comments on "Package management and distribution of your
language" (26 of 42 retrieved).

**Maturity.** Contested — shipped everywhere, agreed on almost nowhere; one shipping language in
the corpus (Par) announces packages and explicitly defers versioning. The fetched replies also
report that the right answer is a function of scale: writing from Futhark's own release process,
one reply calls the packaging procedures of a huge industrial language "not appropriate for a
small language maintained by a handful of people or fewer", ships build instructions and static
binaries instead, and calls packaging for every Linux distribution "a huge time sink" — on the
grounds that you can "always add more bureaucracy and infrastructure" later (comment on "Package
management and distribution of your language").

**Tried by.** Cargo/npm/Go modules; Par (packages shipped, versioning "still out of scope").

**Source.** "Package management and distribution of your language", score 46, 41 comments,
2023-02,
https://www.reddit.com/r/ProgrammingLanguages/comments/10s6rj7/package_management_and_distribution_of_your/
· "r/ProgrammingLanguages on Import Mechanisms", score 80, 30 comments, 2023-04,
https://www.reddit.com/r/ProgrammingLanguages/comments/1340z3r/rprogramminglanguages_on_import_mechanisms/
· "Par has a new home at par.run, plus packages, docs, and new language features", score 65, 25
comments, 2026-05,
https://www.reddit.com/r/ProgrammingLanguages/comments/1t3lez5/par_has_a_new_home_at_parrun_plus_packages_docs/
· "Programming for the second half of the 21 century", score 37, 14 comments, 2024-04,
https://www.reddit.com/r/ProgrammingLanguages/comments/1bsxpcb/programming_for_the_second_half_of_the_21_century/

**Bearing on `quill`.** Genuinely new to the project — no ticket or fog item covers distribution
or versioning. The nearest recorded boundary is *internal*: `declare-bootstrap-compiler-interface-once`
plus `prelude-abi-remaining-spellings` fix the bootstrap↔compiler interface's spellings — an ABI
between `std/` and the compiler, not a package story.

### FFI: fixed-signature wrapper libraries over a full ABI-driven interface

**What it is.** Two horns, both in the corpus. (a) A narrow interop layer: the host registers
external functions, or the interpreter loads a shared library from next to the module — Umka's
`.umi` files — where foreign functions must have *specific signatures* and so hand-written
wrappers call the "actual" library. (b) A full FFI that reads foreign declarations and follows
the platform ABI. The axis's own survey gives the consensus in two bullets: "C-style `.h` files
are *considered harmful*"; "Mistake not the platform ABI for C, nor expect it to cater to anything
more sophisticated than C" — Windows's multiple calling conventions included.

**Buys.** (a) is "a naturally cross-platform interface, which is very hard to achieve with the
'true' FFI"; it ships today (Umka's networking library arrived this way). (b) buys ecosystem
reach: no wrapper maintenance, direct use of C/C++ libraries.

**Costs.** (a) taxes every integration with hand-written wrappers that must be rebuilt as
signatures drift; (b) taxes the language with a C-header parser — the third thread: "making a
parser for C++ is almost as complex as making a full compiler for it" — plus calling-convention
and mangling policy pinned to a platform.

**Maturity.** Shipped — both horns are in production languages (Umka for the wrapper route; the
corpus assumes full FFIs exist everywhere).

**Tried by.** Umka (wrappers), Zig/Lua-style host registration; `quill` has neither.

**Source.** "Umka interpreter now automatically links native shared libraries that implement
external functions", score 24, 1 comment, 2020-12,
https://www.reddit.com/r/ProgrammingLanguages/comments/kmisiz/umka_interpreter_now_automatically_links_native/
· "Foreign function interfaces", score 14, 23 comments, 2025-06,
https://www.reddit.com/r/Compilers/comments/1l6pfpi/foreign_function_interfaces/
· "r/ProgrammingLanguages on Import Mechanisms" (the FFI section), score 80, 30 comments,
2023-04,
https://www.reddit.com/r/ProgrammingLanguages/comments/1340z3r/rprogramminglanguages_on_import_mechanisms/
· "How to interface with C++ code?", score 5, 6 comments, 2017-05,
https://www.reddit.com/r/Compilers/comments/6apozd/how_to_interface_with_c_code/
*(none of the FFI threads was among the 20 fetched — "Foreign function interfaces"'s 23 replies
are unread here, so the two horns above are the bodies' argument, not a concluded debate; the
only FFI-adjacent comment anywhere in the 520 retrieved is a package-thread aside that "FFI
interop and interaction with DLLs needs to be done better", permalink under *Package management*
below)*

**Bearing on `quill`.** Fog, and named: **the library-vs-compiler-machinery boundary (UFCS, FFI)**
— "UFCS and FFI are desirable but should not drive the prototype agenda now." What exists is the
**Primitive** floor (a name, a type, a reducer, a failure behaviour), which is `quill`'s current
answer to "how does foreign capability enter the language".

### Name mangling as a linker policy — and the cost of overloads on it

**What it is.** A language with overloading or nested namespaces must map distinct source names
to distinct linker symbols; mangling is the encoding, and its cost is measurable — the corpus
thread opens with an lld slide noting C++ mangling "increases linking times by increasing the
length of keys into the symbol hash-table". The alternative question (short stable ids, symbol
tables keyed by something else) has no retrieved answer.

**Buys.** Mangling is what makes overloading, namespaces and separate compilation work at all
under a C-style linker — the Cone thread lists it as a hard requirement: "deterministic name
mangling that satisfies the linker."

**Costs.** Link-time cost grows with name length; mangling leaks into debugging and interop (a
mangled name is unreadable in a stack trace — a cost the axis's threads assume); and it is the
tax that pushes languages to *fewer* overloads — that same thread abandons UFCS and
function-overloading partly to escape it.

**Maturity.** Shipped — Itanium/MSVC mangling in every native C++ toolchain `[general knowledge,
not from corpus]`; the corpus shows the complaint, not a shipped alternative.

**Tried by.** C++, Rust, Nim (mangled); the "alternatives" thread found none in its 12 comments
(not among the 20 fetched, so its answers are unread).

**Source.** "Alternatives to name mangling?", score 21, 12 comments, 2021-12,
https://www.reddit.com/r/ProgrammingLanguages/comments/rt3bm9/alternatives_to_name_mangling/
· "Function vs. method namespaces", score 10, 27 comments, 2018-02,
https://www.reddit.com/r/ProgrammingLanguages/comments/7vj0i2/function_vs_method_namespaces/

**Bearing on `quill`.** Not reached — `quill` compiles to Core terms and has no linker, so mangling
is genuinely new to the project; the nearest concept is a **Resolved name** (`x#n`, unwritable in
source), which is hygiene bookkeeping, not a link symbol. If a native backend ever exists this
entry becomes a ticket.

### Where impls live, and how they are imported (coherence across units)

**What it is.** The module-system face of typeclasses — *organizational only; where inference and
instance search happen is the types axis*. Two answers: impls are **global** once a crate is
loaded (Haskell/Rust: same answer no matter when or where the lookup happens — what makes
transitive dependencies and ordered collections sound), or impls are **scoped** — found only
through what the unit opened (Agda, modular implicits, `quill`). A third: impls travel *with the
type*, orphans explicit (Scala's companion/implicit scope).

**Buys.** Scoped resolution keeps conflicting impls from ever meeting (canonicity), and it is the
*only* coherent option for a language whose modules are values — there is no link-time registry
for a module built at run time. Global coherence buys transitive-dependency sanity: a sorted map
parameterised by an ordering is mergeable only if one ordering per type exists program-wide.

**Costs.** Contested on both sides, and the corpus carries the legibility complaint directly: "to
truly understand how a piece of code works, you have to clearly remember which implementation of
these overloaded functions are being used" — scoped resolution's standing debuggability cost. The
global side's own critics note global uniqueness is "inherently non-modular", with newtype
wrappers as the ad-hoc workaround. The fetched comments argue the *pro*-global side in its own
terms, and it has to be read as a trade, not as a correction to `quill`: "Say you have modularized
ordering … If you can have multiple different ordering modules floating around, how do you make
sure you can't accidentally confuse your compiler (and yourself) by trying to compare data with
inconsistent orderings? That's a coherence problem" (comment on "Modules: Overcoming Stockholm
and Duning-Kruger") — without one answer per type, an ordering escapes its owner and two of them
meet. `quill`'s recorded reason for rejecting global coherence (it is unavailable when modules are
values) answers *why the trade cannot be had here*, not *whether the trade is worth having*;
the hazard that comment exposes is the one already written down in Bearing below.

**Maturity.** Contested — a live disagreement; the *quill*-shaped design point (lexical scope plus
named instances) is what modular implicits, PureScript and Idris shipped, per
`topics/impl-visibility.md`'s survey.

**Tried by.** Haskell, Rust (global); Agda, OCaml modular implicits, PureScript, Idris (scoped
with naming); Scala (option B); `quill` (scoped, named impls). The comments name the reading that
decides it: the modular implicits paper as "a really good reference on the (in)compatibilities of
modules and typeclasses", and E. Kmett's "Typeclasses vs The World" talk (comment on "Modules:
Overcoming Stockholm and Duning-Kruger").

**Source.** "Ad-hoc polymorphism is not worth it", score 56, 61 comments, 2024-12,
https://www.reddit.com/r/ProgrammingLanguages/comments/1hg4r9v/adhoc_polymorphism_is_not_worth_it/
· "Inheritance and interfaces: why a giraffe is not a fish" (who declares an impl — creator vs
consumer), score 38, 58 comments, 2026-03,
https://www.reddit.com/r/ProgrammingLanguages/comments/1rnm7xi/inheritance_and_interfaces_why_a_giraffe_is_not_a/
· "Modules: Overcoming Stockholm and Duning-Kruger" (coherence as a named constraint), score 84,
44 comments, 2022-07,
https://www.reddit.com/r/ProgrammingLanguages/comments/vqx19e/modules_overcoming_stockholm_and_duningkruger/

**Bearing on `quill`.** Decided and rejected, both ways: **global coherence for impls is rejected**
("unavailable when modules are values"); **impls arrive via `open`, with named impls first** — a
named impl is a compile-time handle, so erasure and specialization stay possible while the escape
hatch exists; the mandatory-open tax the analysis measured is what `open M.{a, b}` then closed.
The residual hazard is recorded: scoped resolution gives up coherence, so every future ordered or
hashed collection inherits it (`design-trait-library-deriving-and-protocols` carries this).

### The expression problem as an organizational choice

**What it is.** Adding a *case* to a closed set of operations and adding an *operation* to a set
of existing types pull in opposite directions; where each is cheap is a property of how the
language partitions code. The thread's own reaching answer: "What I actually want to do is
provide a pattern matched pattern for a scenario and then the expected behaviour. I don't want to
implement 100 methods" — i.e. open extensibility on the operation axis via pattern matching, not
via subclassing.

**Buys.** Naming the axis tells you where new code lands: new type without touching old
operations, or new operation without touching old types — and whether third-party *units* can
extend either side.

**Costs.** The thread documents the failure mode from both directions (new type → touch all
functions; new function → implement for every type) and offers multimethods/protocols as the
candidate cure without evidence they remove the trade rather than move it. A comment supplies
the sharpest statement of *why* the trade exists, quoting Alexander Stepanov: OOP "attempts to
decompose the world in terms of interfaces that vary on a single type. To deal with the real
problems you need multisorted algebras — families of interfaces that span multiple types" — read
in the same comment as inheritance failing the expression problem by construction, with
typeclasses/traits and multi-parameter typeclasses offered as the candidate algebra (comment on
"What do you dislike about class inheritance?").

**Maturity.** Contested — the thread solicits approaches and shows none measured; the cure
(multimethods in Clojure, protocols) is asserted, not demonstrated, in the retrieved body.

**Tried by.** The author's compiler inherits from one base class that defines a `codegen` method
(its own description) and reports it failing; visitors and multimethods are named as the
alternatives.

**Source.** "The expression problem: or implementing an interface means I have to implement 100
methods", score 24, 35 comments, 2022-12,
https://www.reddit.com/r/ProgrammingLanguages/comments/zyvws6/the_expression_problem_or_implementing_an/
· (comment above) "What do you dislike about class inheritance?", score 58, 97 comments, 2020-06,
https://www.reddit.com/r/ProgrammingLanguages/comments/he2wmh/what_do_you_dislike_about_class_inheritance/

**Bearing on `quill`.** Has it differently: the operation axis is answered by **type-case over open
`Type`** (a decided `quill` feature, not a module question) and the type axis by adding an **impl**
— which, being scoped, must then be brought in by an `open`, so third-party extension of an
existing type is still a module-system act. Note the boundary: this entry is about *where code
lives*, not about type-case's semantics.

### Closed-program dispatch: turning open interfaces into tagged unions at compile time

**What it is.** Open-ended interfaces need vtables because the implementation set is unknown —
but the set *closes* at link time, so a compiler could enumerate every implementation of an
interface and compile the dispatch to a tagged union (static dispatch over a closed sum). A
simpler cousin is monomorphizing functions that take interface arguments — the second thread
hand-writes the C translation of a Go program to show it working.

**Buys.** Removes vtable indirection and layout cost with no source changes; makes dispatch
inspectable in the emitted code; for a language that erases dictionaries anyway, it is the same
optimization stated as a whole-program pass.

**Costs.** The thread itself names the edge cases — "dynamic libraries, reflection" — i.e. any
program that can still grow after compilation breaks it; and whole-program enumeration conflicts with
units compiled and cached separately: either the interface's implementations are visible at the
unit's compile time, or the conversion must happen at link time. Note both threads are
*questions*; neither was among the 20 threads fetched for comment trees, so their 20 and 10
replies are unread here, and no implementing language is named.

**Maturity.** Speculative — argued in two threads, not built in anything named in the corpus.

**Tried by.** Nobody has shipped this that the corpus names; Rust's static trait binding is cited
in the second thread as adjacent, not equivalent.

**Source.** "Compile time conversion of interfaces to tagged unions", score 23, 20 comments,
2025-01,
https://www.reddit.com/r/ProgrammingLanguages/comments/1i91i3c/compile_time_conversion_of_interfaces_to_tagged/
· "Compiling interfaces without a vtable?", score 6, 10 comments, 2021-04,
https://www.reddit.com/r/ProgrammingLanguages/comments/miisi3/compiling_interfaces_without_a_vtable/

**Bearing on `quill`.** Undecided and unasked: `quill` records that compile-time specialization and
dictionary erasure are *optimizations, not initial semantics* (`topics/traits.md` via
`impl-visibility.md`), so the semantics never depend on a whole-program pass — but whether one
unit's elaboration may assume another's impl set is nowhere in the map. It would collide with
units elaborating independently against the base context.

### Multiple dispatch: measure the demand before building it

**What it is.** Select the implementation by inspecting more than one argument's types at the
call. The corpus's contribution is a measurement, not a design: a 2009 study of nine programs in
six dispatch-supporting languages found more than two-thirds of functions monomorphic; of the
rest, nearly all dispatch on one (usually leftmost) argument; three-or-more-argument dispatch
occurs in "perhaps 1% of (generic) function definitions", and most generic functions with
multiple instances have exactly two.

**Buys.** Double dispatch eliminates the visitor pattern outright (the study summary's
conclusion); operator-like cases (the expression problem's binary operations) are the natural
second-argument use. The strongest argument *for* going past two is quoted rather than measured:
Stepanov's "you need multisorted algebras — families of interfaces that span multiple types",
which the comment glosses as multi-parameter typeclasses (comment on "What do you dislike about
class inheritance?") — the measurement above is the counterweight to it.

**Costs.** Beyond two arguments the measured demand is ~1%, while the compile-time and
resolution cost is paid everywhere — the ad-hoc-polymorphism thread's complaints (compile time,
LSP latency, cryptic errors, "which implementation is being used" overhead) all scale with
dispatch machinery, not with how often it is used.

**Maturity.** Shipped — the study examined six shipping languages' dispatch forms (languages not
named in the retrieved body); the *recommendation* to cap at two arguments is analysis, not a
spec.

**Tried by.** The six languages of the 2009 study; `quill` takes a different route entirely.

**Source.** "Study of the actual use of multiple dispatch", score 34, 13 comments, 2023-11,
https://www.reddit.com/r/ProgrammingLanguages/comments/1835ekm/study_of_the_actual_use_of_multiple_dispatch/
· "Ad-hoc polymorphism is not worth it", score 56, 61 comments, 2024-12,
https://www.reddit.com/r/ProgrammingLanguages/comments/1hg4r9v/adhoc_polymorphism_is_not_worth_it/

**Bearing on `quill`.** Has it differently: a **trait op resolves from scope at elaboration** — the
innermost impl, most-precise-impl-wins, resolved against the *use's argument types* — and there
is **no run-time dispatch at all**: evidence is dictionary data the compiler erases. So `quill`
pays the multi-argument *selection* cost at elaboration and never at run time, which is a third
position between single-dispatch OO and Julia-style multiple dispatch.

## Threads worth reading in full

Of these, the ones whose comment trees were fetched (and are therefore quoted above) are: *I
hate file-based import*, *Stockholm and Duning-Kruger*, *Import Mechanisms*, *unqualified
imports*, *giraffe is not a fish*, *class inheritance* and *Separating the type and value
namespaces?*. The rest are read from bodies only.

- **"I hate file-based import / module systems."** (28, 134 comments) — the single sharpest
  statement of the anti-file-path position, seven numbered complaints; its fetched replies are
  the best file-vs-logical standoff in the corpus, top comment (+198) against.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1in4fm0/i_hate_filebased_import_module_systems/
- **"Modules: Overcoming Stockholm and Duning-Kruger"** (84, 44) — Graydon's ten constraints
  glossed one by one; the best checklist in the corpus, even though it is a reader's paraphrase.
  https://www.reddit.com/r/ProgrammingLanguages/comments/vqx19e/modules_overcoming_stockholm_and_duningkruger/
- **"r/ProgrammingLanguages on Import Mechanisms"** (80, 30) — a distilled survey of exposed
  names, code location, resources, FFI and package managers; the closest thing to a field summary
  here, and the source of the Unison/content-addressing and ABI lines.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1340z3r/rprogramminglanguages_on_import_mechanisms/
- **"Why are some language communities fine with unqualified imports and some are not?"** (73, 50)
  — the qualified-vs-glob default as a cultural question, with C++/JS/Python/C# compared.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1kyaaf2/why_are_some_language_communities_fine_with/
- **"Inheritance and interfaces: why a giraffe is not a fish"** (38, 58) — the best write-up of
  nominal vs structural interfaces, grades vs clades, and who gets to declare an impl.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1rnm7xi/inheritance_and_interfaces_why_a_giraffe_is_not_a/
- **"What do you dislike about class inheritance?"** (58, 97) — the largest comment count in the
  axis's top set; the two canonical complaints (can't retrofit interfaces; data encapsulation
  tax) stated by practitioners.
  https://www.reddit.com/r/ProgrammingLanguages/comments/he2wmh/what_do_you_dislike_about_class_inheritance/
- **"Are OCaml modules basically compile-time records?"** (18, 13) — the unify-modules-with-records
  question asked cleanly, with the associated-types-leads-to-dependent-typing warning.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1e6d88x/are_ocaml_modules_basically_compiletime_records/
- **"Study of the actual use of multiple dispatch"** (34, 13) — the only measured usage data in
  the axis; read it before designing multi-argument dispatch.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1835ekm/study_of_the_actual_use_of_multiple_dispatch/
- **"What's the 80/20 of import & module handling?"** (22, 30) — a practitioner's minimum viable
  module-system goal list (visibility, grouping, consistent syntax, explainable resolution).
  https://www.reddit.com/r/ProgrammingLanguages/comments/1r6fhq8/whats_the_8020_of_import_module_handling/
- **"Separating the type and value namespaces?"** (40, 38) — the only *fetched* namespace thread
  with real answers: Common Lisp, Java, C and Haskell each splitting (or not) and what it cost;
  read it next to the name-resolution trio, which stayed unanswered.
  https://www.reddit.com/r/ProgrammingLanguages/comments/m0pki1/separating_the_type_and_value_namespaces/

## Gaps and disagreements

- **Comment coverage is now real but capped.** Trees were fetched for the 20 richest threads of
  this axis, 520 comments in all. Each `/comments/<id>.json?limit=100` fetch returns only ~30
  top-level comments regardless of `limit`, so every thread contributes exactly 26, and the
  remainder sit in unexpanded `more` placeholders — 147 across the set, against trees sized at
  1223. Some individual long comments are also cut mid-sentence by the slice (the Stockholm
  thread's *coherence* gloss is one), so a paragraph that stops mid-thought stops because of the
  harvest. **The other 67 of the slice's 87 threads have no tree at all**, and every entry whose
  Source names one of them rests on that thread's **body alone** — in particular the FFI,
  tagged-union/vtable, cyclic-C and name-resolution threads, and *How import works?* / *How to
  approach a module system?* / the JavaScript global-environment thread. Where a body is a
  question with no fetched answers, the entry says so; nothing argued only in unretrieved
  comments is claimed here.
- **Where the community visibly disagrees, now with names attached:** file-based vs logical
  module paths — +198 for files against +78 for explicit imports from million-line experience,
  no verdict, 38 of 97 unretrieved; unqualified vs qualified defaults — a C# designer blaming
  naming standards and tooling, a Dart designer measuring 90% unqualified and blaming the
  static/dynamic split, and a reply disputing the thread's premise about Python; global vs
  scoped impl coherence — the ordering-module argument is the pro-global case in the corpus's
  own words, and `quill` has ruled the other way with its reason recorded; privacy vs testability —
  "supporting poor design on a language level" against "a lot of what you call good design is
  only good *only* because it facilitates unit testing"; composition vs inheritance — ECOOP 89
  and CLOS quoted against the thread's own conclusion; and the giraffe thread's framing attacked
  from inside by several of its own top comments.
- **Where the corpus is still silent:** the two-mode `pub`/`sig` rule and sharing constraints are
  defended by nobody — the sealing comments describe the mechanism and what it buys, not who
  should adopt it; `with`-style signature refinement appears exactly once, as an ergonomics
  complaint about OCaml, and higher-order functors appear nowhere outside Graydon's checklist;
  ABI/interface *versioning* now has proposals (language-enforced semver on exports, ABI-vs-API;
  calendar "editions" over SemVer) but no thread that implements one; incremental compilation has
  a 46-score link thread with an empty body and a 12-year-old history question, and no design
  write-up; nobody in the retrieved corpus reports building cyclic modules anywhere.
- **Evidence hygiene:** several high-scoring threads are hobby-language self-promotion (Flint,
  Seal, Kal, Flow-Wing, Par) and are cited only as "being tried" — and the Flint thread's own
  comments are the proof that this discipline is needed, tearing the author's claimed paradigm
  down to "it's just structs" and "OOP concepts with different concept names and different
  keywords". Cone (Namespace Games, Function-vs-method), Pipefish (de facto interfaces) and
  Tablam (modules-are-classes) are single-author projects — data points about interest, not
  viability. The name-resolution trio scores 2/3/10 and should be read as one author-shaped
  cluster, not a consensus.
- **What you would need beyond this corpus to decide:** the module-system literature the thread
  now hands you by name (Leroy's *A Modular Module System* and *Abstract Types, Manifest Types,
  and Separate Compilation*; Pierce's modules slides and *TAPL*; Dreyer and Rossberg's MixML;
  Mark Jones's parameterised signatures); the modular implicits paper and Kmett's *Typeclasses
  vs The World* (both already pointed at from `topics/impl-visibility.md`); a production
  incremental-compilation write-up (Zig's talk, not its corpus body); and — for `quill` itself — a
  ruling on the open `design-private-type-visibility-model` ticket. The comments moved that ticket
  forward (they pin "opacity" to opaque-vs-transparent ascription and name the languages that
  only seal data types) but did not settle it: nobody weighed constructors-hidden against
  representation-freedom at a module boundary.

## Dissent and corrections

- **Corrected: comment coverage.** The first pass recorded "comment coverage is thin and must not
  be read otherwise" and said no trees existed for the testing thread, the import-elaboration
  trio and the tagged-union/vtable pair. The first claim is replaced by the real figures (20
  threads, 520 comments, ~30 top-level per fetch, 147 in `more`). The testing thread *was* among
  the 20, so that entry's "comment tree was not fetched" was wrong and is rewritten around its 26
  retrieved comments. The import-elaboration trio (lines in *Import elaborates the unit once*) and
  the tagged-union/vtable pair (in *Closed-program dispatch*) were **not** among the 20 — re-checked
  against the slice's 20 permalinks — so those two claims stand, reworded to say why.
- **Corrected: sealing's evidence.** "The corpus barely argues it: the only evidence here is
  Graydon's one-word constraint 'opacity'" is no longer true. The Stockholm thread's comments
  define the term (opaque vs transparent ascription, hiding type equalities including aliases),
  name who lacks it (Haskell, Rust, Swift — data types only), and state what it buys ("like
  newtypes"); the inheritance thread argues opacity *replaces* access modifiers. What the corpus
  still does not contain is an argument about `quill`'s two-mode rule.
- **Corrected: the Graydon gloss.** "Its 'coherence' reading is software-coupling, not
  instance-uniqueness" remains true of the *post*, and the retrieved comments repair it from two
  directions: the ML reader's row-by-row gloss (generativity, opacity, stratification — its own
  coherence paragraph cut off mid-sentence by the harvest) and a second reader who supplies the
  instance-uniqueness reading ("multiple different ordering modules floating around … compare
  data with inconsistent orderings"). The checklist no longer rests on one reader — but it still
  rests on readers, not on Graydon.
- **Not corrected, deliberately: global coherence.** No comment argues that `quill` is wrong to
  reject it. The pro-coherence comment (ordering modules) is recorded above as a contested
  trade-off with `quill`'s stated reason — global coherence is unavailable when modules are values —
  not as a fix to a decision.
- **Unresolved with this material.** The FFI, tagged-union/vtable, cyclic-C and name-resolution
  threads' own replies could not be read: their trees were never fetched, and nothing in the 520
  retrieved comments answers those questions. Any entry resting on them is still a catalogued
  question, not a finding.

# Design philosophy and process — what a language is judged by, and how the work gets run

What a language is *measured* by (reading versus writing, ergonomics, minimality, complexity), where a
feature is *allowed to live* (keyword, library, macro, tool, convention), whether correctness properties
are design goals, why languages are adopted or abandoned, and how the design work itself is organised —
rationales, versioning, prototypes, self-hosting, and the advice economy around all of it. Not inside the
boundary: the type-system mechanisms themselves (that is `types-and-semantics.md`), the macro machinery
(`macros-and-metaprogramming.md`), and how a compiler's passes are laid out (`compiler-architecture.md`).

## How this was gathered

The corpus is 2538 threads from r/ProgrammingLanguages and r/Compilers, dated 2009-01 to 2026-09,
harvested from top/hot listings and keyword searches over a fixed keyword list; this axis drew on the
98-thread `meta-posts` slice plus `corpus.jsonl` greps for topics the slice missed (`sum type`,
`self-host`, `1.0`, `compat`). Comment trees were fetched for the 20 richest threads of this axis,
yielding 520 comments in `slices/meta-comments.md`; each fetch was limited to `limit=100`, which returns
only about 30 top-level comments, and every one of the 20 headers records 0–388 further comments sitting
in unretrieved `more` placeholders — so "the top comments said X" means the *top* of a thread, never the
whole of it. Two threads cited below from `corpus.jsonl` (`minw5w`, `1upwaeg`) were additionally read
through their raw trees in `comments/`, which is disclosed on those entries; no other raw tree was used.
The corpus is what those two subreddits upvoted, not a survey of the field: score measures agreement with
a self-selected audience, not correctness, and several high-scoring threads are promotion for a hobby
language whose design claims were never tested — cited below as evidence that an idea is *being tried*,
never that it works.

## The ideas

### Read-optimisation beats write-optimisation

**What it is.** Judge syntax by how often a reader has to parse it rather than by how fast the author
types it: a form costs the author a keystroke once per edit and costs every reader a parsing beat on
every encounter for the life of the file. The Writability thread argues the competing axis — keystroke
count on a QWERTY keyboard, shift-chords and hand travel — as a legitimate design input, with a scoring
model for how hard each character is to type.

**Buys.** Reader-side friction is multiplied by every future reader, including the original author six
months later; optimising for it makes the language's cost curve improve with team growth instead
of degrading.

**Costs.** Author-side friction is paid all day, every day, by every writer, and it is the side a
designer can actually measure — the thread's whole method is a keystroke model, and three separate
commenters in it say typing speed is not the point (`Alexander_Selkirk`: code "will be read many, many
times more, than to be written"; `Lich_Hegemon`: "if I have to choose between a language that's twice as
hard to type and one that's twice as hard to read I'll choose the first"; `MCRusher`: "typing it out is
never the hard part"). The other side is held by `wooptyd00`, who argues regex is living proof that
"conciseness is what matters not readability", and by the thread's own finding that people will change a
language's operator to save one shift-chord.

**Maturity.** contested — the corpus contains no measurement of reading cost on either side; the only
quantitative material models typing, and the reading claim is asserted, never tested.

**Tried by.** Python and Go (both ship a canonical formatter); Lua's `~=` and APL/BQN's Unicode
symbols as writability- and layout-driven choices that the thread treats as case studies.

**Source.** Writability of Programming Languages (Part 1), score 86, 80 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/10uzi0q/writability_of_programming_languages_part_1/,
2023-02; the counter-position in Unpopular Opinions?, score 158, 417 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/jd30p7/unpopular_opinions/, 2020-10.

**Bearing on `fun`.** Undecided and unowned: no ticket weighs writability against readability. What
limits the choice is the reader's own rule — keywords are a fixed, reserved set of token kinds while
operators are one uniform token shape decided by the binding table — so keystroke arguments reach the
language through `pub infix` bindings in the Prelude, not through the reader. The map's fog item on the
language's flavour after the macro model settles is where this gets decided, and nothing sharpens it
until then.

### The weirdness budget is overrated

**What it is.** Reject the rule that a new language should stay within the syntactic conventions of C or
Java to be learnable, and deliberately use unfamiliar forms where familiarity would be actively
misleading: familiar syntax carrying unfamiliar semantics is the more dangerous choice, and a language
that *looks* different from the language you came from stops you from applying the wrong reflexes.

**Buys.** Syntax can then be chosen for the semantics it has to express — a pure arrow, an effect row, a
hygienic template hole — instead of being bent to fit what C already spent its characters on, and the
visual difference acts as a context cue when a programmer switches languages.

**Costs.** The other side has better adoption evidence in this corpus: unfamiliar syntax is rejected
before the design is ever evaluated. One commenter in the Zig thread spells it out — people "see the
parens and are like 'don't like that; it's too weird'" and move on to a language with "deep-seated
fundamental design mistakes but a familiar-looking face" — and `mamcx` in the sum-types thread says a
language with `begin/end` instead of braces "will be dismissed outright" no matter how much better it is.
Brutality of that kind is paid at adoption time, not at design time, and no designer observes it on
their own project.

**Maturity.** contested — argued on both sides in high-scoring threads, with no corpus entry reporting a
measured learning-curve comparison.

**Tried by.** Haskell (layout), Smalltalk (keyword messages), APL family (symbols); `brucejbell`, who
made the argument, is describing practice in languages that already succeeded despite or because of it.

**Source.** Unpopular Opinions?, score 158, 417 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/jd30p7/unpopular_opinions/, 2020-10 (u/brucejbell,
score 84, arguing for unfamiliar syntax); adoption counter in Why is Zig so much more successful than
Crystal and Nim?, score 87, 246 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/10hu5md/why_is_zig_so_much_more_successful_than_crystal/,
2023-01.

**Bearing on `fun`.** Genuinely new as a stated position — no ticket records a weirdness budget. The
language has already spent on unfamiliarity without discussing it: reserved keywords are recorded in
CONTEXT as a deliberate departure from Honu, which reserves nothing; a bare lowercase name in a pattern
binds while an uppercase one refers, decided by case rather than by Context; and `^name` pins an existing
term. Those are three unintroduced conventions carrying real semantics, which is exactly the trade this
idea advocates — the cost is that every new reader must be told the rule before their first `match`.

### Syntax is a scarce budget: no synonym forms, few keywords

**What it is.** Treat each additional reserved word and each second way to spell one thing as a permanent
tax on every reader and every code review: the implementation cost of `unless` is trivial, but two ways
to say the same thing "gives them more decisions to make and argue about in code reviews with little
benefit in return", so a good design makes every choice the programmer makes a meaningful one.

**Buys.** A small keyword set shrinks the thing a reader must hold in their head, removes a class of
review argument, and — because syntax is a fixed cost paid by everyone — frees the expressive work to be
done where it costs only its users.

**Costs.** The authors of the losing forms hold the other side, and they are not wrong: `xeow` shows that
`until (feof(f1) && feof(f2))` does not invert to anything as readable as `while (!feof(f1) || !feof(f2))`
— De Morgan changes the sentence, so the keyword was carrying meaning, not keystrokes; `glasket_` argues
`if !thing` invites a forgotten `!` at runtime; `raevnos` and `church-rosser` show `when`/`unless` deleting
a `progn` wrapper in Lisp. Excluding the form pushes the clutter onto every call site instead of onto the
grammar.

**Maturity.** contested — the strongest pro-elimination evidence in the corpus is behavioural (Elixir
deprecated `unless`, on record), and the strongest pro-form evidence is the counter-thread's own examples.

**Tried by.** Elixir (deprecated `unless`), Haskell (no `unless` keyword; `when`/`unless` are library
functions taking a monadic argument), Smalltalk (five reserved keywords total), Common Lisp and Emacs
Lisp (all three of `if`/`when`/`unless` as macros).

**Source.** Why don't more languages include "until" and "unless"?, score 147, 237 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1kggvqt/why_dont_more_languages_include_until_and_unless/,
2025-05.

**Bearing on `fun`.** Already has it, in a form stronger than the thread argues for: the keyword set is
fixed and reserved by decision, while the constructs most languages would add keywords for are already
library bindings — `&&` and `||` ship as Prelude `pub infix` templates expanding to `match` over `Bool`
(`add-short-circuit-and-or-operators.md`, closed 2026-07-30), and comparison and equality are Prelude
definitions over Primitives at `std/stage2.fun:13-25`. The budget is real and its ledger is visible:
`Context` reserves a word, a `Template` costs nothing.

### Expressiveness must pay for its tractability cost

**What it is.** Accept a language anywhere on the line between "any string of characters is a valid
program" and "there is only one valid program", but require each widening to buy something concrete —
expressiveness that gains nothing is not a feature, it is a new class of bug. `moon-chilled` states it as
a general tradeoff and applies it to a specific proposal: structured integer literals and arbitrary-base
notation both open bug classes, and the thread never establishes that anyone needs base 32.

**Buys.** A rule for refusing features that are merely *more*: it converts "wouldn't it be neat" into
"which bug class does this close, and which does it open", and it gives a maintainer a sentence to
decline with.

**Costs.** The counter is explicit in the same thread and comes from the person doing the work: the
post's fourth lesson is "don't do more work to make your language less capable — look for cases where you
can get something interesting for free", and `PL_Design` answers the critic directly, "we don't want to
miss out on something that might be great by assuming we should be overly strict in places where it's not
clear that we should be strict." A cost-accounting rule applied too early prunes the experiments that
later turn out to be the point.

**Maturity.** contested — one side has the argument, the other has the shipped language arguing back,
and neither cites a measured outcome.

**Tried by.** The language whose author posted both the rule and its reversal (`2r0000_001a` accepted as
12, defended then disowned); Zig's `comptime` and C++ templates are named in this corpus as
non-substitutes for parametric polymorphism by a commenter who wants *more* expressiveness, not less.

**Source.** Lessons learned over the years., score 152, 76 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/kro7li/lessons_learned_over_the_years/, 2021-01
(u/moon-chilled score 12 on the expressiveness-tractability tradeoff; u/PL_Design score 3 rebutting).

**Bearing on `fun`.** Named, in the philosophy line: Consistency > Flexibility > Correctness, with
Flexibility defined as "willing to trade theoretical properties such as parametricity for practical
power" and type-case on open `Type` declared acceptable — `fun` takes the "expressiveness must buy
something" position and has written down what it is buying. The refusal side is equally written: the
evaluation budget caps what checking may compute (exceeding it is a compile error naming the call),
termination is never checked, and a depth guard over evaluation was rejected rather than shipped.

### Complexity has to live somewhere

**What it is.** Refuse the goal of eliminating complexity and substitute a placement decision: complexity
that is pushed out of the language reappears in documentation, training, conventions, or the reader's
head, and the only successful version of the move is to give it a *well-defined place* with known
boundaries — the quoted line in the thread (from ferd.ca) is "if you're unlucky and you just tried to
pretend complexity could be avoided altogether, it has no place to go. But it still doesn't stop
existing."

**Buys.** It turns a moral argument ("is this language simple?") into an engineering one ("where does the
hard part live, and who pays for it when?"), which can be answered per feature instead of deferred to the
whole.

**Costs.** Admitting where the complexity went is a sales disadvantage — the competing pitch is that the
language itself is small and clean, and `punkbert` in the Zig thread sells exactly that: "a small language
one can learn completely." A language that documents its complexity honestly competes against languages
that advertise their absence of it, and buyers read the advertisement.

**Maturity.** contested — the placement claim is argued, not measured; the corpus holds it as a quotation
inside a typing debate rather than as a case study.

**Tried by.** Rust (borrow checking: the complexity sits in the checker and in the annotations, and
several commenters in this corpus count that as its real cost), Go (the complexity sits in what the
language refuses to let you write).

**Source.** Static vs dynamic typing, would love to hear your opinions, score 40, 148 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/q62w62/static_vs_dynamic_typing_would_love_to_hear_your/,
2021-10 (the "Complexity has to live somewhere" quotation is posted inside this thread).

**Bearing on `fun`.** The unresolved placement question is on the map as fog: the
library-versus-compiler-machinery boundary (UFCS, FFI) is undecided, and Stage 11's decided direction is
one specific placement — "demote language constructs hardwired in the compiler core down into library-level
definitions … should avoid building new compiler machinery unless strictly necessary." So `fun` has the
rule in practice (complexity moves into the Prelude where a program can see it) and has not written the
general principle anywhere as a decision.

### Control flow belongs in the library, not the keyword set

**What it is.** Once a form can be written as a function or a macro over a smaller core, it *is* a library
item: `when`, `unless`, and `if` in Haskell are ordinary functions taking a monadic argument, `if` is
syntactic sugar for a `case` over two constructors, and Smalltalk gets by with five reserved words with
everything else "achieved by what could have been done in a third-party library."

**Buys.** The language's grammar stops growing while its vocabulary does not: new control shapes can be
shipped, fixed, and deprecated by shipping a new definition, with no reader who has not opened the library
noticing, and no new reserved word competing for the fixed set.

**Costs.** A function-call form reads worse than a keyword form at exactly the place clarity matters —
Haskell's `if` "would just look weird because it would require more parentheses", per the same thread, and
`Tysonzero`'s own point ("functions-are-control-flow") concedes that the payoff depends on the language
paying the parenthesis tax. Tooling also loses its hook: a keyword is a grammar node a reader or a tool
can name, while a library call is just a call.

**Maturity.** shipped — Haskell, Smalltalk and Common Lisp have run this way for decades, and the
corpus's only counter-evidence is readability of the resulting forms, not breakage.

**Tried by.** Haskell (`when`/`unless`/`if` as functions), Smalltalk (five reserved words; `ifTrue:` is a
method), Common Lisp (`unless`/`when` as macros), Emacs Lisp (all three conditional forms).

**Source.** Why don't more languages include "until" and "unless"?, score 147, 237 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1kggvqt/why_dont_more_languages_include_until_and_unless/,
2025-05 (u/0xzhzh score 65, u/Jwosty score 12, u/Tysonzero score 25).

**Bearing on `fun`.** Already has it, and is actively extending it: `Bool` and `if` are Prelude
definitions — `type Bool = False | True` in the prelude with `if` expanding to `match`, which let the
dedicated `Core.If` node and its `FIf` frame be removed — `&&`/`||` are `pub infix` templates, `type` is a
`pub syntax` macro at `std/type.fun:234`, and the umbrella ticket
`specify-stage-11-macro-powered-language-features.md` stays open for successive increments. What remains
hardwired is measured, not assumed: the arithmetic five are still Primitives at
`src/Fun.Compiler/Primitives.cs:44`.

### A small core plus a macro system carries the rest of the language

**What it is.** Build the smallest base that parses and evaluates — `if`, goto, arithmetic, functions,
variable declarations, a basic type system — and put a Lisp-like macro system on it, so that everything
above the base is written in the language rather than added to it. The same thread's sibling claim: the
C preprocessor's "expressive power … for its incredible simplicity" is what makes it worth its sharp
edges.

**Buys.** Every feature after the first ten becomes a definition instead of a compiler change, so the
implementation stabilises while the language keeps growing, and a user can inspect or replace any
abstraction because it is written in the language they already know.

**Costs.** Metaprogrammed syntax hides its own semantics from the reader, and the corpus says so
directly: `hashn` writes that frameworks relying on metaprogramming "aren't necessarily bad, but they
obfuscate a lot … Want to change something? Get a phd in computer science in just under 8 years!" A
small-core-plus-macros language trades the compiler's complexity for the reader's — the reader must now
expand forms in their head, and tooling must run the macro system to know what the code even is.

**Maturity.** contested — shipped at the language level (Lisp, Raku, Elixir's `unless` as a macro
removable by the vendor) and simultaneously blamed, in this corpus, for the worst maintenance experiences.

**Tried by.** Common Lisp, Scheme, Elixir, Raku; the `opa6o5` proposal itself (never built — a data point
about interest, not viability); the C preprocessor, praised by `munificent` (score 44).

**Source.** Would like opinion on programming language idea, score 30, 25 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/opa6o5/would_like_opinion_on_programming_language_idea/,
2021-07; counter in Worst Design Decisions You've Ever Seen, score 152, 305 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/uhtxqi/worst_design_decisions_youve_ever_seen/, 2022-05
(u/hashn score 61); praise for the C preprocessor in Unpopular Opinions?, score 158, 417 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/jd30p7/unpopular_opinions/, 2020-10 (u/munificent
score 44).

**Bearing on `fun`.** This is the decided direction, not a proposal: Stage 11's flagship is demoting
language constructs hardwired in the compiler core into library-level definitions, "proving the type
theory carries its own syntax instead of growing more compiler machinery", with increment 1
(`Bool` + `if`) landed and the ticket `specify-stage-11-macro-powered-language-features.md` open as the
umbrella. The obfuscation cost is why `quote` parses at the definition site and why reflection is total
and round-trips to the identity — a macro's output has to be inspectable or the reader has nothing.

### Promote a pattern into syntax only when the boilerplate is uniform

**What it is.** A criterion for when a design pattern has earned a language form: the pattern must have
been written by hand enough times to be boring, and the form must remove boilerplate uniformly rather
than in one favourite case. The thread lists what already crossed that line — iterators became `for each`,
annotations became decorators, factory constructors, Kotlin's `object` — and asks what the threshold is.

**Buys.** It gives a maintainer an answer to feature requests that is neither "never" nor "yes": point at
the hand-written form and ask whether its repetitions are uniform yet, and postpone the syntax until they
are.

**Costs.** Waiting means users keep writing the pattern by hand, and — worse — the pattern hardens into
idiom and library API that the eventual syntax must coexist with. The other side of the risk is
documented in the corpus: features blessed too early get rescinded, which for a released language means
migrating every existing program (`Dart` 1.0 to 2.0 replaced its type system and "did an enormous
migration of all existing Dart code").

**Maturity.** contested — the corpus shows both halves: patterns promoted and kept (iterators), and a
feature shipped, found wrong, and removed at the cost of a whole-language migration (Dart's optional type
system).

**Tried by.** Dart (demoted its design pattern back out of the language), Java/C#/Dart (iterators promoted),
Kotlin (`object`), JavaScript/Python/TypeScript (decorators).

**Source.** At What Point Does A Design Pattern Become A Language Feature?, score 59, 32 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/j6a904/at_what_point_does_a_design_pattern_become_a/,
2020-10; rescission cost in Has there ever been a new feature added to a language long after 1.0, which
was later removed because of unforeseen problems?, score 93, 42 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/mudz94/has_there_ever_been_a_new_feature_added_to_a/,
2021-04, and Worst Design Decisions You've Ever Seen, score 152, 305 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/uhtxqi/worst_design_decisions_youve_ever_seen/, 2022-05
(u/munificent score 172 on Dart 1.0).

**Bearing on `fun`.** `fun` keeps the ledger this idea needs: every promotion is a ticket with `status`
and, once closed, a `resolution:` field, and every refusal is recorded in the design map's rejected list
with its reason —
`M.(e)` local open, `type X = struct {…}`, a separate type grammar, generated-symbol ids, first-match
member lookup (it is last-match), typed operator macros (deferred by ruling). The hand-written form users
would promote lives in `design-std-library-surface.md` (open, decided 2026-09-27), which is exactly where
a pattern waits before it becomes syntax.

### Products are common because sums were expensive; culture keeps them rare

**What it is.** An explanation for why languages make "this *and* that" easy and "this *or* that" hard: it
is not type theory but representation. Concatenating two representations gives you the product for free;
a sum needs a tag plus space for the largest alternative, which C's contemporaries could not afford, so
they shipped `union` and left the tagging to the programmer — and the habit outlived the memory.

**Buys.** It reframes a type-system gap as a historical accident, which is actionable: if the scarcity is
representation- and culture-driven, a new language loses nothing by starting with sums, and a legacy
language can adopt them without admitting its type theory was wrong.

**Costs.** The competing explanations in the same thread are not weak: `editor_of_the_beast` insists the
tag was genuinely "ridiculous to care about today but try and imagine how much memory they were working
with", the original post blames subtype polymorphism and virtual dispatch, and `mamcx` blames developer
conservatism generally ("you can pick any language feature; if the language has `begin/end` instead of
`{ }` then will be dismissed outright"). If the conservatism account is the real one, then adding sums to a
language changes nothing until the culture moves — the feature is necessary and not sufficient.

**Maturity.** contested — three causes are offered in one thread, none is tested, and the observation
itself (older languages lack sums) is uncontested.

**Tried by.** Kotlin, Swift, Rust, F#, Scala (have them); C++, Java, C#, Objective-C (did not, until
recently); `union` as the C-shaped non-answer, called out in-thread as "some memory with size and
alignment to hold either this or that" rather than a real sum.

**Source.** Why are product types so common while sum types are so rare?, score 97, 115 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/minw5w/why_are_product_types_so_common_while_sum_types/,
2021-04. Raw comment tree read from `comments/minw5w.json` (95 comments retrieved; disclosure: this
thread's comments are not in `meta-comments.md`).

**Bearing on `fun`.** Already has it, and has had it since the core was decided: nominal `Constructor`s,
`Pattern head` resolution by scope set, and `match` with a compile-time refusal for a non-exhaustive one
(`non-exhaustive match: … is not matched`, `Elaborator.Match.cs:72`) — and the evaluator may then assume
a selected arm (`Nbe.Match.cs:81`). There is no scarcity to explain: sums are the ordinary case, and
`Option(A)` in `std/option.fun` carries absence. The conservatism half of the argument is the live part
for `fun`, and no ticket addresses it.

### Type safety earns its keep only by deleting runtime checks

**What it is.** A hard criterion for what a type system is *for*: "the entire point of type safety is:
(0) Proving that a specific runtime safety check is unnecessary. (1) Eliminating it. Type safety proofs
that do not lead to the elimination of runtime safety checks are completely useless." A type that merely
documents, or that the runtime re-checks anyway, has not bought anything.

**Buys.** It makes type-system work measurable — count the checks that vanish — and it puts the burden of
proof on every proposed typing rule to name the runtime cost it removes, which is the standard that makes
a checker worth its compile time.

**Costs.** The other side in the corpus does not accept the accounting: `lassehp` argues correctness and
safety have direct commercial value regardless of check elimination ("it doesn't really matter how fast
your code is, if it gives the wrong result"), and `furyzer00` adds that industry already pays for
correctness through on-call, it just is not currently "financially worth" doing more of it early. Under
the strict criterion, a type that proves a *property* no runtime check would have caught anyway — which is
most of a dependently typed core — is worthless, and that is a position its holders have to defend
explicitly.

**Maturity.** contested — the criterion is asserted once at high score and contradicted twice in a
sibling thread, with no measurement on either side.

**Tried by.** Nobody in the corpus attributes the criterion to a specific language; the practical
instances of check-deletion named elsewhere in this axis are exhaustive `match` (no runtime tag test) and
Rust's borrow checker (no runtime ownership test).

**Source.** Unpopular Opinions?, score 158, 417 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/jd30p7/unpopular_opinions/, 2020-10 (u/[deleted]
score 58); counter in Does the programming language design community have a bias in favor of functional
programming?, score 99, 130 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/uh0sez/does_the_programming_language_design_community/,
2022-05 (u/lassehp score 13, u/furyzer00 score 11).

**Bearing on `fun`.** `fun` already spends its type checking on exactly this ledger, twice: a checked
`match` means `Nbe` does no tag search ("the match was checked exhaustive"), and a pure arrow — empty
effect row — is what lets the checker evaluate a call while checking, within the evaluation budget. What
the criterion would question is the *dependently typed* half, where proofs do not delete a runtime check;
`fun`'s recorded answer is the philosophy line, Correctness ranked third and Flexibility (practical power
over parametricity) ranked above it.

### Optional typing's failure mode is the lowest common denominator; a migration path is what saves it

**What it is.** Two verdicts on the same design, both from the person who shipped one of them. Dart's
optional type system "was supposed to give you the best of both worlds … it ended up being more like the
lowest common denominator of both": no inference, so untyped code was dynamically typed and annotated code
needed *more* annotations than a fully typed language; `dynamic` as a top type meant `List<dynamic>` flowed
into `List<int>` with no guarantee. TypeScript, unsound too, succeeded — because it "lets you keep all of
your existing JavaScript and gives you a path to make that code more maintainable."

**Buys.** It separates the two questions a gradual design has to answer — *is the type system sound?* and
*can existing code migrate into it?* — and shows the second one decides adoption. Interop with the
untyped incumbent is the feature; soundness can be traded.

**Costs.** The escape hatch is contagious: the corpus's own experience report is that in a partially typed
TypeScript codebase "`any`, essentially the opt-out type, is infectious, and wherever you try to cut it
off is unsafe". So the migration path buys adoption at the price of a permanently porous guarantee, and
`jesseschalken` notes TypeScript's types are never used for optimisation — the annotations buy navigation
and review, not runtime safety.

**Maturity.** contested — both outcomes are shipped and measured in the thread: Dart 1.0 failed and was
replaced wholesale, TypeScript succeeded while staying unsound. The corpus attributes the difference to
interop, not to type-theoretic quality.

**Tried by.** Dart (replaced its type system in 2.0), TypeScript (unsound, dominant), Flow, Python's
optional type hints (present as annotations with no enforcement).

**Source.** Worst Design Decisions You've Ever Seen, score 152, 305 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/uhtxqi/worst_design_decisions_youve_ever_seen/, 2022-05
(u/munificent score 172 on Dart; u/munificent score 44 and u/jesseschalken score 33 on TypeScript);
`any`-is-infectious in Static vs dynamic typing, score 40, 148 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/q62w62/static_vs_dynamic_typing_would_love_to_hear_your/,
2021-10 (u/[deleted] score 20).

**Bearing on `fun`.** Genuinely new to the project, and mostly not applicable: there is no gradual tier
and no ambient `dynamic`. The nearest decisions point the other way — an unhandled effect is a compile
error, not a deferred one, and `HandledEffectEscapes` refuses rather than relaxes — while the one
planned loosening, universe levels and level polymorphism, is fog (it would widen `Type : Type` into
`Type(i)`, which is a soundness change, not a migration one).

### Non-nullable by default; unsoundness is opt-in

**What it is.** Make the safe form the one you get by writing nothing: references are non-nullable unless
you ask for nullability, so absence is an `Option`-shaped type you choose rather than an ever-present
part of every type. The thread's formulation of why: "Sum types are opt-in, Null cannot be opted out of.
People wouldn't like Option/Result/etc either if it were on literally everything."

**Buys.** Every ordinary reference dereference needs no check and no thought, and the one place absence is
real gets paid for exactly once, at its declaration — which is the shape of argument that makes null the
most-cited "worst design decision" in the corpus (score 100 in the same thread).

**Costs.** Interop with ecosystems built around a universal null, and code that genuinely deals in
partially-absent data everywhere, gets more verbose: every field of a nullable record carries `Option`
through every construction site. `ebingdom`'s counter is not about ergonomics at all but about lying:
keeping null out is *desirable* because a type checker that tells you an unnecessary null check is
unnecessary is doing its job — the cost falls on those who want the check.

**Maturity.** shipped — Rust's non-nullable default is named as the exemplar in the thread, and every
modern language in the corpus is moving the same way.

**Tried by.** Rust (non-null by default), Kotlin (`?`-marked nullable types), Swift, Haskell (`Maybe`),
Ada (bound forms); Java, C#, C, JavaScript and Go as the universal-null side.

**Source.** Worst Design Decisions You've Ever Seen, score 152, 305 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/uhtxqi/worst_design_decisions_youve_ever_seen/, 2022-05
(u/dskippy score 100, u/imgroxx score 24, u/ebingdom score 30, u/umlcat score 37 defending C's separate
null pointer).

**Bearing on `fun`.** No ruling is needed because no `null` exists: `grep -w null std/*.fun` returns
nothing, and absence is `Option(A)` with its own unit (`std/option.fun`), reached as `Std.Options.f` or
opened bare. The stance is therefore a consequence of the nominal-`Constructor` core rather than a decision
anyone can point at — if a future FFI (fog: the library-versus-compiler-machinery boundary) drags a
nullable foreign type in, that will be the first time `fun` has to state this position, and there is no
ticket for it.

### Underspecification buys optimizations and costs correctness

**What it is.** The open question of whether to specify semantics or leave them open, argued in compiler
terms: an optimising compiler "is basically a small proof engine — it replaces program A with program B
only if it can prove equivalence under the language rules", so a language with weak semantics gives a tiny
rule set while a precisely specified language could encode the assumptions explicitly and let the compiler
earn more rewrites. Undefined behaviour is the extreme version — freedom through underspecification.

**Buys.** Specified assumptions are rewrites the compiler may take and a *user* may reason about;
underspecified ones are rewrites nobody can portably depend on. The thread's concrete proposal is a
rewrite system carrying each rule's assumptions forward with the value, turning the optimiser into a proof
assistant for its own transforms.

**Costs.** The three options do not let you square the circle, as `Inconstant_Moo` puts it: "(1) Have the
error result in undefined behaviour. (2) Have runtime checks for the error. (3) Have the programmer jump
through hoops." Every specification is either weaker than you wanted (check stays) or is a burden you now
impose on every programmer (hoops), and `flatfinger` shows the specification itself can be wrong: two
rewrites valid individually become unsound when a compiler chains them, which is a defect in the language
rules, not the optimiser.

**Maturity.** contested — no production language in this corpus is shown landing the "explicit
assumptions as semantics" middle; the endpoints (C's underspecification, Rust's unsafe-corralled UB) both
ship.

**Tried by.** C/C++ (undefined behaviour as licence), Fortran (contiguity as a specified assumption),
Rust and Zig (UB corralled into explicit unsafe regions, per `mother_a_god`), CompCert and SPARK (verified
correctness of the chain, from the formal-verification thread).

**Source.** Is it theoretically possible to design a language that can outperform C across multiple
domains?, score 53, 168 comments,
https://www.reddit.com/r/Compilers/comments/1r2mr96/is_it_theoretically_possible_to_design_a_language/,
2026-02 (u/potzko2552 score 15, u/Sad-Grocery-1570 score 31, u/Inconstant_Moo score 2, u/flatfinger score 4).

**Bearing on `fun`.** `fun` is on the specified side by construction and there is no open question about
it: NbE defines the value, the one evaluation budget is shared between the checker and the evaluator, and
exceeding it is a compile error naming the call rather than a runtime surprise. Termination is never
checked and divergence is not an effect — recorded as a decision, alongside the rejection of a depth
guard over evaluation — so the boundary `fun` draws is *bounded but specified*, not open. Nothing in the rejected list licenses
a program to behave differently from what the rules say.

### Conversion rules must fail loudly; predictable beats convenient

**What it is.** Give each operator one rule and make the wrong combination an error instead of a
helpful-looking conversion: equality is same-type-and-value, `+` adds numbers, `..` concatenates strings,
and cross-type work goes through named functions. The corpus's worked contrast is Lua against JavaScript,
where `[] == ""` is true, `0 == "hello"` is true, and transitivity does not hold for `==` — so the
language had to invent `===` on top of `==`.

**Buys.** The rules fit in a paragraph, are the same for a human and for an optimiser, and admit no
surprise class of bug: `brucifer`'s point is that Lua shows you can be "equally simple … from both a user
viewpoint and an implementation viewpoint" without the coercion matrix, and that the matrix is terrible
for performance too.

**Costs.** Implicit conversion is what lets glue code and APIs compose without ceremony, and every
language that keeps it keeps it for that reason — PHP's inconsistent conventions, mocked in the same
thread, are the price of the convenience, but JavaScript's global dominance is evidence the convenience
wins adoption regardless. The cost of the strict rule is written at every boundary: literals, foreign
data and mixed-width arithmetic all need explicit bridging code.

**Maturity.** contested — both positions ship at scale (Lua and JavaScript are both top-20 languages),
and this corpus reports no comparative defect rates.

**Tried by.** Lua (five explicit rules), Go (few conversions, no overloading), JavaScript and PHP (the
coercion side), Python (errors rather than silent conversion, compared in-thread).

**Source.** Worst Design Decisions You've Ever Seen, score 152, 305 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/uhtxqi/worst_design_decisions_youve_ever_seen/, 2022-05
(u/brucifer score 80 on Lua versus JS); coercion should be an error in The WORST features of every
language you can think of., score 218, 422 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/jn3n4f/the_worst_features_of_every_language_you_can/,
2020-11 (u/[deleted] score 110), plus `0 == "hello"` at score 69 in the same thread.

**Bearing on `fun`.** Conversion is explicit and the accounting is visible: primitives return `I64` and
the Prelude wraps them (Stage 11 increment 1), equality goes through `Eq` impls with type-specialized
equality decided, and a fixity declaration attaches to a value rather than inventing a coercion rule. The
nearest thing to a conversion policy is the member-lookup ruling — dotted paths resolve to the *last*
member of that name, and first-match was explicitly rejected — which is the same predictability move
applied to names instead of values. No ticket covers implicit conversion, because nothing in the language
performs one.

### A scoped escape hatch, not a contagious opt-out

**What it is.** Where a language must permit something its rules forbid, give it one explicit, locally
written marker rather than a pervasive type that quietly disables checking: `unsafe` in a block is
scoped to the block, whereas `any` in TypeScript "is infectious, and wherever you try to cut it off is
unsafe". The Haskell extreme of the same idea — `-fdefer-type-errors -w` — turns every violation into a
runtime error, and the thread offering it is a satire.

**Buys.** Every place the guarantee is suspended is written down and greppable, so a reviewer can audit
the holes without understanding the type system, and the honest 99% of the code keeps full checking.

**Costs.** A scoped hatch still leaks: `Dykam` concedes "bugs can leak much deeper", and the safe-looking
call sites downstream of an `unsafe` block are exactly where the checker is now lying. The alternative —
a contagious opt-out — at least *tells the truth* about the erosion by spreading visibly. Escape hatches
also invite laundering: once one exists, the pressure is to put the awkward code behind it rather than
fix the design.

**Maturity.** shipped — Rust `unsafe`, C's `restrict`, and TypeScript `any` all exist in production; the
corpus argument is about which failure mode is preferable, not about feasibility.

**Tried by.** Rust (`unsafe` blocks), C/C++ (`restrict`, `volatile`), TypeScript (`any`), GHC
(`-fdefer-type-errors`, cited at score 38 as "how to make Haskell much more exciting to use").

**Source.** Static vs dynamic typing, would love to hear your opinions, score 40, 148 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/q62w62/static_vs_dynamic_typing_would_love_to_hear_your/,
2021-10 (u/Dykam score 5 for scoped `any`, u/[deleted] score 20 for the infectiousness counter);
satire in Beyond Opinionated: Announcing The First Actually Bigoted Language, score 221, 49 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/u2lrbo/beyond_opinionated_announcing_the_first_actually/,
2022-04 (u/Athas score 38).

**Bearing on `fun`.** The pattern already exists, and it is `Borrowed context`: building an id whose
scope set is copied from another syntax object is "the one deliberate way to break hygiene", written as an
ordinary value construction rather than a mode, so every break is visible in the code that performs it and
scope sets stay opaque to macros. Effects are the same shape — a `Heap` is named, `Discharge` drops only
what cannot escape, and `HandledEffectEscapes` is a compile error. What `fun` has no analogue for is a
graded weakening (a `dynamic` tier), and the design map records nothing that would introduce one.

### Tooling and ecosystem are table stakes; design merit is not the differentiator

**What it is.** The adoption argument from the Skew post-mortem: a working implementation, nice syntax,
good codegen, an LSP and a package manager are the *entry fee* that "get you nothing more than a seat at
the table", and the actual cost of adoption is measured in tens of millions of dollars of volunteer time
and money — an estimate of ~$100M attributed to Rust. Singular technical merits do not move people off
incumbents.

**Buys.** It explains the otherwise depressing observation that good languages die and mediocre ones
survive, and it redirects effort: if tooling is the gate, then an unfinished language with excellent
design is not *nearly* finished, it is not started.

**Costs.** The other side has a real case and says it in the same thread: `punkbert` — "it's not
marketing, it satisfies a demand" — and `whatever73538`, who describes network effects and FOMO instead:
the Rust community "were incredibly successful in making everyone believe rust was the language of the
future (back when it sucked balls)". If hype can create the demand it claims to report, then "table
stakes" is a description of a self-fulfilling prophecy, and a project that spends all its effort on
tooling has still not answered why anyone should switch.

**Maturity.** contested — the stakes claim is supported by named cases (Skew lacked a package manager and
died; Gleam has one and is predicted to fail anyway, at score 10) and the demand claim by named cases
(Zig), with no corpus post comparing like with like.

**Tried by.** Skew (no package manager, no debugger — cited as fatal), Gleam, V (stars and funding ahead
of Nim and Zig on the strength of marketing), Rust, Dart (survived on Google's budget plus Flutter).

**Source.** How did Skew fail to succeed as a language?, score 76, 89 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1f8uny3/how_did_skew_fail_to_succeed_as_a_language/,
2024-09 (u/XDracam score 99, u/QuarkAnCoffee score 4, u/cxzuk score 21, u/punkbert score 33 contra,
u/whatever73538 score 33).

**Bearing on `fun`.** The project's own tooling answer is fog: the map's first-class compiler API item
(an LSP, a REPL and a macro on one interface instead of re-implementing the elaborator) is undecided
because no downstream consumer exists yet, and `Fun.Expand` cannot reference `Fun.Compiler` by design — today the
only bridge is the fixed `IMacroRuntime` adapter. The CLI is a stub that exits 1, so by this thread's own
measure `fun` has not paid the entry fee; what it has instead is `docs/STATUS.md` as a single authority
and a conformance suite (954 cases, 0 failed, 2026-09-29) that pins behaviour for whoever writes the tools
next.

### A killer app — or an empty niche — decides more than design does

**What it is.** Adoption attaches to a *use*, not to a language: Dart is "nothing without its killer app:
Flutter", Crystal marketed itself as a better Ruby and the "better Ruby" mindshare was soaked up by Go,
Rust and Elixir first, and Gleam's maintainer answers "why Gleam?" with a niche pitch (batteries-included
tooling on the BEAM, zero-cost interop with the Erlang ecosystem) rather than with type theory.

**Buys.** It gives a project a positioning question it can actually answer — what is this *for*, and who
already has the problem — instead of a feature checklist it can never win.

**Costs.** A niche is also a ceiling: `dgc-8` observes that the new systems languages "are competing with
each other in a way that will leave a relatively clear winner … and the other languages won't be that
popular", so choosing a niche that someone else owns means losing on arrival. And the requirement is not
satisfiable by language work at all — the killer app is usually a different project in a different
language domain, which a language-only team may never build.

**Maturity.** contested — the case evidence is strong (Dart/Flutter, Gleam/BEAM) but the corpus also
contains Skew *with* Figma's use and still failing, and V succeeding with no app behind it beyond hype.

**Tried by.** Dart (Flutter), Gleam (BEAM), Crystal (no equivalent platform — explicitly contrasted with
Elixir's "bulletproof BEAM"), Kotlin/Android, Swift/iOS, V (marketing alone).

**Source.** How did Skew fail to succeed as a language?, score 76, 89 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1f8uny3/how_did_skew_fail_to_succeed_as_a_language/,
2024-09 (u/XDracam score 15 on Dart/Flutter, u/lpil score 5 on Gleam's pitch, u/[deleted] score 12 on
Crystal); niche collision in Odin 1.0 announced (and reflections), score 205, 187 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1upwaeg/odin_10_announced_and_reflections/, 2026-07
(u/dgc-8 score 14).

**Bearing on `fun`.** Genuinely new: no ticket in `docs/wayfinder/tickets/` concerns adoption, users, or a
host application, and nothing in the design map states a purpose beyond the philosophy line. The nearest
artefact is the conformance suite, which measures conformance rather than usefulness. If this idea is
taken seriously the first action is not code — it is a sentence of the shape `fun` is for X, and no such
sentence exists in `README.md`'s philosophy block.

### Systems-language waves follow the backend toolchain, not the ideas

**What it is.** A causal account of fashion in language design: in the mid-2000s targeting JVM/CLR was the
cheap way to inherit a library ecosystem, so the wave was Nemerle, Boo, Clojure, Groovy, Scala; as LLVM
matured, Cranelift appeared, and Rust/Swift/Julia proved standalone code generation viable, the wave
moved to systems languages outside the managed runtimes. Demand-side, the wave is one sentence — "I want C, but without the oddities, with
modern features, and I want fast compile times" — and it will keep producing languages until someone
matures into that gap.

**Buys.** It predicts where the next burst of hobby and startup languages will point, and it tells a
designer that a large part of their competitive position is set by infrastructure they do not control.

**Costs.** Riding a wave means entering a crowded field: the same thread names Zig, C3, Odin, Jai, Hare,
V and more, and `initial-algebra` notes you can "punt most of the complexity off to the hypothetical user
and call it a feature" — the wave rewards entry, not quality. A language aimed at the *other* target (GC,
HVM-hosted, DSL) is invisible in this discourse by construction.

**Maturity.** shipped — this is a description of two observed waves with named languages and dates, not a
proposal; its predictive part is untested.

**Tried by.** JVM/CLR wave (Nemerle, Boo, Clojure, Groovy, Gosu, Scala), standalone wave (Rust, Swift,
Julia,
then Zig, Odin, C3, Hare, Jai), with LLVM and Cranelift named as the enabling infrastructure.

**Source.** Why is everyone creating systems programming languages?, score 229, 254 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1v80op7/why_is_everyone_creating_systems_programming/,
2026-07 (u/dudewithtude42 score 133, u/andreicodes score 124, u/SoSKatan score 25); the 1.0 timeline of
the same wave in Odin 1.0 announced (and reflections), score 205, 187 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1upwaeg/odin_10_announced_and_reflections/, 2026-07.

**Bearing on `fun`.** Not applicable on the axis's own terms — `fun` is not a systems language and has no
codegen target: the pipeline ends at `NbE → value`, the three-project split is about front-end layering,
and the one wave-shaped decision (targeting the CLR) is the resolved fog item that produced the C# port
and the deletion of the OCaml prototype. The transferable part is the observation that an infrastructure
choice outlives its fashion: `Fun.Expand`'s inability to reference `Fun.Compiler` is a layering decision
that will constrain tools long after the macro model settles.

### Governance and maintainer stability decide survivability from the inside

**What it is.** Languages die of their own communities: Nim's account is a chain of small failures — a
core developer quitting over conflict with the owner, an undocumented FFI, a broken installer, generics
that "don't work a lot of times", documentation behind a 60 Euro paywall by the author, an LSP maintainer
leaving — plus a design posture ("this language just does everything that looks cool to the devs"). The
complement is the single-creator theory: survival comes down to whether one creator is "obsessed with
their programming language idea above all their other ideas."

**Buys.** It supplies a checklist of non-language risks that a project can actually inspect: is there a
second maintainer, is the documentation open, does the project have a stated purpose, does one person
hold all the knowledge.

**Costs.** Both accounts are unfalsifiable in this corpus and both have costs on the other side: a
benevolent-dictator with an obsession is how the successful languages got made (the same thread credits
Zig's core team choices), while consensus governance produces the RFC-by-committee slowness that the
"too many languages" commenter rails against. Institutional backing buys survival and buys the
interventions nobody wanted (Dart's 2.0 migration was a corporate decision).

**Maturity.** contested — the corpus offers two post-hoc stories with two sets of examples, and no case
where governance was isolated as the cause.

**Tried by.** Nim (community and leadership breakdown, detailed at score 34 and score 12), Zig (core-team
credibility cited positively), Dart (survived via Google), V (a single creator with marketing), Skew
(a single creator who "has bigger ideas than Skew").

**Source.** Why is Zig so much more successful than Crystal and Nim?, score 87, 246 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/10hu5md/why_is_zig_so_much_more_successful_than_crystal/,
2023-01 (u/TriedAngle scores 34 and 12, u/DriNeo score 12); single-creator theory in How did Skew fail to
succeed as a language?, score 76, 89 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1f8uny3/how_did_skew_fail_to_succeed_as_a_language/,
2024-09 (u/breck score 5).

**Bearing on `fun`.** The governance `fun` has is written down and narrow: every ticket carries the same
front matter (`status`, `labels`, `assignee`, `blocked_by`), 36 of the 202 tickets name an assignee
(`glyh`) and the other 166 are unassigned, several carry the label `wayfinder:grilling`, and the map
states in bold "**Needs the user (grilling), do not implement without it**" for a named set — which is a one-maintainer
design with an explicit human gate rather than a community process. The stability risk this idea names is
real and unaddressed: nothing in the repo records what happens to the project if that gate is unavailable,
and the report culture in `CLAUDE.md` (an action-oriented "here is what I did not verify" report is more
valuable than a green one) is the only continuity mechanism written down.

### 1.0 is a contract, not a date

**What it is.** Treat a first official release as a change of obligation rather than a milestone: being
1.0 "carries a different weight and obligation" than being used in production, because after it, removing
anything is a migration and adding anything is a compatibility question. The corpus's survey question is
whether a feature can ever be rescinded after 1.0 — and the answer it produces (Dart's type system)
shows it can, at the price of migrating every existing program.

**Buys.** It gives the pre-1.0 period a purpose: everything that might be wrong can still be wrong there,
which is the only window in which a genuinely wrong design can be fixed by replacement instead of by
deprecation.

**Costs.** Delaying the contract delays the trust that drives adoption — the same thread notes Zig's
"done when it's done" as a cost to its users, while Odin, Jai, C3 and Hare all use a public 1.0 date as
the signal of seriousness. And the contract cuts both ways: a 1.0 promise kept means carrying a mistake
you have since found, which is how a language accretes the errata a small language was supposed to avoid.

**Maturity.** contested — the corpus contains both a company that broke its contract deliberately and
profitably (Dart 2.0) and languages that treat the date itself as the product, with no comparison of
outcomes.

**Tried by.** Odin (1.0 announced 2026-07), C3 (Q2 2028 planned), Jai, Zig, Hare, V (all referenced in
the 1.0 thread); Dart (1.0 → 2.0 migration); Haskell, which famously shipped a compiler that could
delete your source file on a type error — a joke in this corpus about what users tolerated pre-1.0.

**Source.** Odin 1.0 announced (and reflections), score 205, 187 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1upwaeg/odin_10_announced_and_reflections/, 2026-07
(post body on "weight and obligation"; raw comment tree read from `comments/1upwaeg.json`, 98 comments —
disclosure: not in `meta-comments.md`); rescission in Has there ever been a new feature added to a
language long after 1.0, score 93, 42 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/mudz94/has_there_ever_been_a_new_feature_added_to_a/,
2021-04.

**Bearing on `fun`.** There is no version number to be wrong about, and the substitute contract is
stronger than one: `docs/STATUS.md` is the single authority — "when other docs disagree on completion
status, STATUS wins" — and it is dated per entry (last updated 2026-09-29, 954 cases, 0 failed). The
compatibility mechanism that exists is the differential gate: the OCaml prototype was deleted only once
the harness read `port-fails: 0` over 763 programs, with 34 divergences recorded in
`prototype-divergences.txt` (`delete-the-prototype.md`, closed 2026-09-25). What has no contract at all
is the language's own constructs: nothing promises that a program written today elaborates tomorrow, and
no ticket asks
whether it should.

### Write goals, non-goals and a rationale for every decision

**What it is.** Keep a public document per decision: goals *and explicitly non-goals*, design principles,
and one proposal file per feature — the Carbon layout that the thread holds up ("they've got incredibly
well documented rationale"). The habit forces the negative decisions (what this language will *not* do)
to be written while they are still cheap to change.

**Buys.** Non-goals are what a maintainer reads when a feature request arrives, and rationales are what a
new contributor reads instead of reverse-engineering the code — the thread exists because a stranger spent
a week browsing those files and found them worth copying.

**Costs.** Writing is slow and the documents drift: this corpus's own lore is that ticket prose goes stale
faster than code (`CLAUDE.md` records four times in one session where a ticket's prose was older than the
commit that closed it), and a rationale written early can fossilise a decision that experience would have
overturned. Carbon's documents are also untested evidence — the language ships no production use, so the
corpus demonstrates that good rationales are *readable*, not that they produce good outcomes.

**Maturity.** shipped as a practice (Carbon, Rust's RFCs and Go's design documents all run this way);
the claim that it improves the language is speculative.

**Tried by.** Carbon (goals, principles, one proposal per feature), Rust (RFC process), Go (design
documents), Zig (language reference with rationale); no corpus thread reports a project that tried and
abandoned it.

**Source.** Carbon has well documented design rationales, score 117, 69 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/w3juhj/carbon_has_well_documented_design_rationales/,
2022-07.

**Bearing on `fun`.** Already has it, in a form this thread would recognise:
`docs/wayfinder/fun-design-map.md` splits the record into decided / open / fog, every ticket carries
`status`, `labels` and `assignee` and closed ones add a `resolution:` field, and rejected decisions keep
their reasons (the merged heap effect, rejected for the `Alloc`/`Read`/`Write` split; a separate type
grammar; global coherence for impls — "unavailable when modules are values"). The staleness cost is not hypothetical
here and is partially mitigated by the rule that STATUS files outrank plan prose — which is the
documented answer to exactly the failure mode this idea costs.

### You cannot practice design without constraints

**What it is.** The claim that implementation skill can be drilled but design cannot: you can practise a
lexer, a parser, a weekend Lisp, a stack VM, a backend — and become employable — but design means fitting
a product to many conflicting constraints, so a language written purely "to make me smarter" has no
constraints and therefore contains no design decisions at all. Preference ("I prefer curly braces") is as
far as it goes.

**Buys.** It is a direct answer to the corpus's most common failure mode — a hobby language evaluated as
if it had been designed — and it tells the aspiring designer where the real training is: give the
language a purpose and a user, or accept that you are practising implementation.

**Costs.** It delegitimises the sub's usual activity, which is why the thread's score is **0** with 58
comments: hobby projects *are* where the field's practitioners came from, and `deadwisdom` answers the
whole genre — "You all speak with absolute confidence and know fuck all about anything … Until all of us
move towards information based in actual user research I'm going to ignore all this shit." `Mercerenies`
adds the historical defence: the field is thirty or forty years old, "we all have great ideas and we all
have terrible ideas, and we're not really sure which is which", so experience is scarce regardless of
constraints.

**Maturity.** contested — a zero-score thread with 58 comments is the corpus's own verdict, and the
counter-argument (constraints can be *invented*, and the post's own ETA half-concedes this) is unresolved
in the thread.

**Tried by.** Nobody ships this idea; it is a claim about how people work. The practical instances named
in-thread are courses and weekend implementations, which the claim itself says are not design practice.

**Source.** You can't practice language design, score 0, 58 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1i9euws/you_cant_practice_language_design/, 2025-01.

**Bearing on `fun`.** `fun` has what the claim says is missing: a purpose, written as a priority ordering
— Consistency > Flexibility > Correctness — with three named consequences (one construct for many roles;
types are values; type-case on open `Type` acceptable). That is a constraint set that decides real
conflicts: `struct` = record/module/namespace is Consistency beating the Flexibility of separate
constructs, and trading parametricity for practical power is Flexibility beating Correctness. Under this
idea, `fun` is designing rather than practising — which raises the stakes of the fog items (the language's
flavour after the macro model settles, universe levels) because they are the unconstrained part.

### Ask designers about their mistakes, not their principles

**What it is.** A rule for what advice to collect: principles are post-hoc and context-bound, so the
transferable material is the error list — "It's quite useful to ask other language designers about their
*mistakes*." The corollary in the same thread is the title's own provocation: don't listen to language
designers, because their prescriptions ("I've never needed `goto`, `print`, loops, array indexing in 40
years, so I don't see why anyone else should") are autobiography, not design.

**Buys.** Mistakes are falsified claims — someone shipped the feature and users were hurt — so a corpus of
them generalises better than a corpus of preferences, and it survives the speaker's context changing.

**Costs.** Mistakes are also context-bound and get over-applied: the same thread shows Guido defending
indentation sensitivity while "begrudgingly admitting … it was a mistake", so even a genuine mistake does
not reliably transfer to a language with different constraints. And the position is self-undermining —
`mixedCase_` replies "Sure, worked for golang I guess", i.e. the industry runs on principles somebody
shipped successfully, and `Nuoji` replies that not listening is fine "because then you don't need to".

**Maturity.** speculative — argued at high score in both directions, with no example in the corpus of a
project that deliberately ran on mistake-collection and reported the result.

**Tried by.** Nobody identified; the nearest real practice is language post-mortems (the Dart, PHP, Skew
and Nim threads are exactly that, and they are this corpus's most useful material).

**Source.** "Don't listen to language designers", score 118, 135 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/11hil82/dont_listen_to_language_designers/, 2023-03
(u/nrnrnr score 40, u/mixedCase_ score 89 contra, u/Mercerenies score 18, u/deadwisdom score 16).

**Bearing on `fun`.** `fun` already collects mistakes with the reasons attached, which is this idea's
practical form: the design map's rejected list is a register of errors avoided (`M.(e)` local open;
generated-symbol ids; first-match member lookup — it is last-match; a depth guard on evaluation;
phantom type parameters on a generative former), each with the sentence that rules it out, and
`docs/STATUS.md` keeps the port's own
record: `agree 732 | port-fails 0 | prototype-fails 31` over 763 programs, 2026-09-25.
What no document does is record mistakes the *project itself* made and reversed — the fog entry on
elaborator errors carrying no source position, re-measured 2026-10-01 and found to be a wrong premise in
all three counts, is the one place that discipline appears.

### Decide design questions by running them, not by arguing them

**What it is.** Build the cheapest thing that can answer the question: a rapid prototype of the semantics —
at the cost of compiler speed, generated-code performance, error messages, and maybe type inference —
turns an argument about a feature into an observation, and a day of implementation can kill an idea that
would have taken a year of debate. Interpreting first is the same move with a different shape: get the
syntax and semantics working before code generation consumes the project.

**Buys.** Falsification is cheap and early: the corpus's own example is a serialised-binary-representation
prototype that was "a disaster" within one attempt — malloc, random access, branch prediction — and the
author abandoned the idea permanently, which is a result no thread of arguments would have produced.

**Costs.** The prototype is not the language: the prototyping thread itself lists what you give up —
performance, error messages, low-level output, and possibly inference — and every one of those is a
*design* input, not just an implementation detail, so a prototype can pass on a degraded model. The
opposing position in the corpus is structural: some things cannot be prototyped because they cannot be
added later — Ecstasy's designers argue "Security, for example, is one of those things that you can't
'add' to a design; it needs to be baked in", the same for scalability and density.

**Maturity.** contested — prototyping is universal practice and is defended with worked examples; the
counter (properties must be designed in from the start) is defended by a shipped language's architects,
and no corpus entry compares outcomes.

**Tried by.** The binary-format author (`10tnu0m`); Firefly's bootstrapping on Scala as "a poor mans type
check before the type inference was ready"; the general interpreter-first path in `7aj3nl`; `Wester_West`
rewrote a parser ten times in pursuit of simpler syntax.

**Source.** What was the dumbest thing you implemented/prototyped?, score 55, 46 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/10tnu0m/what_was_the_dumbest_thing_you/, 2023-02;
How to prototype a new language design?, score 7, 14 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/3dout9/how_to_prototype_a_new_language_design/, 2015-07;
counter in Why are you building a programming language?, score 107, 93 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/pi84fo/why_are_you_building_a_programming_language/,
2021-09 (u/L8_4_Dinner score 9).

**Bearing on `fun`.** This is the recorded method: the project's rules demand a measurement before a claim
("Measure a gap in the runner before briefing a fork: one command settles what a paragraph cannot"),
debug by instrumentation rather than by editing cases, and the deletion of the OCaml prototype was gated on
a *measurement* — `port-fails: 0` over 763 programs — not on an argument. The Ecstasy counter applies in
its own vocabulary: the three-project split and the strict phase rule are the things `fun` decided up
front rather than prototyped, because a later retrofit would cross the `Fun.Expand`/`Fun.Compiler` boundary.

### One front end: one representation, one parser, one implementation

**What it is.** Resist every second copy of the language's front of house. The thread argues from the
other direction — retain and *annotate* one structure through every stage instead of converting between
representations, because shared nodes with a property mechanism let analysis propagate by construction and
because SSA-style staging impeded, rather than helped, reasoning about optimisation. The tooling argument
is the same shape: languages end up with several parsers (compiler, LSP, ctags, tree-sitter), "the trend
is to not write the parser 2 or 3 times — Clang unifies them".

**Buys.** One representation means one set of invariants to keep true and one place where a fix lands;
duplicated front ends drift, and the drift is invisible until a tool disagrees with the compiler about what
the program says.

**Costs.** The counter is in the same thread and it is concrete: an IDE needs a concrete tree with error
nodes and recovery, and "99% of the time, input text is invalid … because you're still typing", which a
batch compiler never has to handle — so the second representation is not duplication, it is a different
requirement. An annotated single structure also grows the reach of mutation: every stage can write to
every
node, and nothing in the structure stops a later pass from reading a property a stale pass left behind.

**Maturity.** contested — the corpus contains one explicit practitioner for retention and one for staged
representations, both arguing from working systems, plus the industry observation that most compilers do
convert.

**Tried by.** The thread's own GoldenSystems compiler (retention, hobby scale); Clang/MLIR and Microsoft's
compilers (unified front end for tooling); `cxzuk`'s CST-embedding-its-tree design for an IDE; the general
industry practice of SSA staging.

**Source.** Why not retain the AST?, score 41, 54 comments,
https://www.reddit.com/r/Compilers/comments/1w989ra/why_not_retain_the_ast/, 2026-09; duplication counter
and the IDE requirement in Lessons learned over the years., score 152, 76 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/kro7li/lessons_learned_over_the_years/, 2021-01
(u/oilshell score 8, u/cxzuk score 37).

**Bearing on `fun`.** Fun's answer to "a second implementation" was to remove it: `Surface.t` was deleted
as information-throwing duplication of `Syntax.t` (`delete-surface-ir.md`), and the OCaml prototype was
deleted on 2026-09-25 once the port measured as its superset — one reader, one enforester, one elaborator,
and no second implementation for any tool to disagree with. The retention side is visible in the pipeline
itself: there is
no separate intermediate representation between the reader and elaboration, and `Syntax` forms carry
spans directly. The pressure that would recreate a second front end is the map's first-class compiler API fog
item (an LSP wanting its own view), and `Fun.Expand`'s prohibition on referencing `Fun.Compiler` is what
keeps that pressure from being resolved cheaply.

### Self-hosting is a completeness test with a bootstrap tax

**What it is.** Writing the compiler in the language is "a really telling test — for a compiler is a
really complete program. Recursion, trees, abstractions", so it exercises everything a language claims to
have. The thread's own question is whether it is a *necessary* milestone, and the corpus supplies the
tax: a self-hosted compiler creates version chains (each release must be built by the previous one) and
regression traps (the release that broke the compiler is the release you would use to fix it).

**Buys.** Dogfooding at the maximum: any feature the compiler needs is a feature real users need, and
incomplete parts of the language become impossible to hide — the bootstrapping sequence is also its own
end-to-end test of the toolchain.

**Costs.** Two concrete ones, both asked in the corpus: recursion of failure — "any defect in your lang
could affect the compiler in a nasty recursive way" — and the bootstrap chain, where a language change
forces an ordered rebuild through every historical compiler, and a bug introduced with a new feature
leaves no good compiler to compile the fix. The third cost is scope: a language team that must also be
its own first large user has doubled its workload before shipping anything.

**Maturity.** contested — self-hosting is the norm among the production languages named across this
corpus's self-hosting threads, and the threads arguing about it argue about the costs, not the benefits;
the "not necessary" position has no named successful counter-example in the corpus.

**Tried by.** Zig (self-hosted 2022, cited) and Inko (its self-hosting compiler is a corpus thread in
its own right); ABC's teaching compiler `not-abc` "eventually became self-hosting"; C, C++, Rust, Go,
OCaml, Nim, Raku and Scala are the usual examples but are `[general knowledge, not from corpus]` here;
`fun`'s own prelude is *not* in this category.

**Source.** Value of self-hosting, score 19, 41 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1j60mgt/value_of_selfhosting/, 2025-03; How do you
deal with regressions in a self hosted compiler?, score 49, 20 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/qceodm/how_do_you_deal_with_regressions_in_a_self_hosted/,
2021-10; How to manage language versions in a self hosted compiler?, score 19, 18 comments,
https://www.reddit.com/r/Compilers/comments/tw7san/how_to_manage_language_versions_in_a_self_hosted/,
2022-04; Zig Is Self-Hosted Now, What's Next?, score 87, 29 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/ydrz3k/zig_is_selfhosted_now_whats_next/, 2022-10
(body empty — cited as evidence of the milestone's salience only).

**Bearing on `fun`.** `fun` is deliberately not self-hosting: the compiler is C# (`src/`), and the prelude
in `std/` is written in `fun` but compiled by it — which is the half-test, and the reason
`declare-bootstrap-compiler-interface-once.md` and `restructure-std-into-bootstrap-and-library.md` exist:
the bootstrap layer and the library are separated so the prelude can grow without renegotiating the
compiler's interface each time. The regression trap this idea warns about does not apply (a C# compiler
compiles a broken `std`), and the completeness-test benefit is currently unavailable: nothing yet demands
features large enough to be a compiler.

## Threads worth reading in full

- **Lessons learned over the years.** (2021-01) — five practitioner rules with their own rebuttals attached;
  the best single source here on parsing, leniency, and the expressiveness-tractability trade.
- **Worst Design Decisions You've Ever Seen** (2022-05) — the Dart 1.0 post-mortem (score 172) is the most
  informative comment in the whole axis, and the Lua-versus-JavaScript coercion comparison sits next to it.
- **How did Skew fail to succeed as a language?** (2024-09) — adoption economics argued from four named
  cases, including the counter-case that marketing beat merit.
- **Why don't more languages include "until" and "unless"?** (2025-05) — a clean two-sided argument about
  the keyword budget, with Elixir's real deprecation as evidence.
- **You can't practice language design** (2025-01) — score 0 with 58 comments; read it for the fight, not
  for the thesis.
- **"Don't listen to language designers"** (2023-03) — the advice economy of the sub, and `nrnrnr`'s
  mistakes-over-principles rule.
- **Carbon has well documented design rationales** (2022-07) — the template for goals/non-goals and
  one-proposal-per-feature, plus links.
- **Why is everyone creating systems programming languages?** (2026-07) — the toolchain-wave account of
  language fashion, with the LLVM/Cranelift timeline.
- **Why are product types so common while sum types are so rare?** (2021-04) — three competing causes for
  one type-system gap, argued by people who disagree about 1970s memory.
- **Writability of Programming Languages (Part 1)** (2023-02) — the only quantitative material in this
  axis, and the comment section that refuses its premise.

## Gaps and disagreements

**Coverage.** Comments are the weak point and they are weak in a specific way: the 520 comments in
`slices/meta-comments.md` come from 20 threads, each fetch returning about 30 top-level comments out of
`limit=100`, with 0–388 more sitting unretrieved in `more` placeholders — every one of the 20 headers
records a shortfall. The two threads with 400+ comments yielded 26 retrieved comments each. Everything attributed to a
"[deleted]" user above is a real retrieved comment whose author is gone; nothing was inferred from an
unretrieved branch. Two threads (`minw5w`, `1upwaeg`) are cited from raw trees in `comments/`, disclosed
on their entries; how much of each tree those two fetches returned was not checked against a recorded
shortfall.

**What this corpus cannot settle.** No *position* above is settled by a measurement taken in this
corpus. There is no defect-rate
comparison for coercion rules, no learning-curve comparison for unfamiliar syntax, no read-versus-write
cost study, and no case where governance was isolated as the cause of a language's failure. Where two
sides both ship at scale (nullable defaults, coercion, gradual typing, tooling-versus-design), the corpus
records the disagreement and stops — deciding between them needs a defect dataset or a controlled study,
neither of which exists here.

**Where the community visibly disagrees.** The three loudest live conflicts in this axis: *design
empiricism* (prototype and run real programs) against *design-it-in-first* (security, scalability and
density cannot be added later); *constraints-make-design* (score 0) against *the sub's own practice* of
hobby languages as design work; and *table-stakes adoption* (money and tooling decide) against
*satisfies-a-demand* (design and positioning decide). A fourth, smaller one runs through this axis and the
next: whether a macro-powered small core (fun's Stage 11 direction) merely relocates the complexity that
readers complain about.

**What you would need to read to go further.** The primary literature on gradual typing (this corpus only
has second-hand accounts of Dart and TypeScript outcomes); a real comparative study of error rates under
implicit versus explicit conversion; the post-mortems of a language that *did* die of governance rather
than of irrelevance; and any account of a project that ran on written rationales and shipped — Carbon's
documents are praised here but the thread is from 2022 and reports no production use, so the strongest
example in this
corpus is untested by its own standard.

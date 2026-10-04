# Tooling and diagnostics — the compiler's interface to a human

Everything between a running compiler and the person reading it: what a diagnostic says and where
it points, how a source location survives elaboration, what a REPL or a language server needs the
compiler to hand it, how a tool queries the compiler instead of re-implementing it, and how the
suite that pins behaviour and the docs that teach a language are themselves tools. Not inside the
boundary: how to *write* a parser, how to lay out passes, and runtime debugging of the produced
program except where it is an editor-facing query.

## How this was gathered

2538 threads from r/ProgrammingLanguages and r/Compilers (2009-01 to 2026-09), grepped for
`error message`, `diagnostic`, `span`, `LSP`, `language server`, `formatter`, `REPL`, `debugger`,
`incremental`, `IDE`, `autocomplete`, `hover`, `rename`, `exhaustive`, `unification`,
`tree-sitter`, `golden`, `fuzz`, `doctest` and friends, plus the 24-thread `tools` slice and a
corpus-wide re-grep for tooling terms the slice missed. The corpus is what Reddit upvoted: scores
measure agreement with a self-selected audience, not correctness, and several high-scoring threads
are promotion for a hobby language — cited below as evidence that an idea is *being tried*, never
that it works. Comment trees were fetched for the ten richest threads of this axis, yielding 245
comments; each fetch returns only ~30 top-level comments regardless of `limit=100`, with the
remainder left unretrieved in `more` placeholders. This is the thinnest axis in the survey — only
24 threads in the whole corpus bear on it — so this doc leans more on post titles and bodies than
its siblings do, and four further trees sitting in the raw `comments/` directory for other axes'
threads (`10u74ts`, `gavu8z`, `pxytj7`, `1n41akt`) were used where an entry cites them. Every
Source line below was checked against `corpus.jsonl`.

## The ideas

### Rule-oriented versus action-oriented diagnostic wording

**What it is.** Two fixed styles for the sentence a diagnostic prints. Rule-oriented states the
language rule: `Immutable values cannot be reassigned`. Action-oriented states what the program
did: `Reassigning of immutable value`. The choice is made once per language and then enforced at
every throw site.

**Buys.** One policy decided in advance removes a per-error judgement call from a hundred call
sites, and rule-oriented text stays correct when the same rule is broken by several constructs.

**Costs.** The thread poses the question and never settles it: rule-oriented text is longer and
names a rule the reader may not know yet; action-oriented text can describe the symptom rather
than the violation. Nobody in the retrieved corpus reported measuring either.

**Maturity.** contested — the question was put to the sub and answered both ways in the title
itself; its comment tree was not fetched anywhere in this corpus, so the evidence for either side
is not here.

**Tried by.** Both styles are visible in shipping compilers' output; no corpus thread attributes
a specific language to a specific side.

**Source.** Should error messages be rule- or action-oriented?, score 83, 45 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1fai9o3/should_error_messages_be_rule_or_actionoriented/, 2024-09.

**Bearing on `quill`.** Undecided and unowned: there is no ticket for diagnostic wording. The split
that exists is deliberate — `.expect` may only be the token `error`, so wording is *not* pinned by
the conformance suite, while nine xUnit assertions compare an exact `Message`
(`PrimitivesTests.cs:46,69,89`, `EffectTests.cs:50`, `PreludeTests.cs:40`, `InterleavingTests.cs:19`,
`RecTests.cs:57`, `ReaderTests.cs:65`, `ExpandTests.cs:104`). A wording policy chosen now is
enforced today by whichever test first wrote the string.

### Origin-and-use two-point type errors

**What it is.** Print two locations, not one: where the offending value *originated* and where it
is *used incompatibly*, as a `Note: … originates here` / `but it is required to be … here` pair.
The thread shows this working and then names its failure: when the real mistake is at an
intermediate point of the data flow, both points land in the standard library and the user gets
no hint. Printing the whole inference chain is the obvious fix and is "prohibitively long and
generally not useful".

**Buys.** The two-point shape is cheap to compute and turns most errors into a pair of arrows the
eye can follow, instead of a unification dump.

**Costs.** It silently degrades on exactly the errors people actually make — a wrong element type
propagated through three definitions — and the known mitigation (rank the chain and show only the
guessed cause) fails catastrophically when the guess is wrong, which the thread calls "largely an
unsolvable problem".

**Maturity.** research — implemented in IntercalScript and reasoned about for Haskell; no shipping
production language is claimed here.

**Tried by.** IntercalScript (thread author); Haskell's error-guessing heuristics are referenced
but not named.

**Source.** Strategies for displaying type errors with global type inference?, score 27, 7
comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/i2hfti/strategies_for_displaying_type_errors_with_global/, 2020-08.

**Bearing on `quill`.** Genuinely new, and blocked on the same fog item as everything else in this
section: an elaborator error carries `Budget._site` on exactly two variants
(`EvaluationBudgetExceeded`, `EvaluationFailed`) and there is no origin chain at all, so the
two-point shape needs a span on the form being checked first. See the map's *Diagnostics polish
boundary*.

### A vocabulary budget for diagnostics

**What it is.** Treat the words in a diagnostic as a teaching constraint: bootstrap a small
vocabulary in the reader rather than use the most precise term available. "Fewer terms is more
important than maximally precise terms", because `function` means different things in Rust and in
Lisp. The same thread draws a second rule — "better to be enigmatic than misleading": a vaguely
right pointer costs less than a confident wrong one, because the wrong answer is remembered with
high confidence.

**Buys.** A measurable acceptance criterion for message text (does this introduce a term the
reader has not been given?), and a reason to withhold a highlight: only highlight when you are
sure it helps.

**Costs.** Deliberately less precise messages, and the thread's author explicitly *disagrees* with
the cited study's own conclusion that multiple colour-coded regions should be highlighted — so
the evidence base supports at least two readings.

**Maturity.** contested — one paper (Marceau, Fisler and Krishnamurthi, "On novices' interactions
with error messages", linked in the post), two opposite methodological takeaways inside one
thread.

**Tried by.** nobody is named as shipping a vocabulary-budgeted message set.

**Source.** Creating better error messages, score 45, 13 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/q3yk6x/creating_better_error_messages/, 2021-10.

**Bearing on `quill`.** Directly actionable at the existing funnel: `src/` has ~130
`new FunException(` sites, and the open
`specify-stage-12-macro-diagnostics-and-expansion-ux.md` already carries a preferred rewording
("macro `n` promises Expr(I64), but Bool is expected here") instead of the raw `cannot unify I64
with Bool` from `Unify.cs:92`. Whether those sentences share a vocabulary is nobody's ticket yet.

### Design the checker so the error lands on the mistake

**What it is.** Choose the inference algorithm for the *shape of the error it can print*, not for
the type system it accepts: an algorithm whose failure is local to one subterm beats one whose
failure is a global constraint residual, even if both decide the same programs. The idea's title
is a link post — the body was not captured, only the target
(`blog.polybdenum.com/…/designing-type-inference-for-high-quality-type-errors.html`), so the
specific mechanism is unverified from this corpus.

**Buys.** Puts error quality on the design side of the ledger, where it can be argued before the
checker is written, instead of patched afterwards.

**Costs.** Constrains algorithm choice — bidirectional checking and constraint solving are not
interchangeable once their diagnostics are compared, and the well-typed-program performance cost
of the choice is not discussed anywhere in this corpus.

**Maturity.** research — a described implementation behind a blog post; I did not read the
article, only its headline claim.

**Tried by.** not verifiable from the captured material.

**Source.** Designing type inference for high quality type errors, score 72, 12 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1ipiams/designing_type_inference_for_high_quality_type_errors/, 2025-02.

**Bearing on `quill`.** Has it differently: `quill` already decided bidirectional elaboration with
NbE, and `Unify.cs` reports `cannot unify {Describe(left)} with {Describe(right)}` — a
constraint-shaped message. The room is not in the algorithm but in where the exception is caught
and what form is on the budget when it is.

### Syntax density bounds where an error can be pointed

**What it is.** A notation can be too terse for its own diagnostics: every delimiter removed is a
place the checker can no longer anchor a message to, so the failure surfaces somewhere else. The
thread's worked case is F# — an incomplete `match ... with` is reported in the *next* function, a
missing closing parenthesis likewise — and its question is whether a language in which "everything
has a meaning" leaves the compiler any way to choose the most likely error location, reaching for
error-correcting codes and Hamming distance as the analogy (is there a minimum-distance analogue
for syntax?).

**Buys.** It puts error localisation on the design side, where it is free: the density decision is
made once, before any checker exists, and can be asked as a design question ("where would this
error be reported?") while the grammar is still cheap to change.

**Costs.** Terse notation is exactly what the thread's author wants, so the trade-off is real and
unmeasured: no reply reports a language that got density wrong, and nothing in the corpus measures
localisation quality against token count.

**Maturity.** speculative — asked in a 56-comment thread, answered by analogy and by F# war
stories, measured by nobody.

**Tried by.** F# (the reported negative case, for a notation its user already finds terse);
nobody is claimed to have designed a grammar for localisability.

**Source.** Can a language be too dense?, score 36, 56 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/17be7k0/can_a_language_be_too_dense/, 2023-10.

**Bearing on `quill`.** Open and unowned: the map's fog item on *the surface's flavour after the
macro model settles* is where the question would be asked, and no ticket covers it. For now the
limiting factor is not density but the dropped location — `Driver.cs:38-46` re-wraps
`FunException(e.Message)`, discarding whatever span existed upstream — so a span policy comes
first and density becomes the binding constraint only afterwards.

### Stable error codes with a written explainer

**What it is.** Number the diagnostics and ship a lookup: the terminal prints `Error: inexplicable
occurrence of 'else' [5]`, and the user asks the tool for `why 5` and gets a paragraph explaining
when this error fires, that it is usually a knock-on effect of an earlier one, and where to look.
The explainer text is a maintained document, so the short message can stay short forever.

**Buys.** Diagnostic text stops having to be self-contained. The long explanation is written once,
indexed, testable, and reusable as reference documentation — error text becomes the manual's
first page rather than a string literal.

**Costs.** A second artefact to keep in sync with the checker, and a numbering scheme that cannot
be retired; every code that goes stale teaches the reader something false with the authority of a
compiler behind it.

**Maturity.** shipped — demonstrated with real output in the thread (Charm's `hub why 5`), though
Charm is a hobby language: evidence the mechanism works, not that it scales.

**Tried by.** Charm; error-code catalogues are common practice in production compilers, though no
corpus thread names one.

**Source.** Trying to do errors right, score 66, 37 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/vd86ba/trying_to_do_errors_right/, 2022-06
(the same post also supplies the counter-case: Go's `unexpected validVariableName at end of
statement`, which blames the token after the typo).

**Bearing on `quill`.** Genuinely new. It would land on the stage-12 ticket's open question — "how
they reach the user (REPL, loader, driver)" — and it needs a channel that does not exist yet:
`Elab_error` and `FunException` are errors only, with no non-fatal diagnostic kind, so even the
warning the divergence review wants (an unguarded recursive occurrence is uninhabited) has nowhere
to go.

### Recover from a syntax error instead of failing the file

**What it is.** A parser that, on a bad token, records a local error, skips to a synchronisation
point and continues, so later errors are real. The concrete acceptance test from the thread: "any
syntax error anywhere in the file causes the entire file to red squiggly … IMO this is worse than
nothing at all"; TypeScript is named as the counter-example where several errors each report
locally. A second thread asks for the maxim-level rules nobody writes down ("make sure a blank
document is a valid document", "render everything up until the mistake"), and a third observes
that compilers now spend most of their runs producing diagnostics rather than code. A fetched
comment supplies a mechanism nobody wrote down: break the source into a token tree by indentation
and matching delimiters, and on an error skip the rest of the current subtree and jump back to its
parent (comment, score 8, on Language servers suck the joy out of language implementation).

**Buys.** One error becomes ten; the editor shows a plausible tree under broken input, which is
what every language server needs in order to exist at all.

**Costs.** Recovery is a second, approximate parser: it can report errors that are not there, and
it is roughly "a much bigger endeavour than writing a traditional recursive descent parser" (the
LSP thread). The corpus still contains no comparison of recovery strategies — only the request
and one sketch. The requirement itself is contested in the fetched comments: against "an
error-tolerant parser is really the bare minimum", one reply reports "I never bothered
implementing error tolerance and am perfectly happy without it" (comment, same thread). And a
recovered parse must survive every later pass — one commenter's design keeps an error bit on the
root and forces derived roots to copy it, because passes "combine or replace nodes" and "some of
these passes are probably not going to copy an error bit over" (comment, on Please share your IDE
integration stories,
https://www.reddit.com/r/ProgrammingLanguages/comments/c96soz/please_share_your_ide_integration_stories/),
i.e. a recovered error silently un-squiggles when a pass drops the mark.

**Maturity.** shipped — TypeScript is named as doing it well; the strategy itself is argued, not
measured, in this corpus, and one fetched reply reports shipping without it and being content.

**Tried by.** TypeScript (praised), Borzoi (author gave up: "halts after the first parsing
error"), most hobby language servers.

**Source.** What parsing techniques do you use to support a good language server?, score 66, 52
comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/t4c8ms/what_parsing_techniques_do_you_use_to_support_a/, 2022-03;
plus Good design patterns when writing "forgiving parsers"?, score 38, 29 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/ce5o8d/good_design_patterns_when_writing_forgiving/, 2019-07, and
Any good resources for creating actually modern parsers?, score 41, 12 comments,
https://www.reddit.com/r/Compilers/comments/1ejabxw/any_good_resources_for_creating_actually_modern/, 2024-08.

**Bearing on `quill`.** Open, and already written down: ticket `scope-enforester-improvements.md`
item 1 is "structured errors and fault-tolerant parsing (spans, recovery, incremental)". The
measured blocker is positions, not recovery — `src/Quill.Expand` has 173 `throw` sites carrying no
span, and `Driver.cs:38-46` re-wraps them as `FunException(e.Message)`, discarding the location
that existed upstream.

### Mint the span at the token and thread one integer everywhere

**What it is.** Attach a small integer id to every token at lex time and thread it through every
representation, resolving it to line/column only when printing. A fetched comment reports the same
discipline from the other side: everything retained carries a `sourceLocation` with start/stop line
and column plus a source index, and on a `didChange` everything holding that index is dropped and
re-parsed — because "all the language server interaction occurs via line:column pairs" (comment, on
Advice/best practice/architecture pattern for building language with LSP in mind?). The thread this
comes from frames
it as the answer to a specific objection: user-friendly diagnostics "take a shotgun to your
precious modular design", because context has to flow from the lexer to the deepest check.

**Buys.** Diagnostics become orthogonal to architecture — no phase needs to know where the source
is, only to not drop the integer — and a span costs four bytes, not a source buffer.

**Costs.** Every constructor in every representation grows a field, and the discipline is only as
good as the least careful mapping site; a dropped id prints as "no position" and nobody notices
until a user does.

**Maturity.** shipped — the thread author describes running this in production-adjacent code and
credits Roslyn with the same shape.

**Tried by.** Oil Shell (report in-thread), Roslyn.

**Source.** What I wish compiler books would cover, score 146, 36 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/gavu8z/what_i_wish_compiler_books_would_cover/, 2020-04
(body lists "creating good error messages from type inference errors" and "how to build a good
language server" among the topics books skip; the span-integer mechanism is a top comment,
score 12). Also: How do you get good error reporting once you've stripped out the tokens?, score
17, 48 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1sfcdai/how_do_you_get_good_error_reporting_once_youve/, 2026-04.

**Bearing on `quill`.** Half-built, and the map re-measured it 2026-10-01: `SourceSpan` lives in
`Quill.Kernel`, every `Syntax` node and every `Id(Name, SourceSpan, ScopeSet)` already carries one,
and the reflection round trip must preserve it — but `Pattern` (all fourteen variants) and
`EffectRow` have none, `Budget._site` covers only two error variants, and the 130
`new FunException(` sites bucket 73 (span in scope) / 35 (must be threaded) / 22 (no source form
behind it). The map's own verdict: the cheap move is one line, and the real cost is the nine
exact-`Message` assertions.

### Provenance for generated code: point at what was written

**What it is.** When an error is inside macro-expanded text, print a chain back to the written
form rather than the expansion — the C preprocessor's `note: expanded from macro 'X'` cascade —
or, at minimum, attach the application site so the user is sent to their own line. Without it,
expanded output is a place where errors are reported against text nobody typed.

**Buys.** The compiler can report the truth (this failed inside macro `m`, applied here) without
lying about which file to open, and the expansion stays debuggable after the surface is gone.

**Costs.** Chains get long fast for nested macros, and the expansion can be enormous — a thread
participant's complaint about macro debugging is that "the bug may not be in small examples". A
chain also has to be stored, which means provenance is a field on generated nodes forever.

**Maturity.** shipped — the `expanded from` chain is demonstrated in a corpus post body; the
macro-debugging complaint that motivates it is in another.

**Tried by.** clang's C preprocessor (shown in-thread); language macro systems generally.

**Source.** Macros good? bad? or necessary?, score 54, 96 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1n41akt/macros_good_bad_or_necessary/, 2025-08
(comment, score 11, on debugging generated code).

**Bearing on `quill`.** Open ticket, by name:
`specify-stage-12-macro-diagnostics-and-expansion-ux.md`, whose carried-over list is literally
"structured error spans, traceable expansion output, and user-facing macro error messages". The
good news is structural: a syntax object is *a form, its span, and identifiers carrying scope
sets*, so provenance rides the quoted syntax already — the closed
`expansion-errors-reach-the-user-raw.md` left evaluator errors (`EvalError` from `panic`, field
not found, division by zero) raised inside a macro body still carrying no application site.

### Keep the written name on the binder, for errors only

**What it is.** Core terms are unnamed — de Bruijn indices, no identifiers — but every binder
still carries the spelling it was written with, used only when something is printed. That is the
difference between a budget error naming `Fix/VFix/HFix`'s binder and naming a fixpoint address.

**Buys.** Every diagnostic downstream of evaluation can say `rec map` instead of `frame 4`, at the
cost of one unused-in-the-common-case string.

**Costs.** The name can be stale (a binder renamed at the surface but not re-elaborated) or
absent (compiler-generated binders), so the printer needs a fallback; and names are strings, which
is exactly the cost the name-mangling thread measures — C++'s longer mangled keys measurably slow
linking.

**Maturity.** shipped — this is what debuggable evaluators do; the counter-consideration (mangled
keys are hash-table keys and length costs time) is the subject of the cited thread.

**Tried by.** the thread names C++ as the negative case; `quill` carries binder names "for errors
only".

**Source.** Alternatives to name mangling?, score 21, 12 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/rt3bm9/alternatives_to_name_mangling/, 2022-01.

**Bearing on `quill`.** Already has it: `budget-error-names-no-source-call.md` is closed and
implemented — "core lambdas have no names and the error has no span" became `Fix/VFix/HFix` carry
their binder's name, for errors only. The sibling ticket `declaration-binders-keep-written-names.md`
is the surface half of the same decision.

### A REPL is a language-design contract, not a program

**What it is.** The Lisp REPL works because eight language properties hold *as standardised
behaviour*: functions, classes and methods are redefined at runtime with coherent semantics;
definitions can be deleted; errors are handled interactively rather than killing the process;
types, fields and classes are introspectable at runtime; memory is managed automatically; and the
image is the program. The thread's conclusion is that these are design choices, and several of
them are exactly what other languages forbid — redefining a function in Haskell "can wreak lots
of havoc when modules are separately compiled".

**Buys.** A REPL that can do more than evaluate one expression: edit running code, drill into
state accumulated over hours, recover from an error without restarting. That the contract is
constitutive is argued in a second thread's replies: "can you imagine trying to introduce a Lisp
that doesn't have a REPL? … this is kind of what I was getting at with 'the tooling is the
language'" (comment, on The tooling is the language?).

**Costs.** It is paid for in the language, not the tool — separate compilation, static
exclusivity, and ahead-of-time linking all get weaker; a language that wants those cannot have
this REPL and has to build the weaker one instead. The interactive surface also inherits its
host's failures: a standing complaint about Clojure in that same thread is that it "throws out
Java exceptions left, right, and centre" (comment, score 21, on The tooling is the language?),
so a REPL over a host runtime shows raw host diagnostics unless the language traps them.

**Maturity.** shipped — ANSI Common Lisp is the specification-level example in the thread.

**Tried by.** Common Lisp (SLIME), Unison, Erlang/Elixir, Smalltalk; Python and Node offer a
partial version by letting a global binding be replaced.

**Source.** Why don't more languages implement LISP-style interactive REPLs?, score 74, 92
comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/10u74ts/why_dont_more_languages_implement_lispstyle/, 2023-02
(top comment score 61; comment tree partially retrieved).

**Bearing on `quill`.** Greenfield with sharp constraints: `src/Quill.Cli` prints "the .NET port has
no entry point yet" and exits 1, and `Driver`'s own doc comment already calls itself "the pipeline
as the REPL runs it". Two decided properties pull against live redefinition — compilation units
are base-anchored and not first-class, and prelude syntax arrives under a strict phase rule (only
where `std` is opened, in statement order) — so a `quill` REPL would be re-running elaboration over
a growing context, not swapping entries in an image.

### A REPL needs a second evaluator or a persistent world

**What it is.** The concrete engineering fork, reported by someone hitting it: a compiled
language can evaluate a single line easily, but the next line cannot see the first line's
variables, because they were registers and stack. The three known answers are (a) maintain a
second, interpreted execution path so inputs can be evaluated in a live environment, (b) compile
globals through an environment hash map, or (c) append each input to a buffer and re-run
everything — which "is incredibly fragile (anything with side effects breaks) and means
performance gets worse the longer the REPL runs".

**Buys.** Naming the fork early, because (a) is the answer most static languages take and it is
the one with the stated cost: "you have to make [every language change] in both parts of the
codebase".

**Costs.** Exactly the choice above: double the implementation, or a runtime environment indirection
that is not how the language otherwise runs, or a session that cannot survive a side effect.

**Maturity.** contested — the thread offers all three and endorses none; (c) is called bad by its
own reporter.

**Tried by.** libgccjit-based compiled REPL (report in-thread); Erlang/Elixir hot code loading is
named as needing a build tool to know what must change.

**Source.** Why don't more languages implement LISP-style interactive REPLs?, score 74, 92
comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/10u74ts/why_dont_more_languages_implement_lispstyle/, 2023-02
(comment score 11).

**Bearing on `quill`.** Favourable and specific: `quill` already has one evaluator shared by the
checker (`Kont` frames, one evaluation budget), so answer (a) is *not* a second implementation —
the cost moves to state: what a session's `Context`, `Base context` and accumulated `open`
bindings are when the next input elaborates against them. The suite-redundancy ticket supplies the
payoff figure: one case in its own process costs 0.60–0.85 s of host + JIT, "which a REPL session
amortises".

### Editor-grade line editing in the terminal REPL

**What it is.** Treat the REPL input line as an editor: live syntax highlighting, paren matching
with the cursor jumping to the match, auto-indent, and the rest of a readline experience driven
by the same tokenizer the compiler uses. The thread exists because almost nobody does it — the
author can name only CLISP for paren matching and Deno for highlighting.

**Buys.** The interactive surface stops being the least capable tool in the toolchain, and it is
the cheapest editor integration a language can ship — no LSP, no protocol, one terminal.

**Costs.** A second client for the language's grammar, with its own rendering and keybinding
platforms (the example is Node.js), which drifts from the real reader the moment the syntax
changes.

**Maturity.** shipped — demonstrated working in LIPS Scheme.

**Tried by.** LIPS Scheme; CLISP; Deno (highlighting only).

**Source.** REPL with syntax highlighting, auto indentation, and parentheses matching, score 35, 7
comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1ha9l2b/repl_with_syntax_highlighting_auto_indentation/, 2024-12.

**Bearing on `quill`.** Genuinely new and entirely downstream of the CLI existing. It shares the
cost with every other highlighter below: `quill`'s operators are not keywords — they are entries in
a scope-aware binding table, seeded per unit — so correct highlighting is a resolver query, not a
regex.

### One error-tolerant, lossless front end shared by every consumer

**What it is.** Write the reader once, with three properties: it never fails on bad input, it
keeps trivia (comments, whitespace, broken tokens) in a lossless tree, and it is callable as a
library — so the compiler, the language server, the highlighter and the formatter all consume the
same parse. One thread proposes exactly the refactor ("a single 'source of truth' for
lexing/parsing"); another ships a tool built on that premise, a parser-combinator language whose
lossless tree carries "localized errors as first class citizens" and whose stated consumers are
compiler, IDE/LSP and formatters. The fetched replies sharpen why the two are not separable: in
a language server "the source you'll need to handle is invalid essentially 95% of the time"
(comment, on Please share your IDE integration stories), and one implementer declines to build a
compiler first and a server later because "a non-error-tolerant parser would be essentially
useless for the LSP implementation, so writing one would feel like a waste knowing it'd get
thrown away later" (comment, score 6, on Language servers suck the joy out of language
implementation).

**Buys.** No second parser that disagrees with the first, and diagnostics defined once. The
failure mode it removes is the one a whole thread is about: maintaining separate parsers for
interpreter and LSP, plus a Pygments grammar for the docs.

**Costs.** A lossless, error-tolerant tree is a different and larger artefact than the tree a
compiler wants — it must represent partial and malformed constructs — and it is "a much bigger
endeavour than writing a traditional recursive descent parser". Sharing it also couples the
editor's uptime to the compiler's front-end robustness. A cheaper route is reported: define your
own editor protocol and put a facade in front of it that also speaks LSP, trading an intermediate
step for much easier testing (comment, score 6, same thread).

**Maturity.** shipped — tree-sitter and the Chumsky combinator library are named as existing
options; the in-house variant (Gibberish) is a hobby project, evidence of interest only.

**Tried by.** tree-sitter (used, with stated limits), Chumsky, Gibberish, TypeScript's parser.

**Source.** Advice? Adding LSP to my language, score 34, 15 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1iabvh0/advice_adding_lsp_to_my_language/, 2025-01;
Gibberish — a new style of parser-combinator with robust error handling built in, score 79, 17
comments, https://www.reddit.com/r/ProgrammingLanguages/comments/1pwep69/gibberish_a_new_style_of_parsercombinator_with/, 2025-12;
Language servers suck the joy out of language implementation, score 120, 68 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1nukes9/language_servers_suck_the_joy_out_of_language/, 2025-10.

**Bearing on `quill`.** Has it differently, and the difference is a decision already recorded: `quill`
deleted its surface IR on purpose (`delete-surface-ir`, `syntax-vs-surface-ir-layer`), so the
compiler reads exactly one representation. A lossless trivia-preserving tree for the editor would
be a *third* representation — the thing the project removed. The realistic seam is the one
`first-class-elaborator-api.md` names: reflection over `Expr`/`Decl`/`Pattern` as the library
consumers call, with the enforester's spans preserved.

### The highlighting grammar is a second artefact, and for an enforested surface it must be a query

**What it is.** Syntax highlighting in an editor is served by a TextMate grammar that is separate
from the language's grammar, usually written by hand, and the thread states plainly that
generating one from a context-free grammar "is an open research problem" with only partial prior
art. The consequence for an unusual surface: identifiers that are operators in one unit and
variables in another cannot be coloured correctly by a context-free regex at all.

**Buys.** Highlighting works before any compiler infrastructure exists — it is a few minutes of
keyword and delimiter rules, which is why nearly every hobby language has some.

**Costs.** The grammar drifts from the language forever, and the more interesting the surface
(enforestation, operator overloading by binding) the less correct a static grammar can be. The
corpus contains a live example of the alternative going wrong: ClangFormat "formatted" a language
it does not know, changing text without corrupting it — accidental proof that generic tooling
approximates.

**Maturity.** shipped as a practice (hand-written TextMate grammars everywhere); the derivation
problem is explicitly open.

**Tried by.** the thread's author (hand-written); VSCode's tutorial is the default path; ClangFormat
by accident.

**Source.** What parsing techniques do you use to support a good language server?, score 66, 52
comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/t4c8ms/what_parsing_techniques_do_you_use_to_support_a/, 2022-03;
How come does ClangFormat appear to be able to format my programming language?, score 34, 19
comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/is2ydp/how_come_does_clangformat_appear_to_be_able_to/, 2020-09.

**Bearing on `quill`.** Open, and unusually sharp here: operators are unified into a scope-aware
binding table (`unify-operators-into-scope-aware-binding-table.md`), prelude operator demotion
means an operator is a `pub infix` declaration in `std`, and the strict phase rule means an
imported unit gets no prelude syntax. A correct highlighter therefore has to *elaborate bindings
per unit* — which is a client of the first-class API, and the map's fog item on the surface's
flavour after the macro model settles (possibly a `do … end`-ish shape) is what the grammar would
have to follow.

### Debounce, snapshot, discard: background analysis as a thread

**What it is.** A four-step scheme for analysing while the user types: the UI thread does a cheap
lex for rendering; an analysis thread waits for a pause, locks the token list, tokenises and
parses into its own private copy; it then compares the buffer version and, if anything moved,
throws the results away and starts again; only a clean run publishes the error list. Two of the
pieces arrive with numbers in a fetched comment: a 20k-line file parses in under 20 ms but type
inference takes up to 100 ms, so the server must honour `$/cancelRequest` and work only on the
last of a burst of `didChange`s — "design for $/cancelRequest support from the start" (comment,
on Advice/best practice/architecture pattern for building language with LSP in mind?).

**Buys.** The editor never blocks and the parser never sees a mutating string — two failure modes
removed by construction rather than by locks held across the whole analysis.

**Costs.** Under sustained typing, every analysis run is wasted work, and correctness rests on a
version comparison; results are eventually consistent, so a diagnostic can lag the fix that
removed it. The failure mode the snapshot exists to prevent is documented elsewhere in the
corpus: a background type-checker whose lexer reads from a file handle reports errors against
text the user has since edited, which is why Rust and Go lex from pre-read bytes (Parser and
Lexer bike-shedding, cited below).

**Maturity.** research — the four-step scheme was proposed as a plan with no reported outcome,
but a fetched comment reports the same shape built (drop everything holding a changed source
index, cancel superseded requests) and credits cancellation as "key" to responsiveness; nobody
reports measuring it.

**Tried by.** one practitioner's language server (report in a fetched comment); nobody reports
the four-step scheme as written.

**Source.** Resources on concurrent static analysis in an IDE?, score 32, 33 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/tmbuwh/resources_on_concurrent_static_analysis_in_an_ide/, 2022-03;
plus Parser and Lexer bike-shedding, score 36, 36 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/w9eygt/parser_and_lexer_bikeshedding/, 2022-07.

**Bearing on `quill`.** Genuinely new; the nearest existing machinery is `Loader`'s per-process
per-unit caches, which are exactly what a background run would have to copy rather than mutate.
The discard step is cheap for `quill` because elaboration of a unit is a pure function of its
context — but the map's first-class-API guardrail still applies: every question that evaluates
spends from the one evaluation budget, so a speculative background run is spending the user's
budget on work that may be thrown away.

### Cache parsed units and answer navigation from the cache

**What it is.** The mechanism behind fast go-to-definition: load the workspace into memory, parse
each file once, keep a per-file symbol table keyed by name, and answer lookups from it instead of
re-reading the project. The thread asks the question precisely — do you reparse on every save,
how do you hold a 10 GB dependency tree, what happens when the definition's file changes.

**Buys.** Sub-millisecond navigation regardless of project size, and the ability to jump into a
dependency you never opened.

**Costs.** Memory proportional to the workspace, and invalidation: any edit can invalidate any
dependent file, so either you re-parse eagerly (defeating the cache) or you serve stale results
and reconcile. A fetched comment states the rule that makes this survivable: everything tied to a
compilation unit must be self-contained enough to discard whole, and any side table spanning
files must allow easy evicting of the obsolete file's entries (comment, on Advice/best practice/
architecture pattern for building language with LSP in mind?).

**Maturity.** shipped — this is how the tooling the thread admires behaves.

**Tried by.** VSCode's TypeScript language server (the worked example), rust-analyzer by name.

**Source.** How does a language server for a text editor manage looking up function definitions
and such so quickly?, score 56, 11 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/zmky16/how_does_a_language_server_for_a_text_editor/, 2022-12.

**Bearing on `quill`.** Depends on the fog item by name: a symbol table is a projection of a
checked `Context`, and the map's *First-class compiler API* says the ambition is that "a tool, an
LSP, a REPL and a macro all sit on one surface instead of re-implementing the elaborator". Until
that exists, a navigation feature can only re-run `Driver.Elaborate`, whose doc comment says its
only callers are the runner and the CLI.

### Tooling is a language-design constraint, not a bolt-on

**What it is.** Decide tooling questions while the language is still being designed, because the
answers are already fixed by then: the order of a statement determines what completion can offer
(SQL writes its select list before its sources, so predictive input "was not considered" and still
cannot help), whether the type is known at `.` determines whether method completion is possible at
all, and the module and compilation model determines whether analysis can be incremental. The
thread's thesis is that the tooling *is* the language — "can you imagine trying to introduce a
Lisp that doesn't have a REPL?"

**Buys.** Cheap wins are settled before any code exists: one reply argues for an easier-to-parse
grammar (LL(2), say) over a slightly easier-to-write one, because "tooling will frequently involve
parsing" — C++ is named as the case where many tools just hammer one compiler's parser into
shape — and a new language competes against languages with mature tooling, so shipping some keeps
the fight from being lost before anyone arrives.

**Costs.** Contested in the thread's own replies: successful languages shipped with no tooling at
all (Lua, sed, Make, awk; "there was no Java IDE when Java was released"), Go is named as the
counter-example that shipped tooling at 1.0, and one reply holds that good tooling "can make a
mediocre language better, but … doesn't rescue a language that is already unpleasant" — the
constraint can invert into designing for the tools instead of for the programs.

**Maturity.** contested — one thread argues both sides at length without a verdict; the evidence
is opinion and history, not measurement.

**Tried by.** Go (shipped with tooling at 1.0, in-thread); SQL is the thread's named negative
case; and Kip, where a commenter supplies the missing tooling himself: "We just need an IDE
extension that gets us the vowel harmonized allomorph of each case suffix" (comment, score 3, on
Kip: A Programming Language Based on Grammatical Cases in Turkish,
https://www.reddit.com/r/ProgrammingLanguages/comments/1qg42ci/kip_a_programming_language_based_on_grammatical/).

**Source.** The tooling is the language?, score 70, 36 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/vvhk20/the_tooling_is_the_language/, 2022-07.

**Bearing on `quill`.** Has it in part already, by decisions rather than by ticket: operators are
unified into a scope-aware binding table and the strict phase rule governs what an imported unit
may see — both tooling-facing choices made at design time — so a correct highlighter or completer
has to elaborate bindings per unit, a client of the fogged first-class API. What is open is the
position question this entry names: no ticket records what completion may assume at a given
expansion position.

### The language server should be a client of the compiler, not a second implementation

**What it is.** Design the compiler so the editor's server is a consumer of the same driver the
CLI uses, rather than a re-parse-and-guess layer beside it. The thread puts it as a question —
"a Language Server needs to be built anyway and the code gets duplicated … would it be a good
idea to create a language in such a way that the Language Server is an integral part of the
compiler" — and cites rust-analyzer's absorption into the Rust toolchain as the direction of
travel. The fetched replies add the two concrete routes: Merlin's experience report says classic
lexing-parsing-typing pipelines "can easily be adapted to be incremental for a Language Server,
especially when they are using immutable data structures" (comment, on Advice/best practice/
architecture pattern for building language with LSP in mind?), and a staging step that defers the
protocol entirely — implement the queries as a CLI first (where is this symbol defined, what is
its type), so the LSP arrives last (comment, on Language servers suck the joy out of language
implementation).

**Buys.** One set of answers to "what does this mean", so hover, completion and diagnostics agree
with the build instead of approximately agreeing; and the compiler's own tests cover the editor's
behaviour.

**Costs.** The compiler must be buildable as a library, interruptible, and tolerant of incomplete
input — none of which a batch compiler needs. The opposing thread is explicit that this is what
"takes the joy out": the protocol's ceremony, the error-tolerant parser, in-memory files instead
of disk files, and a query architecture the author calls "overkill for my languages, which are
never going to be used on large projects". Two commenters add the protocol's own cost: UTF-16
code-unit positions are "absolutely insane" and line-based text sync "a questionable design
choice" (comments, scores 74 and 16, same thread) — a representation every span-bearing compiler
must convert into on the wire.

**Maturity.** contested — one thread argues it is the future and cites rust-analyzer; the
highest-scoring thread in this axis argues the whole requirement wrecks the pleasure of the
project and that a partial, buggy server is good enough.

**Tried by.** Rust (RLS replaced by rust-analyzer) — with a correction from a rust-analyzer
team member's comment in the counter-thread: rust-analyzer is "basically a standalone,
latency-sensitive compiler for Rust", sharing libraries with rustc but invoking the compiler
directly only for diagnostics through the build system, so what shipped is a hybrid rather than a
thin client (comment, score 4, on Language servers suck the joy out of language implementation);
and OCaml's Merlin, whose authors' experience report is cited in a fetched comment. The
counter-example in-thread is a hand-rolled Haskell server abandoned mid-way.

**Source.** Language Server built into the compiler?, score 53, 16 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/lz8skk/language_server_built_into_the_compiler/, 2021-03;
against it, Language servers suck the joy out of language implementation, score 120, 68 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1nukes9/language_servers_suck_the_joy_out_of_language/, 2025-10.

**Bearing on `quill`.** This *is* the fog item: `docs/wayfinder/topics/first-class-elaborator-api.md`,
staging step 5 is "build the LSP, the REPL and the CLI on that one API rather than beside it",
with the recorded constraint that `Quill.Expand` cannot reference `Quill.Compiler` and today exactly
one adapter (`IMacroRuntime`) crosses the line.

### Incremental compilation for editor latency — the query graph from the tooling side

**What it is.** The compiler re-architected as a graph of memoised queries keyed on inputs, so an
edit re-runs only the queries whose dependencies changed and an IDE can ask any question at any
time. The mechanism and its full counter-argument are catalogued in `compiler-architecture.md`
under *Query-based incremental compilation — and the case against it*; this entry is the
tooling-facing half — what the graph buys an editor, and what it asks the language to expose. The
opposition on this side is not only the "against" thread: an author in this axis's largest thread
says he read about the architecture and decided it "feels overkill".

**Buys.** Edit-to-diagnostic latency proportional to the change, and a natural shape for
interactive questions (any query can be asked from a tooltip without a special path) — which is
why a fetched comment recommends building the queries as a CLI first (where is this symbol
defined, what is its type, what is its SSA/CFG) so that the LSP protocol arrives last (comment,
on Language servers suck the joy out of language implementation).

**Costs.** The same comment turns the size objection around: if a query architecture is too
complex for a language that will never be used on large projects, then a language server is
equally out of scope (comment, score 74, same thread) — the objection is consistency, not size,
and the alternative on offer in that thread is a hand-written server with no error tolerance,
one reply reports shipping happily. Whether the latency actually pays is unmeasured in this
corpus; the architecture-side costs (cycles, node location data, error propagation) are
catalogued in `compiler-architecture.md`.

**Maturity.** contested — shipped in rust-analyzer and Zig's incremental compilation (both cited
in this corpus), and publicly argued against in a 103-score thread with no rebuttal captured.

**Tried by.** rust-analyzer/salsa — named again in a fetched comment, with ollef's
"query-based-compilers" post as the design walkthrough (comment, on Advice/best practice/
architecture pattern for building language with LSP in mind?) — Zig (`Inside Zig's Incremental
Compilation` is a link post to a talk), Buckleym's rustc queries [general knowledge, not from
corpus].

**Source.** Query-based compiler architectures, score 121, 12 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/hfs53y/querybased_compiler_architectures/, 2020-06;
Against Query Based Compilers, score 103, 29 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1rf9g7j/against_query_based_compilers/, 2026-02;
Inside Zig's Incremental Compilation, score 46, 16 comments,
https://www.reddit.com/r/Compilers/comments/1v951kq/inside_zigs_incremental_compilation_mlugg/, 2026-07.
Neither of the two directly opposed threads had its body or comments captured — only the titles
and scores, which is the thinnest evidence in this document.

**Bearing on `quill`.** Named and deliberately parked: the map's *Content-addressed codebase*
fog item describes Unison's hash-keyed caching as the same move, and records two obstacles
specific to `quill` — scope sets are per-run integer sets needing a scope-normal form for hashing,
and the interleaved driver makes a cache key a *(definition, context)* pair rather than one hash.
`Loader`'s per-process dictionaries are where the current non-incremental behaviour lives.

### Expose the compiler's own tree instead of shipping third-party re-parsers

**What it is.** Give tools the compiler's parsed representation over an API — for outlining,
completion, go-to-definition — instead of forcing every editor feature onto a separately written
parser (LSP, ctags, tree-sitter), which the thread calls "inaccurate or resource-intensive". A
fetched comment gives the checklist version: before writing any IDE integration, make the
compiler (1) report syntax and semantic errors, optimally with an emit-as-JSON option, (2) answer
suggestions from the cursor's position, (3) rename, extract and format, (4) offer smart
suggestions such as auto-import, (5) expose a debugger and stack tracer — "if you are able to
achieve the features above, you can basically integrate your language with any IDE" (comment,
score 9, on Please share your IDE integration stories; the author's own language, Keli, is a
hobby project).

**Buys.** Tools inherit the real grammar, real name resolution and real macro expansion, so a
macro-defined form is navigable without anyone re-implementing it.

**Costs.** Every exposed node is a stability commitment: the internal representation can no longer
change without breaking downstream tools, and the exposure must be versioned or frozen, which is
precisely the cost the compiler-as-a-library entry below pays.

**Maturity.** speculative — argued in the thread as "it should be the norm", with the thread
author unsure whether any language does it; no example is produced in the retrieved material.

**Tried by.** not established in this corpus.

**Source.** Why don't most programming languages expose their AST (via api or other means)?, score
52, 29 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1blldxz/why_dont_most_programming_languages_expose_their/, 2024-03.

**Bearing on `quill`.** The cheapest version already exists and the topic says so: reflection over
`Expr`/`Decl`/`Pattern` is total, and "a macro that merely reflects and rebuilds changes nothing"
— name, span and scope survive the round trip. `first-class-elaborator-api.md` stages this as
step 1 ("finish reflection … it needs no boundary crossing") and explicitly names the rival
hypothesis to be measured: "most of the value may be reachable as a library over reflection
without crossing the project boundary at all".

### Rename and refactoring want stable identity, not stable text

**What it is.** A rename is safe only if a declaration's identity does not ride on its spelling:
the tool rewrites the printed name and the resolver must land on the same binder. The thread
proposes the stronger version — shape the *surface* so common refactorings need few text
operations, by building every construct from one component (a block), so extract-function and
extract-struct are indent-and-wrap rather than a rewrite.

**Buys.** Refactorings become text-shaped edits the user can watch and undo, and identity-based
resolution makes rename sound even where names are shadowed.

**Costs.** Uniform block-shaped syntax sacrifices the conventional notation of each construct
(the thread's own worked example loses `key:` shorthand, gains `this.` and a constructor); and
stable identity must be maintained across re-elaboration, which is a compiler property, not a
tool's.

**Maturity.** speculative — the thread designs a language around the idea and reports that its
first attempt failed at it; no shipping language is claimed.

**Tried by.** Jasper (author's first language, stated as unsuccessful at this).

**Source.** A syntax for easier refactoring, score 31, 41 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/10jw33h/a_syntax_for_easier_refactoring/, 2023-01.

**Bearing on `quill`.** `quill` has the invariant rename tooling needs: M12, "no name is found by its
spelling alone" (macro model), enforced since `resolved-names-forgeable.md` closed and
`elaborator-matches-names-by-spelling.md` deleted both spelling dispatches — an `Id` is certified
by its scope set, so a rename rewrites the displayed name and resolution is untouched. What is
missing is the other half: a way to ask which `Id`s a declaration's uses are, which is an
elaborator query and therefore fog (`first-class-elaborator-api.md`).

### A data-driven expectation suite: pin the value, never the wording

**What it is.** Language behaviour lives as files — a program and its expected result — so the
suite is data, not test code, and can be diffed, sectioned and re-run by any harness. The
reported practice: copy verified output to a `.ref`, compare the parser's printed tree, the
generated text and the program's output against reference files, and treat a negative test as the
same thing minus the run step. A separate decision sits on top: *which* observable to pin — value,
constructor name, `ok`, or `error`.

**Buys.** One source of truth for behaviour, hundreds of cases cheap enough to run on every
change, and the freedom to change message text without touching the suite.

**Costs.** Choosing not to pin wording means wording is untested unless a second, stricter suite
exists; and a comparison surface that is *too* narrow (`Driver.Describe` explicitly allows only
"I64 as its digits, a constructor as its name") hides regressions the suite is structurally
unable to see.

**Maturity.** shipped — the described `.ref` workflow is a working project's current practice.

**Tried by.** the thread author's language project; `quill`'s conformance suite.

**Source.** What testing strategies are you using for your language project?, score 30, 42
comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1juwzlg/what_testing_strategies_are_you_using_for_your/, 2025-04.

**Bearing on `quill`.** Already has it, and the split is the interesting part: `.expect` may be a
value, a constructor name, `ok`, or `error`, and "error wording is not pinned — it is
implementation-specific", so exact messages belong to xUnit instead (`expect_elab_error`/
`expect_expand_error`, an exact `Message`, a type rather than a value). The suite is 961 cases,
0 failed, single-sourced — with two open questions on record: `suite-redundancy-measured.md` and
the closed `coverage-gaps-from-the-mutation-sweep.md`.

### Differential testing against a second implementation

**What it is.** Run the same programs through two implementations — an interpreter and a
compiler, or a prototype and a port — and diff the observable output. The thread reports how
subtle the drift is when you do it by eye: the host-typed interpreter raises a type error where
the code-emitting compiler silently proceeds; shadowing works in one and produces `Identifier has
already been declared` in the other.

**Buys.** Divergences surface as a corpus of programs rather than as a user report, and the pair
of implementations is cross-checked in both directions — neither is the oracle.

**Costs.** You must maintain two implementations of every rule, and when they disagree the corpus
cannot tell you which one is right; the thread's own answer is "know your target language
semantics well, write lots of tests, do fuzzing", i.e. the oracle comes from somewhere else
anyway.

**Maturity.** shipped — practised in the thread; `quill` ran it end-to-end during the .NET port.

**Tried by.** the thread author (interpreter / JS / WebAssembly backends); `quill` (prototype vs
port, `port-fails: 0`, 34 disagreements where the prototype was wrong).

**Source.** Ensuring identical behavior between my compiler and interpreter?, score 56, 26
comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/11mpom9/ensuring_identical_behavior_between_my_compiler/, 2023-03.

**Bearing on `quill`.** Was had, now lapsed: `scripts/differential.sh` and the OCaml prototype were
deleted on 2026-09-25 with the divergence list preserved as
`test/conformance/prototype-divergences.txt`. There is one implementation again, so the only
remaining differential axis is *mutation* — inject a defect, check the suite catches it — which
is exactly what `coverage-gaps-from-the-mutation-sweep.md` did (32 of 70 mutations caught
nothing).

### Fuzz past the checker: grammar-aware, then type-aware generation

**What it is.** Two-stage fuzzing of a compiler. A grammar-driven mutator finds lexer and checker
bugs immediately; then it stops dead, because almost every generated program is rejected by the
type checker and the few that pass are trivial. The proposed answers in the thread: generate
type-correct trees directly, simplify the grammar the fuzzer sees, or target the later phases
explicitly.

**Buys.** Coverage of codegen and the evaluator that random bytes cannot reach, and it is the
only technique in this axis that found bugs nobody thought to test for.

**Costs.** A type-correct generator is effectively a second front end, and type-correctness and
interesting-shape pull against each other — the thread reports zero bugs found after the type
checker with the grammar-based approach.

**Maturity.** research — AFL Grammar Mutator plus a language grammar is reported working for the
early phases; nobody in the thread reports cracking the post-checker problem.

**Tried by.** the thread authors (AFL, libFuzzer, oss-fuzz harnesses); YARPGen is cited as prior
art for C.

**Source.** How to fuzz compiler with type-correct programs?, score 36, 10 comments,
https://www.reddit.com/r/Compilers/comments/1mhdmyc/how_to_fuzz_compiler_with_typecorrect_programs/, 2025-08;
How to use fuzzing to test an arbitrary programming language?, score 32, 18 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/l0doct/how_to_use_fuzzing_to_test_an_arbitrary/, 2021-01.

**Bearing on `quill`.** The analogue already exists in-repo and closed on 2026-10-01: a *mutation*
sweep, where the "generated program" is a defect injected into `src/` and the oracle is whether a
conformance case flips. Its finding is the same shape as the fuzzing threads' — the untested
surface is not the parser but narrow paths (the budget limit check, a ref that is not a
reference, a macro used at the wrong arity) — and its rule, "every case must be proven to catch
something", is the post-checker-fuzzing problem stated as a test requirement.

### Show the work: expansion, inferred type, decision tree, behind a flag

**What it is.** Driver-level introspection commands that print an intermediate on demand: the
expanded form of a macro call, the type the checker inferred for an expression, the decision tree
a `match` compiled to. It is the debugging counterpart of diagnostics — the same information a
hover would show, available from the command line without an editor. Two threads report the
demand at the bottom of the stack with no answer: debugging an interpreter means "putting print
statements everywhere", and a combinator parser that fails reports only that "all options were
exhausted and that's it" (Debugging interpreters/compilers and What's an useful debugging output
for a simple recursive descent parser, both cited below). A fetched thread shows the same
structure in optimiser diagnostics: when a vectoriser declines a
loop, the reason "exists as a value inside the pass, it just doesn't reach the programmer because
it was designed for compiler developers debugging the compiler, not for programmers
debugging/optimizing their code" (Compilers should help developers optimize their code, cited
below).

**Buys.** Macro authors and learners get an answer to "what did this actually become", which is
the question every macro system raises and none of them answer on the surface; and it doubles as
an inspection surface for tests.

**Costs.** Every printed intermediate becomes something users compare against — a de-facto API
that must be kept stable or churn a test corpus — and it exposes internal representations to
anyone writing a script on top of them.

**Maturity.** speculative for the compiler-facing case — the corpus records the *demand*
(compiler books don't cover it; macro debugging is named as hard; two threads ask for parser trace
output) but no thread here describes a shipped flag on a parse or type intermediate. The
optimiser's `-fopt-info-vec-missed` and `-Rpass-missed` are shipped, and the fetched thread
reports them failing on usability: "one's a stderr dump you cross reference by hand and the other
speaks in IR terms rather than yours".

**Tried by.** GCC and Clang's optimiser-report flags and LLVM's opt-viewer, named in a fetched
thread as the shipped, developer-facing narrow version; nobody is named in this corpus as
shipping one for parse or type output; C's preprocessor prints expansion provenance, which is the
narrow version.

**Source.** What I wish compiler books would cover, score 146, 36 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/gavu8z/what_i_wish_compiler_books_would_cover/, 2020-04;
plus Debugging interpreters/compilers, score 32, 21 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/nia51e/debugging_interpreterscompilers/, 2021-05;
What's an useful debugging output for a simple recursive descent parser?, score 13, 17 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/k9u35g/whats_an_useful_debugging_output_for_a/, 2020-12;
Compilers should help developers optimize their code, score 52, 27 comments,
https://www.reddit.com/r/Compilers/comments/1vdv6w9/compilers_should_help_developers_optimize_their/, 2026-08.

**Bearing on `quill`.** Half a seed exists: `Driver.Describe(Value)` is documented as "what a
program produced, as far as the conformance suite may observe it … anything else is a debug form
no case may depend on" — deliberately too narrow to build this on. The macro half is inside the
open stage-12 ticket ("traceable expansion output"), and the type half wants an elaborator query
on a `Context`, i.e. fog step 3 of `first-class-elaborator-api.md` ("current goal type, local
context, `infer`").

### Generate the reference from the program; test the examples in it

**What it is.** Doc comments on declarations, a generator that renders them into a navigable
reference, and documentation examples that are compiled or run as part of the build so they cannot
rot. The thread's answers to "what should reference documentation look like" are all *products*:
the Common Lisp Hyperspec, rustdoc ("so much better UI than the majority of other documentation
generators"), Mathematica's browsable reference, R5RS — a specification people learned the
language from because it doubles as a tutorial. One comment reaches for Emacs's `C-h f` as the
interacting version: ask about the thing under the cursor and jump to its source.

**Buys.** The reference cannot disagree with the implementation, and a doc example that fails is a
test failure rather than a silent lie; the thread also notes Elixir's docs link directly to the
implementation. The fetched replies add rustdoc's concrete mechanism: its search covers a project
and all of its dependencies in one locally generated page (`cargo doc --open`), called "a
significant improvement over manually chasing down a dependency to find its hosted documentation"
(comment, on the gold-standards thread).

**Costs.** Examples become code to maintain and a constraint on what the docs may say; a generator
ties the documentation's structure to the internal declaration model, so refactors ripple into
docs; and doctests only work where examples are complete programs.

**Maturity.** shipped — rustdoc, Mathematica, Racket's documentation-as-product, doctests (named
in-thread as "doc-test (nice)").

**Tried by.** Rust, Mathematica, Racket, Common Lisp, Elixir, Python.

**Source.** What do you consider the gold standards in programming language documentation?, score
51, 50 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/c39ib1/what_do_you_consider_the_gold_standards_in/, 2019-06
(comment tree partially retrieved; doctest remark is from the related first-class-testing
thread, score 42, 53 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/pxytj7/why_dont_more_languages_have_firstclass_testing/, 2022-03).

**Bearing on `quill`.** Missing, and not on any ticket: `std/` carries no doc comments (0 hits),
`docs/STATUS.md` is hand-maintained prose, and the only examples that are guaranteed to work are
the 961 conformance cases — which are tests, not documentation. The nearest existing link in the
other direction is that `.expect` files are already "the language's behaviour as the domain model
specifies it".

### Error text as the manual, checked with a reader study

**What it is.** Two moves that belong together. First, treat the diagnostic as a documentation
deliverable: for a beginner the error message is read more often than any reference page, so its
wording, its vocabulary and its link to a longer explanation get written and reviewed like docs —
the compiler-book thread argues the same from the other side by listing "creating good error
messages from type inference errors" among the topics no book covers. Second, *check* the result
empirically: the survey thread's design is a reusable instrument — take defective programs, show
each respondent the errors from only one system, ask them to rate how helpful each message is,
and treat the rating as the quality gate.

**Buys.** The most-consulted text in the ecosystem stops being written by whichever engineer last
touched the throw site, and "is this message good" becomes a number that can be compared across
revisions instead of an argument in a thread.

**Costs.** A diagnostic becomes a writing project with reviewers; and a reader study is expensive
and weak — between-subjects ratings hide within-system comparisons, and a message can rate well
without helping anyone fix the program.

**Maturity.** contested — one thread's cited study argues for richer, multi-region, interactive
messages while its own author argues for fewer highlights and vaguer text; this corpus holds both
and settles neither. The measurement instrument exists; nobody in this corpus reports using it.

**Tried by.** a research group running the survey (the thread is its recruitment post); no
language is attributed a deliberate error-as-documentation programme.

**Source.** Creating better error messages, score 45, 13 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/q3yk6x/creating_better_error_messages/, 2021-10;
Help us improve type error messages for constraint-based type inference by taking this 15–25min
research survey, score 44, 22 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/11ymq2f/help_us_improve_type_error_messages_for/, 2023-03;
What I wish compiler books would cover, score 146, 36 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/gavu8z/what_i_wish_compiler_books_would_cover/, 2020-04.

**Bearing on `quill`.** The artefacts do not exist: no error catalogue to link from a message, no
`--explain`, and the conformance runner prints nothing at all for an `error` case. The stage-12
ticket's third bullet — "user-facing macro error messages" — is the first place this would be
written. The measurement half is out of reach while wording is unpinned: a study needs candidate
messages, and today only nine exact `Message` assertions in xUnit hold any wording still enough to
compare.

## Threads worth reading in full

- **Language servers suck the joy out of language implementation** (120/68, 2025-10) — the best
  single account of what an LSP actually costs a one-person language: error-tolerant parsing,
  in-memory files, protocol ceremony, and a stated decision that the query architecture is
  overkill.
- **Why don't more languages implement LISP-style interactive REPLs?** (74/92, 2023-02) — the
  checklist of language properties a real REPL requires, plus a practitioner's report of the
  compiled-language fork (second evaluator vs environment map vs re-run).
- **What I wish compiler books would cover** (146/36, 2020-04) — the gap list: parse-error
  messages, type-inference error messages, language servers, incremental compilation, fuzzing —
  with a top comment giving the span-id threading mechanism.
- **What parsing techniques do you use to support a good language server?** (66/52, 2022-03) —
  where the highlighting-grammar-is-an-open-problem claim comes from, and the "one red squiggle
  for the whole file is worse than nothing" acceptance test.
- **Trying to do errors right** (66/37, 2022-06) — worked examples of diagnostics with the REPL
  as the explainer (`hub why 5`), and a documented Go failure case.
- **Strategies for displaying type errors with global type inference?** (27/7, 2020-08) and
  **Designing type inference for high quality type errors** (72/12, 2025-02) — the origin/use
  pair, its documented failure mode, and why the full chain cannot be printed; plus the link post
  the checker-design entry came from, whose body was never captured.
- **Query-based compiler architectures** (121/12, 2020-06) versus **Against Query Based
  Compilers** (103/29, 2026-02) — the same architecture read both ways, three years apart;
  neither body was captured, so read them on Reddit.
- **Advice/best practice/architecture pattern for building language with LSP in mind?** (66/26,
  2021-05) — now with replies: salsa and the query-based-compilers post as the named starting
  points, Merlin's experience report on incrementalising a classic pipeline, and the report that
  `$/cancelRequest` designed in from the start is what makes a slow type checker usable.
- **Gibberish — a new style of parser-combinator with robust error handling built in** (79/17,
  2025-12) — a concrete lossless-tree design with localized errors, and an explicit list of who
  consumes it.
- **What do you consider the gold standards in programming language documentation?** (51/50,
  2019-06) — a decade-old crowd-sourced shortlist of reference documentation worth imitating.

## Gaps and disagreements

**Coverage is thin where it matters most.** The `tools` slice has 24 posts — the smallest axis —
and its comments file covers the ten richest of them: 245 comments, every fetch cut off at ~30
top-level comments with the rest in `more` placeholders. So most entries above rest on a post body
or a title, and when an entry says "the thread never settles it", the answer may sit in an
unretrieved `more` block or in one of the fourteen threads with no tree at all. Three central
threads — `Should error messages be rule- or action-oriented?` and both query-architecture
threads — have no fetched tree anywhere in the corpus. Two entries rest on titles and scores
alone: `Against Query Based Compilers` and `Query-based compiler
architectures` (no bodies, no comments), and `Designing type inference for high quality type
errors` contributed only its headline and its target URL, so the mechanism that entry describes
is marked unverified.

**The corpus did not settle these.**

- *Rule- vs action-oriented wording* — asked, never answered in what was retrieved. Deciding it
  needs a controlled comparison, not a thread; the one research instrument in the corpus is a
  15–25 minute survey of type-error helpfulness (11ymq2f), i.e. the field's own answer is "run
  the study".
- *Where in the inference chain to point* — the origin/use thread calls it "largely an
  unsolvable problem" and proposes nothing measured. To decide, read the Haskell
  type-error-localisation literature the thread gestures at, which is not in this corpus.
- *Query-based architectures* — two opposed high-score threads with no captured argument on
  either side, plus Zig's talk (link post, body not captured). Nothing here can tell you whether
  the architecture pays below some project size; `1nukes9`'s author asserts it does not for his
  languages, which is one data point about a hobby project.
- *Error recovery strategy* — three threads ask how, none answers. The corpus names Chumsky,
  tree-sitter, TypeScript and a Pika-parsing preprint (parse in reverse for optimal recovery, a
  paper-with-implementation that was captured only as an abstract) but contains no comparison.
  The fetched comments add one sketch (token tree by indentation and delimiters, skip to parent)
  and one refusal (no recovery at all, reported as fine), still no comparison.
- *Whether a second parser is really required for editor support* — the strongest claim for
  sharing one front end is a single author's refactor plan and a hobby parser language; the
  counter-claim (tree-sitter "works to an extent but fails with variable references and cannot
  differentiate constructors and functions") is one student's report. The fetched replies add
  weight on both sides: "the source you'll need to handle is invalid essentially 95% of the
  time", and "a non-error-tolerant parser would be essentially useless for the LSP
  implementation" — two implementers, no measurement.

**Where the disagreement is real and productive.** Five contested pairs are recorded as entries
rather than resolved: wording style; message vocabulary (one paper, two opposite readings);
LSP-inside-the-compiler vs LSP-as-a-burden; query-based vs against; and tooling as a design-time
constraint vs tooling built after popularity. For `quill` the query pair is
already answered in effect — the map parks it as fog with two named obstacles — so the useful
next step there is not an argument but the measurement the map asks for: compile time attributed,
or a hash-shaped library story wanted.

**What would need reading outside this corpus.** Any decision about where a span lives should
start from `docs/wayfinder/quill-design-map.md`'s 2026-10-01 re-measurement (it retracted an
earlier premise on all three counts) rather than from these threads — the corpus has no thread
about threading positions through a dependently typed elaborator at all, and the nearest thing
(oil's span-id comment) is about a tree-walking interpreter.

### Dissent and corrections

**Corrected.** One claim in the first pass was too clean: the LSP entry said rust-analyzer's
absorption into the Rust toolchain was "the direction of travel" for a server inside the
compiler. A rust-analyzer team member's comment in the counter-thread corrects it — rust-analyzer
is "basically a standalone, latency-sensitive compiler for Rust", sharing libraries with rustc
and invoking the compiler directly only for diagnostics through the build system (comment, on
Language servers suck the joy out of language implementation). What shipped is a hybrid, so the
entry's `Tried by` now says so.

**Genuine dissent recorded, not resolved.** (1) Error tolerance is called "really the bare
minimum" by one comment and dismissed by another — "I never bothered implementing error
tolerance and am perfectly happy without it" — both in the same thread; the recovery entry keeps
`shipped` and carries the refusal. (2) "The tooling is the language" is argued against inside its
own thread by replies that point at Lua, sed, Make and awk; the new entry is tagged `contested`.
(3) Inside the axis's largest thread, its top comment (score 74) turns the query-architecture
objection around — if a query architecture is too complex for a language that will never be used
on large projects, then a language server is out of scope for it too — which sharpens rather than
settles that pair.

**Not resolved with the material available.** Whether the rule- vs action-oriented wording
question has an answer anywhere: its tree was not fetched in any directory, so its `contested`
tag still rests on a title alone. And no comment in this axis contradicted any `quill` completion
claim; `docs/STATUS.md` was the authority where a claim and the doc could differ.

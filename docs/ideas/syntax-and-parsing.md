# Syntax and parsing — the concrete surface and how it is read

What the written language looks like and what its shape forces: token and lexical design,
separators and whitespace, precedence and operator vocabulary, parsing technique insofar as it
changes the programs you can write, the notation chosen for types, lambdas, blocks and conditionals,
extensible notation from the user-visible side, and the interactive entry point. Deliberately outside
this file: parser-generator tooling, codegen, and type-system semantics except where a syntax
choice forces one (each such case is marked). Where a strong idea is about *how a compiler is
built* rather than *what you write*, it belongs in `compiler-architecture.md`.

## How this was gathered

Primary source: `slices/syntax-posts.md` (260 threads from r/ProgrammingLanguages and r/Compilers,
sorted by comment volume × score × body length), secondarily `corpus.jsonl` (2538 threads) for
topics the slice did not reach.

**Comment coverage for this axis: 20 threads, 520 comments** (`slices/syntax-comments.md`;
`INDEX.md` lists 20 threads w/ comments and 520 comments for `syntax`). Comment trees were fetched
for the 20 threads this axis selected, each with `/comments/<id>.json?limit=100`, and each fetch
still returns only ~30 top-level comments regardless of `limit`; the remainder sit in unretrieved
`more` placeholders (each header records the fact — e.g. "RETRIEVED 26 comments (of 98 in the
fetched tree); 226 more in `more` placeholders were NOT retrieved"). So "the comments said X"
below is a claim about the **top** of a thread, never about its whole argument: 26 comments out of
a 301-comment tree are a sample of what was upvoted first, and for most threads the unretrieved
remainder is invisible to this document. Comments are cited as `(comment on <thread title>)`; the
slices carry thread permalinks only, so no comment permalinks are claimed. Two threads cited below
are not in this axis's slice at all and were read from the neighbouring `meta` axis's tree file
(`slices/meta-comments.md`): *Why don't more
languages include "until" and "unless"?* (237 comments) and *What tiny thing annoys you about some
programming languages?* (391 comments) — each says so at its Source. Several other heavily replied
threads — *Is operator precedence even necessary?* (97),
*Generics syntax in different languages* (87), *Significant Inline Whitespace* (68), *Semicolon
Inference* (65), *Whitespaces around operators sets their precedence* (57), *Syntax Design* (38) —
have **no** comment tree in any slice, so their entries still rest on post bodies alone. The corpus
is what Reddit upvoted, not a survey of the field: popular ≠ correct, and several high-scoring posts
are self-promotion for hobby languages
whose design claims are unverified (tagged as such where cited). Frequency context from the
neighbouring `corpus-census.md`: operator precedence and fixity 84 threads, user-defined operators
36, semicolon/separator inference 61, significant whitespace only 5, normalisation by evaluation 1.

## The ideas

### How a statement ends: write the separator, infer it, or need neither

**What it is.** Three answers to "where does one statement stop": (a) a written token — `;` between
items, newline is whitespace; (b) an inserted token — the reader infers `;` at a newline where the
grammar cannot continue; (c) no token at all — juxtaposed terms are separate statements, as in Lua.

```text
x = 1; y = 2        // (a) written
x = 1               // (b) inferred at the newline
x = 1 y = 2         // (c) neither; whitespace separates
```

**Buys.** (a) makes the grammar position-independent: no line continuation rules, no ASI-style
gambles, and an expression may span lines freely. (b)/(c) remove a character the programmer types
on nearly every line.

**Costs.** (a) costs visible punctuation and forces `,`/`;` where a line break would otherwise
suffice. (b) is the notorious one: the boundary is a reader rule the programmer must simulate in
their head — the corpus's own example is `let foo = bar` followed by `(27)`, which is a call or a
new statement depending on a rule you cannot see. (c) has the same ambiguity with no rule at all:
its poster thread calls it out as requiring a semicolon after all in exactly the ambiguous case.
The comments add four more. Newline-significance blocks block-bodied lambdas: Python ignores
newlines between delimiter pairs, so a statement-bodied lambda would need newlines turned *back on*
inside them, and Python ships single-expression lambdas only (comment by u/munificent on *No
Semicolons Needed*). Go's every-newline-is-significant rule forces a chain to trail its dots —
`thing.` then `.method()` — instead of leading with them (same comment). A REPL must decide whether
a line is complete, which forces a continuation character or an end-of-line operator (comment on
the same thread). And a lexical one: with commas optional, `[-1 -2 -3]` reads as `[-6]`, because
prefix and infix `-` are separable only by the same invisible rule (comment by u/Silphendio on the
same thread).

**Maturity.** `contested` — three production designs, and 61 corpus threads still arguing it. The
fetched comments of *No Semicolons Needed* argue against that thread's own title: "The best
readability is with white space AND punctuation. Not all punctuation is noise" (22 points), "I find
semicolons, brackets, explicit end keywords all good … A couple more characters for that benefit
is always a worthwhile trade" (10), and "i find this ambiguity much more confusing" (15) — comments
on that thread. Neither side offers a measurement.

**Tried by.** (a) Rust, Go-with-`;`, `fun`; (b) JavaScript, Go, Swift; (c) Lua.

**Source.** *No Semicolons Needed — How languages get away with not requiring semicolons* — 122,
103 comments, 2026-03-18, <https://www.reddit.com/r/ProgrammingLanguages/comments/1rx9tcx/no_semicolons_needed_how_languages_get_away_with/>. *Questions about Semicolon-less Languages* — 35, 49 comments, 2024-08-12,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1eq88j0/questions_about_semicolonless_languages/> (the how-do-I-parse-it question, asked openly). *Should i introduce statement terminator?* — 30, 21
comments, 2022-08-09, <https://www.reddit.com/r/ProgrammingLanguages/comments/wjw4fv/should_i_introduce_statement_terminator/> (the `let foo = bar` / `(27)` ambiguity). *Why do C-like languages require semicolons after all
statements, but not after blocks?* — 37, 46 comments, 2023-02-19,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1168rmc/why_do_clike_languages_require_semicolons_after/> (the asymmetry: blocks never need one because they are self-delimiting). *Semicolon Inference* — 36,
65 comments, 2020-04-04, <https://www.reddit.com/r/ProgrammingLanguages/comments/fuze6o/semicolon_inference/> (link post, no body; engagement only).

**Bearing on `fun`.** Already has it, (a): "Newlines are whitespace; `;` is written" — decided
2026-09-14 in `tickets/surface-syntax-braces.md`, with the trailing `;` before `}` discarding the
group's value. Nothing here is open.

### Application by juxtaposition forfeits the newline as a terminator

**What it is.** If `f x y` is application, then a newline can never end a statement: `bar` followed
by `(27)` on the next line *is* `bar(27)`. A juxtaposition language must therefore pick a visible
terminator, a stack of parse contexts (F#'s), or layout — there is no free option.

**Buys.** Juxtaposition removes two characters from every call and makes the language read like the
mathematics it models; that is the entire reason to accept the constraint.

**Costs.** It removes the cheapest statement separator that exists. One corpus poster spells out
the bind precisely: Swift's "inject a semicolon where production stalls" does not work, because in
a juxtaposition language production *never* stalls — it always becomes an application.

**Maturity.** `shipped` (Haskell, ML, OCaml family) for the notation; the terminator question
around it stays `contested`.

**Tried by.** Haskell, OCaml, Agda; the two threads below are from people moving onto it.

**Source.** *End-of-statement inference in language with application by juxtaposition?* — 13, 12
comments, 2023-12-10,
<https://www.reddit.com/r/ProgrammingLanguages/comments/18eve8k/endofstatement_inference_in_language_with/> (the bind stated in full). *How do I parse Haskell style applications in a Pratt parser?* — 30, 35
comments, 2022-01-15,
<https://www.reddit.com/r/ProgrammingLanguages/comments/s4t715/how_do_i_parse_haskell_style_applications_in_a/> (application is an infix operator whose token is whitespace the lexer throws away).

**Bearing on `fun`.** Rejected, and the rejection is load-bearing. Macro calls are ordinary
`f(args)` application — `tickets/unify-macro-call-syntax-with-functions.md`, closed and implemented —
which is *why* a newline can be plain whitespace in `fun`: nothing can continue across it. Any
future move to juxtaposition application would reopen the terminator question at the same time.

### The off-side rule as the only block delimiter

**What it is.** Indentation alone closes a block: no `}` and no `end`. A refinement on offer is to
drop the colon as well — `if x == 5` then a newline, with the indented body as the whole of the
consequent — on the grounds that the off-side rule already carries the structure and the colon
"does nothing".

**Buys.** One less character per header and one less closing token per block; blocks cannot be
mismatched because nothing is matched.

**Costs.** It forbids one-liners (`if cond: foo()`) — the Quartz thesis gives that up deliberately
and reports not missing it — and it makes any re-indentation a semantic edit. Whitespace becomes
part of the program's meaning, so every tool that reflows code must re-read it. The comments add
that the rule is subtler than "indentation": Haskell's layout is *alignment* — the column is set by
the token after `let`/`where`, further indenting just continues the current construct, and the
report's rule lets a line run as long as the parse stays valid, which historically required the
parser to backfeed into the lexer (comments on *No Semicolons Needed*). A simpler mechanism sits in
the same thread: an indent-level stack, where an explicit `;` or a token left of the current level
pops it and ends the construct — Miranda/Admiran, described rather than linked.

**Maturity.** `shipped` (Python, and Scala 3's optional indentation); the colon-free variant is
`speculative`.

**Tried by.** Python; Tuplex (a working implementation, blog-posted); Quartz (a thesis, no release);
Haskell (alignment layout, with a lexer-only de-layouter written to prove the rule fits in a lexer);
Miranda/Admiran; Ante (indents translate to `{`/`}`, with a continuation rule when one appears where
it is not expected) — the last three from comments.

**Source.** *Thesis for the Quartz Programming Language* — 23, 55 comments, 2025-10-02,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1nvvmii/thesis_for_the_quartz_programming_language/> (off-side rule with no colon, no `do`, no `end`; the cost stated by its author). *Indentation
syntax in Tuplex* — 36, 39 comments, 2020-12-01,
<https://www.reddit.com/r/ProgrammingLanguages/comments/k4txhn/indentation_syntax_in_tuplex/> ("easier on the eye, and easier to type"; required rewriting the whole scanner). *End-of-statement
inference in language with application by juxtaposition?* — 13, 12 comments, 2023-12-10,
<https://www.reddit.com/r/ProgrammingLanguages/comments/18eve8k/endofstatement_inference_in_language_with/> ("significant indentation was far easier to implement" than semicolon insertion).

**Bearing on `fun`.** Rejected, with the reason recorded: `tickets/brackets-decide-grouping.md`
(closed 2026-09-27) adopts Rhombus's structural extents explicitly *without* layout — "`fun` has
no indentation-sensitive syntax"; `{}`, `[]`, `()`, `,` and `;` carry all structure. No open ticket
contemplates layout.

### Whitespace decides precedence (significant inline whitespace)

**What it is.** Tighter grouping where the operator sits closer to its operand: `2 * a+b` is
`2 * (a + b)`, `a+ b` is illegal because the two sides of `+` disagree on spacing, `2*(a+b)` resets
by brackets. Dijkstra made the weaker version of the same observation (quoted in the thread):
surround lower binding power with more space, and `p∧q ⇒ r ≡ p⇒(q⇒r)` reads safely without knowing
the order.

**Buys.** The programmer writes the parenthesisation they mean, in the character they already
type — and, as the proposal's author notes, custom operators would no longer need a declared
priority at all.

**Costs.** Spacing becomes semantic, so a formatter is no longer a cosmetic tool: re-spacing
re-parses the program. The proposal's own author calls it "probably not a good idea in the end"
and offers no implementation; this entry rests on a *designed* idea, not a built one. (The
formatting consequence is my inference from the rule, not a claim made in that thread — which had no
tree fetched. A comment elsewhere does name the failure mode it generalises: whitespace sensitivity
creeping into a language that is otherwise not whitespace-sensitive, where a C macro makes `foo
(bar)` and `foo(bar)` mean different things, is called out as creepy precisely because it is rare
and unlooked-for — comment on *What tiny thing annoys you about some programming languages?*.)

**Maturity.** `speculative`.

**Tried by.** Nobody in this corpus has shipped it; one poster runs a related strict left-to-right
language and asks for its downsides.

**Source.** *Whitespaces around operators sets their precedence* — 59, 57 comments, 2021-10-14,
<https://www.reddit.com/r/ProgrammingLanguages/comments/q88a4i/whitespaces_around_operators_sets_their_precedence/>. *Significant Inline Whitespace* — 26, 68 comments, 2026-01-05,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1q4vojb/significant_inline_whitespace/> (the Dijkstra remark and a working no-precedence language looking for its failure modes).
*What tiny thing annoys you about some programming languages?* — 137, 391 comments, 2020-09-05,
<https://www.reddit.com/r/ProgrammingLanguages/comments/in3d8r/what_tiny_thing_annoys_you_about_some_programming/>
(comment evidence only: the `foo (bar)` / `foo(bar)` cost above).

**Bearing on `fun`.** Genuinely new — and it collides head-on with a `fun` decision. Spacing cannot
decide grouping because grouping is decided by declared order relations between groups
(`brackets-decide-grouping.md`), and because a macro's arguments are read as syntax objects whose
extent must be stable under any edit a macro makes. No ticket names it.

### A fixed reserved keyword set, with rule literals compared by spelling

**What it is.** The reader maps a closed list of spellings to reserved token kinds; everything else
is an identifier. Two neighbours: reserve *nothing* (Honu; every word is a normal identifier and
only a form's head position makes it special), or reserve nothing and compare rule literals by
spelling so a word is special only inside the form that mentions it.

**Buys.** A fixed set is checkable — the reader knows the whole grammar's keywords before reading a
line, no parser rule can accidentally capture an identifier, and tooling gets a complete keyword
list for highlighting. The spelling-compare variant keeps `else` usable as a variable name outside
`if`.

**Costs.** Every reserved word is stolen from user namespaces for all time, and the set only grows.
The opposite extreme — no keywords at all — costs naturalness: its showcase language's whole
grammar is six EBNF lines and reads as bracketed function application throughout, which is a real
design position and not one this corpus shows anyone adopting for production code. The comments
argue both directions with specifics: many keywords collide with names programmers choose —
`list = [1, 2, 3]`, SQL's three ways of naming a table `order`, C++'s `using namespace std;`
(comment on *What language features do you "Consider Harmful" and why?*); the growth problem has a
non-syntax answer in the same corpus, Perl's `use v5.34` and Go's `go.mod` pinning letting a
program adopt new syntax without the language growing its reserved list (comments on that thread);
and the far end is Smalltalk's five reserved words — `true`, `false`, `nil`, `self`, `super` — with
the rest left to the library (comment on *Why don't more languages include "until" and "unless"?*).

**Maturity.** `shipped` for reserved sets (C, Rust, Go); the no-keyword variant is a hobby data
point about interest, not evidence of viability.

**Tried by.** C, Rust, Go, `fun`; Crumb (hobby, no-keyword); Honu (reserve-nothing, cited by
`fun`'s glossary).

**Source.** *Crumb: A Programming Language with No Keywords, and a Whole Lot of Functions* — 97, 19
comments, 2023-08-26,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1621mpb/crumb_a_programming_language_with_no_keywords_and/> (the counter-position, with its six-line grammar). *Suggestions for keywords for my new programming
language* — 0, 27 comments, 2026-02-28,
<https://www.reddit.com/r/Compilers/comments/1rh0tzx/suggestions_for_keywords_for_my_new_programming/> (a typical reserved list; score 0, cited as a data point only). *What language features do you
"Consider Harmful" and why?* — 110, 301 comments, 2022-11-13,
<https://www.reddit.com/r/ProgrammingLanguages/comments/ytywy7/what_language_features_do_you_consider_harmful/>
(comment evidence only: keywords colliding with user names, and version-pinned syntax as the
growth answer).

**Bearing on `fun`.** Already has it, and the interesting detail is the *subtraction*. The glossary
(`CONTEXT.md`) fixes it: "Keywords are a fixed, reserved set of token kinds — a deliberate departure
from Honu, which reserves nothing — and operators are one uniform token shape." Then stage 11
increment 2 (`docs/STATUS.md`, 2026-09-16) removed `then`, `with`, `end`, `else` and `Unit` from
that set because no reader rule matched them: a template's rule literals compare by spelling, so the
prelude's `if` form matches `else` as a plain identifier. Reserved set, but only where a rule needs
it — the spelling-compare variant, arrived at from the reserved side.

### Condition-polarity keywords: `unless`, `until`, `when`

**What it is.** A second spelling for a conditional with the polarity flipped: `unless c { … }` for
`if not c { … }`, `until c { … }` for `while not c { … }`, and usually `when c { … }` for a
one-armed `if`. The corpus offers three positions: make them keywords (Pascal, Perl, bash), make
them library
functions (Haskell's `when`/`unless` are ordinary functions taking a monadic argument; Common Lisp's
`unless` is a macro), or make them nothing and negate at the use site — plus a comment proposing
the whole three-clause set `(if COND THEN ELSE)` / `(when COND THEN)` / `(unless COND THEN)`.

**Buys.** `until` is not a pure synonym of `while not`. A trailing test states that the condition
governs what already ran and survives mangled formatting ("`do … until` … makes it clear that the
controlling expression affects a preceding loop rather than a following one"); it saves the
first-iteration special case `while` forces (`first = true; while first or test()`); and inversion
does not always preserve the sentence — `until feof(f1) && feof(f2)` versus
`while !feof(f1) || !feof(f2)` reads as "until both streams are empty" versus "while either stream
still has data". `unless` buys less: Common Lisp's collapses `(if (not foo) bar nil)` into one form
and removes the `progn` an `if` would need. All from comments on the source thread.

**Costs.** The argued-against side comes from the same thread: "When you give users two ways to
express the same thing (`unless foo` versus `if not foo`) with almost no difference between them,
all you're doing is giving them more decisions to make and argue about in code reviews with little
benefit in return" — and the implementation cost being trivial makes that worse, not better.
`unless` is nearly free to write by hand ("doing `not` is relatively trivial"), it reads backwards
to non-native speakers ("my mother tone's equivalent is literally 'if not'"), a forgotten `!` and a
wrong keyword become indistinguishable in review, and conditionals tend to grow an `else` later —
exactly why Elixir deprecated the `unless` it had shipped. The general form of the objection: a
common goal of language design is to keep the syntax small, and every additional syntax is
complexity — something new for readers to learn, that rarely pops up anyway. The permanent cost is
the reserved list itself: every one of these words is taken from user
namespaces forever.

**Maturity.** `contested` — shipped on both sides (`until` in Pascal; `unless` in Common Lisp, added
and then deprecated in Elixir; both as library functions in Haskell), with named reasons argued
across a 237-comment thread and neither side offering a measurement.

**Tried by.** Common Lisp (`unless`, `when`, and `loop`'s `until`/`unless` clauses), Pascal, Perl,
bash, Emacs Lisp (`when`, `unless`); Elixir (removed); Haskell and Smalltalk take the library side.

**Source.** *Why don't more languages include "until" and "unless"?* — 147, 237 comments, 2025-05-07,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1kggvqt/why_dont_more_languages_include_until_and_unless/>
— the comment tree for this one was fetched under the neighbouring `meta` axis (`slices/meta-comments.md`):
26 of 98 retrieved, 143 in `more`, so the retrieved sample carries both sides but not the thread's
whole argument.

**Bearing on `fun`.** Has it differently, and the answer is already half-made: the reserved set is
fixed (`CONTEXT.md`: "Keywords are a fixed, reserved set of token kinds"), and conditionals are not
in it — `if` and `Bool` are prelude forms (`topics/bool-and-if-as-library.md`, umbrella ticket
`tickets/specify-stage-11-macro-powered-language-features.md`). So `unless`/`until`/`when` would be
prelude templates rather than reader keywords: adding or removing one is a library commit and the
reader does not change either way. Open as a library question only, with Elixir's deprecation as the
recorded caution; no ticket proposes them.

### Unicode identifiers — or an ASCII reader that says so loudly

**What it is.** Either the identifier character class is a Unicode standard (with a normalisation
rule), or the source alphabet is fixed and any other character is an error at the character itself.

**Buys.** A Unicode class lets identifiers be written in the language people actually write in —
the corpus contains a Paleo-Hebrew language as proof of appetite. A fixed class means every tool,
from the reader to a grep, agrees on what an identifier is without implementing a character
database, and an encoding decision made once (the thread's Go example: "Source code is Unicode text
encoded in UTF-8") removes a whole class of tool disagreement.

**Costs.** Unicode identifiers bring normalisation, confusables and homoglyphs into the language's
compatibility surface — every reader and every editor must normalise identically, or two visually
identical names are two names (this cost is general knowledge, not from the corpus; no thread here
discusses it). A fixed class costs every non-English programmer, permanently.

**Maturity.** `shipped` on both sides: Go's fixed UTF-8 encoding rule and its tools argument are
quoted in the corpus; Unicode identifiers in mainstream languages are [general knowledge, not from
corpus].

**Tried by.** Go (encoding), JavaScript/Python (identifier class) [general knowledge, not from
corpus], Genesis (non-Latin source, hobby joke), `fun` (ASCII).

**Source.** *Is CF what's actually useful?* — 17, 34 comments, 2021-05-13,
<https://www.reddit.com/r/ProgrammingLanguages/comments/nbi3xh/is_cf_whats_actually_useful/> (Objection 1: specify the encoding outright, as Go does, and tools need less context). *I made an
ancient Hebrew programming language to help programmers speak to God* — 208, 39 comments,
2022-08-06, <https://www.reddit.com/r/ProgrammingLanguages/comments/whilk6/i_made_an_ancient_hebrew_programming_language_to/> (appetite evidence; the post lists "Introducing more ambiguity" as a future feature). *Integrating
type system inside parser?* — 16, 17 comments, 2018-01-20,
<https://www.reddit.com/r/ProgrammingLanguages/comments/7rtx2z/integrating_type_system_inside_parser/> (mentions Unicode identifiers as a design goal).

**Bearing on `fun`.** Genuinely new and undecided anywhere in the map or the ticket list. The
reader is ASCII-only today and fails loudly: `IdStart`/`IdContinue` are `a-zA-Z_` plus digits and
`?!` (`src/Fun.Expand/Reader.cs:23-26`), and any other character throws `unexpected character`
(`Reader.cs:141`). Non-ASCII inside a string literal is unaffected. Worth a decision only when
someone wants it; today nothing does.

### One operator alphabet: a fixed sigil class, maximal munch, no per-operator lexing

**What it is.** All operators are one token shape drawn from a fixed character class; the reader
munches the longest run (`&&` falls out of `&` joining the class, with no rule for `&&`), and an
operator's meaning is a declaration downstream of reading, never a lexer change.

**Buys.** Adding `%%` or `<~>` to the language changes no reader code — the comment above the
character class says exactly that: "a new operator is a declaration, never a lexer change."
Lexing is independent of the binding table, so a shadowed operator still tokenises correctly.

**Costs.** The sigil class cannot be extended by a user, and structural punctuation must be carved
out of it by hand: `|`, `->`, `=`, `:` and `^` keep dedicated token kinds precisely because they
would otherwise be swallowed by maximal munch.

**Maturity.** `shipped` — every language whose operator set is fixed by its grammar (C, Java) does
this [general knowledge, not from corpus]; `fun` is a case with the extension rule made explicit.

**Tried by.** `fun`, C, Java.

**Source.** *Custom operators, are they worth the effort?* — 34, 35 comments, 2024-03-22,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1bkp9ar/custom_operators_are_they_worth_the_effort/> ("from a lexical point of view it is a horror to take all of this into account and then distinguish it
from conventional punctuation" — the cost of *not* having one class). *Priority of the shift
operators `<<` and `>>`* — 17, 47 comments, 2022-06-10,
<https://www.reddit.com/r/ProgrammingLanguages/comments/v9cnzx/priority_of_the_shift_operators_and/> (adjacent-token ambiguity of `<<`/`>>` in C-style grammars).

**Bearing on `fun`.** Already has it, by name. `OperatorChars` is `+-*/%=!<>@~&|` with maximal
munch (`Reader.cs:27-29`, `ReadOperator`), structural punctuation keeps dedicated kinds
(`TokenTree.cs:25-45`), and `tickets/add-short-circuit-and-or-operators.md` records the settled
line: "operator space lexes uniformly, but structural punctuation keeps dedicated tokens."

### Relative order groups: precedence by declaration, never by number

**What it is.** Operators do not carry numbers; they join named groups, and groups declare relations
to each other. `order additive : stronger_than(comparison) assoc(left)` — transitively closed, cycles
rejected at declaration, associativity on the group, and **an undeclared relation is an error, never
a guess**: "`inc` and `++` have no declared order; write `(inc 1) ++ s` or `inc (1 ++ s)`."

**Buys.** No number to assign or argue about (the original complaint that "it is often unclear what
precedence to assign at all"); unrelated operators from different modules cannot silently acquire an
accidental order; the prelude can express a whole chain — `disjunction < conjunction < comparison <
additive < multiplicative < negation` — in six lines.

**Costs.** Every pair you intend to mix without brackets needs a declared relation somewhere,
transitively, or the program does not compile; a module that forgets to relate its operators pushes
parentheses onto its users.

**Maturity.** `research` — implemented in `fun`, with Rhombus's enforester cited as the reference
for erroring on an undeclared order; no production language in this corpus claims the scheme.

**Tried by.** `fun`; Rhombus (the referenced model for hole extents and undeclared-order errors).

**Source.** *Relative vs absolute operator precedence for custom operators (aka. total order or
not)* — 32, 44 comments, 2021-09-26,
<https://www.reddit.com/r/ProgrammingLanguages/comments/pvz2m3/relative_vs_absolute_operator_precedence_for/> (both designs laid out: numeric total order vs declared relative binding strength). *Unambiguous
Operator Specification for Programming Languages* — 26, 44 comments, 2026-09-03,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1w6c8r9/unambiguous_operator_specification_for/> (the counter-position: publish one portable numeric table so expressions travel between languages).

**Bearing on `fun`.** Already has it, fully implemented: `tickets/brackets-decide-grouping.md`
(order groups, transitive relations, undeclared-relation error, `assoc(none)` for `<-`, groups as
ordinary binders resolved by scope set and exported with `pub`). Numeric precedence is gone — a
number where a group belongs is an error naming the new form. This is one of the sharpest
decisions `fun` has made; the corpus's two sides (declare-relatively vs publish-a-table) map onto
it exactly, and `fun` chose the first.

### Precedence-free surfaces: strict left-to-right, or brackets as the only grouping

**What it is.** `1 + 2 * 3` parses as `(1 + 2) * 3`, always, and parenthesisation is the only
grouping mechanism; a weaker variant keeps two or three levels (arithmetic, comparison, boolean)
and nothing else. A third variant, argued in the comments rather than built, goes further: *any*
function may be written infix — `f(x, y)` as `x f y` — with no precedence table at all (like Self),
so `x do-a-barrel-roll-around y` is as valid as `x + y` and only tricky cases take brackets; its
author calls this "the only way that won't invariably surprise the programmer" (comment on *What
Operators Do You WISH Programming Languages Had?*).

**Buys.** One rule instead of a table to memorise; the author of a working strict left-to-right
language calls the combination with interchangeable functions and operators "very ergonomic" —
`1 add 2` and `+(1, 2)` are the same expression. The "few precedence levels" proposal argues that
readers would rather write `1 + (2 * 3)` than trust PEMDAS.

**Costs.** Every expression written the conventional way grows parentheses; mathematical notation,
the thing precedence was copied from, stops working as written. Nobody in this corpus offers a
readability study either way — the argument is entirely from personal preference, which is why
this stays a research-grade claim.

**Maturity.** `research` — one working language described by its author, no production arithmetic
language claimed anywhere in the corpus; the any-function-infix variant is `speculative` (argued in
a comment, no implementation offered).

**Tried by.** The strict left-to-right language described in the significant-inline-whitespace
thread (unnamed); concatenative and APL-family languages get precedence-freeness another way.

**Source.** *Is operator precedence even necessary?* — 28, 97 comments, 2022-06-11,
<https://www.reddit.com/r/ProgrammingLanguages/comments/va8x5q/is_operator_precedence_even_necessary/>. *Significant Inline Whitespace* — 26, 68 comments, 2026-01-05,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1q4vojb/significant_inline_whitespace/> (a strict left-to-right language in daily use by its author). *What Operators Do You WISH
Programming Languages Had? [Discussion]* — 174, 243 comments, 2022-10-21,
<https://www.reddit.com/r/ProgrammingLanguages/comments/ya87l1/what_operators_do_you_wish_programming_languages/>
(comment only: any function may be infix, with no precedence at all).

**Bearing on `fun`.** Has it differently, deliberately: `fun` keeps conventional grouping but
derives it from declared relations rather than a table (`order` groups), so `1 + 2 * 3` works
because the prelude declared `multiplicative` stronger than `additive`, not because a number says
so. Adopting left-to-right parsing would invalidate the prelude's group chain and every conformance
case that writes mixed arithmetic; nothing in the map proposes it.

### User-defined operator symbols: open the sigil space or close it

**What it is.** Whether a program may introduce a *new* symbol — `+++`, `<|>` — or whether the
operator vocabulary is fixed and only existing spellings may be overloaded (Python) or given
implementations by type (Rust's trait approach).

**Buys.** New symbols let a domain write its own notation: ranges, lenses, units, parsers. The
corpus's `.=` proposal is an example of a hole in a fixed vocabulary that someone wanted filled —
`numbers .= map(x => x * x)` for `numbers = numbers.map(...)`.

**Costs.** Lexical horror, in the words of the thread arguing against: distinguishing arbitrary
symbol runs from conventional punctuation, then from each other, "is too syntactic and should be
developed more carefully." Plus discoverability — a reader cannot infer what `<<<` means without
finding its declaration, and libraries can collide.

**Maturity.** `contested` — both sides are shipped (Haskell opens the sigil space, Python and Rust
restrict it), and the fetched comments of *What Operators Do You WISH Programming Languages Had?*
argue it without converging. Open side: Raku ships 219 built-in operators in its standard library
alone and lets users define more; "Would I like Haskell's custom operator creation? Yes. Would I
make byzantine and frustrating to read programs with it? Also yes." Closed side: "languages are
operator scarce because they aren't nearly as readable as keywords or universal"; "An operator has
no descriptive name … Operators are also non-googleable, so there is an assumption that you must
already know what it means"; and how many Haskell programmers can say what lens's `<%@~` does
without looking it up. Neither side offers a measurement, so this stays a disagreement about
audience, not a settled result — but the concrete demands in that thread are narrow (a canonical
modulus `%%`, a real C# proposal linked in the comment; `(x % y + y) % y` written by hand), which is
evidence that demand is specific rather than a call for symbols in general [my reading of the
thread, not a claim anyone made].

**Tried by.** Haskell, Scala, Rust (traits), Python (fixed set), `fun`.

**Source.** *Custom operators, are they worth the effort?* — 34, 35 comments, 2024-03-22,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1bkp9ar/custom_operators_are_they_worth_the_effort/> (the case against; the author settles on Python/Rust style). *An idea for a `.=` operator* — 81, 83
comments, 2021-12-21,
<https://www.reddit.com/r/ProgrammingLanguages/comments/rleiot/an_idea_for_a_operator/> (a fixed vocabulary's missing operator, invented by a user). *What Operators Do You WISH
Programming Languages Had? [Discussion]* — 174, 243 comments, 2022-10-21,
<https://www.reddit.com/r/ProgrammingLanguages/comments/ya87l1/what_operators_do_you_wish_programming_languages/>
(comment tree fetched: 26 of 96 retrieved — both positions above come from that sample).

**Bearing on `fun`.** Has it differently, and the difference is decided: any run over the uniform
operator character class may be declared `pub infix` / `pub prefix`, so the *vocabulary* is open
while the *token shape* is not (`Reader.cs:20-29`). What `fun` deliberately does not do is let a
declaration change how tokens are read. Collision policy (two units declaring the same sigil in one
scope) is the open part; the closed `unify-operators-into-scope-aware-binding-table.md` settled that
fixity is an attribute on a binding.

### Fixity is a binding: learned where the reader is reading, from the same table as names

**What it is.** The parser cannot decide how to group an operator it has not seen declared, so
fixity must come from somewhere: a global table, a numeric precedence in a file header, or — the
interesting version — the ordinary binding table, resolved by scope, so the same sigil may group
differently in two modules and an import brings its fixity with it.

**Buys.** One scope mechanism for names, macros and operators; a module exports its notation with
`pub` like everything else; shadowing works the way readers expect from names.

**Costs.** The reader is no longer context-free in the strongest sense: reading an expression
requires knowing what is bound. That is the price the corpus's own question-asker identifies — the
precedence may be an arbitrary expression, may be defined in another module, and "it'd have to
evaluate it… merging the parser with the evaluation." A language must either forbid computed
precedence or stage the reader behind resolution.

**Maturity.** `research` for scope-resolved fixity (Haskell's per-module fixity declarations are
the shipped partial version [general knowledge, not from corpus]); the chicken-and-egg problem is
`contested` in the thread that asks it.

**Tried by.** `fun`, Haskell, Rhombus (order groups as binders).

**Source.** *Regarding Parsing with User-Defined Operators and Precedences* — 19, 55 comments,
2025-06-08,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1l63ac9/regarding_parsing_with_userdefined_operators_and/> (the full problem statement, including imports and evaluated precedences). *An algorithm for parsing
with user-defined mixfix operators, precedences, and contexts* — 16, 9 comments, 2025-06-20,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1lgcbhe/an_algorithm_for_parsing_with_userdefined_mixfix/> (grammars stored as data; see the next entry).

**Bearing on `fun`.** Already has it: fixity lives on a `Role`, looked up with the token's scope
set — `BinderTable.FindRole(name, Fixity, ScopeSet)` (`src/Fun.Expand/BinderTable.cs:75`), called
from `Enforest.Roles.cs:119,153` — so an operator's grouping is resolved by the same scope-set
machinery as any other name, and order groups are binders (`brackets-decide-grouping.md`, item 3).
This is precisely the design the corpus thread could not reach.

### Per-context mixfix tables — and no syntax declared mid-file

**What it is.** One table of productions per *syntactic position* (expressions, patterns, types,
declarations), each entry a list of keywords that may start or follow a production, with hooks for
what happens on an empty slot and for glued (juxtaposed) neighbours. The same spelling can then
behave differently in different positions. The algorithm's author deliberately rejects syntax
declarations *inside the file being read*, keeping them at module level so both the defining and
the reading file can be analysed.

**Buys.** A language can grow notation per position without a global grammar rewrite, and one
keyword can be a pattern head and a declaration head without ambiguity.

**Costs.** The reader must carry position information it would otherwise discard, and the whole
scheme "does not correspond to any formalism" — as the author says, it becomes the only thing that
can parse your files. That is a real lock-in: no standard tool can be pointed at the grammar.

**Maturity.** `research` — one implementation described in detail, no production language claimed.

**Tried by.** The thread's own language; `fun` reaches a related place through syntactic roles
rather than mixfix tables. A comment names the lineage this family belongs to: "the only thing I
know of" for abstracting the syntax of languages with ordinary infix notation is *Honu: Syntactic
Extension for Algebraic Notation through Enforestation* (GPCE 2012) and the work it spawned,
Rhombus (comment on *Is there a minimum viable language within imperative languages like C++ or
Rust from which the rest of language can be built?*).

**Source.** *An algorithm for parsing with user-defined mixfix operators, precedences, and
contexts* — 16, 9 comments, 2025-06-20,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1lgcbhe/an_algorithm_for_parsing_with_userdefined_mixfix/>. *Functions as patterns or blocks?* — 36, 15 comments, 2026-07-11,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1utp6ma/functions_as_patterns_or_blocks/> (backtick-marked pattern heads making `add 2 to 3` — user notation defined by the pattern, with its
author listing what breaks: no obvious link to anonymous functions, awkward assignment, fiddly
declaration *and* call syntax). *Is there a minimum viable language within imperative languages like
C++ or Rust from which the rest of language can be built?* — 52, 111 comments, 2024-05-07,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1cm8m9o/is_there_a_minimum_viable_language_within/>
(comment evidence only: the Honu/Rhombus lineage above; its tree was fetched, 26 of 97).

**Bearing on `fun`.** Has it differently, and the seam matters: `fun` already assigns every use a
syntactic role and an expansion position (`Decl`, `Pattern`, expression kinds), and a template
declares which role it fills — so per-position notation exists without per-position *tables*.
Whether user templates may declare a role not yet in the grammar is not decided anywhere in the
map; it belongs next to `tickets/scope-enforester-improvements.md`, not to the macro mechanics docs.

### Operator-precedence questions move into the library

**What it is.** The compiler has no opinion about where `not`, `&&`, `||`, `<<` or `/` sit. The
prelude declares them into groups like any user operator, so the strong-vs-weak-`not` split, the
`<<`-near-`*` question and the shift-before-addition question are all resolved by editing library
declarations instead of the compiler.

**Buys.** Three live arguments in the corpus become configuration: C/Java/Rust put `not` next to
unary operators ("strong NOT"), Python/SQL put it below comparison ("weak NOT"), Perl/Ruby ship
both spellings — and a library can express all three by assigning the group. A language shipping
its own arithmetic learns the same lesson: nothing about `+` needs to be in the compiler. The
comments supply the reason a compiler-side table keeps needing edits at all: "C just got the
precedence of the bitwise operators wrong. They should bind tighter than the logical ones" and
"PHP got the precedence of `?:` wrong compared to every other language that has that syntax" (a
reply corrects that the PHP case is associativity, not precedence) — comments on *What tiny thing
annoys you about some programming languages?*.

**Costs.** The prelude's chain becomes part of the language's identity that users cannot fully
override (their operators meet `<-` or `disjunction` through it), and error messages about
grouping now point at library code the user did not write.

**Maturity.** `shipped` — Haskell declares fixity for Prelude operators in the library
[general knowledge, not from corpus]; the strong/weak `not` *split* itself is shipped by the
languages the thread names.

**Tried by.** Haskell, `fun`, Raku (an operator's fixity, argument type checks and implementation
declared in ordinary code as `sub infix:<√> (Int \nth where * >= 0, …)`, shown working in a live
evaluator by a commenter); the alternative (compiler-fixed tables) is everyone else.

**Source.** *The strange operator precedence of the logical NOT* — 38, 30 comments, 2024-06-12,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1dec9rf/the_strange_operator_precedence_of_the_logical_not/> (strong vs weak vs both, with the languages on each side). *Priority of the shift operators `<<` and
`>>`* — 17, 47 comments, 2022-06-10,
<https://www.reddit.com/r/ProgrammingLanguages/comments/v9cnzx/priority_of_the_shift_operators_and/> (why C's inherited table is questioned at all).

**Bearing on `fun`.** Already has it, twice over: `&&`/`||` are `pub infix` templates in the prelude
and prefix `not` is a prelude declaration (`add-short-circuit-and-or-operators.md`,
`explicit-prelude-open-operator-demotion.md`, both implemented), and the prelude declares
`disjunction < conjunction < comparison < additive < multiplicative < negation`. So the thread's
question — which side to pick for logical NOT — is in `fun` a one-line prelude edit, and an
alternative prelude could pick the other side without a compiler change.

### Pratt/precedence climbing: what it makes cheap, and what it cannot express

**What it is.** One recursive expression function whose behaviour at each token is a table entry
(infix: parse right at precedence p+1; prefix: parse at p; atom: stop). Adding an operator is a
row, not a production.

**Buys.** It is the reason user-declared fixity is affordable at all: the parser does not grow with
the operator set. The corpus's praise thread is a practitioner's account of exactly that click —
implementing one turns a confusing recursion into an obvious table. A comment on the parser thread
makes it the standing default: "a really good default starting point is a Recursive Descent Parser
with Pratt Parsing expressions", shown as a hand-written front end with every file under 250 lines.

**Costs.** It handles only what has an operator token. Juxtaposition application has none (its
"token" is whitespace the reader discarded), type-directed grouping has no precedence at all, and a
Pratt driver cannot express a form that consumes to the end of a group — those need something else,
and a language that starts with a Pratt front end tends to discover this late.

**Maturity.** `shipped` — the standard front end for hand-written expression parsers.

**Tried by.** Most hobby and many production languages; `fun` deliberately uses neither.

**Source.** *Pratt parsing is magical* — 86, 20 comments, 2024-12-23,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1hklhsd/pratt_parsing_is_magical/>. *How do I parse Haskell style applications in a Pratt parser?* — 30, 35 comments, 2022-01-15,
<https://www.reddit.com/r/ProgrammingLanguages/comments/s4t715/how_do_i_parse_haskell_style_applications_in_a/> (the failure case: application has no token to hang a precedence on). *what would you use to write
a parser in 2021?* — 81, 98 comments, 2021-12-04,
<https://www.reddit.com/r/Compilers/comments/r8rkzd/what_would_you_use_to_write_a_parser_in_2021/>
(comment only: RDP + Pratt as the recommended default; the same thread's fetched comments are used
by the recovery and context-freeness entries below).

**Bearing on `fun`.** Has it differently, and the difference is measured, not assumed:
`grep -niE 'spec|combinator|pratt' src/Fun.Expand/*.cs` returns **0 hits** (recorded in
`scope-enforester-improvements.md`'s re-derivation). Enforestation drives named order groups by
syntactic role (`Enforest.cs:244`, `Enforest.Roles.cs:80,150,429`) — forms, not operators, consume
input, which is what makes a macro's trailing hole read a whole expression. A Pratt driver would be
a downgrade for `fun`; the ticket records that phase as *dropped (changed shape)*, not pending.

### Parse in reverse: left recursion and error recovery from the other direction

**What it is.** A packrat-style dynamic program run bottom-up and right-to-left over the input, so
left-recursive productions terminate by construction and, because the table is filled from the end,
a syntax error can be recovered from optimally rather than by guessing forward.

**Buys.** Both classic recursive-descent failures go away at once: left recursion (grammar writers
stop contorting productions into right-recursion-plus-precedence) and post-error recovery (the
paper claims *optimal* recovery, which is what an IDE needs), while keeping linear time within a
moderate constant factor.

**Costs.** A reversed driver is a different mental model from "read left to right," the memo table
costs memory proportional to input × nonterminals, and diagnostics arrive from a table rather than
a call stack — spans and expected-token sets have to be reconstructed. The corpus gives no
experience report from anyone running it on a real language.

**Maturity.** `research` — a preprint with an implementation, argued on Reddit, not claimed by any
language here.

**Tried by.** Nobody in this corpus; the paper's own implementation.

**Source.** *[Preprint] Pika parsing: parsing in reverse solves the left recursion and error
recovery problems* — 106, 56 comments, 2020-05-15,
<https://www.reddit.com/r/ProgrammingLanguages/comments/gk1uwh/preprint_pika_parsing_parsing_in_reverse_solves/> (abstract quoted in full in the post; arXiv:2005.06444).

**Bearing on `fun`.** Not applicable now, and the open ticket says why in numbers: the reader is
`Reader.Read(string)` — whole source, no incremental input — and enforestation has no recovery to
improve (`Enforest.RequireAdvance` is a non-advance guard, not recovery). Adopting a reverse driver
would be a rewrite of `Fun.Expand` for a benefit nobody has measured. Filed here because it is the
one parsing-technique idea in the corpus that would change what a `fun` program may contain
(left-recursive user notation) rather than only how it is read.

### Recover to a boundary, so one error does not condemn the file

**What it is.** On a bad term, skip to the next statement boundary or matching closer and keep
reading, so the rest of the file still gets forms, names and diagnostics. The standard the corpus
sets is negative and concrete: a language where "any syntax error anywhere in the file causes the
entire file to red squiggle" is *worse than no tooling at all*; TypeScript is the positive example,
reporting several independent errors each locally scoped.

**Buys.** Every later tool depends on it — highlighting, name resolution, LSP diagnostics all run
on files their user is mid-edit in, i.e. permanently broken ones. The fetched comments add a
compiler-building reason from the same direction: GNU C++ moved its parser off bison to a hand-written
one, and "that was the only realistic way to bring quality of error messages on par with clang";
CPython's generated parser is criticised for needing a whole *second* parsing pass before it can
report anything, and "still terrible" error messages at that (comments on *what would you use to
write a parser in 2021?*).

**Costs.** Recovery invents structure the programmer did not write (an empty group where a
statement should be), so downstream phases must tolerate junk forms; and it is only worth building
against a measured workload, or it is speculation about how often readers fail.

**Maturity.** `shipped` as a requirement (TypeScript, cited in the thread); principled recovery
techniques remain `research`.

**Tried by.** TypeScript; Pika's parser (optimal recovery, see above); `fun` has none.

**Source.** *What parsing techniques do you use to support a good language server?* — 66, 52
comments, 2022-03-01,
<https://www.reddit.com/r/ProgrammingLanguages/comments/t4c8ms/what_parsing_techniques_do_you_use_to_support_a/>. *Good design patterns when writing "forgiving parsers"?* — 38, 29 comments, 2019-07-17,
<https://www.reddit.com/r/ProgrammingLanguages/comments/ce5o8d/good_design_patterns_when_writing_forgiving/> (maxims for fault tolerance; the HTML-parser analogy).

**Bearing on `fun`.** Open, and correctly open. `tickets/scope-enforester-improvements.md`
(status: open, re-derived 2026-10-01 against the C# port) measures the gap precisely: three
message-only exception types, 173 `throw new` sites in `Fun.Expand` none carrying a span, spans
existing on every token but discarded at `Driver.cs:38-46`, and **no workload counted** — the
`.expect` format cannot express two errors from one program, so nobody knows how many cases die in
enforestation rather than elaboration. The ticket's own verdict: the accumulator and recovery phase
is not forkable until that number exists. This entry is the field's argument; the ticket is the
measurement that decides.

### Context-freeness is not what tools need most

**What it is.** The conventional claim — a context-free grammar lets simple tools work one file at
a time — examined and found to be the wrong priority. The thread's five objections: encodings must
agree before parsing starts; project discovery (classpath env vars vs a manifest you can walk up
to) matters more; a grammar can be CF and still scannerless (`x++/foo/i.y` in JavaScript means
lexing needs the parse); libraries can do the parsing for you anyway; and tools often need
*fragments*, not whole programs — give them one fragment production and the grammar class stops
mattering. The fetched comments of *what would you use to write a parser in 2021?* supply the
practitioner version of the premise: ANTLR's context-free model forces imperative actions that sit
outside it — "square peg round hole" — and "modern languages are so rarely context-free these
days."

**Buys.** It relocates the effort: specify the encoding, put project info on the filesystem, ship a
parse library, define a fragment entry point. All four are cheaper than proving a grammar property,
and all four help even when the grammar is *not* CF.

**Costs.** If you accept this, you also accept languages whose tokenisation depends on parsing —
and the observer must then keep a full parse to answer trivial questions like "which identifiers
are used here." That is a real cost the thread acknowledges under Objection 3 before arguing
libraries absorb it.

**Maturity.** `contested` — the thread is itself the counter-argument to the received claim, and no
settled evidence is offered on either side; the parser-thread comments strengthen the counter
without measuring it either.

**Tried by.** The proposal is argued, not shipped; Go (encoding), Rust/Cargo (project discovery)
and JavaScript/Babel are the mechanisms it points at.

**Source.** *Is CF what's actually useful?* — 17, 34 comments, 2021-05-13,
<https://www.reddit.com/r/ProgrammingLanguages/comments/nbi3xh/is_cf_whats_actually_useful/> (all five objections and the conclusions table). *What parsing techniques do you use to support a
good language server?* — 66, 52 comments, 2022-03-01,
<https://www.reddit.com/r/ProgrammingLanguages/comments/t4c8ms/what_parsing_techniques_do_you_use_to_support_a/> (generating a TextMate grammar from a CF grammar is called an open research problem).

**Bearing on `fun`.** Partially already, partly a decision nobody has taken. `fun`'s reader takes
one whole string and produces delimiter groups — so tooling must always run the reader, exactly the
"you need a library" case. The fragment idea maps to something concrete: a bare expression is read
with `?open_prelude` because "a bare expression has nowhere to write the open" (the
`module-level-open-strict-imported-modules` decision) — i.e. `fun` already has an entry point for
fragments, and the conformance runner's `--file` mode uses it. Whether tooling must additionally
survive *broken* input is the recovery question above, still open.

### Everything is an expression; a trailing `;` discards

**What it is.** No statement form exists. A group evaluates its items in order and yields the last;
a trailing `;` before `}` makes it yield nothing instead. Conditionals, matches, loops and blocks
are all forms that produce values, and `return` is not needed because the block's tail *is* the
answer.

**Buys.** One rule replaces the statement/expression split, its parser branch, its unit-value
ceremonies and its special cases (can I use an `if` here? after a `;`? as a function body?). The
corpus's argument for it: "simplified parsing and a simpler grammar… a more predictive and cleaner
language."

**Costs.** Everything must produce something, so a loop or a failed branch needs a value story (a
unit type, or the language admits the question); and statement-position code needs the trailing
`;` discipline to avoid accidental values — which the corpus identifies as the reason C-likes need
semicolons on statements but never on blocks.

**Maturity.** `shipped` (Rust, Swift, Kotlin, Python expressions; C-family statements — both sides
are production).

**Tried by.** Rust, Swift, Kotlin, `fun`.

**Source.** *Expressions vs. statements* — 53, 67 comments, 2026-09-05,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1w89alg/expressions_vs_statements/> (why even a statement-flavoured language's users can't tell the two apart: Perl's postfix `if`,
Python's ternary). *What are the advantages for an imperative language to not be expression based?* —
37, 66 comments, 2023-01-24,
<https://www.reddit.com/r/ProgrammingLanguages/comments/10ke1mj/what_are_the_advantages_for_an_imperative/> (the counter-case, including Rust's semicolon-after-`if` hack). *Why do C-like languages require
semicolons after all statements, but not after blocks?* — 37, 46 comments, 2023-02-19,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1168rmc/why_do_clike_languages_require_semicolons_after/>.

**Bearing on `fun`.** Already has it. A conformance case *is* an expression
(`test/conformance/cases/README.md`), the trailing `;` before `}` discards a group's value
(`surface-syntax-braces.md`), and there is no `return` in the reader's keyword table
(`TokenTree.cs:45-51`) — see the last entry of this file.

### No ternary: the conditional is an ordinary form

**What it is.** Either there is no `c ? a : b` token at all (the conditional is a form or a
library function, like `match`), or — the corpus's own reconstruction — `:` builds an either-or
function and `?` merely applies it, so `cond ? 0 : 42` is two ordinary binary operators:
`0 : 42` is `(b) => if b then 0 else 42`, and `?` invokes it.

**Buys.** The second version deletes a special syntactic category (ternary operators have no
natural overload story — the thread's stated irritation) and lets the conditional be defined from
smaller pieces, exactly as a library would define it.

**Costs.** Laziness must come from somewhere: the thread says outright that preserving C's
lazy-branch semantics "the language needs to be lazy." A strict language taking this route gets
eager evaluation of both branches and must fall back to a lazy primitive or to a `match` form.

**Maturity.** `shipped` for the no-ternary side (expression `if`/`match` in Rust, Swift, Kotlin,
Python — the corpus discusses all four in these terms); `speculative` for the operator
decomposition, which the thread proposes without implementing.

**Tried by.** Rust, Swift, Python (`x if c else y`), `fun`; nobody claimed for `:`/`?` as
functions.

**Source.** *Thinking about "the" ternary operator.* — 31, 74 comments, 2023-10-19,
<https://www.reddit.com/r/ProgrammingLanguages/comments/17bx7nf/thinking_about_the_ternary_operator/>. *Expressions vs. statements* — 53, 67 comments, 2026-09-05,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1w89alg/expressions_vs_statements/> (Python's `x if cond else y` is a conditional *expression*, not an `if` statement — the distinction the
whole thread turns on).

**Bearing on `fun`.** Already has it, and harder than most: there is no conditional token at all.
`Bool` and `if` are library features — `Bool` is a prelude nominal ADT and `if` expands to `match`
(`topics/bool-and-if-as-library.md`, implemented), so `if (c) { t } else { e }` is an ordinary form
whose head and `else` literal come from the prelude. The laziness cost above is therefore already
paid for: branching is `match`, whose arms are not evaluated until selected. Nothing is open.

### Type application is ordinary application

**What it is.** No angle brackets, no square brackets: `Vec(n)` is a type, `f(x)` is a term, and the
same reader rule covers both. One grammar for types means type position has no separate syntax at
all.

**Buys.** The generics-spelling debate (angle vs square vs `Type#(Param)`) disappears, and
dependent types become writable without a second notation — `(n : I64) -> Vec(n)` reads like
everything else. The corpus observes the field drifting this way anyway: inference is already
dropping argument brackets at call sites, "as is in Swift and Rust." The comments add two failure
modes that not having angle brackets avoids outright: C++ nested templates lex `> >` as a right
shift, so a space must be inserted ("Having to put a space between `> >` for nested templates in
older versions of C++ because the lexer got confused"), and Rust needs a `::<>` turbo-fish where
`<` cannot open a type application in expression position [general knowledge, not from corpus] —
the parser-thread comment names the turbo-fish as the detail that lets Rust's syntax be read
without context (comments on *What tiny thing annoys you about some programming languages?* and
*what would you use to write a parser in 2021?*).

**Costs.** The reader cannot tell a type application from a term application, so no error can say
"expected a type here" until elaboration — a class of diagnostics moves out of the reader
entirely. It also removes the visual marker that tells a C++/Java reader they are in type
territory.

**Maturity.** `shipped` (ML family, and Scala's `f[T]` is the square-bracket data point named in
the thread).

**Tried by.** OCaml, Haskell, Scala, `fun`.

**Source.** *Generics syntax in different languages* — 58, 87 comments, 2022-03-20,
<https://www.reddit.com/r/ProgrammingLanguages/comments/tibrzi/generics_syntax_in_different_languages/>. *PL Syntax Going Forward* — 45, 39 comments, 2020-08-14,
<https://www.reddit.com/r/ProgrammingLanguages/comments/i9uuzo/pl_syntax_going_forward/> (the observation that brackets are being dropped at call sites rather than replaced).

**Bearing on `fun`.** Already has it, decided: one grammar for types, so `Vec(n)` and
`(n : I64) -> Vec(n)` are read by the same rules as term application
(`tickets/brackets-decide-grouping.md`'s worked example). The consequence worth naming: a
type-position error can only be raised by the elaborator, which fits `fun`'s split (the reader
decides no forms, resolves no names) but means reader diagnostics stay purely structural.

### Types decide grouping: adjacency as an operator, and type-directed disambiguation

**What it is.** Two variants of letting the type system settle how adjacent tokens group.
(a) *Adjacency as an operator*: `2025 July 19` is a `LocalDate`, `1 to 10` is a `Range`,
`299.8M m/s` is a velocity — a pair of adjacent expressions is a candidate for binding, and the
LHS type decides whether it binds, through `pre`/`postfixBind` methods it declares.
(b) *Type-directed disambiguation*: application by juxtaposition is parsed by trying the
type-correct grouping — `a b c` parses as `a (b c)` when nothing else type-checks, and where
several groupings type-check the program is an error and the user must parenthesise.

**Buys.** (a) gives a domain its own literal notation without touching the base language's
grammar or its types: units, ranges, currencies and short DSLs are written directly in the code.
(b) removes precedence from application entirely — the types are the precedence table, which is
exactly how mathematical prose is read.

**Costs.** Grouping stops being decidable from the text. (a)'s own author: "adjacency is a
two-stage parsing problem… expression grouping isn't fully resolved until attribution. The
algorithm for solving a binding series is nontrivial," and it "shifts the boundary between syntax
and semantics in a way that may feel a little unsafe." (b) adds that its author names himself:
"`a b c d e f g` might mean `_c_ (a b) (d e (f g))`" — long sentences without brackets become a
party trick, and an error message can only say "several groupings type-check", not which one was
meant. Both variants make the reader's output provisional until types exist.

**Maturity.** `shipped` for (a) — Manifold is a real Java toolchain with an IntelliJ plugin and
several libraries built on the feature; `speculative` for (b), argued by its proposer with no
implementation offered.

**Tried by.** Manifold (Java); Agda and Haskell are the thread's reference points for (b)
[general knowledge, not from corpus]; nobody claimed to have shipped (b).

**Source.** *What If Adjacency Were an \*Operator\*?* — 68, 36 comments, 2025-07-21,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1m5qtw8/what_if_adjacency_were_an_operator/> (the shipped version, with its own "Downsides" section — the costs above are the author's).
*Integrating type system inside parser?* — 16, 17 comments, 2018-01-20,
<https://www.reddit.com/r/ProgrammingLanguages/comments/7rtx2z/integrating_type_system_inside_parser/> (the speculative version, asked as a question, with its author raising the confusion objection
himself).

**Bearing on `fun`.** Rejected by the project's split, not by opinion — and the split is the
reason worth writing down: `Fun.Expand` cannot reference `Fun.Compiler`, so no grouping decision
during enforestation may consult a type. That is consistent with what `fun` already allows
type-aware macros, whose *output* is expanded syntactically while only their annotation is
type-aware (`CONTEXT.md`, Macro annotation) — deferring to the elaborator buys annotation, never
extent. Note the interaction with "one grammar for types": `fun` can afford `Vec(n)` reading like
term application precisely *because* grouping is decided before any type is known. No ticket
proposes type-directed grouping.

### Brace bodies everywhere: `fn` marks the lambda, position marks the trailing block

**What it is.** Every body is a `{ … }` group. A lambda writes `fn(x) { … }` so that a bare
`{ … }` is never mistaken for one; a block passed as the last argument needs no marker at all
because it is simply the final group — `callee(arg0, arg1) { block }` expands to
`callee(arg0, arg1, { … })`, which is Swift's trailing closure and Kotlin's trailing lambda. The
alternative spellings on offer in the corpus: `{ a, b => … }` / `(a, b) => …` (arrow style) and
`|a, b| …` (Rust style).

**Buys.** One delimiter for every body, and control-flow-looking syntax that is just a call with a
group argument — which is what lets a library define `if` without the reader knowing `if`. The
trailing form saves the callback's parentheses entirely.

**Costs.** Because a bare group is ambiguous (lambda? block? record?), something must mark the
lambda — `fn`, `=>`, or Rust's pipes; and multi-argument trailing closures need labels (Swift's
`secondClosure:` syntax) or a semicolon-insertion rule to keep `foo { … } bar { … }` from gluing
two calls together. The comments add a physical cost from three unrelated threads: on German
layouts `"{[]}"` are AltGr+7 through 0, "I can't type 'AltGr + Key' on my german keyboard, which is
bad because '{', '|' and many more require AltGr", "curly braces are annoying to a lot of us
because they require reaching with AltGr; semicolons are just shift-comma", and a bracket-heavy
proposal was called "literally the worst to type on most EU keyboards … so many three finger key
combos" (comments on *Been thinking about writing a custom layer over HTML*, *Introducing the Beef
Programming Language*, and *No Semicolons Needed*). Braces are paid for at the keyboard, not only
at the reader — which is the one argument the `do … end` side can make without touching the macro
model (next entry).

**Maturity.** `shipped` — Swift trailing closures, Kotlin trailing lambdas, JS arrow functions,
Rust `|a| …`, all named in the two threads.

**Tried by.** Swift, Kotlin, JavaScript, Rust, `fun`.

**Source.** *Generalizing Ruby block syntax in static languages with currying* — 40, 13 comments,
2021-02-19,
<https://www.reddit.com/r/ProgrammingLanguages/comments/ln9opy/generalizing_ruby_block_syntax_in_static/> (trailing blocks, labelled multiple closures, and the semicolon rule needed to make `while` and
`if` survive without reserved words). *Lambda / Closure Syntax Preferences* — 30, 30 comments,
2020-09-03, <https://www.reddit.com/r/ProgrammingLanguages/comments/im4cy0/lambda_closure_syntax_preferences/> (arrow vs pipe, as a poll — engagement only, no body argument). *Introducing the Beef
Programming Language* — 160, 84 comments, 2020-01-07,
<https://www.reddit.com/r/ProgrammingLanguages/comments/elbt5u/introducing_the_beef_programming_language/> and *Been thinking about writing a custom layer over HTML (left compiles into right)* — 288, 104
comments, 2020-09-08,
<https://www.reddit.com/r/ProgrammingLanguages/comments/ioon55/been_thinking_about_writing_a_custom_layer_over/>
(comment evidence only: the AltGr keyboard cost above).

**Bearing on `fun`.** Already has it, and the `fn` prefix is required by the same decision that
gave braces: `tickets/surface-syntax-braces.md` — "A bare `{…}` is always a block," which is why
`fn(x) -> e` became `fn(x) { e }` rather than `(x) { e }`, and why `if`/`match` heads are
parenthesised so `c { … }` can never read as a record construction on `c`. The trailing-group form
is what `fun` already writes: `if (c) { t } else { e }`, `choose (flag) { … } else { … }`, where the
head is a prelude form and the groups are its arguments.

### Keyword-pair blocks (`do … end`) — the flavour decision that is still fog

**What it is.** Blocks delimited by a matched word pair: `if c do … end`, `fn f() do … end`,
`module M do … end`, or the colon-plus-off-side variant. The counter-position refuses *both*
alternatives — not the colon, not `do`/`then`/`end` — on the grounds that the off-side rule alone
already organises the code.

**Buys.** A closer that names what it closes (`end`, `EndIf`, `EndFunction`) matches how code reads
aloud, survives re-indentation, and — the argument in the corpus — *attracts a different audience*:
a systems language written with braces draws C readers, a language with `end` draws scripters, and
the designer of one thread can't decide because both readings are legitimate. The comments add the
physical half of that argument: letters need no AltGr, so on European keyboards a word pair is
cheaper to type than `{}` (see the brace entry above for the three comments).

**Costs.** A keyword pair must be tracked by the reader as a stack (the corpus's own AEC example
shows what happens when editors assume braces instead), it cannot be reflowed by tools that expect
one delimiter, and `fun` has already paid the maintenance bill once: keyword-pair grouping was
"reimplemented four times" in the prototype, "disagreeing on openers," before braces deleted all
of it.

**Maturity.** `contested` — shipped by Lua, Ruby, Elixir, Ada and Delphi [general knowledge, not
from corpus], rejected outright by the Quartz thesis, and undecided inside `fun`.

**Tried by.** Lua, Ruby, Elixir; `fun` had it and removed it.

**Source.** *Block Delimiters.* — 33, 39 comments, 2018-12-30,
<https://www.reddit.com/r/ProgrammingLanguages/comments/aayvws/block_delimiters/> (the indecision, and the audience-shape argument). *Thesis for the Quartz Programming Language* — 23,
55 comments, 2025-10-02,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1nvvmii/thesis_for_the_quartz_programming_language/> (the refusal: "I also refuse to believe that… the use of 'do,' 'then,' or 'end' keywords is an
effective solution").

**Bearing on `fun`.** This is a named fog item: *"Surface syntax after the macro model settles"* in
`docs/wayfinder/fun-design-map.md` — whether broad syntax should become "possibly less ML-flavored,
Ruby/Elixir-style `do … end`" once the macro expansion model is finalised. Read against
`tickets/surface-syntax-braces.md` (closed 2026-09-14) the two records pull in opposite
directions: braces won the 2026-09-14 decision and deleted the keyword-pair helpers as four
diverging implementations, while the fog item still holds the door open for word pairs at the
level of the whole surface. Anyone re-opening this should start from that ticket's notes, which are
the recorded cost of `do … end` in `fun`'s own history.

### Pipe-shaped chaining, with leading-dot segments

**What it is.** Data flows left to right through a pipeline operator, and a segment may omit its
receiver: `" HELLO WORLD ".strip().lower()` becomes `" HELLO WORLD " -> .strip -> .lower -> print`,
where `.strip` means "call `strip` on what came before." A related shape is the update assignment
`numbers .= map(x => x * x)` for `numbers = numbers.map(...)`.

**Buys.** The reading order matches the evaluation order, method chains stop nesting, and the
leading dot lets a reader skim the operations without repeating the subject. The `.=` form buys the
same for a single update without re-naming the target.

**Costs.** Two notation inventions to learn instead of one (`->` as composition *and* `.` as a
receiver hole), and the receiver hole makes any segment's meaning depend on its left neighbour —
reordering a pipeline changes what each dot refers to. The `.=` proposal's own framing shows the
hunger it addresses is real, but neither proposal in this corpus is implemented. The comments name
a conditional cost for the plain `|>` too: "Without partial application the pipe operator is often
awkward to use and requires introducing lambdas everywhere" — a pipe over partially applied
functions needs partial application, and where the language has none the operator degenerates into
lambda-writing (comment on *What Operators Do You WISH Programming Languages Had?*). Demand is
confirmed in the same thread: "I wish more languages had ML family's `|>` operator … `|>` can work
for arbitrary functions and values", with the ipython shell as the concrete irritation (51 points).

**Maturity.** `research` for the leading-dot form; the plain pipeline operator `|>` is `shipped`
(Elixir and friends [general knowledge, not from corpus]).

**Tried by.** Quartz (thesis, no implementation); nobody claimed for `.segment`.

**Source.** *Thesis for the Quartz Programming Language* — 23, 55 comments, 2025-10-02,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1nvvmii/thesis_for_the_quartz_programming_language/>. *What Operators Do You WISH Programming Languages Had? [Discussion]* — 174, 243 comments,
2022-10-21,
<https://www.reddit.com/r/ProgrammingLanguages/comments/ya87l1/what_operators_do_you_wish_programming_languages/> (demand signal; the largest thread on this axis). *An idea for a `.=` operator* — 81, 83 comments,
2021-12-21, <https://www.reddit.com/r/ProgrammingLanguages/comments/rleiot/an_idea_for_a_operator/>.

**Bearing on `fun`.** Genuinely new — no pipe operator exists (`grep '|>'` over `src/Fun.Expand`
and `std/` finds nothing) and no ticket proposes one. It would be cheap *and* well-behaved: a pipe
is just an operator in a group (`infix (|>) additive`-style, related to whatever it must not
swallow), and `fun`'s order-group rule means the pipe's interaction with every other operator has
to be declared rather than guessed. It belongs with the fog item on library-vs-compiler machinery:
nothing about it needs compiler support.

### Uniform call notation (UFCS): one spelling for calling a function and a method

**What it is.** `x.f(a)` and `f(x, a)` are the same call: a free function can be written in method
position on its first argument, and a method can be written in prefix position. Chains compose
either way.

**Buys.** Extension methods, a pipe-like chain, and "simple types behave like OOP objects… without
requiring extra boilerplate," in the words of the thread that nominates it as the most underrated
feature there is. Library code becomes reachable in whichever order the sentence wants it.

**Costs.** Name resolution acquires a fallback: if `x.f` fails as a member it may still be a free
function, so error messages and IDE completion must distinguish "no such member" from "no such
function either." The proposing thread's own bug list is evidence — namespace-qualified functions
had to be excluded, single-argument functions needed a fix, and generic members were promised
precedence rules with a warning.

**Maturity.** `shipped` (D, Nim, Koka — all named in the thread).

**Tried by.** D, Nim, Koka, the thread author's language.

**Source.** *What underrated feature do you wish you would see in other programming languages?* —
19, 35 comments, 2024-08-20,
<https://www.reddit.com/r/Compilers/comments/1ews70w/what_underrated_feature_do_you_wish_you_would_see/> (the nomination, the examples, and the bug list). *An idea for a `.=` operator* — 81, 83 comments,
2021-12-21, <https://www.reddit.com/r/ProgrammingLanguages/comments/rleiot/an_idea_for_a_operator/> (assumes UFCS as its premise).

**Bearing on `fun`.** Open, and named: the fog item *Library-level features vs compiler machinery*
lists UFCS with FFI as "desirable but should not drive the prototype agenda now." The seam is
decided — `Fun.Expand` cannot reference `Fun.Compiler` — so any UFCS form must resolve during
enforestation against bindings only, with the type-directed half left to whatever a template can
ask of the elaborator. That constraint is not recorded anywhere as a design note; it falls out of
the project split and would be the first thing an implementer hits.

### Continuation-flattening postfix sugar

**What it is.** A postfix marker on a call whose function takes a one-argument callback, so the
rest of the enclosing body *becomes* that callback: `var username = get_input().then!;` continues
inside `then`'s continuation. The corpus lays out the whole family — Haskell's `do` (monad only),
Idris's bang syntax (monad only), OCaml's `let*` (declared operators, any function), Gleam's `use`
(any function, ad hoc) — and asks whether restriction to monads is still justified.

**Buys.** Nesting collapses: `runTransaction(transaction => { … })` becomes a flat sequence of
statements under `runTransaction!`. It generalises the do-notation family from "types with a
`bind`" to "any function that takes a callback," which covers promises, transactions and parsers.

**Costs.** Readability of the marker itself — the thread's own doubts: "this reads a bit weird, and
it may not always be obvious when you want to call `map`, `and_then`, or something else." Beyond
that, an effect system already provides the general version of this flattening (a `perform` is
exactly "continue under someone else's continuation"), so a language with effects is choosing
between two notations for one mechanism.

**Maturity.** `shipped` for the restricted forms (Haskell `do`, OCaml `let*`, Gleam `use`, Idris
bang — all named in the thread); `speculative` for the unrestricted postfix `!` as stated here.

**Tried by.** Haskell, Idris, OCaml, Gleam; nobody claimed for `!`.

**Source.** *Expression-level "do-notation": keep it for monads or allow arbitrary functions?* —
28, 20 comments, 2024-10-12,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1g27n6f/expressionlevel_donotation_keep_it_for_monads_or/>.

**Bearing on `fun`.** Has it differently, and the difference is decided: `fun` has algebraic
effects with deep one-shot handlers, so the flattening is already the language's ordinary control
flow — `perform Log.write(7)` continues under whatever handler encloses it, and no marker is
written. What is genuinely open is the *notation* half: a template can give any
callback-accepting prelude function its own flattening form, since templates are ordinary bindings —
that would be a library experiment, not a compiler change. The thread's "restrict it or not"
question does not arise, because there is no `bind` to restrict.

### Library-defined keywords, and grammars that grow as they are read

**What it is.** Two intensities of extensible notation. (a) A word that looks like a keyword is
defined by the library: `for` written as a macro over `while`, complete with places for proof
obligations, "and then it will be just as if the language had always had `for`." (b) The far
version: the grammar itself changes as the file is read — each read step is a preconditioned
operation whose results "enable new syntax, so the grammar grows as the program is read," making
domain-specific notation a consequence of the effect system rather than a macro system. A third
position, between them: a Lisp's *reader* macros, which change tokenisation itself, so that even
the list-shaped uniformity is optional.

**Buys.** (a) means a language feature costs a library commit instead of a compiler release, and
users get the same power. (b) means a domain module introduces notation as part of introducing its
effects — no separate macro phase at all.

**Costs.** (a) inherits the macro system's whole hygiene and expansion-position contract for every
such keyword — the definition must be as hygienic as any macro, and its errors arrive during
expansion. (b) is far more expensive: the reader stops being a fixed pass, no external tool can
parse a file without running the program's own effect reasoning, and the only implementation in
this corpus is a self-hosted hobby language at score 0. The general warning, from the Lisp-infinite-syntax
thread: once you want actual computation inside the uniform wrapper, you inevitably introduce new
rules — "Lisp has infinite syntax," so uniformity at the outside buys less than it appears.

**Maturity.** (a) `shipped` (Scheme `syntax-rules`, Rust `macro_rules` — named in the Metamath C
post as its model); (b) `speculative` (one hobby implementation, no paper).

**Tried by.** Scheme, Rust, Metamath C (`for` over `while`), `fun` (`if`, `&&`, `type` all prelude
forms); Spine for the growing grammar. The comments add named prior art for (a): Nemerle's syntax
extensions, which reach the token stream the lexer produced; Raku's *slangs*, which alter a module's
syntax and semantics both, with one commenter noting a 30-line module adding an `actor` keyword
(but that Raku's own bootstrap primitive, `KnowHOW`, cannot be jettisoned); and Coalton — an
optimizing compiler for a statically typed language, implemented "just" as a Lisp macro and used in
production (comments on *A cleaner approach to meta programming* and *Are myths about the power of
LISP exaggerated?*). Also named there: *Generalized macros*, an idea for macros that may modify
code surrounding the macro call, which its describer had never seen implemented.

**Source.** *Metamath C: A language for writing verified programs* — 86, 30 comments, 2020-06-21,
<https://www.reddit.com/r/ProgrammingLanguages/comments/hczeof/metamath_c_a_language_for_writing_verified/> (the `for`-over-`while` example, in its "Language extensibility" section). *Spine: a language where
parsing is a nondeterministic effect and the grammar grows as the program is read* — 0, 28
comments, 2026-04-10,
<https://www.reddit.com/r/Compilers/comments/1shmwwk/spine_a_language_where_parsing_is_a/> (hobby project at score 0; a data point about interest only). *Making my own Lisp made me realize
Lisp doesn't have just one syntax (or zero syntax); it has infinite syntax* — 54, 50 comments,
2024-08-03,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1ejaowf/making_my_own_lisp_made_me_realize_lisp_doesnt/> (the reader-macro position and the argument that inner rules multiply regardless). *A cleaner
approach to meta programming* — 45, 89 comments, 2025-10-14,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1o6gdly/a_cleaner_approach_to_meta_programming/>
and *Are myths about the power of LISP exaggerated?* — 91, 100 comments, 2023-07-25,
<https://www.reddit.com/r/ProgrammingLanguages/comments/158iyza/are_myths_about_the_power_of_lisp_exaggerated/>
(comment evidence only: Nemerle, Raku slangs, Coalton, Generalized macros).

**Bearing on `fun`.** (a) already has it, by design — Stage 11 demotes `if` and `&&` to prelude
forms, `type` is a stage-2 std macro, and any new keyword arrives the same way; that is what the
umbrella ticket `specify-stage-11-macro-powered-language-features.md` is for. (b) is explicitly
*not* `fun`'s shape, and the glossary draws the line: the reader "decides no forms and resolves no
names." Extension lives in enforestation — interleaved with expansion, sensitive to bindings — never
in the reader, so a `fun` file's tokenisation cannot depend on the program in it. That is a
deliberate constraint on how far (a) can go: a template can add notation, not add tokens.

### The tooling bill for extensible notation

**What it is.** The counter-argument to everything above: any notation a library defines is
notation an IDE, highlighter, formatter and debugger does not know. In the corpus's framing, full
rewrite-everything metaprogramming is "strictly more powerful" than constrained approaches, but
"you can't expect any IDE features, LSPs, smart syntax highlighters, debuggers, or other tooling
for the base language to automatically work for your DSL."

**Buys.** For the defence: the extension is worth it where the notation *is* the domain — DSLs,
query languages, embedded grammars — and the thread concedes the constrained half of the spectrum
is where most real uses sit.

**Costs.** Every new form is a new case for every tool, and tools that cannot run the macro system
must either approximate (regex highlighter) or give up. The thread's sharpest version of the cost:
if you need the compiler's own analysis passes to transform code soundly, "you're really trying to
write a compiler plugin instead… I wouldn't call that a language feature." The comments answer it
— the objection "code that uses macros is unpredictable … It's also difficult for IDEs to process
macros, because they have to execute arbitrary code" is met with three replies: Lisp macros accept
and return data the compiler understands directly, where Rust's proc macros must parse and
re-serialize their inputs and outputs; the recommended shape is a `call-with` higher-order function
with a thin macro over it, so the macro itself does nothing; and SBCL's own compiler uses macros to
encode ARM64 inline assembly (comments on *Are myths about the power of LISP exaggerated?*).

**Maturity.** `contested` — and now argued in comments rather than only between post bodies. The
sceptic's version is stated in a post; the defence is architectural (data in, data out; the macro
is a thin layer) rather than measured. No tool was tested either way in this corpus, so the
disagreement stands on reasoning, not results.

**Tried by.** The disagreement is argued; Rust (proc-macro tooling), Common Lisp and Racket are the
families being argued about.

**Source.** *Between more constrained, local metaprogramming approaches and full-blown DSL
interpreters, where's the practical use case for LISP-style metaprogramming?* — 32, 29 comments,
2026-08-03,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1veo0it/between_more_constrained_local_metaprogramming/>. *Is CF what's actually useful?* — 17, 34 comments, 2021-05-13,
<https://www.reddit.com/r/ProgrammingLanguages/comments/nbi3xh/is_cf_whats_actually_useful/> (the fragment-production answer: give tools one entry point for partial input).

**Bearing on `fun`.** This is the sharpest reason `fun` decided what it decided, and the decision
already mitigates most of the bill. Grouping is decided by the reader's delimiter groups *before any
macro runs* ("brackets decide grouping… the reader's tree decides extents before any macro
expands"), so a tool can lay out a `fun` file's structure without running a single template —
what it cannot do is know what a head *means*. The residual cost is measured and open:
`scope-enforester-improvements.md` records that expansion errors carry no span at all (three
message-only exception types, `Driver.cs:38-46` discards the ones that exist upstream), so even
the compiler cannot yet point at where a form went wrong. That is the first thing a language
server would need, and it is ticketed rather than disputed.

### An interactive surface built on the whole compiler

**What it is.** A REPL where code can be edited *while it is running* — the Lisp/Smalltalk shape —
rather than a read-eval-print loop over whole definitions. The corpus's question is why almost
nobody outside that family ships one, and whether the answer is technical or fashion.

**Buys.** Feedback measured in seconds instead of builds; definitions can be redefined in place,
which is what makes exploratory work on a typed language tractable rather than merely possible.
The comments give the property a test: "can you run your program, drop into a breakpoint REPL,
recompile a function from the editor window, add a new class, and resume running with the new
definitions in place?" — with the answer that C# cannot and Smalltalk/Lisp can (comment on *What is
it like to write a large project in a dynamically-typed language?*). Practitioner accounts in
another thread describe the payoff: resuming from an unhandled failure while the program runs,
inspecting values as they change, and "remotely live-coding" a stubbed-out server into existence —
and a full-time Common Lisp developer's summary of the alternative ecosystems as "why everyone is
making their life so hard" (comments on *Are myths about the power of LISP exaggerated?*).

**Costs.** A typed language pays elaboration latency at every keystroke-level interaction, and an
*interactive* (edit-while-running) REPL needs the running image to accept replacement definitions
— which pulls against immutable compilation units and against anything cached per file. One thread
already names the combination people want and cannot find: static typing first, inference, plus
"great concurrency" and hot reloading, with Swift dismissed partly on compile times.

**Maturity.** `shipped` (Common Lisp, Scheme, Smalltalk — the threads treat these as the examples);
the *statically typed, interactive, fast* combination is `contested`.

**Tried by.** Common Lisp, Scheme, Clojure, Smalltalk; `fun` has no REPL at all.

**Source.** *Why don't more languages implement LISP-style interactive REPLs?* — 74, 92 comments,
2023-02-05,
<https://www.reddit.com/r/ProgrammingLanguages/comments/10u74ts/why_dont_more_languages_implement_lispstyle/> (edit-while-running as the defining property; the linked resources are in the body). *Statically-typed
interactive scripting languages?* — 34, 39 comments, 2020-11-05,
<https://www.reddit.com/r/ProgrammingLanguages/comments/jos211/staticallytyped_interactive_scripting_languages/> (the demand side, with per-language objections). *What is it like to write a large project in a
dynamically-typed language?* — 90, 148 comments, 2021-12-02,
<https://www.reddit.com/r/ProgrammingLanguages/comments/r6nq30/what_is_it_like_to_write_a_large_project_in_a/>
(comment evidence only: the edit-while-running test above).

**Bearing on `fun`.** Open, and named twice. There is no REPL: `src/Fun.Cli` is a stub that prints
"the .NET port has no entry point yet" and exits 1, and the only way to run a program is the
conformance runner's `--file` mode. What unblocks it is the other named fog item — *First-class
compiler API*: "a tool, an LSP, a REPL and a macro all sit on one surface instead of
re-implementing the elaborator." One detail from the design map decides feasibility: the REPL entry
points already `open std` by default (`explicit-prelude-open-operator-demotion`), so the interactive
surface's context is not the open question — the compiler-as-library interface is.

### No `return`: a block's exit is a value or an effect

**What it is.** Delete `return`, `break` and `continue` from the written language. A block ends by
yielding
its tail; early exit is either an ordinary value passed outward (`else` chains) or an effect
handled by the enclosing code. The corpus's argument: every one of `return`, `break`, `continue`,
`?` and Rust's `for_each`-can't-return is a special ability that the language's own constructs have
and the user's own functions do not — so, as the post puts it, you end up using the built-in
constructs for everything.

**Buys.** The asymmetry disappears: a library function can do everything a control construct can,
because control is not syntactic. Blocks stay expressions, and there is no return-type-altering
exit to type-check around.

**Costs.** Deep nesting gets verbose fast — the thread's own rewrite adds an `else` at every level —
and the alternatives each need machinery: labelled returns (Kotlin), or a handler per exit kind.
A reader used to scanning for `return` must instead follow the value or the effect.

**Maturity.** `shipped` on the expression side (Rust, Kotlin, Python all named in the corpus);
`shipped` for effect-shaped exits where handlers exist.

**Tried by.** Kotlin (labelled `return`, the one the thread credits), Rust, `fun`.

**Source.** *Had an idea for ".." syntax to delay the end of a scope. Thoughts?* — 42, 63 comments,
2025-01-03,
<https://www.reddit.com/r/ProgrammingLanguages/comments/1hse19g/had_an_idea_for_syntax_to_delay_the_end_of_a/> (the case against early exit, with the `else`-cascade rewrite showing what replacing it costs).

**Bearing on `fun`.** Already has it, structurally: there is no `return` in the reader's keyword
table (`TokenTree.cs:45-51`), every case in the suite is an expression, and the general mechanism
for "leave now" is the effect system — `perform` under a deep one-shot handler, with
`HandledEffectEscapes` marking the failure mode. What the corpus thread wishes for — that user
functions could control flow like built-ins — is exactly what `fun` gets from making control
effects library-visible. No ticket proposes adding `return`.

## Threads worth reading in full

- *No Semicolons Needed — How languages get away with not requiring semicolons* (122/103) — the
  closest thing this corpus has to a survey of statement termination; read it before deciding
  anything about separators.
- *Is CF what's actually useful?* (17/34) — the best-written demolition of a received wisdom on
  this axis; every objection is concrete and checkable.
- *What If Adjacency Were an Operator?* (68/36) — a shipped, type-directed notation with its
  downsides honestly listed by its author; the most complete idea-post on this axis.
- *[Preprint] Pika parsing* (106/56) — the one parsing paper to argue its case on Reddit; the
  abstract alone states both problems and the claimed fix.
- *Thesis for the Quartz Programming Language* (23/55) — one person's coherent refusal of colons,
  arrows and `do … end` at once; a good mirror for whatever `fun` decides about flavour.
- *Had an idea for ".." syntax to delay the end of a scope* (42/63) — the best available argument
  against early-exit syntax, made by someone who tried to replace it.
- *Between more constrained, local metaprogramming approaches and full-blown DSL interpreters* (32/29)
  — the sceptical case about notation extensibility, stated at length; its only answers are in the
  Lisp-myths comments, and neither side measured anything.
- *On the design of APLs, LISPs, and FORTHs* (77/57) — what unusual notation costs in cognitive
  load, measured by someone who likes all three.
- *Generalizing Ruby block syntax in static languages with currying* (40/13) — how much machinery a
  trailing block actually needs (labels, semicolon insertion) once you try to generalise it.
- *Why don't more languages include "until" and "unless"?* (147/237) — the best-argued keyword
  question in the corpus: named reasons on both sides, an Elixir deprecation to check, and both
  library and keyword positions taken (tree fetched under the `meta` axis).
- *What Operators Do You WISH Programming Languages Had?* (174/243) — the demand thread; its
  comments are where the operator-open versus operator-scarce fight actually happens.

## Gaps and disagreements

**Coverage: 20 threads, 520 comments, all from the top of their threads.** Each fetch returned 26
top-level comments (the endpoint gives at most ~30) out of trees of 46–100, the rest sitting in
unretrieved `more` placeholders — so the arguments recorded above are the *upvoted* arguments: a
26-comment sample of the operators-wish thread (243 comments) or of *No Semicolons Needed* (103)
is not its argument. Disagreements that are now argued in comments (semicolons, operator openness,
`unless`/`until`, extensible notation versus tooling) are argued at the top of their threads;
replies to those replies are invisible here. Several of this axis's most-engaged threads had no
tree fetched at all — *Is operator precedence even necessary?* (97), *Generics syntax in
different languages* (87), *Significant Inline Whitespace* (68), *Semicolon Inference* (65),
*Whitespaces around operators sets their precedence* (57), *Syntax Design* (38) — so those entries
rest on post bodies and remain contested at the level of positions taken, not of a settled
argument.

**Not covered by this axis's sources.** The corpus has almost nothing on how *effect rows* or
record types are spelled — that material sits with the effects and types threads, not the syntax
ones, and `grep` for row-notation discussions found nothing citable. The same is true for string
literals and interpolation (8 threads, all product announcements) and for comment/doc-comment
syntax (nothing). Treat those as untouched, not as absent.

**Live disagreements worth resolving with evidence, not argument.**

- *Whitespace and layout.* Five corpus threads mention significant whitespace against 84 on
  precedence and 61 on separators — this is a topic the field has largely stopped arguing, which is
  either because it is settled or because nobody wants to reopen it. Deciding it for `fun` needs a
  prototype and a re-indentation test, not more threads.
- *Recovery workload.* The field agrees error recovery is desirable; `fun`'s open ticket is the only
  source here that asks how often it would fire, and it has not been measured. The probe is written
  down in the ticket (run every `error` case and tally which layer refused).
- *Extensible notation vs tooling.* The sceptic's case now has an answer in comments — an
  architectural defence (macros take and return data the compiler understands; the macro should be
  a thin layer over a `call-with` function) — but neither side measured a tool, and `fun`'s own
  counter-evidence, grouping decided before any expansion runs, should be tested against a real
  external tool before it counts as an answer.
- *`do … end` vs braces.* `fun`'s own two documents disagree in direction: the closed
  `surface-syntax-braces` ticket deleted keyword-pair grouping as a four-times-reimplemented
  liability, while the fog item keeps Ruby/Elixir flavour open until the macro model settles. A
  human decision, not a research question.

**What you would need to read beyond this corpus:** the Pika paper itself (arXiv:2005.06444, linked
in its thread) for the recovery claims; Manifold's own documentation for binding expressions as
shipped rather than described; Rhombus's `enforest/main.rkt` — cited by `fun`'s own ticket — for
undeclared-order errors and hole extents; and any production implementation of relative precedence
groups, of which this corpus shows none.

## Dissent and corrections

**Corrections to the first pass.** The header claimed comment coverage for this axis was zero; it
is 20 threads and 520 comments, and every "no comment tree was fetched" note in this document has
been rewritten around what was actually retrieved. Two threads cited for comments (*Why don't more
languages include "until" and "unless"?*, *What tiny thing annoys you about some programming
languages?*) had their trees fetched under the neighbouring `meta` axis rather than this one; both
say so at their Source.

**What the comments contradicted.** (1) *No Semicolons Needed* — its own top comments argue against
its thesis, so the statement-termination entry now quotes the pushback it drew as well as the post,
and gained three mechanisms the bodies never mentioned (statement-bodied lambdas under newline-significance, Go's trailing-dot
chain, line-completeness at a REPL). (2) The custom-operators entry was `shipped` for both sides;
the operators-wish comments split without converging, so it is now `contested` with both positions
stated. (3) The tooling-bill entry said the enthusiasts were unanswered; they answered in the Lisp
myths thread, with an architectural defence rather than a measurement. (4) Smaller sharpenings:
Haskell's layout is alignment rather than indentation, angle brackets cost `> >` and turbo-fish,
braces cost AltGr on European keyboards, and a fixed precedence table has shipped at least two
errors (C bitwise, PHP `?:`) — none of which any post body in this axis stated.

**What could not be resolved with this material.** Whether the retrieved top comments represent
their threads: most of each tree is unretrieved — one syntax thread left 226 comments in `more`,
the until/unless tree left 143, the tiny-thing tree 323 — so no verdict here claims to speak for a
whole thread. Whether the `|>` cost — pipes need partial application — bites anywhere real, since
nobody in the corpus states how common that precondition is. And whether any `contested` tag above
will move: all four argued disagreements offer reasoning,
not data, and settling them would need the unfetched `more` pages or a tool that nobody built.

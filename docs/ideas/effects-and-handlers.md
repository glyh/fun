# Effects and handlers — how a language tracks and performs effects

This axis is about the machinery of effects: algebraic effects and handlers (deep,
one-shot, lexically scoped), effect rows and what they claim on a type, the async
colouring debate, the alternatives (monads, capabilities, exceptions, `Result`),
state and resources as effects, what happens when a handler is left with its effect
still live, and how implementations make handlers fast. Memory management per se is
`runtime-and-memory.md`; general type-inference mechanics appear here only where
effects force them.

## How this was gathered

The corpus is 2538 threads from r/ProgrammingLanguages and r/Compilers. This axis's
slice holds 107 threads ranked by comment volume, score and body length; I read the
bodies of the highest-signal ones and grepped the full `corpus.jsonl` for topics the
ranking missed (`evidence passing`, `multi-shot`, `iterator dispose`, `colorless`,
`delimited continuation`, `capability`, `row polymorphism`, `panic`, `green thread`).
The corpus is **what Reddit upvoted**, not a survey of the field: popular does not
mean correct, and several high-scoring threads are self-promotion for hobby languages
whose design claims are unverified — they are evidence that an idea is *being tried*,
never that it works.

The comment pass fetched trees for the **20 richest threads** of this axis, yielding
**515 retrieved comments** (19 trees of 26 plus one of 21). Each fetch returns only
about **30 top-level comments regardless of `limit=100`** — the rest sit in
unretrieved `more` placeholders (37 across these 20 trees, whose full trees hold 1221
comments), so every deep reply in every thread is missing. Roughly a dozen of the 20
trees are on-axis; the ranking also pulled in a poll-policy thread, two hobby OOP
languages and the Omega Function. Comments are cited as `(comment on <thread title>,
<thread permalink>)` because the slice records thread permalinks only, never comment
permalinks.

Two threads that promise the "are effect systems worth it" arguments could **not** be
recovered: *Are algebraic effects worth their weight?* (75, 41) and *What are the
issues with algebraic effects?* (69, 50) are listed in the fetch queue but their trees
are not on disk, and this pass forbids network requests. The same arguments were
recovered from a third thread's fetched tree instead — *Why Algebraic Effects?*
(86, 58), a bodyless link post whose comments carry the whole debate — plus the
fetched exceptions, error-handling, purity and async trees. Link posts whose comments
were never fetched still contribute only title and score; those are flagged.

## The ideas

### Multi-shot continuations: resuming the same computation twice

**What it is.** The continuation handed to an effect branch is a reusable value: after
`resume(true)` you may call `resume(false)` on the same continuation and take the
other result. Effekt's exhaustive-search example does exactly this — try a branch,
discard the result, try the alternative.

**Buys.** Backtracking, nondeterministic search and "try it the other way" are written
directly instead of being reified as a search tree the programmer builds by hand.

**Costs.** Everything the continuation holds is re-entered: in an imperative language
a resumed continuation re-runs `count++` on captured mutable cells, and file handles
and locks would be acquired twice. One poster's entire case for *single*-continuation
effects is this; a second poster names "multiple resumption wrt. resources" as the
power he is anxious about. Cleanup then has to run on every re-entry (see the
`dynamic-wind` entry). The comments add a *second* objection that is not about
resources at all: effects "allow to not resume, resume once, or even resume multiple
times. This leads to a lot of non-local code that is difficult to understand and
debug, as stepping through the code can jump wildly all over the place" — the complaint
is about reading code, and it would survive even if refs and C frames were no
obstacle (comment on *Why Algebraic Effects?*,
https://www.reddit.com/r/ProgrammingLanguages/comments/1kth7xm/why_algebraic_effects/).

**Maturity.** Contested — one side has the better evidence for imperative languages
(the mutable-state argument is concrete and unrebutted in the corpus), the other side
ships it in a purely functional setting. Of the 515 retrieved comments, none defends
multi-shot on its merits: the arguments run one way.

**Tried by.** Effekt (per the thread); nobody in the corpus ships it with unrestricted
mutation.

**Source.** Single-continuation algebraic effects in an imperative language?, score 18,
10 comments, https://www.reddit.com/r/ProgrammingLanguages/comments/14iz6jf/singlecontinuation_algebraic_effects_in_an/, 2023-06.
Counter-argument: Are algebraic effects worth their weight?, score 75, 41 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1mh3ba8/are_algebraic_effects_worth_their_weight/, 2025-08.
Second counter-argument (comments only): Why Algebraic Effects?, score 86, 58 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1kth7xm/why_algebraic_effects/, 2025-05 — a link
post, so the argument lives entirely in its fetched comments.

**Bearing on `quill`.** Rejected, with a reason recorded: multi-shot conflicts with refs
and C frames, and backtracking is written as data instead. A continuation is one-shot
and is never captured across an extern frame (CONTEXT.md, **Continuation**). The
community's resource objection is the *same* argument E7 records; the comprehension
objection is a different one that E7 does not mention; and E7's other half — the
extern-frame clause — appears nowhere in the corpus, so nothing outside quill corroborates
it either way.

### Checking one-shotness at compile time instead of at run time

**What it is.** Give `resume`'s continuation a linear type: it must be consumed exactly
once, so a second `resume` is a compile error rather than the run-time message
`continuation already used`. A handler that drops its continuation (abort-style) and a
handler that uses it twice are then both visible in its type.

**Buys.** The misuse moves to the definition, where the offending handler is written,
instead of to whichever request happens to be the second one — the same move quill made
with `HandledEffectEscapes`.

**Costs.** Linearity infects the whole type system. The corpus spells the price out:
generics over linear values, `Option<Linear>` and `Box<dyn Trait>` stop being
transparent, and the type system "gets much more complicated" for no benefit a working
programmer can name.

**Maturity.** Speculative — argued, not built. No implementation of a linear `resume`
appears anywhere in the 2538 threads, and the 515-comment second pass supplies none
either: "linear" occurs 34 times in the fetched trees and never next to `resume` or a
continuation.

**Tried by.** Nobody; the question is asked and left open by its asker, and no comment
in the second pass names a language, paper or project that checks `resume`'s linearity
at compile time.

**Source.** Benefits of linear types over affine types?, score 51, 30 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1e1o07f/benefits_of_linear_types_over_affine_types/, 2024-07.

**Bearing on `quill`.** Genuinely new to the project. quill enforces one-shotness at run
time *by choice* (domain-model E7: "enforced by construction, at run time — which is
the model's choice"), and no ticket proposes moving it earlier.

### Evidence passing: route a request without searching the call stack

**What it is.** Pass the handlers a call may need down with the call — a vector of
evidence bound when the row is bound — so a `perform` is an indirect call through the
evidence, not a walk up the stack looking for an installer. The thread cites the paper
that describes Koka's compiler doing exactly this on the way to C; the thread's own
problem is a .NET IL target, where there is no stack to save in the first place.

**Buys.** Dispatch becomes O(1) and needs no stack unwinding, which is what makes
handlers usable where you cannot capture a stack (.NET IL is the thread's case) and
what keeps a C interface free of a runtime search.

**Costs.** Every call in a row-polymorphic function carries the evidence vector: extra
arguments, extra registers, and a threading obligation at every point where a
callback's row is unknown. The mechanism is the same function-parameter passing the
same thread struggles to understand, which is itself evidence of the comprehension
cost. A comment reports the same convergence from the other side — coeffects "desugar
to extra parameters ... so there's nothing left at runtime" — which is one implementor
building evidence passing under another name (comment on *Why Algebraic Effects?*,
https://www.reddit.com/r/ProgrammingLanguages/comments/1kth7xm/why_algebraic_effects/).

**Maturity.** Research — implemented in a research compiler, described but not
reproduced in the thread.

**Tried by.** Koka (per the thread), its Haskell model EvEff; nobody else named in the
corpus.

**Source.** Koka's multi-prompt control monad?, score 24, 7 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/14cqhj1/kokas_multiprompt_control_monad/, 2023-06.

**Bearing on `quill`.** Open implementation question, named in the model: E5 says "the
port may route by evidence instead; the observable rule is the same." Today it routes
lexically — a call with an open tail is `Core.Tunnel { named; handlers }` and a request
skips the handlers its instance does not name (`effect_request.skips`). Switching to
evidence passing changes cost, not behaviour.

### The handler as a first-class value (multi-prompt control)

**What it is.** A handler — or its prompt — is a value: pass it, hold it, install it
on someone else's computation. The thread's concrete object is Koka's multi-prompt
control monad, with a `prompt` operator and a `yield` that bubbles up to its prompt
rather than to whatever happens to enclose the call.

**Buys.** A library can return a configured handler; a scheduler can hold, per task,
the handler that task should run under; two computations can share one handler
instance.

**Costs.** Routing stops being a static fact ("this row was bound to that handler")
and becomes a value lookup, so the escape question becomes a value-lifetime question
instead of a lexical one. Nothing anchors a late call to the place the handler was
written.

**Maturity.** Research — a paper and an implementation exist (the thread cites both),
and the poster cannot understand them, which is a data point about the accessibility
of the idea rather than its validity.

**Tried by.** Koka's control operators; no production language named in the corpus.

**Source.** Koka's multi-prompt control monad?, score 24, 7 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/14cqhj1/kokas_multiprompt_control_monad/, 2023-06.

**Bearing on `quill`.** Genuinely new, and blocked by two settled choices at once:
`resume` is a keyword-backed syntax form and "nothing can capture it" (E9), and a
`match` is not a value. Making handlers first-class would force `HandledEffectEscapes`
to be re-stated as a value-lifetime rule.

### Nearest-enclosing handler vs tunneling to the row's handler

**What it is.** Two rules for dispatch. **Dynamic**: a request jumps to the nearest
enclosing handler in the call stack — how the two hobby systems below behave, and how
Koka and OCaml 5 behave. **Lexical (tunneling)**: a request goes to the handler its row
was bound to, and a handler that merely encloses the call in source skips over requests
from a callback whose row it does not name.

**Buys.** Dynamic needs no annotation discipline and works in a language with no types
at all. Lexical gives parametricity: a function polymorphic in a row cannot observe the
effects inside it, the way a function polymorphic in `A` cannot inspect an `A` — and
it removes *accidental handling*, a handler intercepting an effect raised by a callback
that nothing in its own type mentions.

**Costs.** Lexical routing must be computed: quill wraps such calls in `Core.Tunnel` and
routes by the full effect instance at run time. Dynamic fails silently when a
higher-order boundary is forgotten, which is why the alternatives to it are a
private-effect device or an explicit suppression written at every boundary. The
comments sharpen the dynamic side's reading cost: tracking an effect means walking
"up the callstack to find where any particular handler is installed", and exceptions
are invisible twice over — not in the type, and not at the call site, where a reader
cannot tell whether a function is atomic (body of *Are algebraic effects worth their
weight?*; comments on *Why Algebraic Effects?*,
https://www.reddit.com/r/ProgrammingLanguages/comments/1kth7xm/why_algebraic_effects/).
A row answers the first half only: it says what may happen, not where it is handled.
[The private-effect / explicit-suppression framing and the Zhang & Myers, Lexa and
zero-overhead-lexical-handlers references come from quill's own
`handlers-tunnel-callback-effects` ticket, not from the corpus.]

**Maturity.** Contested — two deployed research languages on the dynamic side, a POPL
line of work on the lexical side; the corpus shows both positions held sincerely and
never joined up.

**Tried by.** Koka, OCaml 5 (dynamic); quill (lexical, implemented); Effekt (lexical by
construction — second-class functions are named in quill's ticket as the rejected
alternative for quill because everything else in quill is first-class).

**Source.** Dynamic Effects System, score 24, 22 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/gmhr8a/dynamic_effects_system/, 2020-05;
What are your thoughts on my Effect System, score 43, 4 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1ex144g/what_are_your_thoughts_on_my_effect_system/, 2024-08
("control flow will jump to the nearest `try-with` block in the call stack" — that
language's spelling).

**Bearing on `quill`.** Already has it, named: handling is lexical, not dynamic
(domain-model E5, implemented 2026-09-15), routed by effect *instance* since
`effects-followups`, with **Accidental handling** as the failure it rules out.

### Effects as implied parameters

**What it is.** Write the effect where an implicit parameter would go, Lean-style:
`divide #(error : $Error Int) (a : Int) (b : Int)`, with a handled form `$Error R` that
takes no argument at the call. Handling becomes supplying a dictionary; the effect
requirement becomes an ordinary implicit argument the signature can mention.

**Buys.** No second annotation syntax — the requirement rides the machinery implicits
already have, including the "supply it implicitly at a distance" convenience; named
effects and other dynamically bound values can share the one device.

**Costs.** "A potentially infinite number of implied parameters" is the thread's own
phrase, and the implicit supply is precisely what makes an effect easy to satisfy from
far away — accidental handling returns under a different name. Signatures stop being
readable as a list of what a function does. A commenter who writes handlers as
functions anyway counts the convenience as zero: "it would be just as easy to pass in
a Logger as a parameter ... The bonus is that we wouldn't even need to declare a
log_handler" (comment on *Why Algebraic Effects?*,
https://www.reddit.com/r/ProgrammingLanguages/comments/1kth7xm/why_algebraic_effects/).

**Maturity.** Research — a toy syntax on a page, never implemented.

**Tried by.** Nobody has shipped this; Lean ships implicit parameters, not effects in
this position.

**Source.** Algebraic Effects as dynamic/implied function parameters, score 24, 18
comments, https://www.reddit.com/r/ProgrammingLanguages/comments/1ml35ua/algebraic_effects_as_dynamicimplied_function/, 2025-08.
Adjacent: Functional Dependency Injection after years?, score 22, 75 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/nf54og/functional_dependency_injection_after_years/, 2021-05.

**Bearing on `quill`.** Has it differently, in the strongest sense quill already has it:
`~>` mints a leading implicit `EffectRow` binder in a signature
(`effect-arrow-syntax`), and because handling is lexical, that row *is* the evidence —
the operation is routed to the handler the row was bound to, without the parameter ever
being written. This is the reading quill's own tunneling ticket gives of evidence
passing.

### A row on every arrow: declared effects, inferred effects, and no silent default

**What it is.** The requirement sits on the arrow it belongs to: `A -> B` is pure,
`A ->{Log, Exc} B` is exactly these, `A ->{_}` asks for inference and is an error when
nothing solves it, `: T` is the pure result form. A body's effects are computed during
elaboration, not by a second walk.

**Buys.** Purity is visible, which is what lets the checker evaluate a call while type
checking and what makes a nominal declared in a call applicative rather than
generative. An unsolved row never quietly means "anything". A practitioner comment
supplies the experience side: writing a compiler with the error row in the signature is
"a breeze ... the compiler forcing me to either add that I can throw ... or handle the
exceptions ... is quite reassuring" (comment on *Why Algebraic Effects?*,
https://www.reddit.com/r/ProgrammingLanguages/comments/1kth7xm/why_algebraic_effects/).

**Costs.** Every effectful signature carries a row, and each annotation is a place to
be wrong: a body performing beyond its row, or a `: T` over an effectful body, is a new
error naming the effects. The one poster who doubts tracking says it plainly — in
years of writing an F# compiler he "has never once" been unable to tell what a call
do. The comments answer with the failure he is not imagining: effects are "too
difficult to track if they're not mentioned in the type", and the reply that Java's
checked exceptions failed on execution, not on the idea (comments on *Why Algebraic
Effects?*, https://www.reddit.com/r/ProgrammingLanguages/comments/1kth7xm/why_algebraic_effects/).
What the comments never supply is a measurement of either side.

**Maturity.** Research as a language feature (Koka annotates a function with its
effects; Unreal's
scripting language reportedly has one); implemented in `quill`.

**Tried by.** Koka, Effekt, quill; Unreal's UnrealScript 6 is asserted by a one-line link
post and nothing in it can be checked.

**Source.** What is the benefit of tracking side effects?, score 44, 25 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/uxh2ze/what_is_the_benefit_of_tracking_side_effects/, 2022-05;
Do you need a type system to have an effect system?, score 19, 14 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1b2y2pf/do_you_need_a_type_system_to_have_an_effect_system/, 2024-02.

**Bearing on `quill`.** Already has it (named): bare arrow is pure; rows sit on arrows;
`->{_}` unsolved at an entry is `UnsolvedEffectRow`; a body under `: T` is
`EffectsInPureResult`. Effect rows are dedicated syntax in their positions pending
[general-set-literals](../wayfinder/tickets/general-set-literals.md), which is **open**.

### Row explosion, and the two remedies: open tails and effect polymorphism

**What it is.** Fine-grained rows accumulate: every library effect another signature
has to admit. The remedies are an open tail (`A ->{IO | r} B`, so a caller's unknown
effects ride through) and a sugar that mints a fresh row in parameter position and
collects it in result position, so a higher-order function does not enumerate its
callback's effects. quill additionally lets a row carry a *set* of tails, so a result
unites several callbacks' rows.

**Buys.** `f : (A ~> B) -> (C ~> D) ~> E` threads two independent callbacks' effects
without naming them; a result that calls a callback gets exactly what the callback
performs.

**Costs.** Rows become variables that must be solved or written, and only rank 1: an
alias like `Callback = Unit ~> I64` mints at the definition that takes it, so the
caller chooses, while a written binder stays rank 2. A tail-only row that nothing
solves is an error, not a default. And the underlying complaint stands — the same
thread argues rows multiply faster than any language or library has a good way to
manage, and its fourth point is an implementation cost the type does not show:
tracking effects forces "either track effects [with] another kind of polymorphism or
disallow returning and storing functions". The comments name the growth from the panic
side — if every division, every index and every allocation can panic, `can Panic`
pollution is everywhere — and the remedy proposed back is the one this entry lacks: an
*assumed* set of effects a programmer can turn off, alongside effect aliases (comments
on *Why Algebraic Effects?*,
https://www.reddit.com/r/ProgrammingLanguages/comments/1kth7xm/why_algebraic_effects/).

**Maturity.** Contested — the mechanism is built, the value of the discipline is
disputed in the corpus's highest-scoring effects thread.

**Tried by.** quill (multi-tail rows, `~>`), Koka (`exn int`-style annotations on
functions); no production language
in the corpus.

**Source.** Are algebraic effects worth their weight?, score 75, 41 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1mh3ba8/are_algebraic_effects_worth_their_weight/, 2025-08
(the "amount of effects seems to increase rather quickly" argument);
Row Polymorphism without the Jargon, score 38, 35 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/g2lm11/row_polymorphism_without_the_jargon/, 2020-04;
Adding row polymorphism to Damas-Hindley-Milner, score 48, 5 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1gab4p6/adding_row_polymorphism_to_damashindleymilner/, 2024-10.

**Bearing on `quill`.** Already has it: `->{IO | r}`, `~>` minting and collecting,
multi-tail rows as a set of row variables. Two open edges: the rank-1 shortcut is
marked `ponytail:` in `effect-arrow-syntax` (a written binder is the rank-2 escape),
and quill "intentionally avoids full lacks-constraint machinery" (algebraic-effects
Phase 6 note) — so duplicate-free rows are normalised, not constrained.

### Rows instead of monads and transformers

**What it is.** Put the requirement on the arrow rather than on the return type: no
`M a`, no bind, no stack of transformers whose order decides semantics.

**Buys.** Ordinary function types stay ordinary; combining is not order-dependent —
"combining monads is order-dependent (some monads don't commute)" is the complaint the
other thread is trying to solve with an `||` operator. Callers see what callees perform
in the signature instead of reading a monad stack.

**Costs.** A monad's type says *which* context a computation is in and sequences it;
an effect row says only what may happen, and sequencing stays the job of ordinary
evaluation. Code that wanted a particular composition has to build it back (that is
what `||`, polysemy-style lifting and `rethrows`/`reasync` are for). The retrieved
comments hold both positions in one thread: "Algebraic effects have an advantage ...
they compose. Monads don't compose. Monad transformers do, but then you have to do a
lot of work" against "my issue with this and effects in general is that they are some
side channel ... while they could have just been values all along" (comments on
*What's your opinion on exceptions?*,
https://www.reddit.com/r/ProgrammingLanguages/comments/o1ye66/whats_your_opinion_on_exceptions/).

**Maturity.** Contested — the corpus has an entire thread asking whether a keyword on
the function would do, and another thread inventing algebra to get monad composition
back, which is the disagreement in two posts.

**Tried by.** quill (rows); Haskell (monads); Swift and Kotlin for the "mark the
function" middle (named in the worth-their-weight thread).

**Source.** Alternative to monads for enforcing purity?, score 39, 45 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/lozq0h/alternative_to_monads_for_enforcing_purity/, 2021-02;
Combining monads/effects is actually easy?, score 6, 8 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1weov94/combining_monadseffects_is_actually_easy/, 2026-09.

**Bearing on `quill`.** Decided in `quill`'s favour by the design, not argued: `std/`
contains no monad and declares no transformer; the effect requirement is
`A ->{E} B`, `perform`, and `match` with effect branches. Whether a `do`-like
sequencing form is ever wanted is untouched by any ticket.

### Which way an effect requirement infects: upward or downward

**What it is.** `async` is **upwardly infectious**: mark a function and every *caller*
must be marked too. `pure` is **downwardly infectious**: mark a function and every
*callee* must be pure. The rule of thumb the thread argues for: you can always call a
pure function; you cannot always call an `async` one — you eventually reach an
interface you do not control.

**Buys.** The direction is a predictor of where you will be blocked: upward
infection hits you at boundaries you must satisfy (a third-party interface, a trait
signature you did not write); downward infection never blocks a caller. The thread's
own edit records that a design can flip its direction.

**Costs.** Downward infection loads library authors with proving and declaring purity;
upward infection loads everyone above a single effectful leaf. Rust's `&mut` and
Java's `throws Exception` are named as other upwardly infectious features with the
same friction. The fetched tree turns the rule of thumb over: in a language pure by
default, purity "really is upwardly infectious, just like `async`", the two directions
are convertible, and what separates them is that upward infection is "the more useful
or powerful version" (top comment on *Thoughts on infectious systems*,
https://www.reddit.com/r/ProgrammingLanguages/comments/vofiyv/thoughts_on_infectious_systems_asyncawait_and_pure/).
Two more comments report the friction directly — async "slams into an immovable
object: a third-party trait method that *doesn't* have async" — and the
counter-position that async is not infectious at all, because "you'll just need to
block on it".

**Maturity.** Contested — argued from examples, no measurement in the corpus.

**Tried by.** D (`pure`), C#/Rust/JavaScript (`async`), Java (checked exceptions).

**Source.** Thoughts on infectious systems: async/await and pure, score 117, 70
comments, https://www.reddit.com/r/ProgrammingLanguages/comments/vofiyv/thoughts_on_infectious_systems_asyncawait_and_pure/, 2022-06.

**Bearing on `quill`.** quill's rows infect *both* ways, and the upward wall is written
down: a body's effects must fit its declared row (`EffectsInPureResult`), and **a trait
impl cannot widen its row past its trait's signature** (noted in
`effects-followups-tunneling` as the reason a test cannot expose a remaining escape
shape). That is the "interface you do not control" case, and it is decided, not
argued.

### Colourless async: put the colour on the call, not the definition

**What it is.** Do not mark the definition; decide at the call site. Zig's version
turns `async`/`await` into a no-op when the callee never suspends — a comment reports
the mechanism as the compiler monomorphising each function async-or-not depending on
what its caller requires, so only `await` remains; the other thread proposes splitting
`go` from a command into an expression that builds an unstarted promise; a third moves
the colour to the call site entirely.

**Buys.** A library does not have to infect its public API with a colour it may not
keep; ordinary functions stay callable from either kind of caller; no "async tax"
you do not own.

**Costs.** Whether a call suspends stops being visible in the type — the thread's own
worry is function pointers, where the compiler does not know whether the callee
suspends, and it cites an article whose example is a segfault from choosing a
function at run time. Large teams with versioned internal libraries lose a
compile-time guarantee they may have been relying on. The comments add the principled
version of the same objection: colour cannot be inferred in general — "For any
sufficiently interesting programming language, this is impossible", citing Rice's
theorem [the slice cuts the sentence off there] — so colourlessness by inference can
only ever be a local trick (comment on the Zig thread,
https://www.reddit.com/r/ProgrammingLanguages/comments/19eewkb/what_are_the_downsides_of_zigs_colorless_approach/).

**Maturity.** Contested — one side's evidence was Zig's shipped colourless experiment,
which a comment on that same thread records as "removed entirely, and has not been put
back in yet" (also general knowledge, not only from the corpus), so the shipped column
is thinner than it looked; the other side has the majority of production languages and
a well-known blog post in its favour.

**Tried by.** Zig (tried colourless async, then removed it); TinyGo, which inserts the
`async`/`await` keywords when lowering to LLVM's coloured async (comment on the Zig
thread) — the same idea run in the opposite direction; Onion (a hobby language that
moves colour to the call site — a data point about interest, not viability); Go is held
up as the good example in the third thread.

**Source.** What are the downsides of Zig's "colorless" approach to async?, score 58,
45 comments, https://www.reddit.com/r/ProgrammingLanguages/comments/19eewkb/what_are_the_downsides_of_zigs_colorless_approach/, 2024-01;
Sync, Async, and Colorless Functions?, score 30, 36 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/15n31u1/sync_async_and_colorless_functions/, 2023-08;
Onion, score 49, 29 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1mkruyr/onion_a_language_design_experiment_in/, 2025-08.

**Bearing on `quill`.** Genuinely new — `quill` has no async mechanism at all and no
concurrency item in the map. The type-level half already exists: `~>` means "the
caller decides what this performs", which is colour determined by instantiation
instead of hidden. What does not exist is anything that would make a row *suspend*.

### Everything concurrent, no `await`

**What it is.** Par: evaluation is automatically concurrent, linear types and duality
channel the communication, and there is no `await` in ordinary code because there is
no synchronous reading to suspend. The flagship example is a concurrent downloader
written without concurrency syntax.

**Buys.** The colour problem disappears rather than being solved: no function is
asynchronous, so no function is un-callable. The second thread shows the downstream
effect — error handling had to be redesigned because manual `case` on `Result` plus
automatic concurrency "leads to losing passion for programming".

**Costs.** Linear types, channel discipline and totality checking come as a package;
and the thing the poster is proudest of is exactly what a reader must relearn. It is a
different programming paradigm by its author's own description. The comments put a
reader against it: without explicit `await` there is "accidental parallelism or lack
thereof ... race conditions or other weird issues", because the control flow a reader
needs is no longer written down. The author answers each objection — dependencies do
impose serialization, deadlocks and races are ruled out structurally — but concedes
the performance half twice: the current runtime is "not the fastest", and on whether
the parallel run helps, "we really have to see", with manual intervention left open
(comments on *What if everything was "Async", but nothing needed "Await"?*,
https://www.reddit.com/r/ProgrammingLanguages/comments/1ozlvuw/what_if_everything_was_async_but_nothing_needed/).

**Maturity.** Contested — an experimental language implements it and its author meets
every objection in the thread, but no measurement backs the speed claim and the
reader-side objection (hidden control flow) is unrebutted. Both positions are argued;
neither is settled by the other.

**Tried by.** Par (hobby/research, not production).

**Source.** What if everything was "Async", but nothing needed "Await"? -- Automatic
Concurrency in Par, score 149, 83 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1ozlvuw/what_if_everything_was_async_but_nothing_needed/, 2025-11;
Error handling with linear types and automatic concurrency? Par's new syntax sugar,
score 39, 14 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1n5osyh/error_handling_with_linear_types_and_automatic/, 2025-09.

**Bearing on `quill`.** Genuinely new to the project; the map has no concurrency item.
The nearest thing quill brings is that any future such feature would have to show in a
row — an unhandled effect at the entry is an error today.

### Two effects instead of one: spawning is not suspending

**What it is.** Split the usual `async` into two effect families: one meaning "this
control flow may suspend" and one meaning "this code touches independent task or
executor state". Starting work (`begin_download`) performs the second but not the
first, so the starter does not become a function that suspends; `finish_download`
performs both.

**Buys.** The caller of a function that only *starts* work is not forced to be
suspending — the upward infection of `async` stops at the spawn site. The row also says
what a function really does: the poster's own example notes that without the second
effect the starter would look pure.

**Costs.** The thread's language has a *closed* set of five effects (`async`, `throws`,
`io`, `alloc`, `task`); a fixed taxonomy means every new mechanism is a new primitive.
Two effects about the same activity invite confused signatures.

**Maturity.** Speculative — one post in a hobby language; the comment tree was not
retrieved.

**Tried by.** NXD-adjacent design in a single unproven language; nobody else.

**Source.** Why spawning work isn't `async` in my language, score 16, 10 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1vgz5nu/why_spawning_work_isnt_async_in_my_language/, 2026-08.

**Bearing on `quill`.** Genuinely new, and it would land exactly where quill's design wants
it: two ordinary effect families on rows, the way `Alloc(h)` / `Read(h)` / `Write(h)`
already split one activity three ways because a read-only caller earns a weaker row.
`quill` has no task or executor notion to hang them on yet.

### Green threads as the substrate instead of effects

**What it is.** Give every task a real (stackful) scheduled thread and let any function
block or suspend without being marked — colourlessness by construction — rather than
compiling each suspending function into a state machine. The async-models thread maps
the landscape: C#'s async, Rust's polling, Ruby's green threads over coroutines,
JavaScript's continuation-passing, Erlang and Go's green threads sleeping on channels.

**Buys.** No annotation discipline at all for suspension, and no separate `async`
typing rule; blocking inside a library is safe rather than forbidden.

**Costs.** A runtime with its own stacks is mandatory — the thread's own summary of
the Go/Erlang comparison is "one requires a runtime, the other doesn't, that's it" —
and a stack per task is memory the compiler cannot see. The comments price both sides:
every Erlang process owns its heap, GC'd independently, and a message send copies into
the recipient's mailbox; a userspace coroutine can be as small as 120 bytes (libaco);
Go's preemption checkpoints are injected by the compiler and do not cover FFI calls
(comments on *What are the downsides of Zig's "colorless" approach to async?*,
https://www.reddit.com/r/ProgrammingLanguages/comments/19eewkb/what_are_the_downsides_of_zigs_colorless_approach/,
and on the async-models thread). The thread explicitly reports **no local maximum**:
nobody in it claims a best answer.

**Maturity.** Shipped — Go, Erlang and Ruby all run this way in production, and the
comments add Java's virtual threads as the same idea in a language that had stackful
threads all along.

**Tried by.** Go, Erlang, Ruby, Java (virtual threads, named in a comment) — with C#'s
async as the contrast the thread opens from; the thread's survey is the evidence.

**Source.** What are you doing about async programming models? Best? Worst? Strengths?
Weaknesses?, score 56, 51 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/zfa7h9/what_are_you_doing_about_async_programming_models/, 2022-12;
Designing async semantics for a new language. What would you do differently?, score 14,
15 comments, https://www.reddit.com/r/Compilers/comments/1wtu8h4/designing_async_semantics_for_a_new_language_what/, 2026-09.

**Bearing on `quill`.** Genuinely new, and in direct tension with a stated rule: a term
needing a sub-evaluation gets a `Kont` frame, never a native one. Stackful green
threads reintroduce a native stack per object-level call. The companion restriction on
continuations — never captured across an extern frame — "has nothing to attach to yet:
there is no extern mechanism" (E7); a green-thread runtime is one thing that would
attach it.

### Exceptions vs return codes, with a measurement attached

**What it is.** Failure as non-local control (throw, unwind to a handler) versus
failure as a second value (`Result`, `option`, an error code) checked at each call. The
low-level thread quotes Midori's measurement: return codes burn a register or stack
slot per call and smear a branch across every call site ("peanut butter"), and on
their benchmarks exceptions came out **7% smaller and 4% faster** geomean.

**Buys.** Ordinary call sites stay ordinary; the Go complaint in the first thread —
"for every line of useful code there's 3 lines of `if err != nil`" — is the whole
argument, and the Midori numbers are the only quantified performance claim in this
entire axis — contested in the next paragraph. A comment adds the one case no value
can cover: an asynchronous exception
is an interruption, "there is no place when you could even place such a value in your
code" (comment on *What's your opinion on exceptions?*,
https://www.reddit.com/r/ProgrammingLanguages/comments/o1ye66/whats_your_opinion_on_exceptions/).

**Costs.** Control flow becomes hard to track, memory-safety languages find unwinding
hard to make safe, people use exceptions for non-exceptional things and as a deep
`return`, and "it's hard to know whether a function could throw" (Java's checked
exceptions were the attempted fix and still are not). The author concedes that last
one. The measurement itself is now contested from inside the thread it was quoted
from: a practitioner who has "deeply optimize[d]" C++ both ways reports "never a clear
conclusion that one was faster than the other", reads Midori's numbers as a property
of that C# compiler and Windows SEH, and says they "should be taken with a grain of
salt" (comment on *Low-level, high-perf languages with proper exception-based error
handling?*,
https://www.reddit.com/r/ProgrammingLanguages/comments/1asxlo1/lowlevel_highperf_languages_with_proper/).
Two further comments explain why Midori could win anyway — explicit `throws` meant
most functions never threw and could be inlined, and lowering exceptions to error
codes makes *every* C# call branch — while the return-code side answers that a tagged
union is "essentially free" with non-nullable references, that error branches are
highly predictable, and that unwinding executes Turing-complete DWARF instructions out
of cold side tables. Nobody in the tree re-measures anything.

**Maturity.** Contested — the corpus's two biggest threads in this axis are on opposite
sides and neither is settled by the other, and the one quantified number in the
argument has been disputed from inside its own thread without being re-measured.

**Tried by.** Java, C#, Python, Go and Rust (on the opposite side), Odin, Zig, Jai, Beef
named as uniformly return-code.

**Source.** What's your opinion on exceptions?, score 117, 103 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/o1ye66/whats_your_opinion_on_exceptions/, 2021-06;
Low-level, high-perf languages with proper exception-based error handling?, score 29,
91 comments, https://www.reddit.com/r/ProgrammingLanguages/comments/1asxlo1/lowlevel_highperf_languages_with_proper/, 2024-02;
Exceptions vs multiple return values, score 11, 12 comments,
https://www.reddit.com/r/Compilers/comments/1fz7cbs/exceptions_vs_multiple_return_values/, 2024-10;
I don't think error handling is a solved problem in language design, score 110, 124
comments, https://www.reddit.com/r/ProgrammingLanguages/comments/1je8job/i_dont_think_error_handling_is_a_solved_problem/, 2025-03
(link post — no body; its tree was fetched in the second pass, so 26 of its comments
inform this entry).

**Bearing on `quill`.** Has it differently: non-local failure is an ordinary effect
family handled by `match`'s effect branches, and `UnhandledEffects` at the entry keeps
it honest; `option` and friends are ordinary values in `std`. The unsettled piece is
`panic`: it is a **Primitive** whose fourth part — a failure behaviour — is exactly
"what division by zero needed and what panic does not fit" (CONTEXT.md), and
`panic-with-unknown-message-fails-checking` settled only *when* it reduces.

### Typed errors as an effect family rather than a `Result` in every signature

**What it is.** A failure is an operation of an effect family; the row says a function
may fail; a boundary handles it. Contrast threading `Result(A, E)` through every
return and every `?`, or `case`-ing on every intermediate.

**Buys.** Error subtyping better than Rust's monad-based `Result` (the stated goal of
one thread); no plumbing in the happy path; the handler can supply a default and turn
the failure back into a value — that is what the `try`-with example in the other
thread (its spelling, not `quill`'s) does with `resume Option::Some(1)`. The comments
supply both prior art and the type-theoretic shape: "academia is cooking up the next
crazy thing: effect handlers", with Koka, Effekt, Links and Unison named as the
type-and-effect languages where "throwing an exception is one such effect" (comment on
*Is there a garbage collected, statically typed language, that has null safety, and
doesn't use exceptions?*,
https://www.reddit.com/r/ProgrammingLanguages/comments/xpxres/is_there_a_garbage_collected_statically_typed/);
Flix is named separately. A third comment gives the calling convention in place of the
dynamic rule: in linear logic's `⅋` a function is called with two continuations, success
and failure, and the callee picks which to invoke — "different to the exception model
of, e.g. Java, where the exception handler is dynamically scoped instead of lexically
passed as an argument" (comment on the low-level-exceptions thread, permalink above).

**Costs.** Loses the forcing function of a `Result` in the type: the return-code side's
whole case is that a value you must look at is easier to audit than a raise you can
ignore. And the habit of handling at the nearest enclosing handler is the failure mode
quill's tunneling rule exists to prevent. The strongest comment against is that a second
channel costs double: "when writing a generic function, a programmer must already
handle generic values ... Adding generic exceptions to the mix *doesn't* remove this
need, it just requires handling generic exceptions *on top* ... double the pain"
(comment on *What's your opinion on exceptions?*, permalink above).

**Maturity.** Contested — this is the axis's central unresolved argument.

**Tried by.** Koka, Effekt, quill (effect side); Rust, Go, Haskell (value side).

**Source.** Single-continuation algebraic effects in an imperative language?, score 18,
10 comments, https://www.reddit.com/r/ProgrammingLanguages/comments/14iz6jf/singlecontinuation_algebraic_effects_in_an/, 2023-06;
Error handling with linear types and automatic concurrency?, score 39, 14 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1n5osyh/error_handling_with_linear_types_and_automatic/, 2025-09.

**Bearing on `quill`.** Already has it: an error family declared with
`effect Name(params) = sig … end`, raised with `perform`, handled in a `match`, with
residual effects preserved. What `std/` does **not** have yet: it declares no effect
family at all — there is no standard error effect, so which shape the library picks is
untouched by any ticket.

### Divergence as an effect

**What it is.** Treat non-termination as one of the things a computation may do: Koka
makes divergence an algebraic effect; Rust treats non-termination as a side effect so
an infinite loop is never optimised away; C++ says a side-effect-free infinite loop is
undefined behaviour and may be removed; Haskell's laziness makes removing it always
fine.

**Buys.** The optimiser gets a licence it can point at, and a language can demand
totality in a row without a separate judgement.

**Costs.** The poster's three wishes — write recursion without proving termination, no
laziness, don't think about termination when writing optimisation passes — may be
mutually exclusive; the thread says so itself. Every row in the program then carries a
bookkeeping effect nobody wanted to write. The comments add the formal frame and a new
cost: whether a compiler may drop an infinite loop is a choice of *semantic
preservation* statement — the CompCert paper's Section 2.1 lets compiled code do
"less" than the source under backward simulation, division by zero included — so the
question is which correctness claim the compiler adopts, not philosophy; and refusing
to drop non-terminating loops forces "now you need to prove termination of loops that
you want to remove using dead code elimination" (comments on
https://www.reddit.com/r/ProgrammingLanguages/comments/tdlff4/infinite_loops_a_sideeffect_or_an_implementation/).
On the other side a Haskell commenter states quill's position as a concession to cost:
the cost of detecting divergence is "great, so in the name of practicality purely
functional languages has to consider it to not be an effect", and another notes that
throwing is only impure "if you consider nontermination an effect" — the two questions
stand or fall together.

**Maturity.** Contested — four major languages, four different answers, all in the
thread.

**Tried by.** Koka (effect), Rust (side effect), C++ (undefined), Haskell (irrelevant).

**Source.** Infinite loops: a side-effect, or an implementation detail?, score 78, 45
comments, https://www.reddit.com/r/ProgrammingLanguages/comments/tdlff4/infinite_loops_a_sideeffect_or_an_implementation/, 2022-03.

**Bearing on `quill`.** Rejected, with the reason recorded: divergence is not an effect,
termination is never checked, and the checker's **evaluation budget** measures work
spent while type checking — calls, conversions, macro applications — and is a compile
error naming the call, not a judgement about whether a term halts. Running a program
spends none.

### Mutable state as a heap-parameterised effect, discharged when the heap cannot escape

**What it is.** Allocation, read and write are three separate effects over a named
heap; every reference is branded with its heap; a definition's heap effects for a heap
that does not occur in its type are dropped at generalisation. That is `runST`'s
soundness condition, met by inference instead of a wrapper.

**Buys.** Internal mutation disappears from the signature — a `make` whose refs stay
inside infers a pure arrow. A library that quietly adds a cache cannot break clients by
making its nominals generative. Read-only code earns a weaker row than code that
writes.

**Costs.** Heap variables appear in rows and in `Ref`'s arity behind the user-visible
spelling; an escaping reference carries its heap into the result type and blocks
discharge; until discharged, allocation shows in types
(`counter : Unit -> Ref(I64) can Alloc(h)`). The thread that derived this from the ST
monad admits he has "done zero research into effect systems" — the idea is easy to
rediscover, which is not the same as easy to get right. The comments disagree about
whether allocation belongs in effects at all: memory allocation "cannot be lumped
together with actual program effects such as console output or HTTP requests, otherwise
there would be no pure functions in practice at all", and whether "the stack [should
be] purer than the heap just because it is automatic" — against the design quill chose, spelled out by
another commenter — "Allocating an unrestricted point must be an IO side effect since
it introduces shared mutable state. Allocating a linear pointer would be pure", with
*Lightweight Linear Types* named as the theory (comments on *The purely functional C?
(or other simple equivalent)*,
https://www.reddit.com/r/ProgrammingLanguages/comments/1dmqtxj/the_purely_functional_c_or_other_simple_equivalent/).
That is a disagreement with quill's decided shape, not with its engineering.

**Maturity.** Research — Haskell's `ST` and Koka's heap effects are the precedents;
implemented in `quill`.

**Tried by.** Haskell (`ST`, and the thread's rank-N encoding), Koka, quill.

**Source.** Deriving an Effect System for the ST monad, score 30, 17 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/pweawl/deriving_an_effect_system_for_the_st_monad/, 2021-09.

**Bearing on `quill`.** Already has it, named: **Heap**, **Reference**, **Mutation
effect**, **Discharge** — `Alloc(h)` / `Read(h)` / `Write(h)` with *no* merged `Mut`
(rejected, so read-only code earns the weaker row), local heaps discharged at
generalisation, and the entry's runtime handler discharging the rest. Top-level refs
are allowed; the heap is never written by the user.

### Should a `for` loop dispose of the iterator it made?

**What it is.** The usual shape of a `for` loop makes an iterator and steps it. If the
loop owns that iterator it should dispose it (C#); if not, the iterator is only
reclaimed at collection time and nobody but the loop could ever close it (Python). The
hard case is an iterable that returns *itself* — a file — where disposal at the end of
the first loop breaks resuming the same file in a second loop.

**Buys.** Disposing closes the hole the poster could find no other solution to: the
"iterator not properly disposed until garbage-collected" case, where the loop's own
author has no handle on it.

**Costs.** Conflates iteration with lifetime — "it seems strange that passing a file
handle to a `for` loop would close the file" — and the resumable-iterator idiom breaks.

**Maturity.** Contested — the thread's own survey is two mainstream languages on
opposite sides, and its conclusion ("perhaps the correct approach is for loops to
dispose") is explicitly hedged.

**Tried by.** Python (no), C# (yes); both named by the poster.

**Source.** Should for loops dispose of their iterators?, score 13, 57 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1rg5nwt/should_for_loops_dispose_of_their_iterators/, 2026-02.

**Bearing on `quill`.** Genuinely new: `quill` has no resource or disposal mechanism and no
linearity, so nothing owns anything. The vocabulary that would carry it already
exists — **Discharge** (drop what cannot escape) and **Handler scope** (what may leave)
are the same shape of rule applied to heaps and effects.

### Cleanup under a continuation you can leave and re-enter

**What it is.** With undelimited continuations a delimited stretch of computation can
be exited and re-entered, so cleanup must run on *every* entry and exit: Scheme's
`dynamic-wind` re-runs the
allocation thunk on re-entry; Haskell's answer to asynchronous exceptions from another
thread is the `bracket` combinator; C++ wraps every allocation in an RAII object because
an exception can fire between allocate and deallocate.

**Buys.** Resource safety holds across non-local exit, including exit nobody wrote at
the site of the resource.

**Costs.** Every allocation gets wrapped; re-entry policy is per resource — the thread
notes a file handle probably should *not* be closed and reopened, while a stdout mutex
should — and Oleg's objection is that `dynamic-wind` does not compose with
abstractions built on continuations (a lock released before the critical section
ends).

**Maturity.** Shipped — `bracket`, `dynamic-wind` and RAII are all in production
languages.

**Tried by.** Haskell, Scheme, C++/Rust.

**Source.** Async exceptions and dynamic-wind: language design, score 14, 9 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/nsyb76/async_exceptions_and_dynamicwind_language_design/, 2021-06.

**Bearing on `quill`.** Largely avoided by construction — one-shot continuations mean no
re-entry loop, and `dynamic-wind`'s re-run-on-each-entry problem does not arise. What
is *not* settled: a stored continuation may be resumed after its branch returns
(schedulers, async) and "resuming re-enters the handler scope", so anything
resource-like held across that resume needs an owner quill does not have yet. Pair with
the `for`-loop entry; no ticket covers either.

### Capability passing instead of (or beside) an effect row

**What it is.** The permission to do something is a value attached to a reference, not
a set on the arrow: generalise Rust's `mut` into user-defined capabilities — `File[Read]`
granted by `open` with `=> self[+Read]`, withdrawn by `close` with `=> self[-Read]` —
so "you cannot read before you opened, or after you closed" is checked per value.

**Buys.** Per-object state machines (opened → closed, machine running → stopped) are
checked where the object is, and the discipline travels with the value rather than
with the function's callers.

**Costs.** Every signature grows an effect clause (`=> p[+A]`, `=> p[-A]`), and a
capability is per-value: "this whole computation may also log" still needs a row on the
arrow. Two systems means two things to explain. A comment names the sharper limit: a
capability cannot say where it may be *used* — "you can't require a function like
spawn_thread to only accept pure functions when it can accept a closure which captures
a capability object" — so the per-value system cannot express the per-callsite rule a
row expresses (comment on *Why Algebraic Effects?*,
https://www.reddit.com/r/ProgrammingLanguages/comments/1kth7xm/why_algebraic_effects/).

**Maturity.** Research — argued in the corpus, plus one hobby language shipping
"explicit effects / capabilities" as a bullet point.

**Tried by.** Rust (`mut` only), Capsicum-style systems [general knowledge, not from
corpus], OriLang (hobby, unverified), Firefly — named in a comment with code: object
capabilities enforce purity there, `loadFile(fs: FileSystem)` can reach nothing but the
file system, and "an object capability can be captured in a closure. Thus it isn't
'infectious' / doesn't 'color' your functions ... on its own it can't be used for
certain effects, such as async/await" (comment on *Alternative to monads for enforcing
purity?*, https://www.reddit.com/r/ProgrammingLanguages/comments/lozq0h/alternative_to_monads_for_enforcing_purity/).


**Source.** Capability systems, score 23, 13 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/kmoygl/capability_systems/, 2020-12;
Working on a new programming language with mandatory tests and explicit effects, score
20, 19 comments,
https://www.reddit.com/r/Compilers/comments/1rkov4c/working_on_a_new_programming_language_with/, 2026-03.

**Bearing on `quill`.** Has it differently, and the word is already load-bearing: quill's
tunneling decision says handlers become **lexically scoped capabilities** — an
operation is routed to the handler its row was bound to. The project has used the
word for something else too: the expander's handle is a capability, not a context
(`expander-handle-is-a-capability-not-a-context`, closed). Rows stay the mechanism;
nothing in the map proposes per-value capabilities.

### Effects as message passing (the process-calculus reading)

**What it is.** Fix one process in a closed process-calculus program and call its
interactions with the rest effects, and the rest's interactions with it coeffects. An
assignable is a `get`/`put` process; exceptions are the program sending an unwinding
message to the continuation; nondeterminism is a coinductively generated choice tree;
fork-join is a master sending return continuations to workers.

**Buys.** One picture covering effects, coeffects and concurrency, with Harper's
Modernized/Concurrent Algol as an existing formalisation the poster is leaning on.

**Costs.** The poster concedes the correspondence proves nothing — "you can model all
computations with lambda calculus or Turing Machines" — and no type system, inference
procedure or cost model falls out of it. It is intuition, not a mechanism.

**Maturity.** Speculative — argued in a single post, no implementation claimed.

**Tried by.** Nobody; no language is claimed to be built on it.

**Source.** Are (co)effects isomorphic to message passing concurrent systems?, score 15,
9 comments, https://www.reddit.com/r/ProgrammingLanguages/comments/1t08my3/are_coeffects_isomorphic_to_message_passing/, 2026-04.

**Bearing on `quill`.** Genuinely new to the project, and it is the only entry here that
would matter to a *future* concurrency story: the map has no concurrency item, so
there is nothing for it to contradict. `quill`'s current routing (`Core.Tunnel`,
`effect_request.skips`) is a direct mechanism with no message passing in it.

### The effect row as a supply-chain audit point

**What it is.** A dependency can only do what its public signatures' rows name: if
formatting a string has no file effect in its row, a dependency that starts touching
files has changed a signature somebody can read. The thread's framing is the
post-npm-attack question: "Why does formatting a string need file access? Something
fishy must be going on."

**Buys.** A class of supply-chain attack becomes visible at the API instead of in
review, and the escape hatch — whatever the language's `unsafe`-like door is — becomes
the single auditable place.

**Costs.** The poster's own counter: a widely used enforced system breeds an escape
hatch, and threat actors move there. Rows also only cover what a language routes
through effects, so a primitive that bypasses the row defeats the whole claim.

**Maturity.** Speculative — a question asked, with no research cited and no
implementation proposed.

**Tried by.** Nobody has shipped this use; `quill` is closer than most by accident.

**Source.** Effect systems as help with supply chain security, score 36, 42 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1npzw1c/effect_systems_as_help_with_supply_chain_security/, 2025-09.

**Bearing on `quill`.** Genuinely new use of something quill already has: every arrow
carries its row, an unhandled effect at a compilation unit's entry is an elaboration
error, and `pub` is a Binding's own notion — so the audit point exists before
anybody asked for it. Untested and unticketed; the nearest hazard is `panic`, the one
primitive whose failure behaviour is unsettled.

## Threads worth reading in full

- **Thoughts on infectious systems** (117, 70) — the upward/downward distinction that
  predicts where an effect annotation will hurt; the single most reusable frame in this
  axis.
- **What are the downsides of Zig's "colorless" approach to async?** (58, 45) — the
  best list of concrete objections to hiding suspension from the type.
- **What's your opinion on exceptions?** (117, 103) — every argument for non-local
  failure, stated by someone who has felt them, against Go's return codes.
- **Are algebraic effects worth their weight?** (75, 41) — the most specific critique of
  effect tracking in the corpus: row growth, reading comprehension, resumption power.
  Its comment tree was never fetched; read it in the browser.
- **Why Algebraic Effects?** (86, 58) — a bodyless link post whose *fetched comments*
  carry the whole pro/con argument: tracking versus inference, `can Panic` pollution,
  capabilities versus effects, and the multi-shot comprehension objection.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1kth7xm/why_algebraic_effects/
- **Single-continuation algebraic effects in an imperative language?** (18, 10) — the
  one-shot-vs-multi-shot argument with actual code showing mutable state re-running.
- **Koka's multi-prompt control monad?** (24, 7) — where evidence passing, prompts and
  "how does this compile without a stack" meet, with the poster stuck mid-understanding.
- **Should for loops dispose of their iterators?** (13, 57) — the sharpest resource
  question here, and the corpus's only thread in this axis with a full argument on both
  sides in the original post.
- **What if everything was "Async", but nothing needed "Await"?** (149, 83) — the
  highest-scoring thread in the axis; automatic concurrency as a paradigm, not a
  feature.
- **LLVM for Functional Languages: Supporting continuations via custom calling
  conventions** (92, 17) — what continuations actually cost: CPS calling conventions
  versus segmented stacks, with the zero-cost claim examined.
- **Async exceptions and dynamic-wind** (14, 9) — the resource-and-continuation
  interaction, including Oleg's composition objection, written by someone who read the
  sources.

## Gaps and disagreements

**Comment coverage is thin, and two trees are missing entirely.** Comment trees were
fetched for the 20 richest threads of this axis: **515 retrieved comments**, but each
fetch returns only about 30 top-level comments regardless of `limit=100`, with 37 left
in `more` placeholders across trees that hold 1221 comments in total — so every deep
reply is missing, and so are all trees outside the 20. The two threads whose titles
promise the "are effects worth it" arguments, *Are algebraic effects worth their
weight?* (75, 41) and *What are the issues with algebraic effects?* (69, 50), are in
the fetch queue but were never fetched; with no network requests allowed here, their
comments stay unread. What those arguments look like was recovered from *Why Algebraic
Effects?* and the fetched exceptions, error-handling, purity and async trees instead —
an approximation, not those two threads.

**The five highest-signal threads on "are effect systems worth it" are link posts with
no body**: *I don't think error handling is a solved problem* (110, 124), *Why I'm
excited about effect systems* (78, 12), *Effect Systems vs Print Debugging* (54, 23),
*On the purported benefits of effect systems* (45, 20), and *Effekt* (86, 24). Their
titles and scores are evidence of interest and of a live disagreement; nothing in
their content can be checked. The first now has 26 retrieved comments, so the
exception-vs-`Result` verdict rests partly on comment material; the other four still
contribute title and score only. A sixth link post, *Why Algebraic Effects?* (86, 58),
is the exception that proves the point: with its comments fetched, a post that says
nothing at all turns into the richest single source in this document.

**Words the corpus never uses — and the comments did not supply them either.** There is
not one occurrence of "deep handler", "shallow handler", "multi-shot" or "effect row"
in the 2538 threads, and still zero across all 515 retrieved comments (verified
again after the comment pass; the plural spellings are absent too). quill's
deep-vs-shallow decision and its row spelling cannot be corroborated or contradicted by
this corpus at all — they are quill's own, and the "shallow handlers are deferred" choice
is untested against outside experience here. Do not read silence as agreement. What the
comments *did* supply, by contrast, is *why* a reader objects to handlers:
comprehension, not mechanism.

**Hobby projects still carry the implementation evidence, but the comments added named
prior art.** Evidence passing, multi-prompt control, `task`-vs-`async`, response types
and the capability proposal each appear in exactly one post about one language that
nobody else uses. What the comment pass contributed is *names* — Firefly for object
capabilities, Flix and Links and Unison alongside Koka and Effekt, TinyGo's
async-insertion pass, libaco's 120-byte coroutines, Java's virtual threads — plus one
first-hand account of writing a compiler with an error row. It contributed no
benchmark: the only quantified performance claim in the axis remains Midori's 7%/4%,
and a practitioner comment now contests it rather than replacing it.

**What would actually settle the open ones.** (1) *Evidence passing vs `Core.Tunnel`*:
a measurement — both are claimed to be constant-time, neither is measured in `quill`.
(2) *Multi-shot*: an implementation with unrestricted mutation and a resource held
across a resume; the corpus argues this and never builds it, and the comment pass adds
a second objection (reading non-local control) that no implementation would answer
anyway. (3) *Colour*: a colourless design with function pointers and a real library
boundary, which is exactly what the Zig thread asks for and does not get — the comments
answer that inferring colour is undecidable in general, so the question narrows to how
much of it a compiler may assume. (4) *Resources*: quill has no disposal mechanism and no
ticket proposing one; the `for`-loop and `dynamic-wind` threads are the two questions
waiting for it. (5) *Divergence, deep/shallow, row spelling*: read papers and
implementations directly — this corpus has nothing on them. (6) *The two missing
trees*: fetch `1mh3ba8` and `11ti9sc` when network is allowed; they are the two threads
most on-topic for this axis and the only ones a reader is told to go elsewhere for.

## Dissent and corrections

Corrections made by the comment pass:

- The old header claim that comment coverage for this axis was "effectively zero" was
  wrong once the second corpus landed: 20 trees, 515 comments, recorded above.
- **Zig's colourless async is not shipped practice.** The first pass wrote "one side has
  shipped practice (Zig)"; a comment on the Zig thread records that Zig's async "was
  removed entirely, and has not been put back in yet" [also general knowledge, not only
  from the corpus]. The entry is retagged around that fact.
- **Midori's 7%/4% is contested, not settled.** It was the axis's only quantified claim;
  a practitioner in the quoting thread disputes it as compiler- and OS-specific and
  nobody re-measures. The exceptions entry now says so.
- "I don't think error handling is a solved problem" is no longer commentless: 26 of its
  comments were retrieved, so the entry cites comment material alongside the title.

Dissent that the entries above carry but that is worth listing: allocation should not be
an effect at all (against quill's decided three heap effects); a second channel for
effects doubles the work of writing generic code; `can Panic`-style pollution is the
growth failure of rows, answered by an assumed default row; and the multi-shot argument
quill records is only half the community's argument — the other half is that handlers are
hard to *read*, which quill's one-shot rule does not address.

Could not resolve with the material available: whether any implementation anywhere
types `resume` linearly (neither the 2538 threads nor the 515 comments name one — "not
found" here means "searched this corpus", not "does not exist"); and what the 41 and 50
comments on the two unfetched threads actually say. No claim above rests on them.

# Compiler architecture — the structure of the compiler's own code

What the compiler is made of and how its parts are wired: how many representations of a program
exist and when you cross between them, how nodes and names are stored, how passes are ordered and
re-run, where the type checker stops and the evaluator starts, how errors and source positions
travel, how the thing is tested and debugged, and which process choices keep a compiler alive long
enough to matter. Every entry is a decision with a price, not background on how compilers work —
the middle end of an optimizing backend is only touched where a structural choice is at stake.

## How this was gathered

2538 threads from r/ProgrammingLanguages and r/Compilers were grepped across several dozen
keywords (`sea of nodes`, `pass manager`, `flat ast`, `error recovery`, `self-host`, `fuzz`,
`desugar`, `de bruijn`, …), then read at the hits. Dates run 2012–2026. A second pass fetched the
comment trees of the 20 richest threads of this axis: **490 comments across 19 trees**, recorded in
`slices/impl-comments.md` ("Land ahoy: leaving the Sea of Nodes" is listed there with its tree
marked not fetched). Each fetch returns only ~30 top-level comments regardless of `limit=100` —
most headers read "RETRIEVED 26 comments (of N in the fetched tree)" — and the rest of each thread
sits in `more` placeholders that were never expanded (35 unretrieved on the biggest thread alone).
So what follows is post bodies plus the head of nineteen threads' replies, not those threads'
arguments in full. The corpus is what Reddit upvoted, not a survey: several high-scoring entries are
self-promotion for hobby languages and are cited only as evidence that an idea is being tried.
Several load-bearing entries are link posts with no body at all and are cited as the position they
point at; of those, only the visitor/Church thread had its comments fetched, so "Query-based
compiler architectures", "Against Query Based Compilers", "Zig Is Self-Hosted Now" and "Inside
Zig's Incremental Compilation" still rest on title and linked URL alone (I fetched no URLs), and
the Sea of Nodes thread's replies were listed but not fetched.

## The ideas

### Measure a layer before you keep it — then delete the redundant one

**What it is.** Count the representations of a program you actually have, ask of each what work
happens only there, and delete any that is a copy of another with fields dropped. Do it by
measurement, not by taste: name the layer, list what reads it, and if the answer is "nothing but
the next conversion", remove it and let the next stage read the real thing.

**Buys.** One conversion removed means one place a field can be silently dropped, one fewer rule
about which node carries spans and which does not, one fewer type for every new contributor to
learn. Every bug class that lives in a translation dies with the translation.

**Costs.** You lose the checkpoint where the program was "clean" — formatters, linters and tools
that wanted the discarded shape must now build it themselves or read the richer one. Deleting the
layer is only free once you have measured that nothing reads it; guessed deletions break callers
you never found.

**Maturity.** shipped — this is ordinary maintenance on production compilers, argued about openly.

**Tried by.** The thread below reports going the other way first: "many switch statements and many
kinds of representation for each stage" collapsed into "one kind of node and handler-based dispatch
per stage".

**Source.** Why not retain the AST?, score 41, 54 comments,
https://www.reddit.com/r/Compilers/comments/1w989ra/why_not_retain_the_ast/ (2026-09). See also
How do you avoid program representation bloat?, score 44, 37 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/l41frg/how_do_you_avoid_program_representation_bloat/
(2021-01), where one author counts four representations and asks whether that is inherent.

**Bearing on `fun`.** Already has it, by name: `Surface.t` was `Syntax.t` with information thrown
away, and it is deleted (`docs/wayfinder/tickets/delete-surface-ir.md`) — the elaborator reads
expanded `Syntax.t`, so ids and spans reach it with no conversion in between. The other layers
(`Core.term`, values) were explicitly *not* examined, so the measurement is half done.

### Retain one structure and annotate it, instead of converting per stage

**What it is.** Keep a single tree from parse to codegen, add fields to it as each stage needs them
(`Quals` for liveness, a type slot, a lowered form), and dispatch per stage over the same nodes.
Sharing is by node identity, so a property minted on a node is visible to everything downstream
that holds the node.

**Buys.** Adding a stage is adding a handler, not adding a type and a converter. Data already
shared between occurrences makes propagation trivial: the thread's example mints liveness tokens
onto children and they propagate for free. Reasoning about the program never involves a
translation step that could be wrong.

**Costs.** The tree accumulates fields most stages ignore, and every stage's invariants get harder
to state ("what is true of a node here?"). Optimization work is where this hurts: the thread's
author admits they have "not focused too much on codegen and optimization", and the poster child
for the opposite choice is SSA, which they report finding an impediment rather than a help. The
cost is borne by the middle end, so this trade is cheap for an interpreter and expensive for an
optimizer.

**Maturity.** contested — the thread is an open question with 54 replies I could not read; both
sides are represented in this catalog.

**Tried by.** Goldensystems GDSL (named in the thread); counter-examples below.

**Source.** Why not retain the AST?, score 41, 54 comments,
https://www.reddit.com/r/Compilers/comments/1w989ra/why_not_retain_the_ast/ (2026-09).

**Bearing on `fun`.** `fun` is on this side for the front half by accident of design: one
`Syntax.t` carries the reader's output through enforestation into elaboration. The annotation half
does not apply — `fun` has no middle end to annotate into.

### Single pass with no intermediate layer at all: emit while you parse

**What it is.** Parse and generate in one pass; the emitted text *is* the representation. Forward
jumps are `jmp DUMMY` placeholders remembered in a list and patched when the target is known — the
author of the second thread names his helpers `emitGoto`, `emitConditionalEarlyReturn` and
`comeFrom`, and says "I guess [those] are the things that would have gone into my intermediate
language, if I'd written one."

**Buys.** The smallest possible compiler: no converter, no second type, no pass ordering. Debugging
is linear because there is one direction of travel. Compile time is a single read of the file.

**Costs.** You cannot compile fragments independently and stitch them, because neither fragment
knows the other's reserved locations; everything must be emitted in one power-through. The second
source is a retrospective: that author states that in hindsight the compiler "should have had" an
intermediate representation with flow of control between source and bytecode, and that its absence
"is what would have saved me some pain". No IR is a shortcut that comes due later.

**Maturity.** contested — two hobby compilers, one proudly, one regretting it. Neither is
production evidence.

**Tried by.** Rockskunk (Pascal → NASM, regex peephole), Pipefish (Go → custom VM).

**Source.** Behold my Abomination: Written in Pascal, Single Pass(ish), No AST, No IR, score 77, 8
comments, https://www.reddit.com/r/Compilers/comments/1vxj95o/behold_my_abomination_written_in_pascal_single/
(2026-08); How the Pipefish compiler works: some highlights and lowlights, score 23, 4 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1is7gst/how_the_pipefish_compiler_works_some_highlights/
(2025-02).

**Bearing on `fun`.** Rejected by construction: `fun` is one path with a real handoff
(`source → reader → enforestation → expanded Syntax → elaboration → Core term → NbE → value`) and
`Core.term` is that intermediate layer. The Pipefish regret is the argument for keeping it.

### Interleave expansion, resolution and elaboration — and accept the cycle on purpose

**What it is.** Instead of a staged pipeline `parse → resolve → expand → resolve → check`, run the
steps per binding: a binding's macros expand, their output resolves, and elaboration of that
binding may need more expansion. The cycle is in the design rather than papered over with a second
name-resolution pass.

**Buys.** Hygiene, lexical scope and types can all be consulted at the point a macro runs, so
macros are not limited to the context of their definition. No duplicated resolution rules for
"before expansion" and "after expansion".

**Costs.** The thread that lays this out lists them precisely: macro-defined macros create a loop
"which kinda seems bad if you want to incrementalize the compiler"; a macro that adds imports
changes the dependency graph for every module that depends on it, not just its own; a macro doing
IO can change an arbitrary other file; and interleaving "seems concerning from a debugging and
maintenance POV, even ignoring issues of accidental non-termination". Incremental build systems
assume a DAG, and this is not one.

**Maturity.** research — Racket and Hackett are the working precedents in the thread; no
production system in the corpus is claimed to do this incrementally.

**Tried by.** Hackett (type checking in the loop), Racket (asked about, unanswered); `fun`.

**Source.** How do you architect a compiler for a language with Lispy macros?, score 15, 23
comments, https://www.reddit.com/r/ProgrammingLanguages/comments/bycyif/how_do_you_architect_a_compiler_for_a_language/
(2019-06).

**Bearing on `fun`.** Already has it, decided: enforestation interleaves with expansion, expansion
interleaves with elaboration *per binding*, and expansion may ask elaboration only through the
fixed `IMacroRuntime` adapter — `Fun.Expand` cannot reference `Fun.Compiler`, so the cycle is
crossed by an interface rather than by shared state. The thread's incrementality objection is live
and unpaid: caching sits in the `Loader`'s per-process dictionaries.

### Desugar into a form you already have, instead of adding a stage

**What it is.** When surface sugar appears, rewrite it to existing core syntax at the earliest
point that knows the names involved, and never introduce a new representation to hold it. If a
sugar is *only* a different way to write a core construct, make it a template for that construct
rather than a new pass.

**Buys.** One fewer stage, and every later stage inherits the sugar's behaviour for free —
including error messages, since the elaborator only ever sees the core form. The "representation
bloat" thread's four-stage growth is what you avoid.

**Costs.** The rewrite must happen where names are still resolvable, which pushes it later than
you would like; do it too early and you need fresh names, too late and you have a second form to
diagnose. Sugar that desugars to something with *different* type behaviour (the self-type thread's
example) can make types infinite and needs cyclic structures to represent.

**Maturity.** shipped — this is what every mature front end does with `for`, `+=`, operator
sections and the like.

**Tried by.** TypeScript/Rust `Self` (proposed desugaring in the thread); `fun` (templates).

**Source.** Desugaring for self types, score 31, 11 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/mtt4gu/desugaring_for_self_types/ (2021-04);
How do you avoid program representation bloat?, score 44, 37 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/l41frg/how_do_you_avoid_program_representation_bloat/
(2021-01).

**Bearing on `fun`.** Already has it: templates desugar to macros
(`docs/wayfinder/tickets/templates-desugar-to-macros.md`, closed and implemented), so the
elaborator never learns a second form; there is exactly one grammar for types and one `struct`.

### One responsibility per pass — a chain of identical small passes beats one clever loop

**What it is.** When several transformations all want to run over a token or node stream, give each
its own pass object and run them in a chain, even if they are three lines long. Never fold a new
transformation into an existing loop because it would be cheaper there.

**Buys.** Each pass has one reason to change and one place to test. A pass can be reordered,
disabled or unit-tested alone. The chain reads as a list of intent.

**Costs.** More objects and one more traversal each; on a hot path the extra passes are measurable.
The Pipefish author's own framing: the shared loop "increased not just in linear complexity, but
also the conditions became more complex and needed more flags … until the whole thing is a festing
pit of Lovecraftian horrors". The general lesson he draws: "just because two bits of logic can go
inside the same loop doesn't mean that they should." The comments add the price from the other
side: writing a compiler with user-friendly errors is "an order-of-magnitude harder. Primarily
because more context is required, and context will take a shotgun to your precious modular design"
(a quote a commenter brings in on "What I wish compiler books would cover",
https://www.reddit.com/r/ProgrammingLanguages/comments/gavu8z/what_i_wish_compiler_books_would_cover/) —
the message a pass emits often needs context the pass itself does not own.

**Maturity.** shipped — the "nanopass" style is named in the visitors thread as relying on code
generation to make this cheap.

**Tried by.** Pipefish (the relexer chain, as the repair); nanopass-style compilers generally.

**Source.** How the Pipefish compiler works: some highlights and lowlights, score 23, 4 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1is7gst/how_the_pipefish_compiler_works_some_highlights/
(2025-02); Pipefish architecture and workflow, score 12, 2 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1py5q42/pipefish_architecture_and_workflow/
(2025-12) — the fixed relexer is described there as working "on an assembly-line principle".

**Bearing on `fun`.** Has it differently, at file granularity rather than object granularity: a
feature's code goes in its own partial file (`Elaborator.<Feature>.cs`, `Nbe.<Feature>.cs`,
`Enforest.<Feature>.cs`) and a shared dispatch switch contributes one case line that calls into it.
The rule being enforced is the same — one feature, one place — with the call graph as the chain.

### The visitor pattern is Church encoding: write exhaustive dispatch instead

**What it is.** A visitor *is* a fold: `visit` takes one function per node shape and returns a
value, exactly as a Church-encoded sum takes one function per constructor. So you do not need the
double-dispatch machinery — a plain function per stage that pattern-matches every node, or a
generated folder, gets you the same abstraction without the class hierarchy. The thread's own
practical framing, from its top substantive comment: *Visitor is simply how you do pattern matching
in languages that don't have built-in pattern matching* — and the reason people mistake it for
iteration is that every GoF example of it uses recursive data structures (comment on "The visitor
pattern is essentially the same thing as Church encoding",
https://www.reddit.com/r/ProgrammingLanguages/comments/kqh9ui/the_visitor_pattern_is_essentially_the_same_thing/).
A second comment writes the Java translation of the essay's `forall` example to show the
equivalence is exact, method by method.

**Buys.** No visitor base classes, no `accept`, no ambient context object threaded through every
method. Exhaustiveness checking does the work that "did I remember to override `visitFoo`?" used to
do by hand. Stages become ordinary functions you can call directly from a test.

**Costs.** The generic-programming shape (visiting *types* rather than values, so one traversal
handles every field of type `expr`) is harder to express without code generation; the visitors
thread lists the unresolved cases — different traversal orders, dense vs selective visits, composing
two passes, early abort, rewriting in place, and speed when visitation itself dominates. In a
non-exhaustive host language you give up the compiler's help. The fetched comments add a second
cost: the identification may not be worth having — if `map` is the visitor pattern then "all
higher-order functions are the command pattern" and currying becomes the factory, which stretches
"pattern" until it names any higher-order function, and patterns are supposed to be workarounds for
missing language features rather than names for features you have (comment on the same thread, as
in Source). A comment on the other side notes Rust has pattern matching and the visitor still earns
its keep when the data structure, not the caller, controls iteration.

**Maturity.** contested — the linked essay argues the identification, and its thread's comments
split: one writes out the Java translation to show the equivalence is exact, another objects that
the identification is meaningless. The 22-comment practical thread ("What's your experience with
visitors?") is still unread.

**Tried by.** Oil deliberately *avoids* generalizing visitors and writes obvious recursive switch
traversals, only two of them; a commenter uses a visitor as the only way to declare a *second* sum
type over a Kotlin sealed-class hierarchy, and a Rust C compiler in this corpus walks its tree with
one.

**Source.** The visitor pattern is essentially the same thing as Church encoding, score 65, 57
comments, https://www.reddit.com/r/ProgrammingLanguages/comments/kqh9ui/the_visitor_pattern_is_essentially_the_same_thing/
(2021-01, link post pointing at the Haskellforall essay of that name); What's your experience with
visitors?, score 19, 22 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/7w3oeh/whats_your_experience_with_visitors/
(2018-02).

**Bearing on `fun`.** Already has it, implicitly: there is no visitor layer. Each stage is a
partial file of dispatch, and the "one case line in a shared switch" rule is the Church-encoded
shape — the sum is the syntax type, the stage is the function.

### Keep analysis out of the IR's construction

**What it is.** Build the intermediate representation first, with a dumb, documented construction
algorithm, then run analyses over it as passes. Sea of Nodes does the opposite and is described in
the corpus as "not just an IR, it is a compilation methodology": peephole optimization, incremental
dominators, global value numbering, dead code elimination and alias analysis are *interwoven* with
how the graph is built. The counter-move, traced in the second thread, is Braun's method: assume
you start from a tree rather than from tokens, pin instructions to basic blocks instead of letting
them float, skip scheduling, and mark blocks in-progress instead of using sentinels for incomplete
phis.

**Buys.** A separable algorithm can be read, tested and reimplemented on its own; the thread's
author says separating the outside steps from core SSA construction is what "brings out the basic
ideas". The fused version is faster and optimizes during construction, but you cannot study either
half alone.

**Costs.** Fusing is genuinely better for performance and for invariants that must hold mid-build
(a value that is already constant-folded never needs a later pass to find it). Separating them can
mean an extra traversal and a period where the IR is knowingly suboptimal. The thread also notes an
unstated requirement of the separated version — Braun's method needs def-use chains maintained
incrementally, which the paper assumes.

**Maturity.** shipped both ways — Sea of Nodes (the corpus cites Click's thesis as its source, and
a V8 blog post documents *leaving* it), Braun-style construction in
production compilers; V8's team published a blog post about leaving the Sea of Nodes.

**Tried by.** Click's Sea of Nodes work; Braun/FIRM; V8 (retreat, per the linked post).

**Source.** What is Sea of Nodes and how is it related to Static Single Assignment, score 36, 23
comments, https://www.reddit.com/r/Compilers/comments/1iu5yg3/what_is_sea_of_nodes_and_how_is_it_related_to/
(2025-02); SSA IR from AST using Braun's method and its relation to Sea of Nodes, score 35, 27
comments,
https://www.reddit.com/r/Compilers/comments/1ivgj5b/ssa_ir_from_ast_using_brauns_method_and_its/
(2025-02); Land ahoy: leaving the Sea of Nodes, score 53, 57 comments,
https://www.reddit.com/r/Compilers/comments/1jjldhu/land_ahoy_leaving_the_sea_of_nodes/ (2025-03,
link post pointing at v8.dev/blog/leaving-the-sea-of-nodes).

**Bearing on `fun`.** Genuinely new — `fun` has no middle end, so there is no SSA form to build.
The part that already applies is the separation: readback, unification and elaboration recurse over
structure as their own passes, and no pass writes analysis results back into `Core.term`.

### Flat, index-addressed trees instead of pointer-linked nodes

**What it is.** Store the tree as an array of nodes; a child is an integer position, not a pointer.
Iterate by index, and skip subtrees with a recorded `subtree_size` instead of recursing. Zig's
self-hosted compiler is the corpus's worked example; Carbon goes further and replaces recursion
from lexing to checking with state machines over the same flat data.

**Buys.** Measured: the thread reports Zig's switch to a flat tree cut compile RAM from ~10GB to
~3GB. Continuous memory, no per-node allocation, no pointer width, and the recursion-limit
workaround (spawning threads near the stack limit) disappears because iteration is explicit.

**Costs.** You stop writing ordinary recursive code and start writing index arithmetic and explicit
stacks, which is harder to read and easier to get wrong; every traversal becomes a loop with a work
list. The thread's author is himself skeptical: he has "never encountered this issue within
production C++ or Rust code" and only triggered recursion limits with deliberately huge one-line
expressions — so the motivating benefit may be narrower than the memory benefit.

**Maturity.** shipped — Zig and Carbon; Rust uses arenas with indices for the same reason.

**Tried by.** Zig (measured), Carbon, Rust (partly).

**Source.** Flat AST and states machine over recursion: is worth it?, score 61, 39 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1co8qpv/flat_ast_and_states_machine_over_recursion_is/
(2024-05).

**Bearing on `fun`.** Not applicable yet and worth deciding before it is: `Core.term` and
`Syntax.t` are pointer-linked and traversed recursively, and the evaluator deliberately does *not*
mirror that — a term needing a sub-evaluation gets a `Kont` frame rather than a native call. The
one measured stack hazard is recorded separately: `Primitives.cs:165`'s refusal is observable only
as a process-killing stack overflow, the one failure a conformance case cannot pin.

### Encode the whole syntax tree in one byte string; each stage is string → string

**What it is.** Serialize the tree into a single append-only buffer and replace pointers with
offsets into it — the thread's prototype used 3-byte "pointers" so a node fits 1 tag + 5 small
fields in 16 bytes. Lexing, parsing and tree construction become `string -> string -> string`;
you can copy an entire tree with a `memcpy` and free it with `free`.

**Buys.** No allocations and no resizes during parsing — only appends. Tree lifetime is the
buffer's, so there is no per-node deallocation and no garbage collector in the compiler. Offsets
are half the width of pointers on 64-bit, and ASTs are pointer-dense, so the win compounds. The
whole multi-stage compiler becomes a pipeline of serialized values, which also solves shipping a
tree between processes (the original motivation).

**Costs.** Every access goes through a small accessor instead of a field, so code is noisier unless
you wrap it in a type-safe API. Debuggers show you bytes. The author did not measure performance
and says so twice: "It's hard to measure without implementing the whole thing twice!"

**Maturity.** research — a working prototype (oheap) exists; no production compiler in the corpus
is claimed to do this.

**Tried by.** Oil shell's `oheap` format (the thread's own prototype).

**Source.** Representing ASTs as byte strings with with small integers rather than pointers, score
24, 51 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/79fkpu/representing_asts_as_byte_strings_with_with_small/
(2017-07).

**Bearing on `fun`.** Genuinely new. The nearest existing decision is the opposite kind of sharing:
`EquatableArray<T>` exists precisely because `ImmutableArray<T>` compares by reference and made
structurally equal records unequal — `fun` chose value-shaped comparison over encoded identity.
Note also that a byte-string tree has no place for scope sets, which are per-run integer sets in
`Fun.Kernel/ScopeSet.cs`.

### Intern names, and hash-cons terms you compare often

**What it is.** Two versions of one idea — make identity structural. (a) Put every identifier,
keyword and literal spelling in a table once and carry an `int` everywhere after the lexer; the
constants table in a bytecode compiler only pays for itself if entries are interned. (b)
Hash-cons whole terms: equal terms are the same node, so alpha-equivalence and type equality are
pointer comparisons.

**Buys.** Name comparison becomes integer comparison; printing is one table lookup. For (b),
convertibility checks in a dependent type checker stop walking structurally equal subtrees, and
sharing means a memoized node is physically the node. The recursive-tree thread's practical
observation is that hash-consing is what makes structural equality, hashing and ordering on
recursive types affordable at all.

**Costs.** A global table needs its own lifetime story and is hostile to parallel unless sharded;
interning strings you will never compare again is pure overhead (the constants-table thread's
objection: "the narrow subset of literals which can be safely interned"). Hash-consing makes
lifetime unbounded — the Appel paper cited in the corpus exists precisely to *collect* the table —
and it turns "two terms are equal" into "did I go through the cons table", so a term built by hand
silently fails to unify with the interned one.

**Maturity.** shipped for names (nearly universal); contested for whole-term hash-consing, which the
corpus treats as a research technique with at most three threads of attention.

**Tried by.** Oil (interned lexemes); Racket and ML compilers (hash-consing, per the recursive-tree
thread); `fun` (`Atom` for names, no term hash-consing).

**Source.** What is the point of having a constants table in designing a compiler without
interning?, score 19, 13 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1qa9a6e/what_is_the_point_of_having_a_constants_table_in/
(2026-01); Which languages have equality, hashing, and ordering on recursive trees?, score 18, 32
comments, https://www.reddit.com/r/ProgrammingLanguages/comments/17tw4nc/which_languages_have_equality_hashing_and/
(2023-11); Hash-Consing Garbage Collection (Appel 1993), score 8, 3 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/khdxkc/hashconsing_garbage_collection_appel_1993/
(2020-12).

**Bearing on `fun`.** Has the first half: `Atom` names are interned and identity matters — which is
why `EquatableArray<T>` was introduced when reference comparison made equal records unequal. The
second half is open and has a specific obstacle: nominal identity is applicative-by-purity and type
equality is type-specialized, so hash-consing types would need to respect both, and nothing
currently needs it.

### Make CPS / ANF / closure conversion an explicit named phase

**What it is.** Take the transformation that is going to happen anyway — turning nested calls into
a linear sequence, lifting free variables, naming every intermediate — and give it a name, a file,
an input type and an output type, rather than doing it ad hoc inside code generation.

**Buys.** The transformation becomes inspectable and testable on its own, and everything after it
sees a much simpler language: the corpus's closure-conversion post is titled for the payoff,
"The Function Out Of Functional Programming" — after the phase, there are no free variables left
to reason about. A named phase also gives you somewhere to put the pass ordering discussed below.

**Costs.** It is another representation to maintain (see the bloat thread), and for a compiler that
already works ad hoc it is a refactor with no user-visible win. The monomorphisation thread notes
the phase ordering is not free either: the low-level form it feeds may not support polymorphism, so
the phase has to run after specialization.

**Maturity.** shipped — standard in every compiler that targets a C-like or machine-level backend.

**Tried by.** Yap (closure conversion, Maranget pattern compilation and shift/reset lowering as
separate passes over one graph); most ML-family compilers.

**Source.** Closure Conversion Takes The Function Out Of Functional Programming, score 21, 3
comments, https://www.reddit.com/r/ProgrammingLanguages/comments/1kmfmeu/closure_conversion_takes_the_function_out_of/
(2025-05); What are common pitfalls and strategies when doing monomorphisation for ML-like
languages?, score 43, 16 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/mj8j8n/what_are_common_pitfalls_and_strategies_when/
(2021-04).

**Bearing on `fun`.** Genuinely new, and the corpus's nearest analogue explains why: `fun` runs on
NbE with `Kont` frames, so continuation structure is the evaluator's own frame stack rather than a
transform the source goes through. If a backend ever appears, this is the phase it will need.

### Decide now whether the IR is polymorphic or monomorphized first

**What it is.** Two stable designs. (a) A polymorphic IR: one copy of each function, type
parameters as first-class fields, passes written once for all instantiations. (b) Monomorphize
early: specialize every function at every instantiation before the main passes, so later passes
only ever see monomorphic terms and never test a type-parameter case.

**Buys.** (b) buys simple passes — the sequent-calculus compiler in the corpus *had* to
monomorphize because its low-level IR's focusing step eta-expands cuts at a known type, which is
impossible at a type variable; specializing the core "allowed me to fully monomorphise higher-rank
polymorphism with very little effort". (a) buys bounded output size and passes that do not need to
understand instantiation at all.

**Costs.** (b) is a code-size multiplier and an undecidability hole: the monomorphisation thread
asks exactly the right question about discarded type arguments, and control-flow analysis is needed
to terminate specialization in general. (a) pushes the complexity into every pass and into
equality — every pass must handle a type variable it cannot inspect.

**Maturity.** shipped both ways (Rust, C++ templates and Swift generics on the mono side; .NET and
JVM on the poly side).

**Tried by.** The sequent-calculus compiler (mono, forced); ML-family compilers (mono); JVM
languages (poly).

**Source.** What are common pitfalls and strategies when doing monomorphisation for ML-like
languages?, score 43, 16 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/mj8j8n/what_are_common_pitfalls_and_strategies_when/
(2021-04); Compiling with sequent calculus, score 47, 30 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1sbr2sy/compiling_with_sequent_calculus/
(2026-04).

**Bearing on `fun`.** Decided in favour of (a) by the type system: traits carry structural
dictionary evidence with most-precise-impl-wins and there is no monomorphization pass, so every
pass in `Fun.Compiler` already handles a rigid variable. The cost shows up as the budget: one
evaluation budget is shared by the checker and macro applications because specialization is not
doing the bounding.

### Declare pass dependencies instead of hand-ordering them

**What it is.** A pass manager where each pass states what it requires and what it invalidates, and
the driver orders them from that graph — as opposed to a literal sequence of function calls, which
is what the Dylan compiler's maintainer says he has today: a "fairly ad-hoc series of function
calls" left behind by 1996 authors who *intended* to model passes and dependencies explicitly but
ran out of time before the compiler was open-sourced.

**Buys.** Adding a pass no longer means auditing every other pass for ordering assumptions; the
analyzer you just wrote runs automatically whenever a caller needs it; invalidation is derived
rather than remembered. LLVM's new pass manager is the corpus's reference implementation of the
idea.

**Costs.** A real dependency model is more machinery than a 12-pass compiler needs, and the
Dylan maintainer frames implementing it as "probably an improvement" rather than a fix for a known
bug — the cost of *not* doing it is deferred, which makes it easy to defer forever. Over-declared
dependencies make the manager conservative and re-run passes for nothing. The comments add named
prior art and a defence of the status quo. One thread quotes the position "I don't believe that we
ever solved the issue of rewrite ordering" and agrees it "is still not really a solved problem in
general", asking for a modern Hoopl — which interleaves analysis and rewriting for you — while
equality saturation is the other answer and is reported as hard to keep from blowing up before it
becomes useful (comment on "Why do modern systems languages rely on compiler heuristics
to reverse-engineer programmer intent?",
https://www.reddit.com/r/ProgrammingLanguages/comments/1vbsv3v/why_do_modern_systems_languages_rely_on_compiler/).
Another commenter defends phase-order dependence outright: it is "fashionable to view 'phase order
dependence' as a bad thing", and removing it by declaring the bad cases undefined behaviour
"eliminates the phase-order dependence 'problem', but is in fact bad for optimization" (comment on
"Introduction to Compilers as an Undergraduate Computer Science Student",
https://www.reddit.com/r/Compilers/comments/1jxb1po/introduction_to_compilers_as_an_undergraduate/).

**Maturity.** shipped — LLVM's new pass manager; unsettled in the long tail of hobby compilers,
which is why the asking thread exists at all.

**Tried by.** LLVM, GCC; Dylan's DFMC wants it and lacks it.

**Source.** Beautiful optimization pass managers, score 28, 3 comments,
https://www.reddit.com/r/Compilers/comments/1ndnp34/beautiful_optimization_pass_managers/ (2025-09);
LLVM's New Pass Manager, score 8, 11 comments,
https://www.reddit.com/r/Compilers/comments/mfkvki/llvms_new_pass_manager/ (2021-03).

**Bearing on `fun`.** Has it differently: there is no pass manager because there is one path and
the interleaved driver is the ordering. The place where dependency declaration would bite is
already ticketed informally — the pipeline wiring checklist
(`docs/wayfinder/topics/pipeline-wiring-checklist.md`) is the hand-maintained version of what a
manager would compute.

### Query-based incremental compilation — and the case against it

**What it is.** Replace the staged pipeline with a demand-driven graph of memoized queries: ask for
any result (type of a definition, expanded body, diagnostics) and the framework recomputes only
what that result's inputs changed since last time. Errors, spans and cycles are the framework's
problem, not yours. The tooling-facing half of this idea — editor latency, and what an IDE needs the
language to expose — is in `tooling-and-diagnostics.md`.

**Buys.** Editor and REPL latency stops being a rewrite: the same queries serve a full build, an
LSP and a REPL. The "compiler as a service" talk named in the corpus is cited by a commenter as
the best explanation of the architecture. Redundant work disappears by construction — the whole
point of the thread that popularized it.

**Costs.** The counter-thread is scored nearly as high (103 vs 121) and exists precisely as a
rebuttal; the corpus records the position, not the resolution. The "Responsive Compilers" follow-up
lists three problems the paradigm still has not solved: cycles have to be handled case by case and
usually by enlarging the unit of computation, which kills memoization; keeping node location data
in a way that preserves memoizability is "nontrivial"; and collecting and propagating errors is
"nontrivial". A practitioner in the LSP thread's replies reported it as overkill for languages that
are never used on large projects.

**Maturity.** contested — shipping in rustc via Salsa and argued against in the same year's
top-level thread.

**Tried by.** rustc/Salsa, Henning Salang's talk; dismissed by the matklad post.

**Source.** Query-based compiler architectures, score 121, 12 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/hfs53y/querybased_compiler_architectures/ (2020-06,
link post → olleff.github.io/blog/posts/query-based-compilers.html); Against Query Based Compilers,
score 103, 29 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1rf9g7j/against_query_based_compilers/ (2026-02,
link post → matklad.github.io/2026/02/25/against-query-based-compilers.html); "Responsive Compilers"
was a great talk…, score 40, 8 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1gjdwl0/responsive_compilers_was_a_great_talk_have_there/
(2024-11); Language servers suck the joy out of language implementation, score 120, 68 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1nukes9/language_servers_suck_the_joy_out_of_language/
(2025-09) — the poster read it and judged it overkill.

**Bearing on `fun`.** Open, and it is the fog item: a first-class compiler API for tools, LSP and
REPL (`docs/wayfinder/topics/first-class-elaborator-api.md`) would want exactly these queries, but
today `Fun.Expand` may ask elaboration only through `IMacroRuntime` and `Fun.Expand` cannot
reference `Fun.Compiler` by design. The interleaved driver is also hostile to it — a cache key
becomes a *(definition, Context)* pair, not one hash.

### Content-address definitions instead of file paths

**What it is.** Identify a definition by the hash of its own tree, store names separately as
metadata, and cache expansion/elaboration results under that hash permanently and across runs —
rather than keying on file path in a per-process dictionary. Change nothing that is hashed and
nothing rebuilds.

**Buys.** Rename and move become free; identical definitions compile once; the cache survives the
process, so an editor restart is not a cold build. The corpus's import survey names Unison's
content-addressable repository as the existing shipped instance.

**Costs.** A hash must be over a canonical form — any per-run state (integer scope sets, fresh
metas, a Context) has to be normalized before hashing or the key is unstable. It also pays off only
with a persistent store: the corpus thread calls the idea "cute until log4j happens", i.e. it moves
your supply-chain problem into content identity. And it is orthogonal to correctness — nothing gets
righter, only faster.

**Maturity.** speculative for compilers-in-general; shipped in Unison's codebase, which is the
only instance the corpus names.

**Tried by.** Unison, ScrapTalk (per the import survey); Nix-style build systems outside this
corpus.

**Source.** r/ProgrammingLanguages on Import Mechanisms, score 80, 30 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1340z3r/rprogramminglanguages_on_import_mechanisms/
(2023-04); Mandala: experiment data management as a built-in language feature, score 32, 9 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/12im9hi/mandala_experiment_data_management_as_a_builtin/
(2023-04), which describes memoized functions versioned in a content-addressed git-style DAG.

**Bearing on `fun`.** Fog, and named as such: the content-addressed codebase
(`docs/wayfinder/topics/content-addressed-codebase.md`). Two obstacles are recorded in fun's own
terms — scope sets are per-run integer sets in `Fun.Kernel/ScopeSet.cs`, so a hash needs a
scope-normal form, and the interleaved driver makes a cache key a *(definition, Context)* pair. The
codebase-layout decisions (the `std` restructure, the bootstrap↔compiler interface) come first
because they would have to be hash-shaped.

### Nameless binders: an Index from the inner end, a Level from the outer end

**What it is.** Store a variable reference as a number: a position counted back from the innermost
binder, or (equivalently, and stabler) a position counted from the outermost. No names, no fresh
generation, no capture-avoidance on substitution — moving a term under a binder is a shift.

**Buys.** Alpha-equivalence is syntactic identity, which matters most in a dependently typed core
where types contain terms and you compare open terms constantly. Capture bugs are impossible rather
than subtle. The thread's sharpest observation: when they go wrong they fail *catastrophically*
rather than quietly, which is a debugging virtue.

**Costs.** The same thread's complaint list: hard to read when debugging, easy to be off by one,
and nearly all literature uses explicit names so you are translating as you read. Terms become hard
to print usefully — which pushes you to build readback/printing early or suffer unhelpful errors.

**Maturity.** shipped — Coq, Agda, Lean and every de Bruijn-based checker; locally nameless and
level-based variants are the acknowledged repairs.

**Tried by.** The CiC compiler in the thread (SML, nameless throughout); `fun`.

**Source.** de Bruijn indices, score 37, 29 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/fwdkv1/de_bruijn_indices/ (2020-04); My
nameless compiler for the CiC, score 53, 13 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/ku0m4f/my_nameless_compiler_for_the_cic/ (2021-01).

**Bearing on `fun`.** Already has it, with both numbers: **Index** is counted back from the innermost
entry and is what a term stores; **Level** is counted from the outermost and is stable as the Context
grows, so that is what a resolved name is located by. The glossary bans the phrase "de Bruijn
number", which is the naming decision made.

### One evaluator serves both checking and running

**What it is.** The type checker does not have a second reduction mechanism: it calls the same
evaluator the program does, under a budget, and compares results by readback. Bidirectional
elaboration then fixes which side is inferred and which is checked, so the checker never needs a
unification algorithm for terms it can simply compute.

**Buys.** One definition of conversion — the checker and the runtime cannot disagree about what a
term computes, because there is only one computation. Every language feature lands once: constants,
effects, records, matches. The corpus's bidirectional thread is about the same economy from the
other end.

**Costs.** The evaluator must be total and interruptible enough for the checker, which is why
budgets exist; and readback must be written even when nobody wants to print a term, because the
checker needs a term to compare against. A bug in evaluation is now a *type* error and vice versa.

**Maturity.** shipped — bidirectional elaboration with a shared evaluator is the standard shape for
dependently typed implementations.

**Tried by.** Agda, Lean, Idris, Coq; `fun`.

**Source.** The appeal of bidirectional type-checking, score 78, 47 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/v3z7r8/the_appeal_of_bidirectional_typechecking/
(2022-06); Bidirectional Elaborators à la Carte, score 23, 4 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1uw01lp/bidirectional_elaborators_à_la_carte/
(2025-10, link post → arXiv 2607.09564).

**Bearing on `fun`.** Already has it, decided: bidirectional elaboration plus NbE, and the budget
that lets the checker spend from the same pool — `Eval_budget`, 1,000,000 calls per request, with
running out reported as `ElabError EvaluationBudgetExceeded` rather than a hang. A macro application
is a call under that same one budget, so expansion cannot outspend checking.

### Interpreter first, then compile it in the interpreter's shape

**What it is.** Build a tree-walking evaluator to fix the language's semantics, then write the
compiler by copying the evaluator's structure case for case — same traversal, same `case`, same
corner cases — swapping "produce a value" for "emit code".

**Buys.** Two things the thread did not expect. First, a permanent oracle: if a program fails after
the compiler exists and passed before, the bug is in the compiler 100% of the time, which collapses
the search space over lexer, parser, front end and test. Second, the shape: the author reports being
"continually surprised … how like the interpreter it is", and that even the data structures match —
the evaluator's environment maps a name to a value, the compiler's maps the same name to a memory
location. Every semantic corner case found during evaluation is found again, in the same place.

**Costs.** You write each feature twice, and the compiler's version is the longer one. The
interpreter's freedom to change semantics cheaply disappears the moment code emission exists: the
author asks whether he would have reworked semantics as freely "if I had a compiler backend and it
had been ten times more effort for each change". A language that turns out wrong early has paid a
full front end for nothing.

**Maturity.** shipped — this is the standard path, and the corpus has a 48-comment thread asking
for its pros and cons.

**Tried by.** Charm/Pipefish (the source of both quotes above).

**Source.** From evaluator to compiler, a true story, score 43, 6 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/18zya3b/from_evaluator_to_compiler_a_true_story/
(2024-01); Pros and cons of building an interpreter first before building a compiler?, score 44, 48
comments, https://www.reddit.com/r/ProgrammingLanguages/comments/1rob2ub/pros_and_cons_of_building_an_interpreter_first/
(2026-03).

**Bearing on `fun`.** Has it differently: the "interpreter" is the NbE evaluator built alongside
the elaborator rather than before it, and the structural reuse is explicit — a binding contributes
entries only through `Binding.Slots()`, which both elaboration and evaluation read, so they cannot
disagree on order or count.

### Two engines give you a differential oracle — or a second liability

**What it is.** Keep an interpreter and a compiler (or two compilers) and run every test program
through both, diffing outputs. Every native change ships with: same program, both engines, diff
must be empty.

**Buys.** Divergence *is* the bug report; you do not have to know what the right answer is to
detect a wrong one. The corpus's differential-testing threads are all variations on this — N+1
versions, grammar mutation feeding it, cross-language code generators. The thread below shows the
failure it prevents: an interpreter that panics on a bad argument while the emitted JavaScript
silently does not, and shadowing that crashes the JS output with "Identifier has already been
declared".

**Costs.** Two implementations means two of every bug and a permanent synchronization tax; the
poster's own question is whether there is "any methodology that would prevent me from introducing
[differences] in the first place", and his stated answer is "know your target language semantics
well, write lots of tests, do fuzzing". Differential tests also cannot catch a semantic bug that
both engines share — you need a third oracle for that. And a second implementation that is kept
alive after one is clearly ahead is pure drag.

**Maturity.** contested — as a testing technique it is shipped (compiler vendors do it); as a
*way to keep two implementations* it is contradicted by this project's own history.

**Tried by.** YARPGen/Xsmith-style C and Racket fuzzers (named in the corpus); a JS+WASM+ASM toy
toolchain in the thread below; `fun`, which used the OCaml prototype as its second engine and then
deleted it.

**Source.** Ensuring identical behavior between my compiler and interpreter?, score 56, 26 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/11mpom9/ensuring_identical_behavior_between_my_compiler/
(2023-03); Detecting C++ Compiler Front-End Bugs via Grammar Mutation and Differential Testing, score
17, 3 comments,
https://www.reddit.com/r/Compilers/comments/v0fvqw/detecting_c_compiler_frontend_bugs_via_grammar/
(2022-05).

**Bearing on `fun`.** Decided, and the decision was to stop: the OCaml prototype was deleted on
2026-09-25 *after* measuring `port-fails: 0` over every program in the repo with 34 disagreements
recorded as prototype defects in `test/conformance/prototype-divergences.txt`. The lesson fun
recorded is the cost side of this entry — do not keep two implementations alive once one measures as
a superset; `CLAUDE.md` now says so explicitly.

### The native stack is not the language's stack

**What it is.** Give the evaluator an explicit frame: when a term needs a sub-evaluation, push a
`Kont` and continue iteratively instead of calling recursively. Readback, unification and
elaboration may still recurse over *structure* — the rule is per object-level call, not per node
visited.

**Buys.** A million nested object-level calls run without touching the native stack, so a
pathological program is a budget error with a call stack, not a segfault. This is the difference
between "the compiler reports `EvaluationBudgetExceeded` naming the call" and "the process dies with
exit 134". The mutation sweep's finding is exactly what this design prevents: one refusal in
`Primitives.cs` is observable only as a process-killing stack overflow — "the one failure a
conformance case cannot pin, and the one runner cannot classify".

**Costs.** The evaluator is harder to read: control flow lives in a frame type rather than in C#
call sites, and every new construct needs a frame case. Frames also cost allocation on paths that
would have been cheap tail calls. And it does not protect the *other* half — elaboration and
readback still recurse structurally, which is where `deep-non-tail-recursion-is-superlinear` and
the `Nbe.cs:586` unreachable-guard finding came from.

**Maturity.** shipped — every production VM and every logic programming system uses frame stacks;
the corpus's version of the problem appears as the flat-tree thread's recursion-limit discussion.

**Tried by.** BEAM, SML/NJ, Prolog systems; `fun`.

**Source.** Flat AST and states machine over recursion: is worth it?, score 61, 39 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1co8qpv/flat_ast_and_states_machine_over_recursion_is/
(2024-05) — the recursion-limit problem, stated without the fix. No thread in this corpus argues
for an explicit frame stack, so the second half of this entry rests on `fun`'s own record rather
than on community evidence.

**Bearing on `fun`.** Already has it, by rule: the evaluator never recurses on the native stack per
object-level call — a term needing a sub-evaluation gets a `Kont` frame — while readback,
unification and elaboration may recurse structurally. The residual risk is measured and ticketed,
not guessed: `Primitives.cs:165` and `deep-non-tail-recursion-is-superlinear`.

### Errors as values: structured, collected, rendered at the end

**What it is.** An error is a value — a sum type with a payload per variant — produced by the
phase, carried up, accumulated, and only formatted into a string at the edge. The alternative the
thread poses is the live choice: print as soon as you have enough for a message, or collect and
print at the end.

**Buys.** Accumulation becomes possible at all (you cannot "return then continue" from an
exception), a test can assert on a constructor rather than on wording, and the same value can be
rendered twice — once for a CLI, once for an LSP. The corpus's test-strategy thread shows the
lightweight version of the payoff: negative tests check that the *right* error was caught without
running the codegen or output steps.

**Costs.** Structured errors are a sum you must keep exhaustive — every new failure mode touches
every consumer that matches it. Carrying them through a pipeline that assumes success means every
function returns a pair, which is the "noise" the spans thread complains about for the same reason.
And you must resist formatting early: a message built at the throw site can never be re-rendered.

**Maturity.** shipped — this is what Rust, Elm and GHC do; the throwing style is what most hobby
compilers do.

**Tried by.** Rust (`Diagnostic` with structured codes), Elm; `fun` (`ElabError`, `Expand_error`,
`FunException`).

**Source.** Reporting errors, score 8, 23 comments,
https://www.reddit.com/r/Compilers/comments/1ezyeie/reporting_errors/ (2024-08); What testing
strategies are you using for your language project?, score 30, 42 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1juwzlg/what_testing_strategies_are_you_using_for_your/
(2025-04).

**Bearing on `fun`.** Has it, with a deliberate exception: `FunException` is a genuine *language*
error and `NotImplementedException("not ported yet: …")` marks an unported path and is deliberately
distinguishable, so a refusal can never satisfy a case expecting `error` — the runner counts it as
a failure and never as a passing `error` case. Rule: exceptions are not control flow; dispatch is
`Result`, `option` or an explicit sum.

### Spans as a side channel, not a field on every node

**What it is.** Carrying a source position on every node of every representation is the obvious
design and the thread's author rejects it: it makes pattern matching noisier, forces a helper even
to ask "is this an underscore", and requires threading the same span through every constructor of
every transform. The alternatives are a side table keyed by node identity, a wrapper that carries
position once around the outside, or a "Trees that Grow" style extension field that most patterns
ignore.

**Buys.** Transforms stay purely about their task; a synthetic node inherits its origin's span by
lookup rather than by an argument nobody wanted. Adding a representation does not multiply span
plumbing.

**Costs.** A side table needs identity that survives rewriting — anything that rebuilds nodes must
re-key it, or positions silently go stale (the incrementality thread's version of this: keeping
location data "in a way that preserves incremental/memoizable behavior" is nontrivial). Wrappers
still appear in signatures; you move the noise rather than remove it. And the paper that solves it
properly (Trees that Grow) is, in the thread's words, "a bit dense".

**Maturity.** contested — GHC carries extension fields (shipped), most compilers thread spans by
hand (shipped), side tables are argued but not evidenced in this corpus.

**Tried by.** GHC (Trees that Grow, named in the thread); ReScript code in the thread's example;
and, from a comment rather than a post, Oil shell: the lexer produces spans that concatenate back to
the original source file, spans have consecutive integer IDs, and one array maps span ID to source
ID — a side table whose invariant is stated (comment on "What I wish compiler books would cover",
https://www.reddit.com/r/ProgrammingLanguages/comments/gavu8z/what_i_wish_compiler_books_would_cover/).

**Source.** Automatically pass source locations through several compiler phases?, score 24, 10
comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1f2sx39/automatically_pass_source_locations_through/
(2024-08); "Responsive Compilers" was a great talk…, score 40, 8 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1gjdwl0/responsive_compilers_was_a_great_talk_have_there/
(2024-11).

**Bearing on `fun`.** Open fog with a measured diagnosis: the diagnostics polish boundary. Positions
already exist — `Budget._site` (`Budget.cs:23`) is set by `Elaborator.At` and `Budget.Where()`
prints it — but `Pattern` (all fourteen variants) and `EffectRow` have no span at all, and
`Driver.cs:38-46` is the funnel that discards token spans. Of the 130 `new FunException(` sites, 73
already have a span in scope, 35 would need it threaded from a caller, 22 have no source form behind
them. The cheap move is one line; its real cost is the nine xUnit assertions that compare an exact
`Message`. Also: `Enforest*.cs` is 2804 lines with 173 throws, **0** carrying a span.

### Error recovery and many errors per run

**What it is.** On a parse error, resynchronize — skip to the next statement terminator, closing
brace or declaration start — record the error, and keep going so the user sees every mistake at
once. The asymmetry the corpus identifies is real: resynchronizing a bottom-up parser is a shift to
"the next important token"; a top-down parser has no such move and the thread's author is stuck.

**Buys.** One round trip instead of ten; the compiler becomes useful as a *diagnostics generator*
even when nothing compiles, which is what the resource thread points out is the common case —
"most of the time they are used to generate error messages and meta-data, not byte code", counting
LSP calls.

**Costs.** Recovery is a second parser: every production needs a "what do I skip" answer, and a bad
resynchronization point turns one error into a cascade of nonsense. A recovered tree is not a valid
tree, so every stage downstream must tolerate holes or you must stop before them — which is why
some compilers recover only through name resolution and not into codegen. The C99 compiler's author
in this corpus reports the failure and its repair from experience: at first "the error would get
propagated up the entire call chain and then the parser synchronized again", and moving the
synchronizer next to the error — let it parse to the end of *that* statement or expression — fixed
it (comment on "I wrote a C99 compiler from scratch",
https://www.reddit.com/r/ProgrammingLanguages/comments/1bvsvby/i_wrote_a_c99_compiler_from_scratch/).

**Maturity.** shipped in production compilers (Clang, rustc, tree-sitter), unresolved as a
technique in the corpus — the asking threads are unanswered by any cited implementation.

**Tried by.** Clang and rustc (per the resource thread's mention); Chumsky and Gibberish (library
support, cited in the corpus); not `fun`.

**Source.** Any good resources for creating actually modern parsers? Things like error recovery and
messages., score 41, 12 comments,
https://www.reddit.com/r/Compilers/comments/1ejabxw/any_good_resources_for_creating_actually_modern/
(2024-08); Managing multiple errors in top-down parsers?, score 4, 5 comments,
https://www.reddit.com/r/Compilers/comments/on72w/managing_multiple_errors_in_topdown_parsers/
(2012-01).

**Bearing on `fun`.** Unruled, and deliberately so: the enforester ticket measured it and concluded
error recovery "has **no workload** and stays unruled" — that framing is recorded as the failure
mode of `type-case-refinement` caught *before* it repeated. Every form the reader sees has a span
and `Driver.cs` throws them away; span-carrying expansion errors is the one buildable item left on
that ticket.

### One conformance suite as the single source of truth — and a sweep that proves it bites

**What it is.** A directory of `<name>.fun` + `<name>.expect` pairs, nothing to register, where
`.expect` is a value, a constructor name, `ok` or `error`. Language behaviour lives there and
nowhere else — internal tests keep only what inspects internals (shapes, round trips, budget
accounting, an exact error constructor, a type rather than a value). Then *verify* the suite by
mutating the compiler: neutralize one guard, and require that exactly one case flips.

**Buys.** One source of truth kills duplication and makes "did we ever test this?" answerable by
ls. The mutation sweep converts a green count into evidence: it found eight rows the suite did not
cover, plus two `src/` findings — a guard unreachable by any program, and the stack-overflow
refusal above. It also found that a *proposed* migration was not worth doing: xUnit → cases would
have been worth it only for `ok` cases, and round two measured **1 of 67** `ok` cases caught by any
of its 73 mutations.

**Costs.** Snapshot-style expectations rot: Yap's author is chasing "replacing 'well, the snapshot
changed' with meaningful tests". A suite that pins error wording makes every message edit a
two-file change — fun deliberately does *not* pin wording, calling it implementation-specific. And
mutation sweeping is manual labour per guard, and cannot express "this case is redundant", which
the separate redundancy measurement had to do by byte-comparison.

**Maturity.** shipped — reference `.expect` suites are how GCC, LLVM and every language repo in the
corpus test; mutation *sweeping* a conformance suite is rare enough that this corpus has no thread
on it.

**Tried by.** The reference-output approach in the testing-strategies thread (`<file>.ref`, used as
the first compiled program of the author's own language); `fun`.

**Source.** What testing strategies are you using for your language project?, score 30, 42 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1juwzlg/what_testing_strategies_are_you_using_for_your/
(2025-04); "How hard could it be?" — a younger me said that once, score 20, 18 comments,
https://www.reddit.com/r/Compilers/comments/1vc0jx1/how_hard_could_it_be_a_younger_me_said_that_once/
(2026-04), for the snapshot-vs-meaningful-tests complaint.

**Bearing on `fun`.** Already has it, twice over: `test/conformance/cases/<area>/<name>.fun` +
`.expect` is the *only* place a source→value-or-error behaviour is tested, and the mutation sweep
runs over it — `docs/wayfinder/tickets/coverage-gaps-from-the-mutation-sweep.md` and
`conformance-runner-aborts-a-mutation.md` (that one's real defect: "a whitelist maintained twice
stops being a whitelist"). Current: suite 961 cases, 0 failed, xUnit 209/209.

### Fuzz with a grammar; raw bytes never parse

**What it is.** Generate inputs from the language's grammar (or mutate real programs by grammar
rules) instead of feeding random bytes to a fuzzer. Byte-level fuzzers die at the lexer: the
thread's own report is that AFL and libFuzzer "did not give any results, since these fuzzers don't
understand the grammar of my language. Therefore, their input data is rejected at the lexical
analysis stage."

**Buys.** Deep coverage of the parser, elaborator and evaluator instead of coverage of your error
path. Grammar mutation plus differential testing is the corpus's cited combination, and Xsmith is
named as writing correct differential fuzzers "with little effort". Regehr's linked post is the
cautionary case: a compiler whose code was vibe-written was repaired by fuzzing — the technique
works even on an author who did not write the compiler.

**Costs.** Writing a grammar-generating fuzzer is real work, and the interesting bugs are usually
*not* parse errors — they need a well-formedness filter or you spend the budget on programs that
legitimately fail to typecheck. Shrinking a failing program to a minimal one is where most of the
value sits and no thread in this corpus describes doing it.

**Maturity.** shipped — YARPGen, Csmith, Xsmith, AFL-on-grammars; explicitly listed as a gap in
compiler textbooks.

**Tried by.** YARPGen (C/C++), Xsmith (Racket, Dafny, SML), oss-fuzz harnesses; not `fun`.

**Source.** How to use fuzzing to test an arbitrary programming language?, score 32, 18 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/l0doct/how_to_use_fuzzing_to_test_an_arbitrary/
(2021-01); Generating Conforming Programs with Xsmith [SPLASH'23], score 5, 0 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/17emgac/generating_conforming_programs_with_xsmith/
(2023-10, link post to the paper); "I Fuzzed, and Vibe Fixed, the Vibed C Compiler", score 64, 43 comments,
https://www.reddit.com/r/Compilers/comments/1rj4d6f/i_fuzzed_and_vibe_fixed_the_vibed_c_compiler/
(2026-03, link post → john.regehr.org/writing/claude_c_compiler.html); What I wish compiler books
would cover, score 146, 36 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/gavu8z/what_i_wish_compiler_books_would_cover/
(2020-04).

**Bearing on `fun`.** Not present, and the honest reason is not laziness: a conformance case can
only assert a source→value/error triple, so a fuzzer needs an oracle — differential against what?
The answer would be the two engines fun no longer has. Fuzzing is therefore gated on either a
grammar generator plus an invariant ("does not crash, does not exceed the budget") or a second
engine, and neither is ticketed.

### Instrument the compiler; do not go exploring with test cases

**What it is.** When a bug appears, add a reusable diagnostic that dumps the intermediate
representation at the point of interest — a graph of the parse, a printout of the resolved context,
a trace of which pass last touched the node — run it once, and read the answer. Do not bisect by
editing the input program.

**Buys.** One run produces the root cause instead of a matrix of inputs. It scales: a tool that
prints any node's state stays useful for the next bug, while a hand-patched test case is thrown
away. The GraphViz thread is the concrete instance — dump the tree, look at it.

**Costs.** Instrumentation is code you have to keep compiling, and a dump that is wrong misleads
worse than no dump. The thread's framing of the alternative is honest: "Feels like 90% of the time
is figuring out WHAT the bug is before I can even try to tackle it" — printing is the cheapest way
to answer that, but only if what you print is the actual intermediate form and not a
reconstruction.

**Maturity.** shipped — universal practice; no competing method appears in the corpus.

**Tried by.** Everyone the corpus names; `fun` makes it a written rule in `CLAUDE.md`.

**Source.** Debugging interpreters/compilers, score 32, 21 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/nia51e/debugging_interpreterscompilers/ (2021-05);
GraphViz Generation from AST, score 38, 5 comments,
https://www.reddit.com/r/Compilers/comments/1jv0yxd/graphviz_generation_from_ast/ (2025-04).

**Bearing on `fun`.** Already has it, written down: *debug via instrumentation, not test-case
exploration* — add logging or a reusable utility that exposes the intermediate representation, and
capture `dotnet build && dotnet test --nologo 2>/tmp/log`. The goal is one diagnostic that pins the
root cause, not a matrix of modified inputs.

### A large test suite is the only defence against an O(n²) feature space

**What it is.** Accept that language development is quadratic in the number of core features —
"for every two 'orthogonal' features there is a corner case", and every optimization multiplies
that again because every feature may fight every optimization — and respond by making the test
suite the primary artifact rather than the code.

**Buys.** The thread's own conclusion is unusually blunt for this genre: the reason Pipefish still
works is "here's my big magical secret … have a big test suite", and "the project should never
have been full of bugs in the first place. It should have been full of verifiable successes,
demonstrated by automated tests. There is no other way to maintain a project that is intrinsically
O(n²)."

**Costs.** A suite that big is itself a project: it must be fast enough to run constantly, it must
have one home (see the conformance entry), and it needs the mutation question answered or half of
it is theatre. The same thread's audience included a developer who abandoned a buggy language for a
fresh one — the alternative the post argues against is starting over, which is cheap today and
O(n²) again tomorrow.

**Maturity.** shipped as advice; unverifiable as a measured claim — the thread offers no numbers.

**Tried by.** Pipefish; `fun` (961 conformance cases + 209 xUnit).

**Source.** Langdev is O(n²), score 64, 30 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1gpe6ai/langdev_is_on²/ (2024-11).

**Bearing on `fun`.** Already has it, and the corner-case law has a fun-specific name: types are
values and type-case on open `Type` is acceptable, which is *how* `fun` keeps n down — Consistency
> Flexibility > Correctness buys one construct (`struct` = record/module/namespace) instead of
three features that each have to work together. The test-suite half is the conformance suite plus
the sweep above.

### Backend: C, LLVM, or your own — and what a second backend costs

**What it is.** Three stable choices, each a different bet about where your maintenance effort
goes. (a) Emit C: no codegen of your own, every platform's optimizer for free, debuggable output.
(b) Link LLVM: real optimization and a mature register allocator, at the price of a huge dependency
whose C++ API is explicitly described in the corpus as "not particularly stable" and which is
"forked and patched by basically all languages that use LLVM's backends — Rust, Swift, Zig, Odin…
and that is an absolute no-go for me". (c) Write your own: full control, no dependency, and you
personally own register allocation.

**Buys.** (a) is the cheapest path to a working language and the thread's respondents treat it as
morally fine — "Even C++ started out like this." (c) buys compile speed: the ideal-IR thread wants
a backend that is "very fast (unlike LLVM)" with "a codebase that is tested (no tests in QBE)". (b)
buys optimizations you will never write. The comments on the C thread add a debugging freebie:
record which of your source lines produced each emitted line and emit C's `#line` directive, and
and gdb steps through your language's own source (highest-scored comment on "Is it okay to compile
down to C?", https://www.reddit.com/r/ProgrammingLanguages/comments/qvqa1i/is_it_okay_to_compile_down_to_c/).

**Costs.** (a) inherits C's undefined behaviour in your output and makes your debug output C's
debug output; (b) is megabytes of dependency, an unstable API and a fork to maintain; (c) is the
most work by far and the reason hobby compilers stall at register allocation. The Go-vs-C thread
adds a fourth variable: emitting C means *you* now own a garbage collector unless you pick an IR
that already has one. The comments on (a) sharpen both halves: a naive `<<` (or `+`) translation
trips undefined behaviour, so avoiding UB forces you to emit *less* portable C; C has no guaranteed
tail-call elimination, so you lose calling into other languages; and you need not use all of C —
one commenter "used it like a slightly better assembly language". On (b) a commenter states the
instability plainly: "LLVM IR is a moving target and most serious users end up forking the whole
compiler suite to deal with bugs and breakage" (comments on the C thread, as in Source).

**Maturity.** shipped — all three are in production use (C: TCC and the early C++ front ends; LLVM: Rust,
Swift, Zig; own: performance-critical compilers).

**Tried by.** C2 (has both C and QBE backends and wants a fourth option), Skew (JS, TS, C#, C++
backends), chibicc-derived compilers; Nim (C as its default target), Seed7, Chicken Scheme and ATS
are named in the C thread's comments, and `libgccjit` is proposed as a fourth option between C and
LLVM — stabler API, more platforms; not `fun`.

**Source.** Is it okay to compile down to C?, score 75, 71 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/qvqa1i/is_it_okay_to_compile_down_to_c/ (2021-11);
Go vs C as IR?, score 42, 35 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1ekch7h/go_vs_c_as_ir/ (2024-08); What would
an ideal IR look like?, score 25, 35 comments,
https://www.reddit.com/r/Compilers/comments/1g0chuu/what_would_an_ideal_ir_intermediate/ (2024-10);
The agony of choice: C++, Rust, or anything else, score 31, 65 comments,
https://www.reddit.com/r/Compilers/comments/1ganq6q/the_agony_of_choice_c_rust_or_anything_else/
(2024-10).

**Bearing on `fun`.** Undecided and unscheduled: there is no backend, `src/Fun.Cli` prints "the
.NET port has no entry point yet" and exits 1. Because the host is .NET, option (c) would mean
writing a code generator *and* a collector; option (b) means a native dependency; option (a) means
emitting C from `Core.term`. Nothing in `docs/wayfinder` picks one — the closest is the fog item on
the library-vs-compiler-machinery boundary (UFCS, FFI), which would have to be settled first. Note
the second-backend cost either way: Skew shipped four and still died of a small standard library.

### Write the compiler in a memory-safe host language

**What it is.** Choose a host with a real type system and a garbage collector (or ownership) so
that the compiler's own memory bugs cannot masquerade as language bugs — the failure mode where
your type checker segfaults and you cannot tell whose fault it is.

**Buys.** Sum types and pattern matching are the compiler's natural shape, and the corpus's survey
of 45 community languages found no dominant host: 17.8% C++, 15.5% C, 11.1% Rust, then Haskell,
Java, self-hosting, Python at 6.6% each — so the choice is not constrained by convention. The
thread on Go notes a practical draw: GC and single-binary cross-compilation for free.

**Costs.** A GC'd host makes the compiler's own allocation behaviour part of your compile-time
profile, and the survey's own conclusion is that no single language predominates, so a parser
generator targeting "the compiler host language" cannot exist. The thread on Python is the other
side: bootstrap compilers don't need speed, yet Python is still rare — which suggests the real
cost is ecosystem tooling (LLVM bindings, CLI ergonomics), not raw speed. Choosing Rust for the
portfolio is itself a career trade-off the corpus debates separately. The Go thread's own
practitioners contradict the neutral framing above: "As someone who's writing a compiler in Go, I
don't recommend it if you can avoid it. It doesn't have algebraic datatypes and exhaustive pattern
matching", and a second commenter says algebraic data types "are essential to building language
tooling" (comments on "How good is Go for writing a compiler?",
https://www.reddit.com/r/ProgrammingLanguages/comments/15gz8rb/how_good_is_go_for_writing_a_compiler/).
The countercase in the same thread: Go's GC "is rarely a problem for a short lived program like a
compiler", and its profiling tooling offsets the rest.

**Maturity.** shipped — the survey is of shipping compilers.

**Tried by.** C/C++ and Rust dominate; Haskell (Idris), Java, F#, OCaml all appear. C# gets an
unsolicited endorsement in the Go thread's replies: "I found C# quite pleasant to use. Pattern
matching is really quite flexible and comfortable to use nowadays", plus LLVM bindings.

**Source.** Languages Used to Implement Compilers, score 54, 41 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/ays7d7/languages_used_to_implement_compilers/
(2019-03); How good is Go for writing a compiler?, score 54, 62 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/15gz8rb/how_good_is_go_for_writing_a_compiler/
(2023-08);
The agony of choice: C++, Rust, or anything else, score 31, 65 comments,
https://www.reddit.com/r/Compilers/comments/1ganq6q/the_agony_of_choice_c_rust_or_anything_else/
(2024-10).

**Bearing on `fun`.** Already has it: the implementation is C# on .NET 10, which is why
`EquatableArray<T>` and the immutability rules exist — value-shaped records in a safe host. The
three-project split (`Fun.Kernel` → `Fun.Expand` → `Fun.Compiler`) is the compiler-safety analogue
of what a memory-safe host gives you for free: a reference that crosses the boundary is a build
error, not a runtime crash.

### Refuse in the compiler rather than guess — and say which rule refused

**What it is.** When a pass would need an expensive or heuristic decision, raise a compile error
that names the construct and let the programmer restructure, instead of running an NP-hard or
fragile algorithm silently. The thread's instance: if live values exceed registers, error out
rather than spill, on the argument that "the user can do a better job deciding what should be
spilled than the register allocator".

**Buys.** Compile time stops being exponential in the worst case, the pass gets *simpler* — which
is a maintainability win, not just a speed one — and the failure is attributed to a place in the
source instead of to a mysterious slowdown. The register allocator was 74% of compile wall time
before the punt.

**Costs.** It is an ergonomics tax on the user, and a blunt one: perfectly ordinary programs stop
compiling. The thread is a single hobby project with 24 replies I could not read, so there is no
independent confirmation that the trade holds up. It also sets a precedent — the next NP-hard pass
has an easy fallback of "make it an error" until the language is unusable.

**Maturity.** speculative — one implementation, no independent evidence in this corpus.

**Tried by.** Maxon (the thread); `fun`, in a different currency.

**Source.** Our compiler refuses to spill inside a hot loop — it raises a compile error, score 16,
24 comments, https://www.reddit.com/r/Compilers/comments/1wmkdyu/our_compiler_refuses_to_spill_inside_a_hot_loop/
(2026-09).

**Bearing on `fun`.** Has it in three places, by ruling rather than by accident: the shared
evaluation budget returns `ElabError EvaluationBudgetExceeded` naming the call instead of hanging
(no surface syntax exists to raise it); `HandledEffectEscapes` is a refusal rather than a
re-interpretation; and `NotImplementedException("not ported yet: …")` marks an unported path so a
refusal can never be mistaken for a language `error`.

### Self-hosting — and the regression trap it creates

**What it is.** Rewrite the compiler in your own language once the language is stable enough. The
corpus treats it as both the strongest test of a language ("a compiler is a really complete program.
Recursion, trees, abstractions…") and a genuine operational hazard.

**Buys.** The language is exercised by the hardest program its author will ever write; the
bootstrap doubles as the standard library's stress test; a self-hosting compiler is the demonstration
(Zig's thread is titled for the milestone). Skew's compiler is cited as "a masterpiece of simplicity
and elegance, and a great showcase for the language itself".

**Costs.** The trap is stated precisely in the regressions thread: you add a feature, start using
it in the compiler, later find a regression introduced with it — "I don't want to use the latest
version because it's buggy. I can't use the last version without the regression because it doesn't
support the new syntax." The self-hosting thread adds the other cost: "any defect in your lang
could affect the compiler in a nasty recursive way", and a language that is inherently slow
pressures you to keep the compiler in C forever. The bootstrap thread's comments add a reason to
keep the chain short that is not about regressions: less code to audit "in order to verify the
claim that your compiler is definitely upholding the behaviour it's supposed to" — mrustc exists
mostly to shortcut Rust's chain, and Live Bootstrap to make the chain reproducible (comment on
"The Compiler Apocalypse: a clarifying thought exercise for identifying truly elegant and resilient
programming languages",
https://www.reddit.com/r/ProgrammingLanguages/comments/1qqo3s2/the_compiler_apocalypse_a_clarifying_thought/).

**Maturity.** shipped — Zig, OCaml, Rust (partly), D are all named; the failure mode is documented
by people who hit it.

**Tried by.** Zig (milestone thread), Skew, the 16-year-old's toolchain, dozens of hobby languages.

**Source.** How do you deal with regressions in a self hosted compiler?, score 49, 20 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/qceodm/how_do_you_deal_with_regressions_in_a_self_hosted/
(2021-10); Value of self-hosting, score 19, 41 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1j60mgt/value_of_selfhosting/ (2025-03); Zig
Is Self-Hosted Now, What's Next?, score 87, 29 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/ydrz3k/zig_is_selfhosted_now_whats_next/ (2022-10,
link post → kristoff.it/blog/zig-self-hosted-now-what/).

**Bearing on `fun`.** Out of scope by ruling, and the reason is recorded: the OCaml prototype was
deleted rather than promoted, and `docs/wayfinder/tickets/declare-bootstrap-compiler-interface-once.md`
is the standing ticket for the boundary a bootstrap would need. The regression trap is exactly what
one conformance suite exists to prevent — a language behaviour tested in exactly one place survives
a compiler rewrite; a behaviour tested in the compiler's own test harness does not.

## Threads worth reading in full

- **Langdev is O(n²)** (64/30) — the clearest statement of why feature interaction, not feature
  count, is the maintenance problem, and the case for the test suite as the primary artifact.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1gpe6ai/langdev_is_on²/
- **How the Pipefish compiler works: some highlights and lowlights** (23/4) - a maintainer's
  post-mortem on his own architecture: the loop he wishes he had made a chain, the missing IR he
  only identified in hindsight, and the fragmented single-source-of-truth he fights with getters.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1is7gst/how_the_pipefish_compiler_works_some_highlights/
- **Why not retain the AST?** (41/54) — the best-argued case *against* a staged pipeline, with
  54 replies (unretrieved) that would settle it.
  https://www.reddit.com/r/Compilers/comments/1w989ra/why_not_retain_the_ast/
- **Automatically pass source locations through several compiler phases?** (24/10) — the spans
  problem shown as code, before and after, with the helper functions it forces.
  https://www.reddit.com/r/ProgrammingLanguages/comments/1f2sx39/automatically_pass_source_locations_through/
- **Query-based compiler architectures** (121/12) and **Against Query Based Compilers** (103/29) -
  read as a pair; they are the same year's argument about the same architecture.
  https://www.reddit.com/r/ProgrammingLanguages/comments/hfs53y/querybased_compiler_architectures/ ·
  https://www.reddit.com/r/ProgrammingLanguages/comments/1rf9g7j/against_query_based_compilers/
- **How do you architect a compiler for a language with Lispy macros?** (15/23) — the maintenance
  and incrementality case against macro/phase interleaving, stated by someone who does not have to
  implement it.
  https://www.reddit.com/r/ProgrammingLanguages/comments/bycyif/how_do_you_architect_a_compiler_for_a_language/
- **SSA IR from AST using Braun's method and its relation to Sea of Nodes** (35/27) — the clearest
  account in the corpus of what fusing analysis into IR construction costs a reader.
  https://www.reddit.com/r/Compilers/comments/1ivgj5b/ssa_ir_from_ast_using_brauns_method_and_its/
- **Representing ASTs as byte strings with small integers rather than pointers** (24/51) — a
  complete, honestly-unmeasured design writeup of the most extreme data-representation choice here.
  https://www.reddit.com/r/ProgrammingLanguages/comments/79fkpu/representing_asts_as_byte_strings_with_with_small/
- **From evaluator to compiler, a true story** (43/6) — the best evidence for "build the evaluator
  first", including the oracle effect nobody plans for.
  https://www.reddit.com/r/ProgrammingLanguages/comments/18zya3b/from_evaluator_to_compiler_a_true_story/
- **de Bruijn indices** (37/29) — a practitioner's cost list for nameless binders, written by
  someone who chose them for a dependently typed language and is now paying.
  https://www.reddit.com/r/ProgrammingLanguages/comments/fwdkv1/de_bruijn_indices/
- **What I wish compiler books would cover** (146/36) — the syllabus gap: error messages,
  incremental compilation, LSP, and fuzzing, none of which the standard books teach.
  https://www.reddit.com/r/ProgrammingLanguages/comments/gavu8z/what_i_wish_compiler_books_would_cover/

## Gaps and disagreements

**Coverage improved and is still thin.** First, comment trees were fetched for the 20 richest
threads of this axis — 490 comments across 19 trees — but each fetch returns only ~30 top-level
comments regardless of `limit=100`, with the remainder left in unretrieved `more` placeholders (35
on the largest thread alone), so every reply cited above is the head of its thread and the
counter-arguments beneath it are unread. Second, several load-bearing sources are **link posts with
no body**: "Against Query Based Compilers", "Query-based compiler architectures", "Zig Is
Self-Hosted Now" and "Inside Zig's Incremental Compilation" are cited as the position their linked
URL states, and none of those four trees was among the twenty fetched — I did not fetch the URLs
either, no network. The visitor/Church thread was recovered from its comments (see that entry), and
"Land ahoy: leaving the Sea of Nodes" was listed in the slice with its tree marked not fetched —
its body is one line, not zero, and the argument lives in the v8.dev link and in replies I did not
get. Third, topics where I expected corpus threads and found none are listed below rather than
papered over.

**Where the corpus did not settle anything:**

- **Mutation testing, property tests and shrinkers have essentially no presence — and the comment
  pass did not change that.** Searching all 490 retrieved comments for mutation, shrink,
  QuickCheck, hedgehog and property-based returns nothing on compiler testing; the only fuzzing
  talk in the comments is why textbooks omit it ("fuzzing and testing exist in this strange area of
  software engineering as applied to compiler construction … too much other material to cover")
  plus one unanswered question asking whether compiler fuzzing differs from fuzzing any other
  program.
  One thread on grammar mutation for differential testing; nothing at all on mutating the
  compiler's own guards, nothing on property-based testing of a type checker, nothing on shrinking
  a failing program. The mutation-sweep entry above therefore rests on `fun`'s own ticket record,
  not on community evidence. To decide it independently you would want to read a mutation-testing
  study applied to a compiler, not Reddit.
- **Hash-consing is near-absent** — three low-score threads (32, 13 and 3 comments). It is listed
  because it is in scope and because the one substantive thread (recursive-tree equality) argues
  for it; treat the maturity tag as inference from adjacent practice, not from this corpus.
- **Decision trees vs backtracking for pattern matching has no thread, in the posts or the
  comments.** Maranget is named once, in a passing reference inside a hobby language's pipeline
  description; a search of the 490 retrieved comments for Maranget, decision tree and backtracking
  finds nothing. `fun` compiles to decision trees in `Fun.Kernel` and this document has no
  community evidence to weigh against that.
- **Content addressing** is one line inside an import survey plus a hobby project; the fog item in
  `docs/wayfinder/topics/content-addressed-codebase.md` is better evidence than anything here.
- **Bisecting a miscompile now has one worked example, read but only half retrieved.** "A super
  sneaky post SSA LICM bug that's not talked about much..." (20/28) is read: its own edit records
  what the thread settled — the bug "was found to not be LICM at all, it just happened to be the
  only pass that exposed the hidden flaw in my SSA reconstruction logic", credited to the
  commenters. The post body shows the whole mechanism (an invariant `temp = 0` hoisted out of a
  nested loop makes a phi that was dead become live, and `factorial(12)` returns 9 instead of 6),
  but the 28 comments that did the locating were not fetched, so the *method* is still unread. A
  second data point is in this corpus: a C compiler author implemented PDB/debug-info support
  before he implemented divides, because "it is very annoying to debug misscompilations without
  debug info" (comment on "My C-Compiler can finally compile real-world projects like curl and
  glfw!"). **Incremental re-elaboration and concrete pass ordering in real projects remain
  absent** — no thread, no worked example, in posts or comments. What the comments add is only
  adjacent: a request for "a modern take on Hoopl" because rewrite ordering is "still not really a
  solved problem in general", an anecdote about four optimizations where leaving out the fourth
  ruined the rest (the teller then reported he could not find the talk again, so it is
  unverifiable), and a defence of phase-order dependence itself — all three cited in the pass-order
  entry above.

**Where the community visibly disagrees** (each with both sides cited above): how many
representations of a program to keep (retain-and-annotate vs staged conversion vs none at all);
whether analysis belongs inside IR construction (Sea of Nodes vs Braun, with V8's retreat as a
third position); query-based incrementality (121 upvotes vs 103 upvotes, five years apart);
interleaving macro expansion with the rest of the pipeline (fun's decision vs the DAG objection);
whether calling the visitor pattern "Church encoding" is a useful identification or a stretch of
the word "pattern" (its thread's comments, both sides quoted in that entry); whether a Go host is
viable for a compiler (the post's draw vs three commenters who advise against it, one of them
writing a compiler in Go right now); and whether two
implementations are an oracle or a liability — which `fun` resolved by measurement and deletion, a
resolution the corpus's differential-testing threads do not address at all.

**What you would need to read to go further:** the linked blog posts and papers behind the link
posts above (ollef's query-based compilers post, matklad's rebuttal, the V8 Sea-of-Nodes post, the
Haskellforall visitor essay, Salsa's documentation, Zig's incremental-compilation internals); the
`more` placeholders in `impl-comments.md` for the nineteen threads whose heads are quoted here;
Maranget on pattern-match compilation for the decision-tree question; Trees that Grow for the spans
question; and a real compiler's pass-ordering code (LLVM's new pass manager, or Dylan's DFMC
`optimize.dylan`, which the pass-manager thread links directly).

## Dissent and corrections

- **Comment coverage corrected.** This document's first pass recorded zero comment trees for this
  axis; the figure that replaced it is in "How this was gathered" — 490 comments across 19 trees
  for the 20 richest threads, ~30 top-level comments per fetch, the rest unretrieved.
- **Corrected: the visitor/Church entry's maturity.** It read "research — argued in the linked post,
  with a 22-comment practical thread I could not read"; its own thread's comments were fetched and
  they disagree with each other, so it is now `contested` with both positions quoted.
- **Corrected: "leaving the Sea of Nodes" is not bodyless.** It carries a one-line body ("Feel free
  to ask if you have any questions"), so the body-only pass overstated it — and its comment tree was
  listed in the slice with a "not fetched" note, so its replies stay unread either way.
- **Corrected: the Go host-language entry's neutrality.** The post's draw (GC, cross-compilation)
  stands, but three commenters advise against Go for want of sum types and exhaustive matching —
  one of them from inside a Go compiler he writes today — and the entry now says so.
- **Unresolved with the material available:** what "Query-based compiler architectures", "Against
  Query Based Compilers", "Zig Is Self-Hosted Now" and "Inside Zig's Incremental Compilation" each
  settled in their replies. None of those four trees was fetched in any slice, no network was
  allowed, and their bodies are empty — so those entries still rest on the linked URL's position
  plus the score, exactly as the first pass said. The same is true, in smaller degree, of every
  thread cited above whose reply count exceeds what `impl-comments.md` retrieved.
- No entry was added or changed on the strength of an upvote, a joke, or an "this already exists in
  X" remark without detail; disagreement is recorded in the entries rather than used to delete
  them.

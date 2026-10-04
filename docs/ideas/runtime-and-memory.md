# Runtime and memory — how values live and what the evaluator may do

This axis is about the machine under the language: how memory is reclaimed (tracing, reference
counting, ownership, arenas, hybrids, nothing at all), how a program is executed (tree-walking,
bytecode, JIT, AOT, C), how a value is physically represented and passed (tagged words, boxed vs
flat, calling conventions, closures, tail calls), what the concurrency runtime looks like, and
what numerics and data primitives cost. Out of boundary: type-level ownership, linearity and
lifetimes as *type-system* ideas, which are `types-and-semantics.md` entry 28 — this doc covers
their runtime consequence only, and says so where the two meet. Interop's *design* side is
`modules-and-abstraction.md`; how many passes a compiler has and how a backend is chosen are
`compiler-architecture.md` (its entries "Backend: C, LLVM, or your own", "The native stack is not
the language's stack", "Interpreter first, then compile it in the interpreter's shape" overlap
this axis and are cross-referenced rather than repeated); effect rows, handlers and async colouring
are `effects-and-handlers.md`.

One distinction runs through the whole doc: `quill` is an evaluator with no backend, so the ideas
that matter *now* are the ones already decided inside it — one table of primitives, checked
integer edge cases, closures as ordinary values, `Kont` frames instead of native recursion, and
what `std/` puts behind `String` and `Tuple`. Value representation, layout, calling conventions,
collectors, interpreter dispatch, JITs and every scheduler are decisions that become due only
when a backend ticket opens. Each entry's Bearing line says which side it is on.

## How this was gathered

The corpus is 2538 threads from r/ProgrammingLanguages and r/Compilers, collected through Reddit's
own JSON endpoints, dated 2009–2026. This axis's slice holds 260 threads ranked by comment volume
× score × body length (2014-11 to 2026-09); I read the bodies of the highest-signal ones and
grepped `corpus.jsonl` for topics the ranking missed. Eleven of the threads cited below are not in
the slice and were read from `corpus.jsonl`: first-class strings, the string-model survey,
arrays-as-tuples, actors (Aether), goroutine-style M:N, Higher RAII, and five title-only threads
(the three tail-call posts, functional GPU programming, implementing arrays in a minimal ML) —
each is marked as outside the slice in its own Source line.

**Comment coverage**: comment trees were fetched for the 20 richest threads of this axis, yielding 518
comments, and ten of those trees are load-bearing below (manual memory, GC-later, Rust, virtual
memory, UB, the RC trap, untagged floats, C-vs-LLVM, closures-without-GC, M:N). The fetch is
shallow by construction: each tree is capped at ~26 top-level comments against a tree listed at
24–99 comments, with 68 replies left in `more` placeholders in the nulls thread, 6 in the Rust
thread and 9 in the lesser-languages thread. Twelve slice threads are link posts with no body at
all — I cite those as the position they point at and say so. And this is **what Reddit upvoted**,
not a survey of the field: popular ≠ correct, and several high-scoring threads here (Sric,
Nirdosha, Aether, Thiran, Stasis, Mismo) are self-promotion for hobby languages whose design
claims are unverified — cited only as evidence that an idea is being tried.

## The ideas

### Get the collector in before the language grows roots

**What it is.** If the language is going to have a collector, wire allocation and reference
discovery in first, before the implementation has a dozen ways to allocate and hide a pointer.
The claim, quoted from *Crafting Interpreters*: "The collector must ensure it can find every bit of
memory that is still being used… there are hundreds of places a language implementation can
squirrel away a reference. If you don't find all of them, you get nightmarish bugs. I've seen
language implementations die because it was too hard to get the GC in later."

**Buys.** Every feature added afterwards — closures, strings, objects, caches — allocates through
paths the collector already knows, so adding a feature never means re-auditing allocation. The
thread supplies the failure cases: Mono retrofitted Boehm's conservative collector as a
"temporary measure" and never got an accurate one in; Objective-C started with refcounting, tried
GC and went back; Python's refcount layout is now so load-bearing that replacing it with a
parallel-friendly tracing collector is "incredibly hard to change now".

**Costs.** The quote is about planning, not obligation, and the thread says so twice: a language
that picks ownership from day one "doesn't *need* a GC, and the quote doesn't apply". A retrofit
is expensive, not impossible — tdammers' version of the argument is narrower and better: GC needs
attachment points, and a codebase with "a dozen different ways of allocating memory" has to unify
them all first. One commenter notes the counterexample that survives retrofit: a language whose
request-scoped lifetime let it skip GC entirely (PHP).

**Maturity.** `shipped` as engineering guidance, but the evidence is anecdotal: named project
histories in a comment tree, not a study.

**Tried by.** Mono, Objective-C, Python, PHP (the last as the escape hatch that avoids the
question by having no long-running process).

**Source.** What are some examples of language implementations dying "because it was too hard to
get the GC in later?", score 135, 81 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1eeraq9/what_are_some_examples_of_language/
(2024-07) — 26 comments retrieved, including the Mono and Objective-C accounts and a quote of
Hertz & Berger's 2005 measurement (below). · Garbage Collection · Crafting Interpreters, score
135, 27 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/e41idc/garbage_collection_crafting_interpreters/
(2019-11) — link post carrying the original quote.

**Bearing on `quill`.** Today the question is the CLR's, not `quill`'s: the evaluator allocates `Kont`
frames, `Value`s and `Atom`s on the host heap and no part of `quill` can hide a managed reference
from the .NET collector. What must not be left late *inside the language* already isn't: mutation
is three heap effects `Alloc(h)`/`Read(h)`/`Write(h)` with `Discharge`, and there is deliberately
no merged `Mut`. Which collector a future backend uses is undecided and appears nowhere in the
wayfinder — genuinely new to the project.

### Reference counting against tracing: the corpus does not settle this

**What it is.** Two answers to "when does memory come back". Tracing walks live data periodically
and reclaims the rest; reference counting frees an object the moment its count hits zero, with
cycle handling bolted on separately (weak references, trial deletion, a tracing pass, or the user).
The strongest RC case in the corpus is Perceus-style counting for strict immutable languages: the
compiler inserts drops at last use, and immutability lets it do reuse analysis (FBIP/FIP) so a map
over a linearly used list mutates cells in place instead of reallocating.

**Buys.** RC: no global pause, cost attaches to visible ownership/last-use boundaries, and in a
pure language most counts can be elided or turned into in-place reuse — Lean 4's RC paper reports
beating GHC, ocamlopt and MLKit on everything tested. Tracing: cycles are a non-issue, compaction
fixes fragmentation, and the whole thing is tunable without touching user code.

**Costs.** Both sides are contested in the retrieved trees, and the contestation is the point.
RC's two structural problems: cycles need a second mechanism (the thread's own author wants one
"explicitly and statically checked" and has no idea how to express it), and a single decrement can
cascade through a whole tree — munificent: "decrementing that last reference can trigger a
cascade of dereferences… I wouldn't consider ref-counting itself a 'cure' for predictability."
The headline RC claim — predictable pauses — is rebutted directly by gasche: "In GCs if you want
to avoid pauses, you need an incremental GC. With RC if you want to avoid pauses, you need
incremental freeing… I don't see how one would be 'more legible' than the other." Counters also
pollute layout (the OP's own worry: packed arrays stop being packed) and go atomic across threads.
Tracing's cost side is measured in the same corpus: with 2× memory GC runs ~70% slower than
explicit management, 3× gives ~17%, 5× roughly parity (Hertz & Berger 2005, quoted in the
GC-later thread), and one commenter's rule of thumb in the manual-memory thread is "2x holds
pretty well… sometimes as much as 5x was required to meet performance parity".

**Maturity.** `contested` — and the split is by language shape, not by taste. For strict
immutable functional languages the RC side has working implementations (Lean 4, Koka, Glyph); for
mutable shared code the corpus shows no RC win, and Python's refcount-plus-GIL is called
"incredibly hard to change". Nobody in the retrieved comments claims a cycle-free mutable
language.

**Tried by.** RC: Python, Swift, C++ (`shared_ptr`), Lean 4, Koka, Glyph (a hobby ML, described by
its author), PHP (no cycle detector until v5/v7). Tracing: Erlang (its GC named as the reason one
poster doubts RC), Candy (the poster's immutable fiber language, weighing the two), and Java, the
CLR and V8 — Sun/Oracle's Java collectors, Microsoft's CLR collector and Google's V8 collector are
all named as successes in the retrieved manual-memory tree.

**Source.** Is reference counting a trap?, score 59, 67 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1uf6xc8/is_reference_counting_a_trap/
(2026-06) — 26 comments retrieved; this is where gasche, munificent and the Glyph author argue.
· Can reference counting really be as competitive with tracing? (In a mostly-functional
language), score 47, 41 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1anyq4j/can_reference_counting_really_be_as_competitive/
(2024-02) — the Perceus question stated as hesitation, body only. · Garbage Collection in
Languages with Immutable Types, score 35, 42 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/mh1l45/garbage_collection_in_languages_with_immutable/
(2021-03) — the same choice for an Erlang-like immutable language; body only. · Functional
Programming and Reference Counting, score 56, 34 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/i1s8m0/functional_programming_and_reference_counting/
(2020-08) — Lean 4's numbers, body only. · Are people too obsessed with manual memory
management?, score 154, 82 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/110gitm/are_people_too_obsessed_with_manual_memory/
(2023-02) — the named tracing collectors (Sun/Oracle's Java collectors, Microsoft's CLR
collector, Google's V8 collector) are in its retrieved tree.

**Bearing on `quill`.** Neither strategy applies today: values live on the host heap and the
language has no reclamation construct at all. The one ownership-shaped rule `quill` does have is
type-level and already decided — `Discharge` lets a definition's local heaps vanish at
generalisation, so an internal `Reference` leaves a pure signature, which is runST's condition met
by inference instead of a wrapper. Whether a backend counts or traces is undecided; the
type-level half of ownership (linearity, borrowing as types) is `types-and-semantics.md`
entry 28 and is not re-argued here.

### Hybrid: a collector plus an opt-out

**What it is.** Keep a garbage-collected default and offer one or more manual or counting
strategies alongside it, so pause-sensitive code has somewhere to go without leaving the language.
D is the named production example (collectable by default, manual allocation alongside, a
programmable collector that can be told not to run inside a critical section). The corpus's hobby
version proposes three explicit modes: reference counting (default), ownership without a compile
time borrow checker — checked at run time, panicking like an out-of-bounds index — and arena
allocation with a bump allocator and a live-object counter.

**Buys.** The hot path stops being hostage to the collector: arenas per frame or per request are
described in the retrieved trees as the standard game-server pattern ("allocate everything in it,
draw it, then drop the whole arena"; "an arena per incoming request… dropped after the response is
sent"), with the frame itself as the allocation unit ("A simple bump the pointer allocator can
work with it, as long as you track which frame you are working with"). GC's absence in those
regions is the point of the hybrid.

**Costs.** Two escape hatches are two languages: the hobby proposal itself concedes its modes
differ in syntax (`Tree` vs `Tree*` vs `Tree**`), its ownership mode is "a bit slower than in
Rust" because of run-time borrow checks, its arena mode needs either two pointers per object or
an arena-id-plus-offset, and a tagged pointer "is not portable". A mixed model also means every
type must work under every strategy — the question the D thread's poster asks his readers ("Is
there a catch that I'm missing here?") whose answers were never fetched. LXR (reference counted
pointers *inside* a tracing collector for pause reduction) shows the boundary between hybrid and
"advanced RC" is blurry — one commenter: "there is not even a clear distinction between a highly
advanced tracing GC and a highly advanced ref counting / ownership GC."

**Maturity.** `shipped` — D is a production language with this model. The three-mode proposal is
a hobby design, evidence of interest only.

**Tried by.** D; LXR (a research collector: reference-counted pointers to cut pause time); an
unnamed compiled-to-C language proposing RC + ownership + arena; Swift, whose later versions add
statically checked borrows over refcounted objects (`Span`, `~Escapable`) to keep RC traffic out
of hot loops — a hybrid of counting and borrowing.

**Source.** Why isn't D-style, hybrid memory management, with both GC and manual heap more
popular?, score 60, 53 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/r4x2do/why_isnt_dstyle_hybrid_memory_management_with/
(2021-11) — body only. · Hybrid Memory Management, score 34, 49 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1h80vre/hybrid_memory_management/ (2024-12)
— the three-mode design, body only. · Low-Latency, High-Throughput Garbage Collection, score 24,
7 comments, https://www.reddit.com/r/Compilers/comments/1er6p3c/lowlatency_highthroughput_garbage_collection/
(2024-08) — LXR, body only. · The borrow-over-RC remark is in the retrieved tree of Is reference
counting a trap? (2026-06).

**Bearing on `quill`.** Genuinely new, and further off than it looks: `quill` has exactly one kind of
mutable cell (`Reference`, branded by its `Heap`) and one reclamation story — the host's. A
language-level escape hatch from collection would have to coexist with `Alloc(h)`/`Read(h)`/
`Write(h)` and with `Discharge`, which currently assume every heap is collected or not-managed-at
-all; nothing in `docs/wayfinder` asks the question. Say plainly: this matters only at a backend.

### Single ownership and borrowing as the no-collector runtime route

**What it is.** Give every value exactly one owner; the compiler proves each borrow ends before
the value is freed, so memory comes back without a collector. The runtime consequence is that
deallocation is inserted at a proven point (RAII's destructor call), not discovered. The type-level
machinery (lifetimes, affine types) is `types-and-semantics.md` entry 28; what follows is what it
costs the *machine*.

**Buys.** No collector, no pauses, no counters in the hot path — and, as one commenter puts it,
the more interesting win: a whole bug class ruled out. Memory-safety-by-construction is why
"GC incurs a speed penalty" gets reframed in the same thread as a *control* penalty: you trade
when-it-happens, not correctness. Allocation also gets explicit: custom allocators are a library
feature, and Roslyn's C# compiler is quoted as passing structs by `ref` specifically to avoid the
GC.

**Costs.** Data structures that assume shared cyclic links fight the model: "you cant write the
efficient doubly linked list without using unsafe"; even a singly linked list is "very difficult to
write in Rust efficiently", and its destructor needs an explicit version to avoid a stack overflow
— deallocation with ownership or counting is recursive, so freeing a long chain is itself a deep
call. The whole-graph arena alternative reclaims nothing until the graph dies. And the corpus's
standing complaint is the human cost: syntax-heavy annotation, "systems programming in a way it
deems safe", complexity that a poster calls a dealbreaker. Fragmentation, the classic argument for
manual control, is disputed in the same thread: modern allocators plus typical usage show "NO
fragmentation overtime".

**Maturity.** `shipped` (Rust, C++ RAII, Swift's ARC as counted ownership) and `contested` as a
universal answer — the thread that asks whether it is *the* ultimate solution has 99 comments and
most retrieved ones say no, naming tracing for servers, counting for destructors-you-can-see,
Rust-style control for the hot path, and "sometimes… you can even get away with not freeing memory
at all and just bump allocating everything".

**Tried by.** Rust, C++, Swift, Zig; Mismo (a designer's five-part "bindings" system over
affine types, no collector); Hylo and ATS mentioned in the retrieved trees of the RC-trap thread
(2026-06); languages that tried it and stopped are not named here.

**Source.** Does Rust have the ultimate memory management solution?, score 24, 99 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/102ugt7/does_rust_have_the_ultimate_memory_management/
(2023-01) — 26 comments retrieved, 6 more unretrieved. · Are people too obsessed with manual
memory management?, score 154, 82 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/110gitm/are_people_too_obsessed_with_manual_memory/
(2023-02) — 26 comments retrieved; allocator control, Roslyn's `ref` structs, the fragmentation
rebuttal. · Ownership vs full immutability, score 72, 49 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/uxtcme/ownership_vs_full_immutability/
(2022-05) — cited as the corpus's other ownership thread; body only, not read in depth here. ·
Designing Mismo's Memory Management System: A Story of Bindings, Borrowing, and Shape, score 18,
16 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1lk0wai/designing_mismos_memory_management_system_a_story/
(2025-06) — body read; `var`/`let`/`ref` bindings and the design's growth into five parts.

**Bearing on `quill`.** `quill` has no linearity and no borrow checker, and the wayfinder does not
propose one — the type-level side is explicitly parked in `types-and-semantics.md`. What `quill`
already has is the ownership rule it actually needs for purity: a `Reference` that cannot escape
its definition loses its heap through `Discharge` at generalisation, so local mutation never
appears in a signature. Everything else here (inserted frees, recursive destructors) is a backend
fact and currently belongs to .NET.

### Arenas and regions: free an extent, not an object

**What it is.** Allocate into a bump pointer area and reclaim the whole area when a syntactic
extent ends, instead of collecting individual objects. Language 84 is the corpus's fully described
version: one area per `Do`, per `Begin … End` block, and per loop with no state variables — no
annotation, because the compiler derives the extents from syntax, using the fact that no value
escapes them. Mutable objects there are fixed-size "scratchpads" containing no references, so
nothing in them pins the area. In the Rust/game-server world the same idea is a library: an arena
per frame or per request, dropped wholesale.

**Buys.** Allocation is a pointer increment; reclamation is O(1) with no traversal, no counters
and no pauses. The design's own claim: "you can easily tell, by looking at your program's syntax,
where all memory-management operations happen" — determinism by construction, and "difficult to
write a good garbage collector… I'm hoping to skip that difficulty altogether." Commenters add the
pattern's reach: trees built out of array indices into one arena sidestep both the borrow checker
and pointer chasing.

**Costs.** Nothing whose lifetime exceeds the extent can be stored — Language 84 forces dataflow
through references-free scratchpads or files precisely to keep that true, which is a large
language-design constraint, not an implementation trick. Anything the arena holds stays alive until
the whole extent ends, so a per-request arena keeps every short-lived allocation until the response
sends. In a collected language the arena is invisible to the collector: "there's often no way to
tell the runtime not to traverse individual objects within the arena", and in a borrow-checked one
arena libraries fork the standard collections to work at all. Reserved-but-uncommitted OS memory
(a related trick from the virtual-memory thread) trades this for page-table and TLB-miss costs and
fails outright on 32-bit address spaces.

**Maturity.** `shipped` as a pattern (arena allocators in Rust and game engines, per the retrieved
trees; embedded practice); the *syntax-inferred* variant with no annotations is `research` —
Language 84 is a hobby language's release post.

**Tried by.** Language 84 (0.4 release), Rust arena libraries, game and web-server code described
by commenters, Vale (named in the hybrid thread), Aether (arena allocators listed in an actor
runtime's bullet points).

**Source.** Region-based memory management in Language 84, score 13, 24 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/68pmyv/regionbased_memory_management_in_language_84/
(2017-05) — the full mechanism, body read. · What do you get when you cross block structure with
arenas?, score 32, 20 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/113lg0j/what_do_you_get_when_you_cross_block_structure/
(2023-02) — title as position; body not read. · Replacing the Heap with a Compacting Stack
Allocator, score 54, 50 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/wccavi/replacing_the_heap_with_a_compacting_stack/
(2022-07) — title as position. · The per-frame/per-request quotes are in the retrieved tree of Is
reference counting a trap? (2026-06); reserve-and-commit is in the retrieved tree of How useful
can virtual memory mapping features be made to a language or run time?, score 26, 78 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1naux68/how_useful_can_virtual_memory_mapping_features_be/
(2025-09).

**Bearing on `quill`.** Beware the word: `quill`'s `Heap` is a type-level *brand* on references —
what `Read(h)` acts on and what `Discharge` may drop — not a bump area, and the glossary bans
`region` outright (region inference's word). Arena-per-extent is genuinely new to the project and
backend-only. The idea that *does* rhyme with `quill` is Language 84's: extents inferred from
structure rather than annotated, which is exactly how local-heap discharge already works.

### Deterministic destruction: run the destructor where you can see it

**What it is.** Tie reclamation to a syntactic event — a scope end, a statement end, a last use —
so release happens at a place the source shows, instead of whenever a collector gets around to it.
The construct is RAII: the compiler decides *when* by deciding where the value's lifetime ends,
including the awkward case of a value *returned* from a function, whose death is later than the
callee's frame.

**Buys.** Resources with side effects — files, sockets, locks, mappings — get released at a point
a reader can name, which is the argument the corpus keeps making against tracing: destruction is
"undeterministic and unpredictable. This is an issue for resources that need guaranteed cleanup,
like (memory mapped) files". It also makes reclamation cost local: the drop is where the code is.

**Costs.** The hard case is the one the corpus thread is stuck on: a returned or nested-call
temporary (`do_something(get_object())`) must outlive both frames, so the compiler needs statement
temporaries, a pending-drop list, or a delayed count decrement — C++ answers it with lifetime
extension to the end of the statement, which the poster found only by reading generated assembly.
Second cost: the drop itself can be deep. Ownership and counting both free chains recursively, and
the Rust thread reports an explicit destructor being needed to avoid a stack overflow when a list
dies. Third: whenever the compiler *cannot* see the end — a closure that escapes, a value stored
in a structure — deterministic destruction quietly stops being deterministic, which is why the
escape hatch in the closure threads is a destruct entry in a vtable.

**Maturity.** `shipped` — C++, Rust, and any counting language with `__del__`/`close`. The
returned-value timing question in the corpus is open *for the poster*, not for the field.

**Tried by.** C++ (statement-scoped temporaries), Rust (`Drop`), the hybrid language's counting
mode ("allows calling a custom \"close\" method, if needed"), Odin (whose position is that it
does not want destructors at all — "More accurate would be that Odin doesn't have or want
destructors").

**Source.** Object Lifetimes - how can a compiler determine when to call the destructor of an
object returned from a function?, score 23, 5 comments,
https://www.reddit.com/r/Compilers/comments/hufog0/object_lifetimes_how_can_a_compiler_determine/
(2020-07) — body read; the godbolt/RC-timing question. · The custom-`close` remark is in the body
of Hybrid Memory Management (2024-12). · Higher RAII, and the Seven Arcane Uses
of Linear Types, score 57, 24 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1crqhz3/higher_raii_and_the_seven_arcane_uses_of_linear/
(2024-05) — link post with no body; cited as the position, read from `corpus.jsonl`. · The
"guaranteed cleanup" argument and the recursive-free remark are in the retrieved trees of Is
reference counting a trap? (2026-06); the stack-overflow destructor remark and Odin's "no
destructors" are in the retrieved trees of the Rust thread (2023-01) and the closures thread
(2024-11).

**Bearing on `quill`.** `quill` has no destructors, finalisers or `close`: the only time-sensitive
runtime behaviour it owns is effect handling — deep, one-shot continuations, with
`HandledEffectEscapes` refusing to let a closure whose row names a handled effect leave the
handler's region. Resources are not modelled yet; if IO ever arrives, "who releases it and when"
is an effect question before it is an allocation question. New to the project, undecided.

### Conservative versus precise: does the collector get to guess?

**What it is.** A conservative collector treats every machine word that looks like a heap address
as a live pointer; a precise one only follows pointers it can identify from stack maps and type
information. The corpus's thread asks the exact trade question a hobby compiler faces: Boehm (BDW)
is a drop-in that already works, versus building a precise collector with LLVM shadow stacks or
per-target stack maps.

**Buys.** Conservative: no compiler integration, no stack maps, no per-platform work — "an
existing conservative collector is so much easier and still quite fast". Precise: no false
retention, and the collector can move objects (compaction, locality) because it knows which words
are references.

**Costs.** Conservative retains garbage whenever a non-pointer bit pattern aliases an address —
the OP quotes BDW's own claim of ~0.03% of allocated memory, and the unsigned-long thread is a
student discovering the same failure from the other side: an unrelated integer on the stack marks
a chunk reachable "and then it will not be collected even though it should be", which is a
retention bug you cannot see. Precise costs real work: stack maps built differently per target,
which the OP calls "the grizzly business", or a shadow stack updated at runtime. False retention
is not only a leak: a conservative scan is also why some payloads (integers stored where pointers
are read) must be encoded defensively — the flip side of the tagged-word problems below.

**Maturity.** `shipped` on both sides — Boehm in production C/C++ programs, precise collectors in
the JVM and CLR [general knowledge, not from corpus]; the corpus itself shows the question being
decided by a hobby author, with no retrieved answers (that thread has 5 comments, none fetched).

**Tried by.** BDW/Boehm (recommended in the thread); Mono, retrofitted onto Boehm and stuck there
(per the GC-later tree); LLVM shadow stacks and stack maps (enumerated as the alternative).

**Source.** Precise vs conservative garbage collectors, score 21, 5 comments,
https://www.reddit.com/r/Compilers/comments/1d2o0ut/precise_vs_conservative_garbage_collectors/
(2024-05) — body read; comments not fetched. · Tricking the garbage collector with unsigned
longs?, score 50, 32 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1124opt/tricking_the_garbage_collector_with_unsigned_longs/
(2023-02) — body read; the false-retention discovery. · The Mono retrofit is in the retrieved
tree of the GC-later thread (2024-07).

**Bearing on `quill`.** Backend-only and undecided. Note for the day it matters: an evaluator whose
values are .NET records is traced precisely by construction, and the parts a future collector
would have to see are exactly the parts `Kont` frames and `Value`s already are — managed objects.
Nothing in `docs/wayfinder` discusses collectors at all.

### Heapless: decide the memory budget before the program runs

**What it is.** No dynamic reclamation because there is no dynamic allocation: reserve everything
up front, give every resource a fixed upper bound, and never ask the OS for more. The corpus's
name for it is "heap less or tiger-style programming", described as standard embedded practice;
the concrete implementations are a language compiled to WASM with static memory allocation, and
the OS-level version — reserve a huge address range and commit pages only when touched.

**Buys.** Zero allocation at run time, so no allocator latency and no fragmentation, and a hard
bound you can prove: "this allows you to potentially batch allocate the whole program as one block
of memory ahead of time". For a WASM target it fits the platform, where memory is one contiguous
region that can only grow — the allocator problem shrinks because there is nothing to free.
Latency-critical code (audio, embedded, safety-critical) gets it by construction rather than by
tuning.

**Costs.** Every upper bound must be known and budgeted: no dynamic arrays, "instead every upper
bound must be budgeted ahead of time", and data that outgrows its budget is a redesign. The stack
version has its own problem, spelled out in the dynamic-stack thread: fixed offsets from the frame
pointer are what makes a frame cheap, and any variable-sized object destroys them — including the
return value, whose size may not be known until the last line. The virtual-memory version trades
allocation cost for page-table weight: bigger tables, longer walks on a TLB miss, mapping changes
causing shootdowns — and on a WASM-style heap you cannot shrink at all.

**Maturity.** `shipped` — embedded practice per the corpus's own description. Stasis is hobby
evidence of the language-level version, compiler "has many bugs".

**Tried by.** Embedded and safety-critical systems (described, not named); Stasis (WASM, static
allocation); Go-style guard-page stacks and reserve/commit allocators at the OS level; the
heapless proposal is one comment in the RC-trap tree.

**Source.** Stasis - An experimental language compiled to WASM with static memory allocation,
score 26, 9 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1mbf2b4/stasis_an_experimental_language_compiled_to_wasm/
(2025-07) — body read. · What's so bad about dynamic stack allocation?, score 26, 36 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1fvbyw5/whats_so_bad_about_dynamic_stack_allocation/
(2024-10) — body read; offsets, resizing, returns. · Making an allocator for a language targeting
WASM, what is the "good enough" algorithm?, score 21, 28 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1cq9t83/making_an_allocator_for_a_language_targeting_wasm/
(2024-05) — body read; the grow-only heap. · The tiger-style argument and the reserve/commit and
TLB remarks are in the retrieved trees of Is reference counting a trap? (2026-06) and How useful
can virtual memory mapping features be made to a language or run time? (2025-09).

**Bearing on `quill`.** Contradicted by the evaluator's shape, so worth saying plainly: NbE builds
closures, module values and `Kont` frames on a heap that grows with the program, and compilation
units are *reused* precisely because they are base-anchored rather than statically placed. The
compile-time half does look heapless — the prelude elaborates once and is carried as terms — but
that is cache, not allocation. Nothing in the wayfinder considers a static-memory backend; if one
ever appears it meets `Core term` and `Value`, both written for a managed heap.

### Memory safety with neither a collector nor a borrow checker

**What it is.** Keep the low-level features — pointers into host structures, manual-looking data —
and close the holes with targeted run-time checks and narrow static rules instead of a general
collector or lifetime system. Umka's recipe is the concrete one: forbid pointer casts that widen a
base type or reach a pointer through it; dynamically check array and string casts; forbid reading
pointers out of deserialised data; make a weak pointer dereference go through a checked cast that
returns `null` if dangling; and settle the returning-a-local question with a *simplified* escape
analysis — if a function never increments a reference count, everything stays in a conventional
stack frame, otherwise a reference-counted frame is used.

**Buys.** No collector pauses and no lifetime annotations, while still being able to claim "no
segmentation faults except bugs". The escape-analysis rule is the notable move: it gives the
returned-local guarantee with a single syntactic condition a one-pass compiler can check, instead
of Rust-style lifetime analysis or full Go-style escape analysis. Run-time checks are predictable
in cost (a cast, a bounds check) and localise failure to the operation that was wrong.

**Costs.** Checks are checks: the cast rules forbid legitimate shapes (renaming a pointer type
through a cast is no longer legal), the weak-pointer protocol is a convention users must follow,
and a run-time panic replaces what a static system would have rejected. The strong claims in this
corner come from hobby projects: Sric advertises memory safety with "no borrow checking, lifetime
annotations" and Nirdosha advertises *proofs* — free of use-after-free, data races, deadlocks and
integer/buffer overflow, with an SMT solver deciding bounds and a run-time guard only where Z3
cannot — but Nirdosha scores 0 with 12 comments and both are self-reported, with benchmarks the
corpus cannot check.

**Maturity.** `shipped` for the check-everything-at-the-seam approach (Umka is a used embedded
scripting language); `speculative` for the provable version (Nirdosha is research-stage,
single-person) and for Sric's marketing claim.

**Tried by.** Umka; Sric (transpiles to C++); Nirdosha (Rust + LLVM + Z3); Rust's safe subset,
quoted in the UB thread as having "a mathematical proof that the safe subset of the language
cannot invoke UB".

**Source.** Memory safety in Umka, score 19, 9 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/m2yf86/memory_safety_in_umka/ (2021-03) —
body read; the four mechanisms. · Sric: A new systems language that makes C++ memory safe, score
18, 50 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1l1fab2/sric_a_new_systems_language_that_makes_c_memory/
(2025-06) — body read; hobby self-promotion. · Nirdosha – a systems language proven free of GC,
races & deadlocks, score 0, 12 comments,
https://www.reddit.com/r/Compilers/comments/1vy7b7n/nirdosha_a_systems_language_proven_free_of_gc/
(2026-08) — body read; hobby self-promotion, cited as an attempted proof-based route. · Recent
trend towards more UB (?), score 75, 77 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/13usqwq/recent_trend_towards_more_ub/
(2023-05) — 26 comments retrieved; the Rust-safe-subset proof remark.

**Bearing on `quill`.** `quill`'s safety is all up front: the checker refuses, evaluation raises a
`FunException`, and an unported path raises `NotImplementedException("not ported yet: …")` so it
can never masquerade as a passing `error` case. There are no run-time casts to check because
there is no pointer type — `Reference` is opaque and heap-branded. This idea is not applicable
today; it becomes relevant only for a backend that exposes host memory.

### What a collector actually costs (and when it runs)

**What it is.** Not the choice above, but its bill: memory headroom, a trigger policy, pause
management, and the layout work collectors cannot do for you. The trigger question is a real
design surface the corpus finds under-documented — free-memory thresholds, elapsed time, an
explicit `System.gc()`, allocation since last run, stack size, or a combination.

**Buys.** A collector buys batch reclamation: dealing with dead memory "all at once" is cheaper
per byte than per-object freeing (one commenter's laundry analogy), and with enough headroom it is
essentially free — the measurement quoted in the GC-later tree says 5× memory roughly matches
explicit management, and 3× is ~17% behind. Modern
generational/concurrent collectors also never visit garbage during mark, which one commenter uses
to rebut the cache-thrashing story: moving and generational collectors "don't even visit garbage
at all", while *reference counting* must touch every shared object on every copy. Incremental
marking work is genuinely improvable: a 160-point post in this axis's slice claims 30% faster JS
collection from better scheduling maths.

**Costs.** Memory: the 2×–5× headroom rule of thumb, restated as "GC trades off space to achieve
time". Predictability: a fair summary from the same thread — "It's tightly coupled to everything
in your whole program… overhead peanut-buttered across all uses of non-primitive types… its
performance is unpredictable" — plus, on constrained hardware, roughly twice the DRAM to refresh
and unpredictable pauses on phones. Locality: compaction does *not* fix access order — "Moving
collectors pack objects near each other, but that's no guarantee that they will be packed *in the
order that they are accessed*" — so layout control still belongs to the programmer. And the
trigger policy is a permanent tuning knob: the corpus lists at least six trigger schemes with no
consensus, which is itself the cost.

**Maturity.** `shipped` — these are the operating costs of production collectors; the measurements
quoted (Hertz & Berger 2005) are a real paper, quoted secondhand in a comment tree.

**Tried by.** Java, C#, V8, Erlang, Azul's C4 (named in the virtual-memory tree) — all named in
the corpus as GC success stories; the trigger menu is what a hobby implementer is choosing among.

**Source.** When to trigger garbage collection?, score 40, 43 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1el3772/when_to_trigger_garbage_collection/
(2024-08) — body read; the trigger menu. · Are people too obsessed with manual memory management?,
score 154, 82 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/110gitm/are_people_too_obsessed_with_manual_memory/
(2023-02) — 26 comments retrieved; headroom numbers, the locality rebuttal, the GC critique. ·
Making JS Garbage Collection 30% faster by differential calculus, score 160, 16 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/yegnk7/making_js_garbage_collection_30_faster_by/
(2022-10) — link post, position only. · Distilling the Real Cost of Production Garbage
Collectors, score 44, 31 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/udt2pf/distilling_the_real_cost_of_production_garbage/
(2022-04) and Garbage collection with zero-cost at non-GC time, score 57, 32 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/15o1wxa/garbage_collection_with_zerocost_at_nongc_time/
(2023-08) — both link posts, positions only, comments not fetched. · The Hertz & Berger numbers
appear in the retrieved tree of the GC-later thread (2024-07).

**Bearing on `quill`.** Not to be confused with `quill`'s *evaluation budget*: that is how many
semantic steps the checker may spend while type checking — one budget shared by the checker and
macro applications, raisable per evaluation, spent by nothing at run time — and depth fuel was
rejected as the mechanism. Budget measures compute, not memory; a collector's headroom problem
begins only when there is a backend to have a collector, which there is not.

### Tree-walking to bytecode: the first performance wall

**What it is.** Interpret `Core` terms directly (walk the tree, dispatch on node kind, allocate a
result) until that is too slow, then compile to a compact instruction sequence and interpret
that. The corpus shows the move as a rite of passage: one thread's title *is* the result — "I
revamped my tree walking interpreter into a bytecode VM, and now I'm much happier with the
performance" — and the question thread reports a Python tree-walker taking 2 minutes for
`fibo(35)` and asks whether a bytecode VM in Python would even help.

**Buys.** Bytecode shrinks each step from a node subtree to a few bytes, flattens control flow,
and lets the interpreter keep the working set in a small loop over an instruction array; register
flavours further reduce instruction count. It is also the prerequisite for everything later:
profiling, tiering and a JIT all need a stable instruction representation.

**Costs.** You now have two representations and a compiler to maintain between them, plus bytecode
design decisions (stack vs register, constants vs immediates, instruction width) that the corpus's
threads are full of and that never end. The tree-walker's advantage — semantics in one place,
trivially extensible — is spent. And the win is not automatic: the Python question thread cannot
know in advance whether a VM *in Python* beats its tree-walker, and the register-vs-stack thesis
poster is explicitly hunting for literature because the answer is not obvious.

**Maturity.** `shipped` — the standard path; *Crafting Interpreters* made it the default sequence,
with the tree-walking half "very slow to the point that it's basically unusable" by its own
author's admission quoted in a separate thread.

**Tried by.** The 119-point thread's author (unnamed in a bodyless link post); Lox — the book the
register-vs-stack thesis builds on — and Charm, the builtins thread's own earlier tree-walker; and
the ~11K-line Nim JS engine ("Bali") whose interpreter was overhauled to a dispatch table
alongside two JIT tiers.

**Source.** I revamped my tree walking interpreter into a bytecode VM, and now I'm much happier
with the performance, score 119, 26 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1612opy/i_revamped_my_tree_walking_interpreter_into_a/
(2023-08) — link post, position only; comments not fetched. · Is implementing a bytecode
interpreter in an interpreted language (like python) worth it?, score 69, 37 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/gyf1l6/is_implementing_a_bytecode_interpreter_in_an/
(2020-06) — body read. · Looking for bibliography on register-based vs stack-based virtual
machines for an undergraduate thesis, score 25, 22 comments,
https://www.reddit.com/r/Compilers/comments/1srtwvn/looking_for_bibliography_on_registerbased_vs/
(2026-04) — body read; the comparison dimensions. · Tear it apart: a from-scratch JavaScript
runtime with a dispatch interpreter and two JIT tiers, score 47, 9 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1mducvv/tear_it_apart_a_fromscratch_javascript_runtime/
(2025-07) — body read; the modern end state.

**Bearing on `quill`.** `quill` is not a tree-walker over surface forms; it is a term evaluator over
`Core`, driven by an explicit frame stack — `switch (term)` for the term cases and
`stack.Pop()` for `Kont` frames — and it serves both checking and running (see
`compiler-architecture.md`, "One evaluator serves both checking and running"). A bytecode layer
would be a *second* engine with a differential-oracle problem ("Two engines give you a
differential oracle — or a second liability"), and no backend exists to hold it: `src/Quill.Cli`
still prints "the .NET port has no entry point yet" and exits 1. This idea matters only at a
backend, which is undecided and unscheduled.

### Interpreter dispatch: the loop is the program

**What it is.** In a bytecode interpreter, the fetch-decode-execute loop's branch is the
hottest instruction in the program, and there is a literature on making it cheaper: `switch`
dispatch, jump tables, computed `goto` (direct threading), tail-called handlers, profile-guided
handler ordering, and dynamic superinstructions that fuse hot sequences. The corpus's threads
cover the whole menu and its counter-questions: at how many opcodes does `switch` stop being the
best, and is an array of handler functions with a call per opcode faster or slower?

**Buys.** These techniques aim at the same thing — keep the instruction pipeline full by making
the dispatch branch predictable — and they are the entire gap between a naive interpreter and a
"shockingly fast" one without any machine code generation. The Bali thread even reports a
dispatch-table overhaul as part of a real engine's release.

**Costs.** Portability and readability: computed `goto` is a GCC extension, tail-call dispatch
needs guaranteed tail calls in the host, and threading models encode operands differently (the
Rust enum thread's dilemma: a `match` over opcodes with payloads makes every instruction the size
of the largest one, inflating the instruction stream). Opcode count compounds it — the builtins
thread counts ~60 extra opcodes just for its language's primitives, lengthening a `switch` that a
quoted source warns only becomes the bottleneck "with a sufficient number of case branches (a few
hundred or more)". And the literature is old: the state-of-the-art thread complains that nearly
all of it targets branch predictors from *circa* 2000.

**Maturity.** `shipped` — computed goto and threading are in production interpreters; CPython's
and LuaJIT's use of them is [general knowledge, not from corpus], while the corpus itself shows a
poster trying both and papers on threading models and handler ordering. Superinstructions and
handler ordering are `research`-grade refinements the corpus can only point at papers for.

**Tried by.** The poster of "optimize this bytecode interpreter more?" (computed goto, register
file, tail calls — still unhappy); the builtins-thread poster (a `switch` over opcodes, then
inlined bytecode); Bali (dispatch table); the register-vs-stack thesis (two interpreters built to
be measured).

**Source.** Is it possible to optimize this bytecode interpreter more?, score 43, 30 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/12o49eo/is_it_possible_to_optimize_this_bytecode/
(2023-04) — body read. · The heart of a VM, and how to do builtins, score 20, 23 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/18wfw1c/the_heart_of_a_vm_and_how_to_do_builtins/
(2024-01) — body read; `switch` vs jump table vs function array, opcode-count growth. · What is
the current research in, or "State of the Art" of, non-JIT bytecode interpreter optimizations?,
score 24, 10 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1dzjvz6/what_is_the_current_research_in_or_state_of_the/
(2024-07) — body read; superinstructions, threading, handler ordering, and the staleness
complaint. · Stack VM in Rust: Instructions as enum?, score 33, 57 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1f4ek8e/stack_vm_in_rust_instructions_as_enum/
(2024-08) — body read; uniform instruction width.

**Bearing on `quill`.** Has it differently: `quill`'s dispatch is a C# `switch` over `Core` term kinds
inside the frame-stack loop, and the operand stack is the `Kont` machine that already exists for
evaluation, not a bytecode stack. There is no instruction stream to thread, no handler ordering to
profile and no superinstructions to fuse — every one of these ideas presupposes a backend. What is
decided is the rule that shapes the loop: the evaluator never recurses on the native stack per
object-level call.

### Primitives: one declaration table, or opcodes, or library code

**What it is.** Decide where the operations no user can write live — `+` on machine integers,
string equality, `panic`. Three answers from the corpus: hardcode special types and values into
the runtime; export them as items linked to the host; or write them in the language like user
code. A fourth, from the builtins thread: inline each builtin as generated bytecode, which is
macro expansion applied to the instruction stream.

**Buys.** One table (name, type, how it reduces) means the checker and the evaluator read the
same declaration and cannot disagree about a primitive's type — a single point of truth, and the
reason a primitive's failure behaviour can be part of its declaration rather than scattered. Host
linking gives performance for free; in-language definitions give the language a self-hosted
core.

**Costs.** Every primitive that becomes an opcode lengthens the dispatch switch (the builtins
thread: +60 opcodes for its core functionality, more as types grow) and pins the bytecode format.
Host linking means the runtime's ABI and the host's type layout leak into the language. In-language
definitions mean the bootstrapping problem: something must exist before the language can describe
it — the corpus's author solved it by generating and substituting bytecode, and admits being "not
wedded to the solution".

**Maturity.** `shipped` — the corpus poses the choice and does not resolve it (that tree was not
fetched); all three variants are described as things posters have built.

**Tried by.** The posters of both threads (a map from names to host functions in a tree-walker,
then an array-of-functions or inlined bytecode in a VM); `quill` ships the one-table design.

**Source.** How do you implement primitives?, score 50, 41 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/tcf95q/how_do_you_implement_primitives/
(2022-03) — body read; the three options; comments not fetched. · The heart of a VM, and how to
do builtins, score 20, 23 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/18wfw1c/the_heart_of_a_vm_and_how_to_do_builtins/
(2024-01) — body read; the inlining route and its opcode cost.

**Bearing on `quill`.** Already has it, by name: `Primitives.Declarations` is the one table of
primitives — name, type, reducer — and the elaborator's primitive types and the evaluator's
reductions both derive from it, so "nothing else lists primitives"
(`unify-primitive-declaration.md`). The base context binds each named primitive as a defined
entry. The vocabulary is settled too: `quill` calls them *primitives* and the glossary bans
`builtin`, `intrinsic` and `native`. A primitive's failure behaviour rides on the declaration
itself, which is what division by zero needed.

### Emitting C: inherit a toolchain, its ABI, and its undefined behaviour

**What it is.** Lower to C source and let an existing C compiler do code generation,
optimisation, debugging info and platform support. The corpus's version is a stage-1 compiler
already doing this, weighing C against LLVM for a stage 2 — and the wider thread asking why LLVM
became the default over GCC at all.

**Buys.** Everything free: the C ABI (calling conventions, varargs, struct passing, linking)
comes with the compiler rather than being implemented; every platform's optimiser; human-readable
output you can debug; and fast builds are possible if you pick the fast C compiler — one measured
example compiles a 40k-line app in 0.10s natively, 0.33s through the C route with tcc, against
51s invoking `gcc -O3`. Two posters left LLVM for exactly this shape (one now emits CIL during
development and C99 for deployment, "another factor 2 of performance").

**Costs.** You inherit C's semantics: undefined behaviour in your generated code becomes your
bugs, and the thread record in `compiler-architecture.md` adds that you also own a collector
unless your intermediate form already has one. You pay the C compiler's optimisation time when
you want optimisation (the same measurement: `gcc -O0` at 6.5s, `-O3` at 51s), and you depend on
an external toolchain that must exist wherever your compiler runs. LLVM, the alternative, is
described by a maintainer as "slow at codegen", not helping with the C ABI, and "easy at first,
then increasingly harder" — with several projects said to be eyeing other options.

**Maturity.** `shipped` — both routes are in production (LLVM: Rust, Zig and Odin named as LLVM
users in a retrieved comment; C: TCC-backed pipelines and the transpilers listed under Tried by).

**Tried by.** The thread's OP (stage 1 already targets C), Oberon+ (CIL then C99), Flint
(transpiles to C, per `corpus.jsonl`), Sric (emits readable C++), Aether (targets "readable C
code").

**Source.** C or LLVM for a fast backend?, score 39, 65 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/13y6nol/c_or_llvm_for_a_fast_backend/
(2023-06) — 26 comments retrieved; the build-time measurements and the "LLVM is slow at codegen"
account. · Why don't more new languages compile with GCC instead of LLVM?, score 83, 49 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/11edmdj/why_dont_more_new_languages_compile_with_gcc/
(2023-02) — body read; the default-question. · Challenges writing a compiler frontend targeting
both LLVM and GCC?, score 59, 36 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/11kxwql/challenges_writing_a_compiler_frontend_targeting/
(2023-03) — title as position; body not read.

**Bearing on `quill`.** The choice itself is `compiler-architecture.md`'s entry "Backend: C, LLVM,
or your own — and what a second backend costs" — not repeated here; what this axis adds is the
runtime consequence: emitting C hands `quill`'s calling conventions, its ABI and its undefined
behaviour to a C compiler, and either adopts Boehm or makes `quill` own reclamation. Undecided and
unscheduled; the fog item that must be settled first is the library-vs-compiler-machinery boundary
(UFCS, FFI), and interop's design half is `modules-and-abstraction.md`.

### AOT by default, JIT only when you must

**What it is.** Compile ahead of time for predictable performance; add a JIT only under conditions
you cannot get around. The corpus puts the sharpest version in a quote from GraalVM's Thomas
Wuerthinger: "I would recommend JIT only if you absolutely have to use it" — the two "have to" cases being an
unknown target platform (AOT fixes you to a microarchitecture) and a language too dynamic to
compile well ahead of time.

**Buys.** Predictability and footprint. AOT gives fast startup (5 ms for a Java app, per the
quote), lower memory — "all this metadata you need to later just in time compile at runtime takes
up a lot of memory" — and no deoptimisation surprises: modern JITs profile early and rarely
recompile, so early behaviour predicts the whole run, badly. Build machines are cheaper than
production machines, so AOT's build-time cost is the cheap one.

**Costs.** The JIT's case is real: unknown hardware, dynamic languages, and the original claim in
this axis's threads — JIT can outperform AOT "once they've done their initial warm-up and
profiling", dropping unreachable branches and inlining without growing the binary. The same OP
claims JIT+compact-GC beats manual+AOT for business/server work; the rebuttal in that thread's
tree: "I'd like to see the many cases" — "practically speaking due to their typically much lower
optimization budget, they don't tend to, on statically typed languages." Profile pollution is the
JIT's structural cost: the same application "you run it twice and the peak performance is
completely different", and an uncommon case can run specifically slow because the optimiser tuned
for the common one.

**Maturity.** `contested` — the default is argued, not settled. The AOT side has a practitioner's
detailed account and shipped GraalVM Native Image; the JIT side has every major dynamic-language
engine and the OP's performance claims. The corpus does not benchmark them against each other.

**Tried by.** AOT: GraalVM Native Image, any compiled language. JIT: V8, HotSpot, JavaScriptCore
(named in the quotes), plus the hobby tiered engines below.

**Source.** "I would recommend JIT only if you absolutely have to use it" - The Future of Java:
GraalVM, Native Compilation, Performance – Thomas Wuerthinger, score 46, 4 comments,
https://www.reddit.com/r/Compilers/comments/1rcwkwb/i_would_recommend_jit_only_if_you_absolutely_have/
(2026-02) — the quoted interview, body read. · What would a programming language designed
specifically for simple/fast/thorough JIT compilation look like?, score 39, 44 comments,
https://www.reddit.com/r/Compilers/comments/1b0p627/what_would_a_programming_language_designed/
(2024-02) — body read; the JIT-first design question. · Are people too obsessed with manual
memory management?, score 154, 82 comments (2023-02) — the JIT-vs-AOT claim and its rebuttal in
the retrieved tree (permalink above, under the ownership entry).

**Bearing on `quill`.** Genuinely new to the project and, today, meaningless: `quill` runs programs
by evaluating them, `src/Quill.Cli` has no entry point, and nothing in the wayfinder discusses code
generation at all. One clarification worth keeping for the day a backend arrives: the checker's
*evaluation budget* — semantic steps spent while type checking, shared with macro applications —
is a compile-time device and has nothing to do with compiling code at run time.

### Tracing JITs versus method JITs: the corpus's one settled-looking argument

**What it is.** Two ways to specialise at run time: record a hot path's execution and compile the
trace (tracing), or compile a whole method/function at a threshold (method/tiered). The thread
frames it as a reversal of a 2010 prediction that tracing had won.

**Buys.** Tracing sees across calls and optimises the actual hot path, including superinstructions
of execution rather than of code; method compilation sees the whole function, profiles once, and
composes with baseline tiers. The end state every major JS engine chose: a fast interpreter plus
one or more JIT tiers (Ignition/TurboFan, LLInt/Baseline/DFG/FTL, Baseline/IonMonkey,
Simple/Full).

**Costs.** Tracing's cost, per the thread's framing: compilation time on the recorded path,
guards everywhere, and abandonment — Firefox "left TraceMonkey far behind", later dynamic-language
JITs (Julia, Pyston) avoided tracing, and by 2017 LuaJIT had 27 commits in a year while PyPy
survives only by being *meta*-tracing. Method compilation's cost is the one Wuerthinger describes:
warm-up, profile pollution and metadata footprint. Neither is free to add: a tiered system means
two compilers, a tiering policy and a deoptimisation path.

**Maturity.** `contested` in name only: the thread asks whether method JITs won, and the evidence
it marshals — engine after engine converging on interpreter-plus-tiers — favours the method side;
the counter-evidence is that LuaJIT and PyPy still exist and that nobody in the thread posts a
benchmark. Call it `contested`, method side ahead on shipped evidence.

**Tried by.** V8, WebKit, SpiderMonkey, ChakraCore (method/tiered); LuaJIT (tracing), PyPy
(meta-tracing); Bali, the ~11K-line Nim engine (baseline + midtier with its own IR).

**Source.** Have tracing JIT compilers lost?, score 26, 10 comments,
https://www.reddit.com/r/Compilers/comments/7pf8b1/have_tracing_jit_compilers_lost/ (2018-01) —
body read; comments not fetched. · Tear it apart: a from-scratch JavaScript runtime with a
dispatch interpreter and two JIT tiers, score 47, 9 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1mducvv/tear_it_apart_a_fromscratch_javascript_runtime/
(2025-07) — body read; the hobby end-state. · How do JIT compilers actually jump to the code they
write?, score 53, 30 comments,
https://www.reddit.com/r/Compilers/comments/1u9sxau/how_do_jit_compilers_actually_jump_to_the_code/
(2026-06) — cited as the mechanics thread; body not read here.

**Bearing on `quill`.** Not applicable, and worth saying so: no instruction stream, no profiles, no
tiers. The nearest thing `quill` has to "compilation at run time" is macro expansion during
elaboration, which is under the evaluation budget and produces syntax, not code. If a backend
ever appears, this is where its second and third years go.

### Packing a value into one machine word: NaN boxing, pointer tagging, and their rivals

**What it is.** In a dynamically typed runtime, every value must fit one word. NaN boxing parks
tags in the unused payload of IEEE doubles (only NaN patterns are free); pointer tagging parks
tags in low alignment bits or high address bits; the corpus's rival scheme skips tagging by
*making the encodings disjoint* — flush denormals to zero and adjust return values so no legal
float's representation collides with any legal pointer, leaving pointers and floats untouched as
they move. A fourth trick from the same threads: compress two words into one (a 39-bit object
pointer plus a 25-bit vtable pointer, using alignment, the way the JVM compresses oops).

**Buys.** One-word values mean a uniform `Value` slot, no heap allocation for numbers, one-word
moves and one-word slots in structures — "the only number representation: `double`s" becomes a
design feature rather than a compromise. NaN boxing is battle-tested: JavaScriptCore, SpiderMonkey
and LuaJIT are named as doing it (or its nun-boxing variant) in a retrieved comment. The untagged
scheme's specific prize is a conservative collector's dream: "any C/C++ compatible garbage collector
like Bohem or MPS will know how to follow the pointer whether it's in registers or the stack or
stored without having to be told how to unpack them".

**Costs.** The bit budget is the whole argument. NaN boxing gives pointers roughly 47 usable bits
and the OP worries about architectures heading the other way; a retrieved reply confirms the
worry from the other side — Intel/AMD and RISC-V "are starting to extend the typically 48 bit
address space to 57 bits", so "don't design your language around the assumption that 64 bit
pointers are actually 48 bit pointers". The untagged scheme's cost is numeric: flush-to-zero
kills gradual underflow, and the replies give the losses in detail — exact subtraction of close
floats, compensated summation, reliable scalar products — plus reproducibility complaints about
Intel's non-standard defaults. Its own author concedes it "is almost guaranteed to be only as fast
as NaN boxing". Fat-pointer compression adds a hard placement constraint: vtables must live in one
contiguous span, at odds with ASLR and linking.

**Maturity.** `shipped` for NaN boxing and pointer tagging (JSC, SpiderMonkey and LuaJIT named in
a retrieved comment, V8 and Smalltalk in the CPython body); `speculative` and `contested` for the
untagged float scheme — argued in one thread, no implementation named.

**Tried by.** LuaJIT, JavaScriptCore, SpiderMonkey (nan/nun-boxing, named in a retrieved comment);
Smalltalk (tagged pointers, "even Smalltalk used it in the 80s", from the CPython thread); the
untagged scheme's poster (Ludus) and its critic (imachug, who calls the trade-offs not meaningful);
the M:N goroutine language claims NaN-tagging in 2.5k lines of C.

**Source.** NaN Boxing, Pointer Tagging, and 64-bit pointers, score 40, 28 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/whwe4g/nan_boxing_pointer_tagging_and_64bit_pointers/
(2022-08) — body read; the 47-bit worry. · You don't need tags! Given the definition of ieee 754
64 bit floats…, score 58, 51 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1nd8lw6/you_dont_need_tags_given_the_definition_of_ieee/
(2025-09) — 26 comments retrieved; the scheme, the numerics rebuttal, the conservative-GC prize,
and JSC/SpiderMonkey/LuaJIT as prior art. · Compressing fat pointers into 64 bits, score 44, 24
comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/l38m3u/compressing_fat_pointers_into_64_bits/
(2021-01) — body read; the 39+25 split. · The 57-bit remark is in the retrieved tree of the
virtual-memory thread (2025-09); the tagged-integer and Smalltalk remarks are in the body of the
CPython thread (2025-11).

**Bearing on `quill`.** Not available, and for a structural reason worth naming: NaN boxing
presupposes a floating-point type, and `quill` has none — `AtomTy` is `I64, Unit, Char, String,
Scopes, Absurd`, with no `F64` anywhere in `Primitives.Declarations`. What `quill` has instead is
one word per literal on a managed heap: `Atom.I64(long)` is a record, boxed like every other
value, and the CLR decides tagging if any. Representation is a backend decision; the language-level
decision it must not contradict — one number type, checked — is already made.

### Uniform boxed values versus flat, unboxed layouts

**What it is.** Two ends of one axis: represent every value as a heap object with a type tag
(CPython's model), or lay values out flat and unboxed where the static type allows (value types
in C#/Java VMs, unboxed fields in a compiled struct). The question is really "what does a VM's
stack hold, and when does a value have to become a box?".

**Buys.** Uniform boxing is the simplest possible runtime — one `Value` shape, one copying rule,
one equality — and CPython shows the mitigations: statically allocated small integers, a free
list, and a pool allocator over a 1 MB arena carved from an `mmap`, so allocation rarely reaches
`malloc`. Flat layouts buy cache density: arrays of unboxed integers are the difference between a
tight scan and a pointer chase.

**Costs.** The CPython post is the cost statement: *every* integer is a heap-allocated
`PyLongObject` with no tagged-pointer optimisation, so "the fast and likely path (using a regular
sized integer) is pessimized by the slow and unlikely path (using a really big integer)" — and
boxing every integer is called out as bad for performance. Uniformity also has a floor price: a
tagged union of all types pads to the largest member — the C VM thread's `Value` struct lands at
16 bytes with explicit padding, or 9 packed with alignment sacrificed — "and that would throw off
the alignment and slow shit down". Flat layouts cost you the uniformity: every operation now needs
the static type, which is why the limitations thread describes the CLR as supporting value types
"by accessing types at run-time" and lists "avoid keeping/passing/synthesizing types at runtime"
as an unsolved constraint.

**Maturity.** `shipped` both ways — CPython (boxed), CLR and JVM value types (partially flat),
LuaJIT/V8 (tagged, in between). The corpus argues the trade; it does not dispute that both ship.

**Tried by.** CPython, CLR (`struct`), JVM, the C-VM poster weighing union-plus-tag, C# value
types asked about in the "how are value types implemented in VMs" thread.

**Source.** How often does CPython allocate?, score 27, 9 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1ooesjp/how_often_does_cpython_allocate/
(2025-11) — body read; the allocation mitigations and the boxing critique. · writing a bytecode VM
in C, and curious as to how runtime types are handled, score 19, 46 comments,
https://www.reddit.com/r/Compilers/comments/1q4y9vj/writing_a_bytecode_vm_in_c_and_curious_as_to_how/
(2026-01) — body read; the 16-byte `Value`. · How are value types implemented in VMs?, score 21,
12 comments, https://www.reddit.com/r/ProgrammingLanguages/comments/10tmxt8/how_are_value_types_implemented_in_vms/
(2023-02) — body read. · Limitations on memory representations, score 16, 34 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/ac2k25/limitations_on_memory_representations/
(2019-01) — body read; the constraint list.

**Bearing on `quill`.** Has it differently: `quill` is uniformly boxed *by its host* — every `Value`
and `Atom` is a .NET record, every literal a heap object — and there is no static-type-driven
unboxing anywhere in the evaluator. What the language does decide today is the *type-level*
product: `Tuple(n, T1, …, Tn)` is the flat product with `tuple_arity(n)` computing its arity, and
`*` is only multiplication (`I64 * Bool` as a type is rejected). Flat runtime layouts for those
tuples are a backend question; the type-level answer is decided.

### Calling conventions are a language decision wearing an implementation costume

**What it is.** How arguments and results travel: which registers, how many, what the callee may
clobber, what happens to a struct that does not fit. The corpus's thread treats the standard ABI
as a set of *choices* rather than a fact of nature — per-function conventions with exactly the
register count a function needs, arguments and results in opposite directions so callers can reuse
their own registers, and a clobber log consulted instead of evacuating everything.

**Buys.** Measured wins from that thread: raising argument registers from 8 to 16 made a benchmark
run 2× faster; returning in the same registers that pass arguments beat `sret` on an M1, where
"loads and stores are incredibly expensive… returning values via sret while 30 registers might be
unused is crazy". Getting a conventional ABI right at all is worth a release: one compiler spent
six months adding System V ABI support.

**Costs.** Every departure from the standard convention is an FFI problem: interop means speaking
the *callee's* convention, and the corpus's binding threads are literally titled "A chaos of
calling conventions" — bind a library and you inherit a different convention per library and per
platform (the same is true of varargs and of how structs pass). A bespoke convention also
interacts with register allocation: the more registers you dedicate to arguments, the fewer the
callee keeps; clobber knowledge only helps if call targets are known. And there is a literature
gap the corpus names: a 9-comment thread asking for *any* survey comparing conventions.

**Maturity.** `shipped` — SysV and AArch64 ABIs are the ground every FFI stands on; the
per-function-convention ideas are `research` (one poster's benchmarks, unreproduced).

**Tried by.** The "Calling conventions" poster (AArch64, custom conventions); a Go-flavored
Pascal compiler binding Raylib; a compiler that took six months to add System V; Cranelift
(passing `extern "C"` structs, per the slice).

**Source.** Calling conventions, score 42, 26 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/vitqlq/calling_conventions/ (2022-06) —
body read; the register-count and `sret` measurements. · Are there any articles out there
summarizing and comparing different calling conventions?, score 39, 9 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1ho5lrj/are_there_any_articles_out_there_summarizing_and/
(2024-12) — title as position. · Raylib bindings for my Go-flavored Pascal compiler: A chaos of
calling conventions, score 34, 7 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/fm09zx/raylib_bindings_for_my_goflavored_pascal_compiler/
(2020-03) — title as position. · After 6 months of doing literally nothing, I finally added
System V ABI support to my compiler, score 24, 0 comments,
https://www.reddit.com/r/Compilers/comments/1hg626g/after_6_months_of_doing_literally_nothing_i/
(2024-12) — title as position.

**Bearing on `quill`.** No ABI exists, because no code generation exists. Two boundary notes for
when one does: interop's *design* side (what an FFI looks like to a programmer) is
`modules-and-abstraction.md` and the recorded fog item on the library-vs-compiler-machinery
boundary (UFCS, FFI); and `quill`'s own calling convention today is C#'s — every evaluator helper
call, every `Kont` frame push, is a host stack frame with a host convention.

### Closures: who owns the captured environment

**What it is.** A closure pairs code with the environment it captured. The representation decision
is where that environment lives and who frees it: heap-allocated and collected (JS, Java), a
compiler-synthesised structure of exactly the captured fields with no allocation at all (C++),
a statically checked borrow into the enclosing frame (Rust), or a discriminated function type
where only non-escaping closures are free (ATS's three kinds: non-capturing, GC-managed,
manually-managed with a linear type guaranteeing the free).

**Buys.** Capturing makes functions composable — callbacks, iterators, hooks — and the C++ route
shows the cheap end: a closure "essentially creates a new type for each closure which is the right
size to store captured variables", so common closures never touch the heap. The stack-only variant
(the reason Odin is asked about) is cheaper still: a closure that only travels up the call chain
can keep its captures in the frame and die with it.

**Costs.** Odin's spec line is the argument against: "Odin only has non-capturing lambda
procedures. For closures to work correctly would require a form of automatic memory management
which will never be implemented into Odin." The thread's title is the question, and the retrieved
answers say the premise is wrong — C++ and Rust have closures and no collector — but they pay for
it: capture *mode* becomes programmer-visible (copy, move, by-reference), because without a
collector or a lifetime system only the programmer knows whether a capture may outlive its
source; "you are the borrow checker. Only you, the programmer". Capturing by reference is where
C++ closures "don't work correctly" (a retrieved comment): escape a reference capture and the
closure dangles. The manual-management variant needs a second entry point — "one virtual function
to invoke the closure and another to destruct it" — and the GC variant needs the collector. Whole
programs can also often delete closures entirely, which is why closure elimination is a paper
topic.

**Maturity.** `shipped` — closures without a collector exist (C++, Rust, ATS), closures with one
are everywhere (JS, Java, Go); Odin's refusal is a shipped position, not a theorem. The claim
*closures require AMM* is answered by counterexample in its own thread.

**Tried by.** C++, Rust, ATS (three function kinds), Odin (refuses), JavaScript, Java, `quill`
(closures are ordinary values).

**Source.** can capturing closures only exist in languages with automatic memory management?,
score 42, 60 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1gplj9l/can_capturing_closures_only_exist_in_languages/
(2024-11) — 26 comments retrieved; C++/Rust/ATS counterexamples and the capture-mode argument.
· Can a language without automatic memory management have closures?, score 30, 31 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/wnroii/can_a_language_without_automatic_memory/
(2022-08) — body read; the stack-only proposal. · The Cost Of a Closure in C, score 69, 20
comments, https://www.reddit.com/r/ProgrammingLanguages/comments/1pk7hbz/the_cost_of_a_closure_in_c/
(2025-12) — link post, position only. · Looking for a paper about whole-program closure
elimination, score 27, 11 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1hxcsms/looking_for_a_paper_about_wholeprogram_closure/
(2025-01) — title as position.

**Bearing on `quill`.** Already has it: a closure is an ordinary value (`Value.VLam` over a
`Closure` of environment and body), there is no non-capturing variant, and the runtime question
"is the capture still alive" does not arise on a managed heap. The one capture question `quill`
*did* have was identity, and it was ruled and fixed —
`closure-capture-identity-two-answers.md`. Modules are the same mechanism by design: a module
value "captures the context it was written in, like a closure". Who frees a capture is the CLR's
affair until a backend says otherwise.

### Tail calls and deep recursion: what the runtime promises about frame growth

**What it is.** Two faces of one promise. Tail call elimination lets a recursive call reuse the
caller's frame, so tail recursion runs in constant space; tail recursion modulo cons extends that
to the common `f(x) :: rest` shape so `map` stays stack safe without an accumulator. The wider
form is not recursing at all: an explicit frame stack (or a state machine over a flat structure)
so object-level calls never consume host stack. `compiler-architecture.md`'s "The native stack is
not the language's stack" covers that mechanism; this entry is about the *promise* and what it
costs.

**Buys.** Stack safety for the shape every functional program writes: with TRMC, `map` "would be
stack safe… and likely even faster than non-TRMC versions because they would likely need
reversing". Guaranteed TCO is what lets a language write loops as recursion without a loop form —
and it is listed alongside precise GC, low-cost C FFI and value types as one of the features a
memory representation must somehow accommodate all at once.

**Costs.** The corpus's own record of refusal: PureScript decided *against* TRMC "because they
have other ways to trigger stack safe recursion"; Haskell gets the same shape for free from
laziness, which does not help an eager language; and the idea, born in the 1970s, still has almost
no eager-language adopters, which suggests real implementation friction (the poster guesses
"preventing other optimizations, creating memory problems"). Where TCO is absent, deep recursion
is a host-stack problem with a host-stack failure: C segfaults, `-O2` turns infinite recursion
into a skipped loop, and a compiler can inherit the same exposure through monomorphisation (the
"handling pathological recursion" thread's own compiler loops forever on an infinite type).
Forced non-tail recursion elsewhere has its own bill: freeing a linked structure recursively needs
an explicit destructor to avoid a stack overflow.

**Maturity.** `shipped` for TCO (Scheme and the ML family generally are [general knowledge, not
from corpus]; OCaml gained TRMC via the PR the corpus links) and for the frame-stack alternative;
`contested` for whether a language should promise TCO at all — the corpus records the
"limitations" thread asking whether guaranteed TCO can coexist with precise GC and a cheap FFI,
and a PureScript refusal as a deliberate "no".

**Tried by.** OCaml (TRMC added), PureScript (declined TRMC), Elm (evaluating it), C (tail calls
as an optimisation, with a thread documenting a shape it cannot handle), WebAssembly (tail calls
as a proposal, per title).

**Source.** Which languages support Tail Recursion Modulo Cons?, score 44, 20 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/sasrrt/which_languages_support_tail_recursion_modulo_cons/
(2022-01) — body read; the adoption record and PureScript's refusal. · An instance where a tail
position call in C cannot be tail call optimized, score 28, 4 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/9yanx7/an_instance_where_a_tail_position_call_in_c/
(2018-11) — link post with no body, outside the slice; read from `corpus.jsonl`. · WebAssembly
tail calls, score 12, 1 comment,
https://www.reddit.com/r/Compilers/comments/12dy4oz/webassembly_tail_calls/ (2023-04) — link
post with no body, outside the slice; read from `corpus.jsonl`. · Handling pathological recursion
cases., score 20, 23 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1gqinsn/handling_pathological_recursion_cases/
(2024-11) — body read; the C failure modes and compiler-side runaway. · Flat AST and states
machine over recursion: is worth it?, score 61, 39 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1co8qpv/flat_ast_and_states_machine_over_recursion_is/
(2024-05) — body read; the recursion-limit motivation (Zig's flat tree cut compile memory from
~10 GB to ~3 GB; Carbon's state machines), also cited by `compiler-architecture.md`. · Limitations
on memory representations, score 16, 34 comments, (2019-01) — permalink above, under
uniform-boxed values; guaranteed TCO in the feature list. · Parsing Protobuf at 2+GB/s: How I
Learned To Love Tail Calls in C, score 47, 6 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/my2720/parsing_protobuf_at_2gbs_how_i_learned_to_love/
(2021-04) — link post with no body, outside the slice; read from `corpus.jsonl`.

**Bearing on `quill`.** `quill` promises neither TCO nor a constant-space tail call — no document in
`docs/wayfinder` mentions tail calls — so the honest statement is: undecided, and unasked. What it
does promise is the architectural half: a term needing a sub-evaluation gets a `Kont` frame, never
a native one, so deep non-tail recursion costs one heap frame per object-level call. That was
measured rather than asserted: the ticket
[`deep-non-tail-recursion-is-superlinear.md`](../wayfinder/tickets/deep-non-tail-recursion-is-superlinear.md)
**closed on 2026-09-27**, because the port runs 2× per doubling (400k in 5.65 s) where the
deleted prototype ran 4× (14.40 s), the quadratic term having been OCaml's stack-scanning GC. The
ticket's own closing note is the remaining exposure: readback, unification and elaboration may
still recurse over structure, and that path was never re-measured. If a backend ever needs stack
safety as a *language* guarantee, that is a new ruling, not an implementation detail.

### M:N scheduling: green threads over OS threads

**What it is.** Multiplex M user-scheduled tasks onto N operating-system threads instead of giving
each task an OS thread (1:1). The runtime grows a scheduler, task states, blocking-operation
handling — and, in the corpus's fullest example, channels as ring buffers with a blocked task
rolled back and parked in a lock-free queue.

**Buys.** Millions of cheap tasks: at 256 KB of stack an OS thread allows only ~8k of them in a
32-bit process, and Linux's tens of thousands versus macOS's ~2000-thread ceiling make 1:1
portability-hostile at high task counts. You also get to schedule: cooperative tasks, blocking I/O
sent to a separate pool while the task parks, and (per the goroutine example) non-blocking channel
semantics without a global interpreter lock. For "web stuff" a commenter calls green threads
concurrency proper and OS threads parallelism — most servers want the former.

**Costs.** The corpus's abandonment record is concrete and unusually well measured: a lightweight
task costs ~30 µs with M:N versus ~35 µs backed by an OS thread — "A difference of only 5
microseconds really is not that interesting" — and moving to 1:1 let Inko delete ~2246 lines,
"about 10% of the entire codebase". Blocking calls need their own pools (Inko ran GC on its own
thread pool and migrated tasks around blocking reads); C interop needs thread pinning because C
code may depend on thread-local storage, and calling back from C needs a barrier; and "writing a
good scheduler is really hard… I am not convinced *I* can write such a good scheduler." The kernel
is also not built for it: it steals threads for blocking operations and offers no timer-based
preemption, which is why Rusky points at scheduler activations / user-mode scheduling as the
interfaces that would be needed. Rust tried both and kept 1:1, with M:N surviving as a library
"that few use".

**Maturity.** `contested`, with the 1:1 side holding the concrete measurements and the M:N side
holding deployments — nginx is called "a green threading middleware" in the retrieved tree, and
P6/MoarVM builds M:N atop its own thread abstraction — while the OP's conclusion is conditional:
M:N wins for "millions of very short lived tasks… and a limited amount of virtual memory", and
its scheduler must beat the kernel's, which it may not.

**Tried by.** Inko (abandoned M:N for 1:1), Rust (same, as a library), FreeBSD (moved to 1:1, per
a comment), Go (goroutines; [general knowledge, not from corpus]), P6/Rakudo (both substrates),
nginx-style servers (per a comment), a <3k-line C goroutine VM (green threads over a lazy
OS-thread pool, with an atomic per-object lock and refcount handoff through channels).

**Source.** The value of M:N scheduling over 1:1 scheduling, score 39, 45 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/a2nvyj/the_value_of_mn_scheduling_over_11_scheduling/
(2018-12) — 26 comments retrieved; the Inko measurements, Rust/FreeBSD abandons, Rusky's kernel
argument, thread-limit numbers. · I wrote an M:N scheduled(goroutines) scripting lang in <3k lines
of C…, score 37, 7 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1smg7an/i_wrote_an_mn_scheduledgoroutines_scripting_lang/
(2026-04) — read from `corpus.jsonl`, outside the slice; the ring-buffer/parking design and its
benchmarks.

**Bearing on `quill`.** Genuinely new and unasked: `quill` has no concurrency primitive, no scheduler
and no threads, and neither `CONTEXT.md` nor the wayfinder mentions any. The closest existing
machinery is *conceptual*: effect rows say what a function may perform and handlers are deep and
one-shot, which is the vocabulary a task runtime would sit on — but a scheduler is not an effect,
and modelling green threads would be a language decision first. Matters only when a runtime exists
to schedule on.

### Actors: shared nothing, so there is no memory model to publish

**What it is.** Concurrency as isolated processes exchanging messages, with the runtime owning
scheduling and delivery. The corpus's compiled example lists the machinery: lock-free
single-producer/single-consumer queues per actor, per-core actor queues, work-stealing fallback
scheduling, adaptive batching, zero-copy messaging, NUMA-aware allocation, arena allocators — and
the language-enforced part: "isolation enforced at the language level. Developers do not manage
threads or locks directly."

**Buys.** Two things at once. Programmatically: no locks in user code, no data races by
construction, because nothing shared can be mutated — which is how a language sidesteps the
question "what memory model does the language specify" instead of answering it. Structurally: a
natural unit of scheduling and of failure, and (in the corpus's own framing) protocol-first design
— the chess-clock thread's actor example is really an argument that an actor system's interface is
a protocol, not a set of methods.

**Costs.** Message passing has its own tax, all visible in the corpus's bullet lists: queues and
schedulers to build correctly, per-core balancing, batching policy, and the latency of a round
trip where a call would do. Isolation forbids shared mutable state outright, so anything that
*wants* to be shared (a cache, a scene graph) must be re-modelled as messages or partitioned.
And the specific design questions the chess-clock thread opens — who may send what to whom, what a
protocol forbids its implementer from knowing — do not go away, they just move from types to
message schemas.

**Maturity.** `shipped` — Erlang/OTP and Akka are the corpus's reference systems (named in the
actor thread and, for Erlang's tracing collector, elsewhere). The compiled example is a hobby
release: "Current work focuses on refining the concurrency model, validating performance
characteristics", i.e. unvalidated.

**Tried by.** Erlang, Akka, Sophie (a poster's in-design language), Aether (compiled to C,
hobby), Candy (Erlang-like fibers over immutable data, weighing RC vs tracing).

**Source.** The Actor Model and the Chess Clock, score 24, 9 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1238nxa/the_actor_model_and_the_chess_clock/
(2023-03) — body read; the protocol argument. · Aether: A Compiled Actor-Based Language for
High-Performance Concurrency, score 45, 18 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1rbnwiw/aether_a_compiled_actorbased_language_for/
(2026-02) — read from `corpus.jsonl`, outside the slice; self-promotion, cited as an attempt (a
sibling posting at score 35 exists on r/Compilers).

**Bearing on `quill`.** Nothing in `quill` is shared-nothing: references on heaps are mutable, and
`quill`'s answer to races is *not* an actor model but the effect system — `Read(h)`/`Write(h)` are
rows a function must declare, and a handler's escape rules stop closures smuggling handled
effects out. A memory model question does not yet exist for `quill` because there is no
inter-thread anything; if actors or threads ever arrive, "what does the language promise about
reads and writes across tasks" becomes a real and currently unanswered design question — and,
notably, no thread in this corpus answers it for anyone (see Gaps).

### The GPU as a callable: SIMT behind the language's own call syntax

**What it is.** Stop treating shaders as a separate string-embedded language and make a GPU
kernel an ordinary value: calling a shader-shaped function builds a closure-like object holding
the inputs and the graphics state, and a `Draw` dispatches it. The corpus's design: parameters
"from" an array are per-vertex attributes, "as" a value is a uniform, and the body between
`Rasterize` and `End Rasterize` is the kernel.

**Buys.** One call syntax for CPU and GPU work, closures as the uniform-capture mechanism (a
function called outside `map` just "evaluates the args into an object to use later"), and the
shader becoming inspectable and composable by the host language instead of living in a string.
The tensor-first sibling makes the wider promise: tensors, structured control flow,
ownership-aware mutation, autodiff and CPU/GPU backends in one system instead of Python plus
frameworks.

**Costs.** The design's own weaknesses are visible: a closure-like object per draw call allocates
before every dispatch, exactly where a graphics loop wants none; the shader body introduces a
separate execution model (interpolation, rasterisation state) that the call syntax papers over
rather than removes; and nobody in the thread can point at an existing language doing this — the
poster's premise is "I have not seen much abstraction built around passing data to glsl shaders".
The tensor-first sibling is candid that it is "not performance-competitive yet".

**Maturity.** `speculative` — argued and prototyped by hobby languages, not shipped in a
production one; two posters, nine and eight comments respectively.

**Tried by.** The GLSL-as-closure language (unnamed in the body), Thiran (tensor-first, CUDA/PTX,
v0.1.0), and — as the surrounding field — functional GPU programming with cycle-count semantics
(a thread asking for alternatives).

**Source.** Attempting to innovate in integrating gpu shaders into a language as closure-like
objects, score 35, 9 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1iso5f6/attempting_to_innovate_in_integrating_gpu_shaders/
(2025-02) — body read. · I built a small tensor-first programming language with native CPU/GPU
compilation, autodiff and ownership, score 24, 8 comments,
https://www.reddit.com/r/Compilers/comments/1wras8b/i_built_a_small_tensorfirst_programming_language/
(2026-09) — read from `corpus.jsonl`, outside the slice; self-promotion with published bad
benchmarks. · Functional GPU programming: what are alternatives or generalizations of the idea of
"number of cycles must be…", score 22, 29 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/12bll1a/functional_gpu_programming_what_are_alternatives/
(2023-04) — title as position, outside the slice; read from `corpus.jsonl`.

**Bearing on `quill`.** Genuinely new; nothing in `CONTEXT.md` or the wayfinder mentions GPUs,
kernels or arrays of them. The one structural rhyme: the shader-as-closure move is exactly `quill`'s
"modules are first-class values" argument applied to kernels — a callable value that captures what
it needs — so the *shape* would not be foreign to the language. The execution model would be.

### Specify integer edge cases; do not leave them to the machine

**What it is.** Decide what overflow, division by zero, shift-by-too-much and out-of-range
conversion *mean*, in the language, rather than declaring them undefined so the optimiser can
assume they never happen. The corpus shows the spectrum as a live argument: Zig says "UB is fine
as long as you trap in debug builds", Odin "Avoid all undefined behaviour", C3 "Avoid UB as far as
possible as long as this doesn't mean adding extra checks", C leaves the size of `int`
implementation-defined and array overrun undefined, Rust confines UB to an explicit `unsafe`
block.

**Buys.** Predictable programs and a language that can be specified: a defined edge case is a
value or an error a test can pin, which is what "once you define a behaviour you can't undefine
it" buys — compatibility forever. The corpus also makes the honest framing explicit: what C calls
undefined is sometimes really *implementation-defined* (int size) or *unspecified* (argument
evaluation order), and conflating the three is the confusion, not the checking. Checked arithmetic
costs one compare and a branch per operation and lets the runtime report "integer overflow in
<op>" instead of silently wrapping.

**Costs.** Checks are instructions, and on the other side sits the optimiser's argument: UB exists
because some behaviours cannot be specified portably and because checking everything "might simply
prove very difficult to the point of preventing a language specification from existing" — and
because "once you define a behavior you can't undefine it" also traps *you*. The width question is
separate: a bytecode VM poster asks whether to implement 32-bit math at all, i.e. how many integer
widths a language must expose before the cost outweighs the portability. And `unsafe`-free safety
is not free: the UB thread's own note is that low-level Rust (custom allocators, still unstable)
does meet these questions again. Unsigned widths get no dedicated argument in this corpus: the
only thread where an unsigned type matters is the GC student's, where an `unsigned long` on the
stack is read as a pointer (see the conservative-collector entry) — a representation hazard, not
a width debate.

**Maturity.** `contested` — the corpus's 77-comment thread is an argument about how much to
define, not a survey; the checked-arithmetic position is what several production languages shipped
(Rust's safe subset, Zig's debug traps) while C/C++'s optimisation-driven UB remains the other
pole.

**Tried by.** Zig (debug traps), Odin (avoid UB), C3 (balance checks), Rust (UB only in `unsafe`),
C/C++ (undefined by design), `quill` (checked `I64`).

**Source.** Recent trend towards more UB (?), score 75, 77 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/13usqwq/recent_trend_towards_more_ub/
(2023-05) — 26 comments retrieved; the three-way behaviour taxonomy, the language positions, the
Lattner reference. · Bytecode VMs, should i even bother to implement 32 bit math?, score 11, 18
comments, https://www.reddit.com/r/Compilers/comments/na0stx/bytecode_vms_should_i_even_bother_to_implement_32/
(2021-05) — body read; the width question.

**Bearing on `quill`.** Already has it, and it is one of the few runtime decisions `quill` has made
for real: `I64` `+ - * /` are checked, overflow (including `min_int / -1`) and division by zero
fail with a `FunException` naming the operation — "integer overflow in <op>". That behaviour is
part of a primitive's declaration (`Primitives.Declarations`), not a scattered branch. The corpus's
unsigned-integer question does not arise: `AtomTy` has no unsigned type, and no floating point
either. The width question (`I64` only) is decided by the same table.

### Strings: first-class in a language that otherwise manages memory manually

**What it is.** Give a low-level language a string type people can use without thinking: layout
(bytes, terminator, capacity), copy semantics (by value? by reference? copy-on-write?), and
allocation (who pays for `"foo" + a + "bar"`?). The corpus's survey names four models with their
trade-offs: C-style fixed mutable null-terminated buffers; C++-style resizable; Java/Go-style
immutable shared strings; and QBASIC-style value copies.

**Buys.** The model decides everything downstream. Immutable shared strings are "safe, can be
shared by many structures" and make aliasing a non-issue; a counted mutable buffer makes
append and in-place edit fast when you hold the only reference; keeping a null terminator "for
safe interop with C" costs nothing and unblocks every C library — which is the reason a
maintained terminator appears in the surveyed design.

**Costs.** Each model's list is the price list. Immutable: "you must use a StringBuilder or
[]byte if you want to make edits or efficiently concatenate". C-style: `strlen` is linear,
buffers cannot grow, and "if there are other references to the string you are in trouble". Value
copies: "you either need to do lots of copying or copy-on-write", and copy-on-write "push[es] a
check on every write". Shared mutable strings: resizing is allowed only at one reference, which
is a rule the type system must express or the runtime must check. And the manual-management
question is sharpest here — the original poster frames it as a dilemma: concatenation can leak if
memory is manual, but ARC or mark-and-sweep "would need… special cases all over the language",
so strings become the feature that decides the memory model rather than the other way round.

**Maturity.** `contested` — every production language has *a* string, and the corpus thread that
surveys the models reaches no verdict (comments on it were never fetched; the survey itself is the
evidence).

**Tried by.** The surveyed designs: C, C++, Java, Go, QBASIC, plus the poster's own interpreter
(reference-counted mutable buffer with size/capacity and a null terminator, resizable only when
solely referenced).

**Source.** How would you best implement first class strings suitable for a low level language?,
score 26, 69 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/9tj6ka/how_would_you_best_implement_first_class_strings/
(2018-11) — read from `corpus.jsonl`, outside the slice; the three questions and the
leak-versus-special-cases dilemma. · What string model did you use and why?, score 34, 38 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1rhg3x4/what_string_model_did_you_use_and_why/
(2026-02) — read from `corpus.jsonl`, outside the slice; the four models with pros and cons.

**Bearing on `quill`.** Has it, minimally: `String` is one of `quill`'s six atoms (`AtomTy` =
`I64, Unit, Char, String, Scopes, Absurd`), host-backed, and the primitives table gives it exactly
`eq_string`/`neq_string` plus `panic`'s message type — no concatenation, no slicing, no length.
The library decision is recorded and deliberate: **`Std.Strings` does not exist**; `Strings` is
*named* in the std-library surface ticket and explicitly "not declared" in the first cut
(`design-std-library-surface.md`). So the representation question (bytes vs UTF-16, mutable vs
immutable) is still open *and* unasked — the corpus says it is the choice that drags the memory
model along with it, which for `quill` means it belongs with the std surface decision, not with a
backend.

### Arrays and tuples: one flat product, or two types

**What it is.** A tuple is heterogeneous and indexed by literal position; an array is homogeneous
and indexed by any expression. The corpus's proposal is to stop having both: let arrays *be*
tuples whose components happen to all have one type, and allow dynamic indexing exactly where the
tuple is homogeneous — "why not just have an array type like `[f64; 3]` literally just be a type
alias for a tuple type `(f64, f64, f64)`?".

**Buys.** One representation, one set of operations, one set of generic rules, and no conversion
tax at boundaries — the poster's motivation is concrete: a `(f64, f64)` position and a
`[f64; 2]` position from a geometry library are "both just ways of grouping two floats together"
and the mismatch forced "a bunch of conversion functions". Static indexing, destructuring,
packing and layout would all work for free on arrays, and dynamic indexing would work wherever it
is type-correct.

**Costs.** Type-level: dynamic indexing a heterogeneous tuple is ill-typed, so the rule must be
"is allowed only on homogeneously-typed tuples", which makes one operation's availability depend
on a property of the type rather than on the type's identity — inference, error messages and
generic code all have to cope with that. Representation-wise the two want different things: arrays
want stride-indexed flat storage, tuples want positional packing; merging them either flattens
tuples into arrays (losing per-slot types in the runtime) or keeps two layouts under one type.
And the poster asks the question the corpus cannot answer here: "Do any existing languages take
this approach? Are there any downsides here that I'm not thinking of?" — 81 comments exist and
were never fetched.

**Maturity.** `speculative` — argued in one high-scoring thread; I could not retrieve a single
reply, so I cannot claim anyone has shipped it or rejected it.

**Tried by.** Nobody I could verify. The thread asks for precedents and the answers were not
retrieved.

**Source.** Why not treat arrays as a special case of tuples?, score 52, 81 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1smkdsb/why_not_treat_arrays_as_a_special_case_of_tuples/
(2026-04) — read from `corpus.jsonl`, outside the slice; body read, comments never fetched. ·
How to handle fixed-size arrays, score 6, 12 comments,
https://www.reddit.com/r/Compilers/comments/1go1ub6/how_to_handle_fixedsize_arrays/ (2024-11) —
title as position, from the slice. · Implementing arrays (and hash tables and ..) in a minimal ML
with a C API, score 11, 24 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/13zcm5i/implementing_arrays_and_hash_tables_and_in_a/
(2023-06) — title as position, outside the slice; read from `corpus.jsonl`.

**Bearing on `quill`.** Has it half-decided, and in the direction opposite to the proposal: `quill`
has tuples and no arrays. `Tuple(n, T1, …, Tn)` is a built-in type former whose type is
`(n : I64) -> tuple_arity(n)`, arity computed from `n` (a negative `n` is an evaluation error), and
it reduces to the flat product; projections are ordinary. There is no array type in `std/` and no
dynamic index expression over a tuple, so the poster's rule — dynamic indexing where homogeneous —
is exactly the undecided half. That is a language question (`types-and-semantics.md` territory for
the type, this doc for the runtime layout), not a backend one, and it has no ticket.

### Layout is a language decision: alignment, padding, field order

**What it is.** Decide how a record's fields sit in memory: in written order with padding (C),
sorted by decreasing alignment with size forced to a multiple of alignment (Rust), or reordered
freely for minimal size (the corpus's proposal), plus the run-time question of whether a stack
machine's operand slots are packed byte-by-byte or aligned.

**Buys.** Compaction is measurable in the thread's own examples: C's layout for `{i8, i64, i8}` is
24 bytes where a reordered layout wants 10; Rust's alignment-multiple rule turns a nested 9-byte
record into 16 and the pair into 24 where 10 suffice. Allowing size not to be a multiple of
alignment (the poster's `Maybe Int` = 9 bytes, align 8) removes a whole class of padding at the
cost of one careful rule at array-element boundaries. Alignment on the VM's own stack avoids the
host doing extra work per unaligned access.

**Costs.** Field order stops being source order, so layout cannot be predicted from the
declaration — languages cope by saying "you can't depend on the layout" (Zig, quoted) or by
simply not reordering (C). Reordering with free sizes is combinatorial: the poster suspects
NP-hardness ("feels like the knapsack problem") and notes that the literature mostly assumes
size is a multiple of alignment, which makes the problem trivial and the papers inapplicable.
Alignment has its own price: padding is wasted bytes, and a packed VM stack trades those bytes
against slower access — the stack-VM poster is choosing between 4-byte and host-8-byte slots and
cannot tell which is right from the literature.

**Maturity.** `shipped` on all three positions — C (no reorder), Rust (reorder by alignment),
Zig (layout not part of the contract) are surveyed in the body; the free-size optimal ordering is
`speculative` (a forum brain-teaser with a published solution, no implementation named).

**Tried by.** C, Rust, Zig, Haskell (pointer-based, per the survey), LLVM (could not be pinned
down by the poster), Plum (the language being designed); the VM-alignment poster's stack machine.

**Source.** Field reordering for compact structs, score 29, 36 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1h4y7wl/field_reordering_for_compact_structs/
(2024-12) — body read; the C/Rust/Zig/Haskell survey with byte counts. · Memory alignment for
stack-based virtual machine, score 26, 11 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/13xdkmr/memory_alignment_for_stackbased_virtual_machine/
(2023-06) — body read; packed-vs-aligned operand slots. · Feedback request - Tasks for Compiler
Optimised Memory Layouts, score 13, 3 comments,
https://www.reddit.com/r/ProgrammingLanguages/comments/1l37lcl/feedback_request_tasks_for_compiler_optimised/
(2025-06) — title as position.

**Bearing on `quill`.** Layout does not exist yet — structs are fields and methods in the
elaborator and a `Value` at run time, with no backend to lay them out in. Two representation
choices *have* been made, both for correctness rather than speed: kernel records store their
sequences as `EquatableArray<T>`, because `ImmutableArray<T>` compares by reference and made
structurally equal records unequal; and a struct's type is its fields, with methods excluded from
matching. Padding, field order and element stride are all still open — no ticket, no fog item,
not in the wayfinder.

## Threads worth reading in full

- **Is reference counting a trap?** (59, 67 comments, 2026-06) — the best single argument about
  RC in the corpus: a game/audio author's requirements, a Perceus-insider reply, and rebuttals
  from gasche and munificent on the predictability claim.
- **Are people too obsessed with manual memory management?** (154, 82 comments, 2023-02) — GC's
  actual costs (headroom, control, unpredictability) stated fairly and then corrected: compaction
  is not locality, and fragmentation is smaller than the folklore.
- **What are some examples of language implementations dying "because it was too hard to get the
  GC in later?"** (135, 81 comments, 2024-07) — retrofit failure stories (Mono, Objective-C,
  Python) plus a quoted measurement of what extra memory a collector needs to keep up.
- **The value of M:N scheduling over 1:1 scheduling** (39, 45 comments, 2018-12) — the rare
  thread where the poster publishes his own measurements and then changes his mind: 5 µs of
  savings, 2246 deleted lines.
- **You don't need tags!** (58, 51 comments, 2025-09) — a genuinely original representation idea
  and a genuine numerical-analysis rebuttal in the same tree; also the best roundup of who uses
  nan/nun-boxing.
- **C or LLVM for a fast backend?** (39, 65 comments, 2023-06) — measured build times for the
  emit-C route and a maintainer's account of what LLVM does not do for you.
- **Region-based memory management in Language 84** (13, 24 comments, 2017-05) — the clearest
  written account of annotation-free, syntax-inferred arenas, including why its mutable objects
  contain no references.
- **can capturing closures only exist in languages with automatic memory management?** (42, 60
  comments, 2024-11) — closure representation across C++, Rust and ATS, and why capture mode is
  the programmer's problem without a collector.
- **"I would recommend JIT only if you absolutely have to use it"** (46, 4 comments, 2026-02) —
  GraalVM's Thomas Wuerthinger on profile pollution, JIT metadata footprint, and the two cases
  where a JIT is unavoidable.
- **Which languages support Tail Recursion Modulo Cons?** (44, 20 comments, 2022-01) — an
  adoption survey that records refusals with reasons, which is rarer than adoption lists.

## Gaps and disagreements

**What this corpus did not contain.**

- **Memory models.** No thread in 2538 discusses memory models: `happens-before`, `sequential
  consistency` and `data race` (as a title) return zero hits, and `memory model` returns only
  incidental uses. The nearest claims are Aether's language-enforced isolation and Nirdosha's
  "provably free of… data races" (score 0, self-reported). Whether a language should *specify* a
  memory model is therefore unanswered here; to settle it you would need the actual literature
  (the Java Memory Model, work-stealing scheduler papers) — [general knowledge, not from corpus].
  Work-stealing appears only as a bullet in one hobby runtime's feature list.
- **Inline caches and type specialisation.** `inline cache` and `polymorphic inline` return zero
  hits; the corpus's nearest thing is tiered interpretation ("profiling based tiering") and
  handler-ordering research. Any claim about inline caches in this doc would be invented, so
  there is none.
- **Tail call arguments in depth.** Three of the five tail-call sources are link posts with no
  body, and the one thread that *asks* "are there reasons to NOT support TRMC" is present only
  through its original post — the 20 answers were never fetched. The "why mainstream languages
  skip tail calls" question is therefore opened, not answered, by this corpus.
- **Direct collector comparisons.** "Distilling the Real Cost of Production Garbage Collectors"
  (44, 31 comments), "Making JS Garbage Collection 30% faster" (160, 16), "An experimental O(1)
  Garbage Collector" (88, 18) and "Garbage collection with zero-cost at non-GC time" (57, 32) are
  all link posts with unfetched comment trees; the only quantitative GC memory numbers in this
  doc come from a paper abstract quoted secondhand in a comment, and the only pause comparison is
  one poster's own benchmark repository. Nobody here has measured collectors against each other
  under controlled conditions.
- **Coverage mechanics.** 518 comments across 20 threads, each a top slice of a tree listed at
  24–99 comments (68, 9 and 6 replies unretrieved in three trees). Twelve slice threads have no
  body at all. Eleven cited threads are outside the slice — first-class strings, the string-model
  survey, arrays-as-tuples, Aether, goroutine M:N, Higher RAII, and five title-only posts (three
  on tail calls, functional GPU programming, implementing arrays in a minimal ML) — read from
  `corpus.jsonl`, with their comments never fetched.
- **Hobby evidence.** Sric (memory safety without a borrow checker), Nirdosha (proofs), Aether
  (actors), Thiran (GPU), Stasis (static allocation) and the hybrid-RC/ownership/arena language
  are all self-promotions with single-digit comment trees. They show what people are trying, not
  what works; every one is tagged as such above.

**Where the community visibly disagrees.**

- **Reference counting versus tracing**, by language shape: Perceus-style RC for strict immutable
  code has implementations and numbers; mutable shared code gets no RC defence past "use weak
  references" and an admission that the poster's own language makes users prevent cycles. The
  predictability claim for RC is directly rebutted and nobody rebuts the rebuttal.
- **M:N scheduling**: measured abandonment (Inko, Rust, FreeBSD) versus deployments kept — nginx
  is called "a green threading middleware" in a retrieved comment and P6/MoarVM supports both
  substrates (Go's goroutines being the general-knowledge example the corpus assumes). The
  corpus's own conclusion is conditional on task length and available memory — which means the
  disagreement is probably about workloads, not about facts.
- **JIT as a default**: a performance claim ("JIT can greatly outperform AOT") met with "I'd like
  to see the many cases", against a practitioner's detailed case that JIT profile pollution makes
  performance *unpredictable*. No benchmark thread adjudicates it.
- **How much to define**: the UB thread is a 77-comment argument between "undefined means the spec
  is silent" and "undefined should mean what the hardware does", with language designers'
  positions quoted on both sides.
- **Untagged floats**: the flush-to-zero scheme versus numerical-analysis objections (exact
  subtraction, compensated summation, reproducibility). The scheme's author concedes only the
  speed, not the numerics; the critics concede only the cleverness, not the correctness. This one
  would need an implementation to settle, and none exists in the corpus.

**What this doc does not know about `quill`.** Two claims in it rest on `quill`'s own records rather
than on the corpus — that `Kont` frames make deep recursion linear, and that
`Primitives.Declarations` is the single source for primitive types and reductions — and both are
measured or ticketed (`deep-non-tail-recursion-is-superlinear.md`, `unify-primitive-declaration.md`).
Everything marked "backend-only" is backend-only *given that no backend exists*, which
`STATUS.md` and the `Quill.Cli` stub confirm today; if a backend ticket opens, the entries marked
that way turn into due decisions rather than trivia.

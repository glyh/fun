# Design ideas, by axis

## What this is

This is a catalogue of programming-language design ideas, gathered by reading
r/ProgrammingLanguages and r/Compilers, scoped into nine axes — one document per axis —
with each idea tagged by maturity and mapped onto the `fun` project's current state in a
"Bearing on `fun`" line. To say it plainly: this is a *catalogue of ideas*, not a
specification for any language, and not a survey of the field. It records what working
language designers argue about, what has shipped, and what is still contested — nothing
here decides anything for `fun`, and the deciding is done in `docs/wayfinder/`.

## How it was gathered

- **Sources.** r/ProgrammingLanguages and r/Compilers only. Threads were read through
  Reddit's own JSON endpoints from a logged-in browser — no third-party scraper, no
  aggregator, no papers, no mailing lists.
- **Scale.** 2538 unique threads, collected from top/hot listings at several time windows
  plus keyword searches over a fixed keyword list, spanning 2009-01 to 2026-09. Full
  comment trees were fetched for the richest threads in each axis; the slices in
  `/tmp/reddit/slices/` hold the per-axis material the writers used.

### The biases, stated as limitations

- **This is what those two subreddits upvoted, not a literature review.** Score measures
  agreement, not correctness. Popularity is not evidence an idea works: several of the
  highest-scoring threads are self-promotion for hobby languages whose design claims were
  never tested. A high score tells you an idea is *being discussed*, nothing more.
- **It over-represents hobby language projects and recent work.** Amateur enthusiasm is
  the native dialect of both subreddits; a thread from last month with a working demo
  outvotes a decade of production experience.
- **It under-represents industrial experience and non-English/non-Western practice.**
  War stories from compilers teams rarely get posted, and when they do they are rarely
  written in English by people who are not also posting on Hacker News. Ideas discussed
  only in Chinese, Japanese, Korean, Russian, or German-language communities are largely
  absent even where those communities are large.
- **Comment coverage is partial by construction.** Even where a comment tree was fetched,
  Reddit's `limit=100` returns only about 30 top-level comments; the rest sit in
  unretrieved `more` placeholders. Each slice's comments file states its own shortfall.
  So "the top comments said X" is a statement about the top of a thread, not its whole
  argument.
- **A term appearing often is a signal of live interest** — which is exactly why it is
  worth cataloguing. Frequency in this corpus means "people are building on this idea now",
  not "this idea is correct". Read the two claims as distinct, always.

The way to tell a well-supported idea from an upvoted one is the `Maturity` and `Tried by`
fields, plus each doc's "Gaps and disagreements" section: `shipped` with named production
users is one thing; `speculative` with nobody having built it is another, however loud the
thread was.

## How to read an entry

Every idea in the nine axis docs uses the same fields:

| Field | What it answers |
| --- | --- |
| **What it is** | The construct, the rule, or the algorithm — concretely, sometimes in three lines of pseudocode. |
| **Buys** | What it makes possible or cheap, as a concrete consequence. |
| **Costs** | What it makes harder, slower, or impossible. Every entry has one; an entry with no cost is an advertisement. |
| **Maturity** | One of `shipped` (in a production language), `research` (experimental language or paper with an implementation), `speculative` (argued, not built), `contested` (a live disagreement — the entry says which side has better evidence). |
| **Tried by** | Named languages and projects that took it. "Nobody has shipped this" means nobody has shipped this. |
| **Source** | The Reddit thread title, score, comment count, permalink, and date — including threads that argued *against* the idea. |
| **Bearing on `fun`** | How it lands on this project: already has it (named), has it differently, open/undecided (named ticket or fog item), rejected (with the recorded reason), or genuinely new. |

"Bearing on `fun`" is written in `fun`'s own vocabulary as recorded in `CONTEXT.md`, and
it reflects two documents: `docs/STATUS.md` for what is actually built, and
`docs/wayfinder/fun-design-map.md` for what is decided, open, or fog. **`STATUS.md` wins**
where any of these docs disagrees with it on completion status — if an axis doc says
something is built and `STATUS.md` does not, believe `STATUS.md`.

## The axes

| File | Covers | Read this one when... |
| --- | --- | --- |
| `syntax-and-parsing.md` | Notation, grammar, parsing technique as it shapes notation | you are deciding the surface syntax, or wondering what a parser choice costs you later |
| `types-and-semantics.md` | Inference, polymorphism, subtyping, data modelling, laziness/purity | you are asking what the type system should promise and how much the user should have to write |
| `effects-and-handlers.md` | Algebraic effects and handlers, effect rows, async colouring, resources | you are modelling effects, async, or resource safety — close to `fun`'s own effect rows |
| `modules-and-abstraction.md` | Module systems, visibility/sealing, structs-vs-classes, compilation units, interop | you are deciding what a `struct` is and where a boundary between compilation units lies |
| `macros-and-metaprogramming.md` | Hygiene, expansion order, staging/comptime, alternatives to macros | you are working in enforestation or deciding what happens before and after expansion |
| `compiler-architecture.md` | Pipeline/IR structure, pass management, incremental/query compilers, error recovery, testing and debugging a compiler, maintainability | you are deciding how the pipeline is organised and how it stays debuggable |
| `runtime-and-memory.md` | GC/RC/ownership/arenas, bytecode vs JIT vs tree-walking, value representation, tail calls, concurrency runtime, numerics/strings | you are deciding how values are represented and what the evaluator is allowed to do |
| `tooling-and-diagnostics.md` | Error messages, spans and provenance, REPL/LSP, compiler-as-a-library, diagnostic tests | you care about what a user sees when things go wrong, or about tools built on the compiler |
| `design-philosophy-and-process.md` | Writability vs readability, feature selection and where a feature lives, adoption, design process, implementation strategy | you are weighing whether to add a feature at all, or deciding where it should live |

Beyond the entries themselves, each axis doc ends with two short sections worth
jumping to directly: **Threads worth reading in full** (5-10 high-signal threads, one line
on what each is good for) and **Gaps and disagreements** (what the corpus did not settle).
The second of those is where to look to find out what a doc does *not* know.

### The one measured document: `corpus-census.md`

Every other file here is judgement over a corpus. [`corpus-census.md`](corpus-census.md)
is a measurement: how many of the 2538 threads mention each of ~55 design concepts. It is
worth reading before the axis docs, because it inverts the subreddits' reputation.

Discussion volume sits almost entirely on *implementation* questions — backends and codegen
(453 threads), intermediate representations and lowering (314), interpreter strategy (292),
module systems (265) — while the techniques with the best cost/benefit ratio are nearly
invisible: normalisation by evaluation has **1** thread, de Bruijn indices **2**, decision
trees for pattern compilation **6**. That asymmetry is the point:

- **The top of the table** is where the arguments are, so it is where design *trade-offs*
  are best documented. Go there for "what are the options and who disagrees".
- **The bottom of the table** is where the settled technique is. Go there for "what should
  I just do" — and note that these docs have less to say about it, because nobody argues
  about it.

So do not read the census as a priority ranking. A concept with 400 threads is contested;
a concept with 2 threads may simply be correct.

## Where to start

- **"I am deciding the surface syntax next."** Start with `syntax-and-parsing.md` for the
  notation and parsing trade-offs, then `macros-and-metaprogramming.md` — in `fun` the
  surface *is* enforestation, so syntax and expansion are one decision — and skim
  `types-and-semantics.md` for how much type syntax real systems impose on users.
- **"I care about keeping the compiler maintainable."** `compiler-architecture.md` first,
  then `tooling-and-diagnostics.md` (error recovery and diagnostic tests are
  maintainability too), then `design-philosophy-and-process.md` for how projects actually
  sequence implementation.
- **"I want to know what is genuinely contested."** Go to `types-and-semantics.md` and
  `effects-and-handlers.md` for the live type-system and effect disagreements, then
  `runtime-and-memory.md` for the ownership-vs-GC and evaluation-strategy fights, and
  finish with `design-philosophy-and-process.md`. Anything tagged `contested` in those
  four is a real split, not a settled result.
- **"I want the fog items."** Start with `design-philosophy-and-process.md` for how a
  project should hold open questions, then `modules-and-abstraction.md` and
  `tooling-and-diagnostics.md` — they cover `fun`'s recorded fog: universe levels, the
  first-class compiler API for tools/LSP/REPL, the content-addressed codebase, and the
  library-vs-compiler-machinery boundary (UFCS, FFI).
- **"Just tell me what the field is busy with."** [`corpus-census.md`](corpus-census.md),
  which is one page and quantitative.

## Provenance and reuse

- The raw corpus is `/tmp/reddit/corpus.jsonl` (2538 threads, one JSON per line) with
  per-axis slices under `/tmp/reddit/slices/`. **That is a scratch location and may not
  survive** — treat it as working material, not an archive. The durable artifact is these
  documents.
- Every entry cites a Reddit permalink, so any claim can be re-checked by hand against
  the original thread and its comment tree.
- These docs are a snapshot of the corpus as collected through 2026-09-30. Threads after
  that date are not in them, and a thread's score or comment count may have moved since.

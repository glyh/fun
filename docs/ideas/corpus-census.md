# Corpus census — what these two subreddits actually talk about

A frequency count over the whole collected corpus, so that "the field is interested in X" is a
measurement rather than an impression.

**Method.** 2538 unique threads from r/ProgrammingLanguages (1564) and r/Compilers (974),
collected via Reddit's own JSON endpoints. Each thread's title and body were matched against a
fixed list of ~55 concept patterns (case-insensitive, word-bounded). The number reported is
**how many threads mention the concept at least once**, not how many ideas it yielded and not
how strongly anyone holds it.

**Verified, not assumed.** The first pass produced an FFI count of 432 and a Unicode/string
count of 235, both wrong: `ffi` was matching *traffic* and `rope` was matching *property*. All
acronyms and short tokens are now word-bounded, and recurring generic words were removed
(`backend`, `effects`, `region`) where they inflated a topic. The raw counts below were
spot-checked by printing sample matching titles. Treat these as an order of magnitude, not a
precise rank — a concept one maniacal thread discusses forty times counts once.

## The numbers

| threads | concept |
| ---: | --- |
| 453 | backend / codegen (LLVM, WASM, compile-to-C) |
| 314 | intermediate representations & lowering |
| 292 | interpreter strategy (tree-walk / bytecode / JIT) |
| 265 | module systems & imports |
| 213 | garbage collection |
| 209 | generics & monomorphization |
| 193 | source generation vs in-language macros |
| 180 | bootstrapping & self-hosting |
| 143 | error messages & diagnostics |
| 138 | closure representation & cost |
| 128 | records & variants |
| 126 | SSA & sea of nodes |
| 122 | interop / FFI |
| 120 | parser technique |
| 107 | pass management & pipeline order |
| 105 | ownership & borrowing |
| 104 | type classes / traits / implicits |
| 98 | immutability & purity |
| 92 | sum types & exhaustiveness |
| 92 | reflection / syntax objects |
| 84 | operator precedence & fixity |
| 78 | algebraic effects & handlers |
| 76 | visibility & sealing |
| 76 | parser generators |
| 69 | subtyping |
| 66 | reference counting |
| 64 | integer sizes & overflow |
| 63 | Hindley-Milner inference |
| 61 | semicolon / separator inference |
| 54 | arenas & regions |
| 54 | compile-time evaluation (comptime / const) |
| 52 | async / await colouring |
| 47 | concurrency runtime |
| 47 | tail calls |
| 45 | incremental / query-based compilation |
| 45 | compiler testing (fuzzing, differential, golden) |
| 40 | bidirectional type checking |
| 37 | linear & affine types |
| 36 | dependent types |
| 36 | user-defined operators |
| 33 | strings & Unicode representation |
| 31 | package management & versioning |
| 30 | monads |
| 29 | gradual typing |
| 26 | structural vs nominal typing |
| 22 | laziness |
| 21 | hygiene |
| 21 | memory model & atomics |
| 18 | formal semantics & type soundness |
| 15 | value representation & boxing |
| 14 | row polymorphism |
| 10 | refinement types |
| 9 | functors / first-class modules |
| 6 | decision trees / pattern compilation |
| 5 | significant whitespace |
| 2 | de Bruijn indices & binders |
| 1 | normalisation by evaluation |

## What this does and does not say

**Does.** It says where the *volume of discussion* is. The top of this table is dominated by
backend and implementation questions — LLVM vs a custom backend, IR design, tree-walking vs
bytecode — not by exotic type theory. That is a real finding, and it is the opposite of what
these subreddits' reputation would predict. It also says which ideas have a large, live
audience: algebraic effects at 78 threads is a lot for a feature one shipping language has.

**Does not.** A high count is not evidence an idea is good, and a low count is not evidence it
is bad. Several mechanisms with the strongest theoretical backing and the clearest practical
payoff are near the bottom — normalisation by evaluation (1), de Bruijn indices (2), decision
trees for pattern compilation (6) — because practitioners *use* them silently rather than
arguing about them. **The bottom of this table is where implementation-grade technique lives.**

That asymmetry is the single most useful thing this census shows, and it is also the reason the
catalogue is not ordered by popularity:

- **The top of the table** is where the arguments are, so it is where design *trade-offs* are
  best documented. Go there for "what are the options and who disagrees".
- **The bottom of the table** is where the settled technique is, so it is where the design
  *ideas* are least contested and least discussed. Go there for "what should I just do".

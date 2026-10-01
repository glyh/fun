---
title: The conformance suite's redundancy, measured
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by: []
---

# The conformance suite's redundancy, measured

Asked 2026-09-28 — "the tests are slow, and a lot of them catch nothing; we can remove those" — and
the measurements say something different from the premise, so they are recorded before any
deletion. A reading pass over the 945 cases built a per-case signature (constructs used, `.expect`
class, program normalised by abstracting identifiers, constructors and literals); a byte-level pass
then verified its findings independently, which is how three of them were caught.

## The speed question is not the suite

| measured | |
|---|---|
| the suite, 945 cases in one process | **5.58 s** ≈ **5.8 ms/case** |
| xUnit's slowest single test | **75 ms**; most `< 1 ms` |
| xUnit *run* wall clock | **11 s** — VSTest's per-test overhead (~50 ms × 208), not the tests |
| `dotnet test` with a rebuild / `--no-build` | 16.8 s / 13.2 s |
| `dotnet run --project test/Fun.Conformance` | 11.7 s for 5.6 s of work — a 6 s build |
| one case in its own process | 0.60–0.85 s, all host + JIT (`DOTNET_TieredCompilation=0` is *slower*) |

Deleting cases is therefore a weak lever: **all 367 `core-*` cases together are ≈ 2 s.** The costs
are (a) **rebuilds** — build once and then invoke the built DLLs (`dotnet test --no-build`, and
`Fun.Conformance.dll` directly instead of `dotnet run --project`); (b) **VSTest's ~10 s** for 208
tests against 5.8 ms/case in the conformance runner — which is the repo's own rule anyway: *a test
that is only "source → value or error" belongs in `cases/` and only there*; (c) **0.6 s per probe
process**, which a REPL session amortises.

## The redundancy, measured

- **10 program texts (comments stripped) are shared by 2+ cases.**
  - **4 pairs are deliberate**: their programs are identical but their `.unit-*.fun` differ (2–3
    lines), so the *unit* is the subject —
    `trait-generic-impl-bound-in-imported-module`, `unit-macro-sees-earlier-binding`,
    `port-macro-private`, `trait-generic-impl-bound-through-import`. **These stay**; a text-level
    dedup would have deleted real coverage.
  - **6 are genuine duplicates**, three of them cases whose *name* claims a distinction their files
    do not contain.
- **231 cases pin `error`** (24%). Large groups: exhaustiveness/match coverage (47), record/struct
  shape (19). Small and distinct: missing impl (21), refusal naming the offending term (10),
  unification (8), parse (6), ambiguity (4) — each member pins a different rule.
- **`core-NNN` is not the filler.** Of 338 ported cases, **0** are textually subsumed by a named
  case, and only 1 of 2 core error cases shares a signature with a named one. The redundant mass is
  the **newer `elab-NNN` port**: 54 of 115 `elab` error cases share a signature with a named case,
  in spelling-only clusters — `is_zeroish` ×4, sig-arg ×3, field shape 6→3 (differing by a stray
  `;`), match-coverage pairs, generative symbol-table 6, `std` empties ×3.

## Decision 2026-09-28: fix three, delete three

Delete `macros/core-201` (identical to `core-200` including its comment), `elaborate/elab-009`
(identical to `elab-008`), `elaborate/elab-092` (its `ok` is subsumed by `values/core-141`'s
`True`). For the other three, **write the spelling the name claims** rather than deleting the pair:
`macros/core-279` ("syntax with no holes" against `core-189`'s "operator prefix"),
`values/core-022` ("signature sugar argument" against `core-020`'s "module signature argument"),
and `imports/re-export-selective-unit-macro` (whose unit is byte-identical to
`re-exported-unit-macro`'s, so the *selective* re-export its name promises is nowhere in the case).
A case whose name overstates it is worse than a missing case.

## Still open, deliberately

1. **The `elab-NNN` spelling clusters** (~15–20 files) — each group needs its own call, because
   `cases/README.md`'s rule is that a *spelling* can be the subject. Not decided here.
2. **The mutation experiment** (running when this was written): break one small thing in `src/` at a
   time and record which cases fail. That is the only honest measure of "a case that catches
   nothing", and it must filter any further deletion list before one is proposed.
3. **The xUnit → `cases/` migration** for tests that are only "source → value or error": follows the
   repo's rule and costs ~10× less per test. Not yet sized.

## Corrections 2026-09-28, from the fork that ran the deletions

Two of the three "fix the spelling" items were wrong, and one of them was wrong because of *my*
check, so both are corrected here rather than in a commit message.

1. **`imports/re-export-selective-unit-macro` is not a duplicate at all — leave it.** Its wrapper
   unit already writes `export (import "mac").{same};` while its twin writes the wholesale
   `M = import "mac"; export M;`, and that difference arrived with `71bd812` (the selective-open
   work). My byte-check compared only the *first* unit file of each pair (`ls … | head -1`) and so
   reported "UNITS IDENTICAL" for a pair whose difference lives in the **second** unit. The four
   deliberate pairs stay deliberate, and this is a fifth one.
2. **`values/core-022` cannot be fixed as intended: its spelling is gone by design.** The original
   pair distinguished `sig x : I64 end` from `module { pub x = I64 }` — the prototype let a *module*
   stand in for a signature. The port refuses that as a parameter type
   (`a module is not a type; only a signature is`), which is a deliberate divergence. So the case
   should **pin that refusal**, not re-write a sugar the language no longer has.
3. **`macros/core-279` is fixable**, and the fork found the spelling: a hole-free multi-token
   template — `{ syntax two_tokens { two tokens => 42 }; two tokens }` → `42` — against
   `core-189`'s single-token operator prefix. Its probe confirmed the value.

**So the decision becomes: delete three files, fix one case (`core-279`), and turn `core-022` into
the refusal case.** `re-export-selective-unit-macro` was never in scope.

### Landed 2026-09-29 — `0abdf5f`, and two corrections to the above

Both edits are on `main`, suite re-run by the integrator: **947 cases, 0 failed**. The deletions
had already landed; this commit is only the two case fixes.

1. **Item 3's spelling does not work in the port.** `{ syntax two_tokens { two tokens => 42 };
   two tokens }` fails here with `ELAB syntax branch pattern must start with declared head:
   two_tokens` — a branch pattern's head must be the declared-head *atom*, not the first token of
   a multi-word spelling (the port's other multi-token headless forms agree:
   `bind $name $value in $body`, `extract { $body }`). The claim "its probe confirmed the value"
   was never re-run against this build. What landed is the nearest form that does work and still
   meets the name — hole-free, multi-token, distinct from `core-189`'s single-token prefix:
   `syntax two_tokens { two_tokens 7 7 => 42 }; two_tokens 7 7` → `42`.
2. **Item 2 landed with `.expect` = `error`, not the message.** The refusal itself is verified
   (`ELAB a module is not a type; only a signature is` through the single-file runner), but
   `cases/README.md:35-37` makes `error` deliberately coarse — a case pinning a *particular*
   message belongs in xUnit, and no case in the repo string-matches one. The program is now the
   module-in-type-position shape: `(fn(m : module { pub x : I64 }) { m.x })(module { pub x = 42 })`.

## Round two: the wide sweep, 2026-09-28

73 mutations attempted, 70 recorded (26 behavioural flips, 47 refusal-removals, 3 lost), 7 of them
prelude-wide collapses; **63 discriminating** (19 flips + 44 refusals). Artifacts: `/tmp/mut/table.tsv`,
`/tmp/mut/fails/<idx>-<tag>.txt`.

| view (collapses excluded) | caught by 0 | 1 | 2–5 | 6+ |
|---|---|---|---|---|
| everything | **848 / 942** | 83 | 11 | 0 |
| flips only | 880 | 53 | 9 | 0 |
| refusals only | 910 | 30 | 2 | 0 |

**The deletion question is answered in the negative, and the reason is methodological:** a case
caught by zero of *my* mutations is invisible to a mutation I never wrote — a different refusal, a
lexer spelling, an invariant case such as the 67 `ok` cases (of which exactly one was caught). The
sample is 19 discriminating flips deep, so "90% caught by nothing" is a statement about the mutation
set, not about the cases. **No further deletions are proposed**, and the three byte-identical files
removed earlier stay the only safe deletions this investigation found.

**Two findings that do matter — both the opposite of the hypothesis:**

1. **Refusal-removals buy ~16× the error-case coverage of flips.** Error cases caught: 2/231 by flips
   versus 34/231 by refusals, and refusals alone rescued 32 cases from zero-caught (29 `elaborate`,
   3 `values`, all `error`). So round one's "227 inert `error` cases" was *partly* an artifact of its
   own mutation set — and still mostly true, because 44 refusals is thin: 23 of them caught nothing.
2. **The 32 zero-catch mutations are the interesting result** — 9 flips and 23 refusals. About a third
   look like genuinely *untested behaviour*: a duplicate effect surviving normalisation, two
   same-shape distinct declarers, pattern-synonym arity and tuple-arity variants (only type-parameter
   over-supply has a case), most of the expander's macro-position and arity checks, the enforester's
   empty-block and block-export checks, reference arity, and **the evaluator budget, which has no
   conformance case at all**. The rest look *redundant* — another check already refuses the same
   input (`ref-export-clash`, `ref-selopen-unknown-member`, `ref-effects-poly-arrow`, several
   reflection and effect checks). Candidates for *new* cases, not for deletion.

Also worth its own look: **removing the match non-exhaustiveness check and the rec-group mix check
made the suite crash** rather than report a failing case — the runner has an invariant-failure path
for four exception types, and something escapes it. **Answered 2026-10-01**: an NRE
(`Nbe.Match.cs:126`) and an `InvalidCastException` (`Elaborator.RecTypes.cs:139`) escaped, both on
paths no program reaches; the runner was the bug and now reports them per case as a `hard failure`.
See [a mutation aborts the conformance run](conformance-runner-aborts-a-mutation.md) — which also
corrects this sweep's own table: the rec-group mutation caught `elab-164` alone, not `elab-109`.

**Areas the sweep could not mutate**, so their cases remain unmeasured: `Driver`, `Loader`,
`Reflection`, `PreludeAbi`, the `Core.*` traversals, deeper `Unify`, `Nbe.Structs`/`Nbe.Rec`,
`Expander.Imports`, `Reader` beyond a caret flip, `Syntax.Map` beyond one flip.

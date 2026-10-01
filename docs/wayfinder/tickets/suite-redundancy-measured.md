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

1. **The `elab-NNN` spelling clusters** — each group needs its own call, because
   `cases/README.md`'s rule is that a *spelling* can be the subject. **Sized 2026-10-01**: the scope
   is two areas (179 `elaborate/` + 78 `values/`), **zero** of them byte- or
   whitespace-identical, so the call is a human read of ~19 semantic clusters — the table with a
   recommendation each is under [Inventory](#inventory-fork-2026-10-01--both-open-items-sized).
   Still not decided here.
2. **The mutation experiment** (running when this was written): break one small thing in `src/` at a
   time and record which cases fail. That is the only honest measure of "a case that catches
   nothing", and it must filter any further deletion list before one is proposed. **Ran twice** —
   round two below, and its two aborting mutations are fixed and filed
   ([a mutation aborts the conformance run](conformance-runner-aborts-a-mutation.md), 2026-10-01).
3. **The xUnit → `cases/` migration** for tests that are only "source → value or error": follows the
   repo's rule and costs ~10× less per test. **Sized 2026-10-01: 8 of 123 methods (6.5%), and it is
   not worth doing** — no file is mostly migratable, the target format's `ok` cases are the ones
   round two measured as nearly invisible to a sweep (1 of 67 caught), and three tests cannot be
   expressed as one value at all. See the same [Inventory](#inventory-fork-2026-10-01--both-open-items-sized).

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

## Inventory (fork, 2026-10-01) — both open items, sized

Read-only pass; no file, test or case was changed. Counts and the negative checks below were
re-verified by the integrator on `86b4166` (the per-cluster *readings* are the fork's, spot-checked
by hand for cluster 1 only — treat the others as a careful reading, not a measurement).

**Item 1's scope is larger than the ticket says, and its shape is different.** The `elab-NNN`
numbers live in **two** areas — `ls */elab-*.fun` gives **179** in `elaborate/` and **78** in
`values/` — and several of the clusters named here (`is_zeroish`, `sig-arg`) are `values/` cases.
More importantly, the premise that a byte-check can find them is **false**:

```
md5sum elaborate/elab-*.fun values/elab-*.fun | awk '{print $1}' | sort | uniq -d | wc -l   → 0
# whitespace-collapsed, both areas, same result                                                    → 0
find test/conformance/cases -name '*.unit-*.fun' | wc -l                                    → 69
ls test/conformance/cases/elaborate/ | grep -c unit                                          → 0
```

So there are no byte-identical `elab-*` files anywhere, and no `elab-*` case has a unit sibling
(the 69 unit files are all in `imports/`, where the `head -1` mistake happened and stands corrected).
**Every cluster below is a *semantic* near-duplicate: the call has to be a human read.**

| # | members | the precise difference | call |
|---|---|---|---|
| 1 | `elab-093`/`135` | identical rule (*missing field `y`*); 093 writes `struct {x: I64; y: I64}` and 135 `struct { x: I64; y: I64; }` | **delete one** (keep the `;`-terminated spelling, already canonical) |
| 2 | `elab-094`/`136` | same (*extra field*), differs only by the trailing `;` | **delete one** |
| 3 | `elab-095`/`137` | same (*duplicate field*), differs only by the trailing `;` | **delete one** |
| 4 | `elab-096` | duplicate field **in the declaration** — alone, distinct rule | keep |
| 5 | `elab-064,065,066,068,069` | five *different* cross-evaluation paths (symbol as arg / through `name` / a third fresh evaluation / a value's own type / a private member via re-export) | keep — each pins a path, and the payload type differs from the named twins |
| 6 | `elab-088,089,090,091` | the **argument's** spelling is the subject: missing member / wrong type / not public / not a module | keep all four |
| 7 | `values/core-020` + `values/elab-080` | `m.x` vs `open m; x` — two ways to reach a signature member | keep both |
| 8 | `values/core-072…077` | type-case dispatch, one case per scrutinee type; zero/`False` are the controls | keep all six (the ticket says ×4; it is ×6) |
| 9 | `elab-169,170,171` | non-exhaustiveness, once per literal type | keep |
| 10 | `elab-167,168` | pattern/scrutinee mismatch, two directions | keep |
| 11 | `elab-174`, `182` | or-pattern coverage vs missing third arm — different rules | keep |
| 12 | `elab-183,184` | depth of the pattern spine | keep |
| 13 | `elab-114,115,116` | parameter-type identity through a type synonym vs a direct nominal | keep (114 arguably subsumed by 116) |
| 14 | `elab-077,078` | anonymous struct **type** vs one with a computed public member | keep |
| 15 | `elab-075,076` | unknown member vs private member | keep |
| 16 | `elab-054,198` | empty module **as a value** vs **as an impl body** | keep |
| 17 | `elab-199,205` | effect-row order-insensitivity; 199 is the trivial control | keep 205, 199 is a control |
| 18 | `elab-249,250` + `refs-nonreference` | three cases, one rule (`deref(1)` / `1 <- 2` / the named copy) | **delete one numbered twin**, keep the named case |
| 19 | `elab-118,119` | bare `self` at top level vs `self` in a non-method binder | keep both |

**The overstatement risk here is inverted.** No `elab-NNN` name can overstate anything (they are
numeric); the risk is a numbered member silently **re-stating a named case**: `elab-064` ≡
`values/nominal-generative-symbol-table-rejects-other`, `elab-065` ≡ `…-rejects-name-across`,
`elab-068` ≡ the own-type half of `…-shares-own`, and `elab-249/250` ≡ `refs-nonreference`. By
`values/core-022`'s precedent the call is *pin the refusal, don't restate it* — but each of these
also pins the rule at a **second payload type** (`Sym(String)` vs `Sym(I64)`), which is a defence
clusters 1–3 do not have.

### Item 3 — the xUnit → `cases/` migration, sized

```
grep -h '\[Fact\]' *.cs | wc -l        → 108      (26 files; 15 [Theory] methods, 101 InlineData)
grep -n 'Driver\.Describe\|Driver\.Run(' *.cs | wc -l → 16 call sites: 9 assert a value, 7 a message
```

123 test methods, 209 executed cases. Of the 16 sites that could even be a case:

| bucket | methods | cases |
|---|---|---|
| **(a)** source → value or `ok`, nothing else | **8** | 8 |
| **(b)** inspects internals (`CLAUDE.md` names them: token/syntax shapes, reflection round trips, decision trees, budget accounting, `Value`/`Term` identity, exact messages) | 112 | 196 |
| **(c)** borderline — needs a decision | 3 | 5 |

Bucket (a), complete: `InterleavingTests.AMacroBodySeesAnEarlierBinding` (needs a `.unit-u.fun`, the
unit is inline in C# today), `MacroTests.AQuoteFillsItsHoles`, `MemberTests.
AnOpenNameMayBeShadowedByAPublicMember`, `PreludeTests.StdIsTheUnitWhichReExportsTheBootstrap`,
`TraitTests.{OpeningAModuleTwiceIsNotAnAmbiguity, TraitOpChoosesByArgumentType,
AChoiceWaitsForItsArgumentType}`, `RecTests.LazyDeltaComparesCallsWithoutUnfolding` (no assert at
all — it would become `.expect` = `ok`, which the format does support: it means "elaborates", the
program is not run).

**So the migration is 8 of 123 methods — 6.5%, and 8 of 209 executed cases, 3.8% — and no file is
*mostly* (a)**: the heaviest is `TraitTests` at 3 of 14. Three reasons not to do even that much yet:

1. **`TraitTests` should stay whole.** Its remaining 10 methods assert the exact `FunException`
   string, and the file's own premise is that the *message* is the assertion. Migrating 3 and
   leaving 10 fragments one argument.
2. **It moves against this ticket's own measurement.** The migration's payoff is cheap `ok` cases —
   and round two found that of **67 `ok` cases, exactly one** was caught by any of 73 mutations.
   Adding more of them is adding cases the sweep cannot see.
3. **What cannot express it is real**: two source programs in one test (`TraitTests.
   MatchingIgnoresNonFieldMembersWhereEqualityDoesNot` — one program pins `1`, the other the
   mismatch, and the juxtaposition *is* the test); a negative message assertion with no value
   (`MemberTests.APrivateBindingMayShadowAPublicOne`); and multi-row `[Theory]` methods whose rows
   would each become a file (`RecTests.DivergenceWhileCheckingIsABudgetError`, 3 rows).

**Not checked by this pass** (stated so a later reader does not over-read it): neither the suite nor
`dotnet test` was run, so every `error` claim is read from a sibling `.expect`; the mutation
experiment was not re-run, so nothing here says whether any bucket-(a) method catches something no
case catches; the prototype tree was not searched for a corresponding pin; and `values/elab-*` was
clustered only against `elaborate/` — a dedicated pass over `values/` alone may find clusters this
table merged away.

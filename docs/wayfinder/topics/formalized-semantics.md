# Formalized Semantics for `fun` — Handover Context

## What is `fun`

`fun` is an experimental dependently-typed programming language compiler/interpreter, currently implemented in OCaml. The core is `core_tt`: a dependently typed kernel with bidirectional elaboration, normalization by evaluation (NBE), implicit arguments, nominal ADTs, structural records/modules, pattern matching, traits, algebraic effects, and mutable references.

**Pipeline:**
```
source → Raw_syntax → Enforest Syntax.t → Expand + Lower → Surface.t → Elaborate → Core.term → NBE → value
```

**Key types (lib/core_kernel/core.ml):**
- `Core.term` (~15 constructors): de Bruijn-indexed core language — `Var`, `Lam`, `Ap`, `Let`, `Pi`, `U`, `Prod`, `Fix`, `Match`, meta-variables, nominal/effect definitions, `Stx` (macro syntax objects), module/struct constructors
- `Core.value` (~20 constructors): semantic domain — `VLam`, `VPi`, `VNominal`, `VEffect`, `VNeutral`, `VFlex`/`VRigid`, `VCon`, `VStx`, `VCont`, `VProd`, `VRecord`, `VU`, `VAtom`, `VRef`, etc.

**Key functions:**
- `nbe.ml:eval` — evaluates `Core.term` to `Core.value` via environment
- `elab_infer.ml:infer` — bidirectional elaboration from `Surface.t` to `(Core.term * Core.value)`
- `unify.ml:unify` — structural unification on values with Miller pattern unification for flex metas

**Library graph:**
```
core_tt_kernel → core_tt_syntax → core_tt_expand → core_tt_loader → core_tt_typecheck
                              core_tt_interp ──────────────────────┘
                              core_tt_match ───────────────────────┘
```

**Roadmap status (now the direction [map](../fun-design-map.md)):**
1. Regression coverage — ongoing
2. Type-case/generic programming — complete
3. Traits/ad-hoc polymorphism — mostly complete
4. Algebraic effects hardening — complete
5. Mutable references — complete
6. Macro system — in progress, the main remaining item
7. **CLR/C# rewrite** — on the active agenda, not a distant cleanup
8. Deferred/minor items
9. Post-rewrite diagnostics polish

## The Core Idea: Formalized Semantics as Truth

### Problem

Recurring bugs in the compiler, especially around:
- De Bruijn index mismatches between elaboration and NBE
- Wrong unification cases, spine length mismatches, occurs-check failures
- Constructor resolution failures in nested modules
- Scoping bugs when adding new fields to `struct_binding` variants

The code is intricate and structural correspondence between stages must be exact.

### Proposal

Write a **formal specification** of `Core.term`, `Core.value`, and the evaluation/typing rules in a proof assistant (Lean 4 or Coq). This spec serves as the **single source of truth** for the core type theory, independent of the implementation language.

### Why this works across rewrites

The project roadmap explicitly targets a CLR/C# rewrite (item 7). The formalization is the **stable artifact** — it doesn't change when the implementation language changes.

```
Lean/Coq spec (stable)
     │
     ├── Today: AI checks OCaml matches spec
     └── Future: AI checks C# matches spec
```

### The AI as Correspondence Engine

Rather than manual theorem proving, the AI acts as a **structural diff engine**:
- Feed it the Lean spec + OCaml/C# implementation
- It checks: constructor-for-constructor, case-for-case, dispatch order, argument shapes
- Flags mismatches: missing case, swapped args, wrong de Bruijn shift, etc.

This is **not** formal verification. It's AI-assisted structural correspondence checking — catching the class of bugs that come from the implementation diverging from the intended semantics.

### What this covers vs. what it doesn't

| Covers | Doesn't cover |
|--------|---------------|
| `Core.term` constructors | Parser (Menhir/tokenizer) |
| `Core.value` constructors | Macros / enforestation |
| NBE `eval` dispatch | Scope sets / hygiene |
| Unification rules | String/position manipulation |
| Elaboration typing rules | File I/O / module loading |
| Alpha-equivalence / quoting | Error message formatting |

The sweet spot is the **core type theory pipeline**: elaboration, unification, evaluation. These are the areas where structural bugs (wrong case order, de Bruijn off-by-one, missing spine extension) are most damaging and hardest to catch via testing alone.

## Concrete Plan

### Phase 0: Scope the spec

Define in Lean/Coq:
1. `Term` — mirrors `Core.term` constructors 1:1
2. `Value` — mirrors `Core.value` constructors 1:1
3. `eval : Env → Term → Value` — mirrors `nbe.ml:eval` NBE evaluation
4. (Optional) `infer : Ctx → Surface → Term × Value` — mirrors elaboration

Start with just **Phase 0.3** (`eval`) — it's the smallest, most regular, highest-bug-density function.

### Phase 1: AI-verified correspondence (current OCaml)

After writing the spec, use AI to:
1. Check every `term` constructor in OCaml has a corresponding Lean constructor
2. Check every `value` constructor matches
3. Walk through `eval`/`apply_result` case by case, verifying dispatch order, spine handling, closure application

### Phase 2: Property-based bridge

Use QCheck to fuzz the OCaml implementation against the spec:
- Generate random `Term` values
- Evaluate in OCaml
- Manually verify expected `Value` against the spec's expected behavior
- Catch divergence early

### Phase 3: C# rewrite verification

When the C# rewrite begins, the same spec serves as the compliance checklist. AI verifies the C# port matches the spec exactly as it did for OCaml.

## Key Architecture Notes for the Formalization

### De Bruijn representation

- `ix = int` — index into the local environment (distance from binder)
- `lvl = int` — level in the global environment (distance from root)
- `bd = Bound | Defined` — distinguishes lambda/Pi binding from let-definitions
- `env = value list` — head is most recently bound

### NBE structure

```ocaml
eval : MetaContext.t -> env -> term -> value
```

Core application dispatcher (`apply_result` / line ~432 of `nbe.ml`):
- `VLam` → beta-reduce (push arg onto env, eval body)
- `VFix` → unfold, re-apply
- `VNeutral` → extend frames, try primitive reduction
- `VFlex`/`VRigid` → extend spine
- `VNominal`/`VEffect`/`VCon` → extend params/spine
- `VCont` → resume continuation

### Unification

Miller pattern unification for flex metas:
- `VFlex {id; spine}` unified against any value
- Spine must be distinct rigid variables
- Solution: rename RHS, wrap in lambdas

Mismatch types:
`NonLinearSpine | OccursCheck | CannotUnify | TupleLengthMismatch | SpineLengthMismatch | NeutralHeadMismatch | FrameMismatch | NominalMismatch | EffectMismatch | EffectRowMismatch`

### Known bug patterns (from `CLAUDE.md` and the direction map)

1. **De Bruijn index mismatches** between elaboration and NBE — `List.nth` failures, `VNeutral` where `VLam` expected
2. **rest = [] bugs** in parser — discarding unconsumed tokens silently
3. **Missing field propagation** when adding new fields to `struct_binding` variants (must update: `lower_surface.ml`, `surface_to_syntax.ml`, `expand.ml` add_scope, `enforest_template.ml`, `elab_surface_rewrite.ml`)
4. **Constructor resolution phases** — `find_nominal_template_opt` resolves by name; pattern synonyms for ADT constructors work only if ADT was elaborated in a previous binding group
5. **Named comparison vs. nominal identity** — code comparing nominals by `String.equal` on names instead of the `id` field

### Choice of proof assistant

| | Lean 4 | Coq |
|---|---|---|
| Learning curve | Moderate | Steep |
| OCaml extraction | No (compiles to C) | Yes (mature, used in CompCert, CertiKOS) |
| Type-theory fit | Excellent (dependent pattern matching, `inductive` like OCaml) | Excellent |
| For spec-only use | Ideal — clean syntax, `#eval` for testing | Works but heavier |
| Community | Growing fast, good for "spec as documentation" | Established, better for certified extraction |

**Recommendation for spec-only approach:** Lean 4. Cleaner syntax, `#eval` for testing the spec, closer visual resemblance to OCaml pattern matching. The lack of OCaml extraction is irrelevant since the spec won't be extracted — it's a design document that AI consumes.

**If extraction is ever desired:** Coq.

## Provenance

This proposal originated as a standalone handover note (drafted in a separate
`fun` worktree). It is preserved here as a fog-stage direction; whether and when
to pursue it hangs on the [CLR/C# rewrite shape](../fun-design-map.md#fog).

## Key Files to Reference

- `lib/core_kernel/core.ml` — `Core.term`, `Core.value`, `env`, `closure`, `spine`, `cont`
- `lib/core_kernel/syntax.ml` — `Syntax.t`, `struct_binding`, `pat`
- `lib/syntax/surface.ml` — `Surface.t` (lowered AST, input to elaborator)
- `lib/backend/interp/nbe.ml` — NBE evaluation (`eval`, `apply_result`, `force`, `quote`)
- `lib/semantic/typecheck/elab_infer.ml` — bidirectional elaboration
- `lib/semantic/typecheck/unify.ml` — structural unification
- `lib/semantic/typecheck/elab_ctx.ml` — elaboration context
- `CLAUDE.md` — project conventions, common bugs, style guide
- [direction map](../fun-design-map.md) — decisions, open tickets, and fog (incl. the C# rewrite)
- [`docs/STATUS.md`](../../STATUS.md) — current implementation status
- `lib/expand/enforest.ml` — parser / enforestation (source of `rest=[]` bugs)

## Recurring Conventions (from `CLAUDE.md`)

- `(wrapped false)` everywhere — all `.ml` are flat top-level modules, no `.mli` files
- `Syntax.t` is the surface AST node, NOT an OCaml `t` type alias
- `Core.term` = elaboration core term, `Core.value` = evaluated value
- `and` in type definitions links mutually recursive types across files
- No exceptions as control flow
- No test-driven special cases
- Line count limit: 3000 lines per file (`test_line_counts.ml`)
- `rest = []` is almost always wrong in the parser

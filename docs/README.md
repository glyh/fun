# Docs — navigation index

This directory holds plans, design specs, status snapshots, and reference notes for the `fun` compiler. It is purely documentation; no code lives here.

## Source-of-truth rules

- **Root docs** (`AGENTS.md`, `ROADMAP.md`, `TODO.md`, `README.md`) define conventions, roadmap, and project overview.
- **`docs/STATUS.md`** is the canonical current implementation snapshot. When another doc disagrees with it, STATUS.md wins.
- **`docs/plan-for-macros/STATUS.md`** is the canonical macro status; all other macro docs defer to it for completion status.
- **`docs/HANDOVER.md`** is a historical snapshot (Stages 8–10 completion + infrastructure). It is accurate as of its write date but may lag behind STATUS.md.

## Directory overview

### Category 1 — Design / language features

| # | File | Topic |
|---|------|-------|
| 1 | `1.struct_as_module.md` | Struct-as-module unification |
| 2 | `2.dependent_types.md` | Dependent types in core_tt |
| 3 | `3.wiring_checklist.md` | Pipeline wiring checklist |
| 4 | `4.core_tt_records.md` | Record type support |
| 5 | `5.core_tt_type_specialized_equality.md` | Type-specialized equality |
| 6 | `6.core_tt_qualified_paths.md` | Qualified paths in core_tt |
| 7 | `7.algebraic_effects_plan.md` | Algebraic effects plan |
| 8 | `8.regression_coverage_plan.md` | Regression test coverage |
| 9 | `9.type_case_generic_programming_plan.md` | Type-case / generic programming |
| 10 | `10.record_type_reflection_plan.md` | Record type reflection |
| 11 | `11.trait_plan.md` | Traits / ad-hoc polymorphism |
| 12 | `12.trait_module_stdlib_plan.md` | Trait stdlib / pub semantics |
| 13 | `13.generated_symbol_cleanup_plan.md` | Generated symbol cleanup |
| 14 | `14.references_plan.md` | Mutable references |
| 15 | `15.enforest_improvement_plan.md` | Enforester improvements |
| 16 | `16.private_type_visibility.md` | Private type visibility |

### Category 2 — Snapshot / handover

| File | Contents |
|------|----------|
| `STATUS.md` | **Canonical** detailed current implementation status |
| `HANDOVER.md` | Historical snapshot: Stages 8–10 completion + infrastructure |

### Category 3 — Macro system

| Path | Contents |
|------|----------|
| [`plan-for-macros/`](plan-for-macros/) | All macro design, implementation plans, and reference papers |
| [`plan-for-macros/STATUS.md`](plan-for-macros/STATUS.md) | **Canonical** macro status |

## Quick links

- [AGENTS.md](../AGENTS.md) — project conventions, recurring patterns
- [ROADMAP.md](../ROADMAP.md) — design philosophy and ordered work
- [TODO.md](../TODO.md) — current bugs and feature backlog
- [STATUS.md](STATUS.md) — detailed current implementation snapshot

---
title: "Port: type-case, record and struct-type patterns, pattern synonyms"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Port: type-case, record and struct-type patterns, pattern synonyms

Wave 2 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- Type-case: `match (T) { I64 => …, Option(A) => … }` on a type, with branch
  refinement (18 cases blocked today), including the pattern heads that name type
  formers.
- Record patterns `P{x = p, y}` (11) and struct-type patterns `struct { x : p }` (9).
- Pattern synonyms: `pattern Name(params) = rhs` / `pub pattern` in blocks and
  modules, and their use.

## Decided rules to read first

[pattern-head-accepts-type-formers](pattern-head-accepts-type-formers.md),
[bare-constructor-pattern-resolves-by-name](bare-constructor-pattern-resolves-by-name.md)
(a bare head resolves like any name), the E11 note on type-case heads in
`Core.CPatNominalHead` (`core.ml`), `CLAUDE.md` "Pattern synonyms". Type-case
refinement's cost is a known prototype hotspot
([type-case-refinement-walks-whole-context](type-case-refinement-walks-whole-context.md)):
port the rule, not the walk. Effect branches belong to the effects fork.

## Progress (2026-09-16)

Merged from `port/patterns` (`5cf6b9c`, `f076fa5`, merges `796faa6`, `47411cd`; one
conflict in `Core.cs`: both forks had added the same environment update as
`Replace` and `With`, kept once as `Replace`). Record patterns `P {x = p, y; _}`
(missing/unknown/duplicate fields are errors); type-case on the atom types with a
required fallback, refining a matched type variable in context and expected type;
struct-type patterns `struct { x : p; _ }`; nominal type heads `Opt(I64)`/`Opt(x)`,
formers included, matched by instance (E11), arms run in order after the decision
tree checks exhaustiveness. Newly passing: values core-156; elaborate elab-188–190,
elab-044, 046, 047; shared `values/type-case-refines-variable` (7),
`values/type-case-struct-field-type` (3).

**Prototype defect** (ticketed by the integrator):
[type-case-former-head-arity-from-template](type-case-former-head-arity-from-template.md).

**Decided (user, 2026-09-16): a synonym's arguments bind by parameter name.**
`pattern Flip(a, b) = Pt(b, a)` means what its definition says: in
`match (Pt(10, 20)) { Flip(first, second) => first }`, `first` is `a`, which sits
in `Pt`'s second slot, so the result is 20. The prototype substitutes by position
(result 10), which makes the parameter names meaningless: a defect. A synonym's
head resolves like any other bare pattern head, through its binder or an open
(the constructor-pattern decision); the prototype's `UnknownConstructor` for a
synonym reached through `open` is a defect too. Both go in as shared cases with
the correct result, listed in `prototype-divergences.txt` with a ticket:
[pattern-synonym-arguments-bind-by-position](pattern-synonym-arguments-bind-by-position.md).

**Follow-up:** matching a nominal head evaluates the head term with a nested
`Eval` (`ponytail:` in `Nbe.Patterns.cs`) rather than a `Kont` frame; its depth is
the head term's, not the program's call depth.

## Resolution (2026-09-16)

Pattern synonyms merged from `port/patterns` (`6a6b770`, merge `5e945fb`):
`pattern Name(params) = rhs` in blocks and module items (`pub pattern` too); the
right-hand side elaborates once where written, every binder in it a parameter
bound exactly once; a use resolves the name like any bare pattern head (binder,
`open`, or `M.Flip`); arguments bind by parameter name. Shared cases
`values/pattern-synonym-binds-by-name` (20; prototype gives 10) and
`values/pattern-synonym-through-open` (7; prototype `UnknownConstructor "Swap"`) are
listed in `prototype-divergences.txt`; `values/pattern-synonym-agrees` (3) agrees.
C# 206/650; xUnit 90.

**Follow-ups:**
- **Block synonyms are a C#-only form.** The prototype has no `pattern` block
  statement (`unexpected token in expression`); the port accepts one because this
  ticket's scope said "blocks and modules". No shared case covers it. Decide
  whether a block may declare a synonym (a binding in a block is otherwise legal)
  or remove it from the port.
- Still "not ported yet": synonyms whose pattern does not fix the scrutinee or
  parameter types, and synonyms over a type-case pattern.

---
title: Template and operator heads resolve by scope set
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
decided: 2026-09-14 (grilled)
assignee: glyh
resolution: Decisions 2, 3, 4 and 6 enforced; decision 1 enforced for what a syntactic role can see, with the expander-driven loop itself carried into M9; decision 5 carried into M9. Raw tokens carry scope sets and ids take them; a role is a binder resolved by scope set (largest subset, ambiguity loud); definition contexts scope their tokens (inside edge, plus a scope per role-declaring statement over the statements after it); template intro scopes go on replacement tokens, so generated syntax is hygienic (decision 6, with name-position holes for use-site names). Roles are binders in the expander's table too - syntax declarations survive as SyntaxBinding / SyntaxDef, imported roles are seeded - and the one binder funnel reports RoleConflict in either order, exempting application-written binders and fixity attached to its value; an open supplying a role's name is OpenSuppliesRole at elaboration. Suite green, 907 tests.
closed_date: 2026-09-14
blocked_by:
---

# Template and operator heads resolve by scope set

## Decision (macro model M7, pass one I4c; revised by grilling 2026-09-14)

Fixity, precedence and "this name is a syntax form" are a binder's
**syntactic role**, resolved by scope set. ~~A later binder of the same name
takes the role away~~ — superseded: **a syntactic role never mixes with any
other binder of its name** (see below).

## Grilled decisions (2026-09-14)

1. **Arrangement: the expander drives the enforester (Honu).** Raw tokens
   carry scope sets. A definition context (unit, module, block) is enforested
   one form at a time; each form is expanded — its binders, roles and macro
   output registered — before the next form's tokens are enforested. Definition
   contexts are sequential (a module cannot forward-reference), so there is no
   two-pass partial expansion. This is what lets generated syntax reach later
   forms once templates are macros (M9 blocker 1).
2. **Every binder's region is reader-delimited.** Bodies are brace groups; see
   [surface-syntax-braces](surface-syntax-braces.md). No add-parse-strip
   mechanism, and no macro can move where a body ends.
3. **Mixing is an error.** A name with a syntactic role (macro, template,
   operator) cannot also be bound as anything else where both are visible:
   ```fun
   syntax answer { | answer => 42 }
   do { answer = 7; 1 }       # error at the binder, even if unused
   fn(answer) { 1 }           # error
   open M;                    # error at the open once M's members are known (elaboration)
   ```
   - **Fixity attaches, it does not bind.** `pub not = fn…; pub prefix (not) 30`
     is one binding with a role; `(not)` is the value, `not x` the prefix form.
   - **Reported at the binder**, not at a later use.
   - **Decided by scope set, not spelling.** A macro-introduced binder carrying
     its own intro scope does not conflict (hygiene, M12).
4. **Syntax shadows syntax** by scope set: an inner or later `syntax` / `infix`
   of the same name wins ("7I generated syntax later-wins" keeps passing).
5. **A captured `{…}` block is captured unparsed** (`$b:block`) and enforested
   only where the output places it — embedded DSLs can take raw tokens.
6. **Generated syntax is hygienic, exactly like values** (decided 2026-09-14,
   during implementation). A `syntax` / `infix` / `prefix` declaration a
   template or macro writes names its role with an id carrying the
   application's intro scope, so user code cannot see it. To generate callable
   syntax the name comes from the use site — a captured id, which may name a
   generated declaration and head its rules:
   ```fun
   syntax make_inc { | make_inc $(n: ident) => multi { syntax $n { | $n $x => $x + 1 } } };
   make_inc inc;
   pub result = inc 5          # 6
   ```
   or a macro building an `Id` with a received id's scopes (M11). **Grill Q1's
   example 3 was wrong as stated:** it showed `make_inc; inc 5` working with a
   name written inside the template, which decision 3's hygiene note (and M2)
   rule out.

## Resolution (2026-09-14)

**Enforced.**

- **Roles resolve by scope set.** A raw token carries a scope set; every id
  enforestation writes takes its token's. A role (`syntax`, `infix`, `prefix`,
  a macro operator) is recorded with its name token's scope set, and
  `Binding.find_operator` takes the largest subset of the occurrence's scopes;
  incomparable candidates are an ambiguity error. Equal scope sets (two imports
  of one operator) take the one added last.
- **Definition contexts scope their tokens** (units, modules, structs, blocks —
  `Enforest_util.map_context_statements`): an inside-edge scope on the whole
  context, and a statement that declared a role adds its own scope to the
  statements after it. So a role is visible after its declaration and inside
  it (recursion), not before it, not outside its context — **syntax shadows
  syntax** lexically (decision 4) — and a template's replacement, read at each
  instance, sees roles as of its definition (M10 for roles).
- **Generated syntax is hygienic** (decision 6). A template instance adds its
  intro scope to the replacement's tokens before they are read; captures keep
  their own. A hole may name a generated syntax declaration, head its rules,
  or name an operator symbol (`syntax $n { | $n $x => … }`, `infix ($op) …`),
  and identifier captures keep their token (operators included).
- **No mixing** (decision 3). Syntax and fixity declarations survive enforestation
  as binders — `Syntax.SyntaxBinding` in binding lists, `Syntax.SyntaxDef` in
  blocks, reflected both ways as `DeclSyntax` / `RawSyntaxDef` — and the roles
  an import harvests are seeded into the expander (`Parse_expand.recording_imported_roles`),
  so every role is a binder in the one expander table (`Binding.Role`, or
  `Macro`). `Expand_ctx.bind`, the funnel every binder goes through, raises
  `Expand_error.RoleConflict` when a binder of the other sort is visible with
  the new one — in either order, for every binder kind — unless the scopes the
  new binder has beyond it include an intro scope (an application wrote it). A
  fixity-only declaration attaches to the value visible where it is written;
  any other binder of that name is new.
- **Opens.** The expander notes, per open label, the roles visible where the
  open is written and those declared in its region (an imported unit's own
  roles excepted from its own open); the elaborator, once it knows the open's
  members, raises `OpenSuppliesRole` (through the macro runtime's
  `roles_in_open`).

**Carried into [M9](templates-desugar-to-macros.md).**

- **Decision 1's loop.** Enforestation still runs before expansion. What a
  syntactic role can see is already right: under no mixing, a value binder's
  scope never changes which role an occurrence has, so the scopes that matter
  to roles — definition contexts, template instances, imports — are the ones
  the enforester now puts on tokens. The loop that expands each form before
  the next is read has its first consumer in M9: a template as a macro whose
  output declares syntax.
- **Decision 5.** `$(b: block)` matches one brace group and parses it at the
  capture; capturing it unparsed needs a reflection of raw tokens for macro
  parameters, which is M9's.
- **The region rule** in `Expand.add_id_scope_if` stays: template output is
  still parsed before the expander adds value binders' scopes to the tree.

**Known gaps.**

- An `open` of a non-name module expression (`open (import "x")`) has no
  written scope set to test visibility against, so only unit-wide (imported)
  roles are noted against it, not roles the unit itself declared before it.
- An open's check runs where the elaborator has the macro runtime; the macro
  driver installs it only after a unit's bindings are elaborated, so opens a
  unit elaborates during its own driver run are not checked.
- Roles imported inside a block stay in that block by copying the enforester's
  table (`with_operator_scope`), since an imported role has no scope set of its
  own - an imported template's replacement must see it.

## Rejected

- Enforester-tracked binders without scopes (misses macro output and M9).
- Add-scope / parse / strip over undelimited bodies, and binding declarations in
  template patterns (`$body:expr in $x`) — unnecessary once bodies are groups.
- `do … end` / `fn … end` keyword groups (Elixir-like) — replaced by braces.
- An open's value silently taking a role away, or never taking it away.

## Today

`Binding.find_operator` is string-keyed and newest-wins, because enforestation
runs before expansion and no scope sets exist yet. Its comment defers
scope-keyed resolution to "the interleaving driver". The prefix-template and
operator paths in `enforest.ml` consult it by written name.

## Notes

This needs scopes at parse time, which is the fixed-passes defect the surface
pass named (enforestation entirely before expansion). It likely needs Honu's
arrangement: body forms are enforested lazily as expansion reaches them, after
the enclosing binders have added their scopes. Grilled: see above.

Sequencing: lands with, or after, [surface-syntax-braces](surface-syntax-braces.md)
(the reader groups must exist before enforestation can be driven form by form).

# Domain model — surface and enforestation

Second pass of the model asked for by
[domain-model-surface-enforestation](../tickets/domain-model-surface-enforestation.md).
Pass one modelled the [elaborate ↔ evaluate boundary](core-tt-domain-model.md);
this pass models everything between the source text and that boundary: the
reader, enforestation and expansion. Vocabulary lives in the root
[`CONTEXT.md`](../../../CONTEXT.md).

The *model* is the macro-system design: Honu enforestation, Flatt's sets of
scopes, and an ordered, binding-at-a-time interleaving of expansion with
elaboration (papers in [`macro-system/papers/`](../macro-system/papers/), design in
[macro-interleaving-design](macro-interleaving-design.md));
the glossary follows it. Where the implementation disagrees, the implementation
is the defect.

**Refreshed against main on 2026-09-15.** The pass was written when the
implementation ran enforest, expand, lower and elaborate as fixed passes, keyed
operators by string, and had three hygiene contracts. Since then `Surface.t` is
deleted ([delete-surface-ir](../tickets/delete-surface-ir.md)), roles resolve by
scope set (M7), templates are macros driven form by form (M9), and every
application shares one contract (M2). The sections below describe main; the
defects the pass found are listed at the end with their (closed) tickets.

## The three layers, and what each one commits to

| layer | produces | commits to |
|---|---|---|
| reader (`Raw_syntax`, `Token_tree`) | tokens and delimiter groups; each token carries a scope set | grouping only — no forms, no names resolved |
| enforestation (`Syntax.t`, `Enforest`) | typed forms, ids carrying scope sets | what a *form* is, one form at a time as expansion reaches it |
| expansion (`Expand`) | the same tree, alpha-renamed; the elaborator's input | which binder each occurrence denotes |

**The reader commits to nothing but grouping** (S1, enforced by construction).
`Token_tree.t` is `Token | Group(delimiter, items, span)`. Keywords are fixed
token kinds the lexer owns; operators are one uniform `Operator of string`. A
token's scope set starts empty; enforestation adds the scopes of the definition
contexts, template instances and binders around it before the token is read, so
a syntactic role resolves against it. Brackets decide grouping: `{}`, `[]`, `()`
group and `,`/`;` separate, so an extent is never a parser guess
([brackets-decide-grouping](../tickets/brackets-decide-grouping.md)).

**Enforestation is parsing interleaved with macro expansion** (S2). There is no
fixed grammar for extensible positions: roles decide fixity and order, and a
syntax form's use is read by its rules. The expander drives the enforester: a
`{ … }` body stays a `Block` of tokens and a unit's items stay `Items` until
expansion reaches them, one form at a time with the roles bound so far (M9).
A role resolves by scope set (`Binding.find_role`, largest subset; incomparable
candidates are an error) and never mixes with another binder of its name
(`RoleConflict`, `OpenSuppliesRole`). Precedence is an **order group**, a role
binder: transitive, associativity on the group, an undeclared relation an error,
an ungrouped role weaker than every grouped one.

## Scope sets and resolution

**Resolution is sets-of-scopes** (S3, enforced by construction).
`Binding.resolve`: candidates are the binders sharing the occurrence's
*written* name; keep those whose binder scope ⊆ occurrence scope; the **largest**
binder scope wins; two incomparable candidates raise `ambiguous binding` —
loud, not last-wins.

**Scopes are minted per binder and added by one traversal** (S4, enforced by
construction). Every binder form mints a fresh scope and adds it through
`Expand.mapper`, the one traversal every scope, intro, rename and syntax-form
fill goes through. The source-span region rule (`span_contains`,
`add_id_scope_if`) is deleted: templates instantiate during expansion, so a
binder's scope reaches its whole body.

**Every application mints a use-site and an intro scope** (M2). `Expand.application`
adds both to what an application receives and flips the intro scope on what it
returns; declarations it returns into a definition context lose the use-site
scope on their binders (Flatt 2016). Syntax forms, procedural macros, operator
macros and typed macros all go through it.

**Expansion alpha-renames** (S5, partly enforced). `Lam`, `Let`, parameter and
macro binders get a fresh, unwritable resolved name `name#n` from one global
counter, and expansion is idempotent (a resolved name is never re-minted).
**Distance:** type, constructor, trait, effect and module-item binders still bind
with `resolved_name = written name`, and the elaborator looks them up by that
string
([declaration-binders-keep-written-names](../tickets/declaration-binders-keep-written-names.md)).

**Unbound names inside an open resolve to an open choice** (S6, enforced for
locals). A name no binder takes, or that only a binder outside the open takes,
becomes `OpenChoice { opens; fallback }`; the elaborator picks the first open
that has the member (`Elab_ctx.lookup_choice_opt`). **Distance:** a name with no
binder and no open still resolves by spelling in the base context, and an id
spelled with `#` is trusted as already resolved
([resolved-names-forgeable](../tickets/resolved-names-forgeable.md)).

**Hygiene governs bare names; members are labels** (S7, enforced). Scope sets
ride on `Syntax.id`. A path's head (`M` in `M.x`, a pattern constructor head, an
effect operation's family) is an id; the members after it are labels resolved
by their container and rightly carry no scopes.

## One hygiene contract

The model has one contract for every macro and template application: an intro
scope and a use-site scope, quoted ids resolving at the definition site, splices
keeping their scopes, and output expanded in place. It holds on every path
today:

| path | runs | heads | spliced args | written literals | output |
|---|---|---|---|---|---|
| procedural (`macro`) | expand time | resolved name, scope-aware | reflection keeps scope sets | quoted, definition site | expanded in place |
| typed procedural (signature promises a type) | elaborator time | macro runtime lookup | same; typed args elaborated once | same | checked, expanded in place |
| syntax form (`syntax`, operators) | expand time (`Instantiate`) | role by scope set | captures filled by `Expand.fill` | replacement parsed where written | expanded in place |

The table the pass originally recorded (three contracts, a dropped scope set on
the untyped path, use-site literals on templates, unexpanded type-aware output)
is history; see the defects below.

## The seam

There is no hand-off tree: the elaborator reads expanded `Syntax.t`, so spans
and ids reach it. It holds no expander state beyond its `macro_runtime`
capability (pass one's I4e), through which it looks macros up and runs
applications. `Core.StxExpr` remains as reflection scaffolding.

## Defects the modelling found

Each lives in its ticket, all closed:

- [procedural-macros-capture-use-site-variables](../tickets/procedural-macros-capture-use-site-variables.md)
  — scope sets died at the macro value boundary.
- [template-literals-resolve-at-use-site](../tickets/template-literals-resolve-at-use-site.md)
  — `&&`/`||` were corruptible by a use-site binder.
- [type-aware-macro-output-is-not-expanded](../tickets/type-aware-macro-output-is-not-expanded.md)
  and [block-local-macros-leak-by-written-name](../tickets/block-local-macros-leak-by-written-name.md).
- [delete-surface-ir](../tickets/delete-surface-ir.md) — the lowering seam.

Still open and found later: S5's and S6's distances above, and
[struct items](../tickets/struct-open-does-not-scope-over-con-fields.md) now read
in source order (closed).

## What the port's types should be named after

- **Reader** for the grouping pass; **Group** for its one structural notion.
- **Form** for a typed node; **Syntax object** for the macro-visible tree
  (`Syntax.t`) whose ids carry scope sets.
- **Resolved name** for the alpha-unique key a binder is registered and
  rewritten to. Every binder gets one; the type-namespace exception (S5) is a
  defect.
- **Intro scope** and **Use-site scope** for the two fresh scopes every
  application mints.
- **Template** (syntax form) for the pattern rewrite; **macro** for the
  procedural one. Two declaration forms, one hygiene contract.
- **Role** and **Order group** for what a name means to the enforester.
- **Quoted syntax** and **Borrowed context** for how a macro writes names.

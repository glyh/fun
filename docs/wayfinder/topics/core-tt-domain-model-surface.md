# Domain model — surface and enforestation

Second pass of the model asked for by
[domain-model-surface-enforestation](../tickets/domain-model-surface-enforestation.md).
Pass one modelled the [elaborate ↔ evaluate boundary](core-tt-domain-model.md);
this pass models everything between the source text and that boundary: the
reader, enforestation, expansion, and the lowering seam. Vocabulary lives in
the root [`CONTEXT.md`](../../../CONTEXT.md).

The question *is one of these layers removable* is owned elsewhere:
[syntax-vs-surface-ir-layer](../tickets/syntax-vs-surface-ir-layer.md) compared
the two trees and recommends deleting `Surface.t`. This document records what
each layer *is* and what invariants hold across it — the part a port (or that
deletion) must carry regardless of how many trees survive.

The *model* is the macro-system design: Honu enforestation, Flatt's sets of
scopes, and an ordered, binding-at-a-time interleaving of expansion with
elaboration (papers in [`macro-system/papers/`](../macro-system/papers/), design in
[macro-interleaving-design](macro-interleaving-design.md));
the glossary follows it. The S-invariants below describe the current
implementation, which imitates that model imperfectly: it runs enforest,
expand, lower and elaborate as fixed passes (so lowering, and `Surface.t`, have
no counterpart in the model), keys operator lookup by string, and mints no
use-site scope for any application and no intro scope for procedural macros.
Where the two disagree, the implementation is the defect.

## The four layers, and what each one commits to

| layer | produces | commits to |
|---|---|---|
| reader (`Raw_syntax`) | tokens and delimiter groups | nothing — no forms, no names resolved |
| enforestation (`Syntax.t`, `Enforest`) | typed forms, ids carrying scope sets | what a *form* is; where macros interrupt the parse |
| expansion (`Expand`) | the same tree, alpha-renamed | which binder each occurrence denotes |
| lowering (`Surface.t`, `Lower_surface`) | the elaborator's input | strips spans, scope sets, `MacroDef.kind`, `SyntaxOperatorUse.unit` — nothing else |

**The reader commits to nothing but grouping** (S1, enforced by construction).
`Raw_syntax.t` is `Token | Group(delimiter, items, span)`. Keywords are fixed
token kinds the lexer owns; operators are one uniform `Operator of string`. No
form is decided, no name is resolved — a group is a group whether it becomes a
type annotation, a tuple, or a macro argument. This is what makes syntax
extension possible at all: a macro receives raw material, not pre-parsed
someone-else's-decision.

**Enforestation is parsing interleaved with macro expansion** (S2). There is no
grammar the enforester serves: the operator table decides precedence and
fixity, and a macro head *hijacks* the parse — the macro's own parser consumes
the remaining tokens and returns a form. This is where I4c lives bodily:
operator lookup is string-keyed and newest-wins because scope sets do not exist
yet at parse time (`Binding.find_operator`'s comment defers scope-keyed
resolution). The syntactic role of a name is decided here, by different rules
than every later resolution.

## Scope sets and resolution

**Resolution is sets-of-scopes** (S3, enforced by construction).
`Binding.resolve`: candidates are the binders sharing the occurrence's
*written* name; keep those whose binder scope ⊆ occurrence scope; the **largest**
binder scope wins; two incomparable candidates raise `ambiguous binding` —
loud, not last-wins. This is Flatt's model, with the ambiguity made an error
rather than a silent pick.

**Scopes are minted per binder and added within source regions** (S4, enforced
by construction, with one hole). Every binder form (`Lam`, `Let`, `TypeDef`,
type params, `open`) mints one fresh scope and adds it to the ids inside its
region — where *region* is a **source span**, and `span_contains` treats any
synthetic span as always-inside. Two consequences, one wanted and one not:

- ids macro-*written* with synthetic spans receive every enclosing binder's
  scope — the machinery procedural-macro hygiene tests pass through;
- ids *substituted* into an expansion receive it too — see D-A below. The
  region rule approximates "written in this expansion", and the approximation
  is exactly what fails.

**Templates additionally mint an intro scope** — a fresh negative-int scope
(`fresh_intro_scope`) stamped on everything the expansion produced, so one
expansion's ids are distinguishable from another's. An intro scope
distinguishes; it does not bind. Procedural macro output gets none.

**Expansion alpha-renames values only** (S5, enforced by construction). `Lam`,
`Let`, method and macro binders mint a fresh scope *and* a fresh
`resolved_name` (`x` becomes `x__0` on collision); their occurrences are
rewritten to it. Type, constructor, trait and effect names bind with
`resolved_name = written name` — they pass through unchanged, distinguished by
scope sets only until lowering throws the scopes away (the IR-layers ticket's
finding 3). **Lowering is therefore only safe because expand already ran** —
which is why the two lowering sites that skip expand
([type-aware-macro-output-is-not-expanded](../tickets/type-aware-macro-output-is-not-expanded.md))
are defects and not just asymmetries.

**Resolution is two-tier, and the second tier is a string** (S6, unchecked
convention, load-bearing). An id that resolves to nothing at expand time keeps
its *written* name; the elaborator resolves that string against its flat
namespace (pass one's I4). The tier is what lets a macro write `Syntax.var("True")`
and reach the prelude constructor — macro-written free names have no scope set,
so the first tier can never see them. It is also unhygienic by construction:
the name resolves by spelling, so it lands on whichever binder of that
spelling the elaborator's context holds — the outer one, if the inner was
renamed ([block-local-macros-leak-by-written-name](../tickets/block-local-macros-leak-by-written-name.md)
is the macro-table side of the same fall-through). The port must know this
tier exists and why, or it will "fix" it and break every macro that writes a
prelude name — or keep it and inherit the leak.

**Hygiene governs only bare-name ids** (S7, enforced by construction).
Scope sets ride on `Syntax.id`; members (`FieldAccess`'s string), pattern
constructor heads (`PatCon`'s strings), effect operations and record fields are
plain strings throughout. This is the syntactic mirror of pass one's I4b: the
bare-name namespace is exactly the hygienic one, and the member namespace is
exactly the elaborator-resolved one. A macro that writes `M.field` writes the
field name as a string and means it.

In the model the split is *bare names vs labels*, not *bare names vs
everything else*. Members are labels, resolved by their container, and rightly
carry no scopes. Pattern constructor heads are bare names: `PatCon` holding
strings is a defect, the pattern-position twin of
[template-literals-resolve-at-use-site](../tickets/template-literals-resolve-at-use-site.md).

## Three macro paths, three hygiene contracts (today)

The model has one contract for every macro and template application: an intro
scope and a use-site scope, quoted ids resolving at the definition site, splices
keeping their scopes, and output expanded in place. The table records how far
each implementation path is from it; every bold cell is a defect.

| path | runs | heads keyed by | spliced args | written literals | output |
|---|---|---|---|---|---|
| untyped procedural (`macro`) | expand time | resolved name (scope-aware) | value round-trip — **scope set dropped** | resolve in caller (two-tier) | re-expanded |
| type-aware procedural (`: Expr(A)`) | elaborator time | resolved name | same round-trip loss | same | **never expanded** |
| template (`syntax`, `pub infix`) | parse time, at use site | operator table (string) | direct `Syntax.t` splice — hygienic | **resolve at use site** | re-enforested in place |

The template row's literals are the hole in "templates are hygienic": splices
are clean (no round-trip), but ids *written in the replacement* resolve against
the caller — which is how a use-site `False = 42` turns `&&` into a constant-42
machine ([template-literals-resolve-at-use-site](../tickets/template-literals-resolve-at-use-site.md)).

The untyped row's splice column is
[procedural-macros-capture-use-site-variables](../tickets/procedural-macros-capture-use-site-variables.md),
now diagnosed: `Macro_eval.value_to_id` hardcodes `scope = Scope_set.empty`,
so a spliced occurrence forgets its occurrence scope, receives only the
macro binder's fresh scope (S4's always-inside rule), and is captured.

The type-aware row's output column is the IR-layers ticket's finding 4.

**The implementation has no single hygiene invariant yet** — there are three,
one per path, only one of which (untyped heads) is both hygienic and tested.
What the one contract *is* is settled (above, and in `CONTEXT.md`); the defect
tickets are the distance to it, joined by
[macros-have-no-quoted-syntax](../tickets/macros-have-no-quoted-syntax.md) for
the written-literals column.

## The seam (today)

No counterpart in the model: expansion and elaboration interleave binding by
binding, so there is no hand-off tree. Recorded because the port must know what
the current tree carries, and because
[delete-surface-ir](../tickets/delete-surface-ir.md) removes it.

What the expander hands the elaborator: a `Surface.t` whose value names are
alpha-unique strings, whose type-namespace names are written strings, whose
spans and scope sets are gone, and whose `StxExpr` nodes are *unstripped*
`Syntax.t` — opaque syntax deliberately smuggled past lowering for the macro
reflection ADTs. `Surface.t` is not a language level; it is an erasure of one,
plus one escape hatch.

The elaborator holds no expander state — only the `macro_runtime` capability
(pass one's I4e) and a copied macro table (I4d). Every macro it runs returns
`Syntax.t` that skips the first tier entirely on the type-aware path.

## Defects the modelling found

Not restated here — each lives in its ticket:

- [procedural-macros-capture-use-site-variables](../tickets/procedural-macros-capture-use-site-variables.md)
  — diagnosed this pass: scope sets die at the macro value boundary.
- [template-literals-resolve-at-use-site](../tickets/template-literals-resolve-at-use-site.md)
  — found this pass: `&&`/`||` silently corruptible by a use-site binder.
- [type-aware-macro-output-is-not-expanded](../tickets/type-aware-macro-output-is-not-expanded.md)
  and [block-local-macros-leak-by-written-name](../tickets/block-local-macros-leak-by-written-name.md)
  — found by the IR-layers research; this pass places them in the model
  (S5's skipped-expand seam, S6's fall-through).

## What the port's types should be named after

- **Reader** for the grouping pass; **Group** for its one structural notion.
- **Form** for a typed node; **Syntax object** for the macro-visible tree
  (`Syntax.t`) whose ids carry scope sets.
- **Resolved name** for the alpha-unique key a binder is registered and
  rewritten to — the string the elaboration context is keyed by. Every binder
  gets one; the implementation's type-namespace exception (S5) is a defect.
- **Intro scope** and **Use-site scope** for the two fresh scopes every macro
  or template application mints. Today templates mint only the first and
  macros neither.
- **Template** for the pattern rewrite; **macro** for the procedural one. Two
  declaration forms, one hygiene contract — the three contracts in the table
  above are the implementation's defects, not a distinction to keep.
- **Quoted syntax** and **Borrowed context** for how a macro writes names. The
  implementation's string fall-through (S6) has no name in the port: it is
  replaced, see [macros-have-no-quoted-syntax](../tickets/macros-have-no-quoted-syntax.md).

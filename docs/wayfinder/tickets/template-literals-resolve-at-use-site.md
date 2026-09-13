---
title: Template literals resolve at the use site
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Template literals resolve at the use site

## Question

A `pub infix`/`pub prefix` template's replacement is re-enforested **at the use
site** (`Enforest_template.expand` → `parse_expr_with_captures`), so ids written
literally in the replacement resolve against the *caller's* bindings, not the
declarer's. A use-site binding of a prelude name silently rewrites the
template's meaning.

## Evidence

Found by the [domain-model surface pass](domain-model-surface-enforestation.md).
The templates in question are the prelude's own (elab_prelude.ml:71-72):

```
pub infix (&&) 4 Left ($a, $b) -> match $a do True -> $b | False -> False end;
pub infix (||) 3 Left ($a, $b) -> match $a do True -> True | False -> $b end;
```

`$a`/`$b` are holes (spliced captured syntax — hygienic). But `False`/`True` in
the branch *bodies* are literal ids, and they resolve at the use site:

```
do False = 42; (1 > 2) && 99 end   =>  42      -- the && template's own `False` became the caller's 42
do True  = 7;  (2 > 1) || 99 end   =>  7       -- likewise `||`'s `True`
```

Both answers are wrong (`&&` should yield `False`, `||` should yield `True`),
and both are **silent** — no error anywhere. `pub False = 42` anywhere above a
use of `&&` corrupts it. Shadowing `True` instead moves the corruption to the
 scrutinee position. The same holds for any user `syntax` template whose
replacement mentions a bare name: the meaning follows the caller, not the
declarer.

Not affected: `pub prefix (not) 30;` — no replacement, still a primitive behind
operator syntax, so nothing to capture.

## Why the current mechanism misses it

`instantiate_template_replacement` stamps the enforested replacement with one
fresh **intro scope** (`fresh_intro_scope`, negative counter — disjoint from
expander scopes), so successive expansions are distinguishable. But an intro
scope *distinguishes*; it does not *bind*. The literal's scope set is
`{intro}` ∪ whatever use-site binder scopes its use-site span picked up, and
the declarer's bindings are nowhere in it — so resolution either misses
(deliberate fall-through to the elaborator's namespace, which is where
`False`/`True` usually come from) or hits a use-site binding first, whichever
the scope arithmetic picks. Both outcomes were observed.

## Decided (2026-09-14)

[M9](../topics/core-tt-domain-model-macros.md): a template is sugar for a
macro whose body is the replacement as quoted syntax, parsed at the
definition. That resolves literals at the declaration by construction; the
options below are superseded. Lands with
[macros-have-no-quoted-syntax](macros-have-no-quoted-syntax.md).

## Direction (superseded)

Literal ids in a replacement should resolve in the **declaration context**, the
way they would if the template's body had been written where it was declared.
The template already records `declaration_span`; what is missing is the
declarer's scope environment at instantiation time. Options to weigh:

- Stamp replacement ids with the *declaration-site* scope set captured when the
  template was declared (the `inherited_captures` plumbing already threads
  declaration-time context through `env.template_captures` — but for captured
  syntax, not for scope stamps).
- Resolve replacement literals to resolved names **at declaration time**
  (pre-emptively alpha-rename), leaving only holes to instantiate — the
  procedural-macro `MacroDef` path already does exactly this to its body, which
  is why macros do not have this defect.
- Scope-keyed operator resolution (deferred repeatedly, see I4c and
  `Binding.find_operator`'s comment) subsumes the fixity half of this; the
  literal-resolution half is independent of it.

Whichever is chosen, the two prelude repros above become regression tests; the
fix changes `&&`/`||` expansion for shadowing programs, so land it with the
[regression-coverage](../../topics/regression-coverage.md) suite in mind.

## Second symptom: pattern heads

Pattern constructor heads are plain strings (`Syntax.PatCon`), so they carry no
scope set at all. A template or macro that writes `match x do True -> … end`
gets whichever `True` the caller has in scope. In the model a pattern head is a
bare name (`CONTEXT.md`) and resolves at the definition site like any other
quoted id.

---
title: Does a macro own its output, or does the site?
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
---

# Does a macro own its output, or does the site?

Raised 2026-09-18 while deciding what `export` means in a block
([port-stage2-residue](port-stage2-residue.md) item 1). Deferred deliberately: the
35 cases were bought with the site rule instead. **Decide this before a second
declaration macro exists.**

## The two mechanisms

A declaration macro's output is adapted to where it was used. Today that happens
**at the site, behind the macro's back**:

```
pub type Color = Red | Green
```

`type_decls` emits three decls, none of them `pub`:

```
rec Color = enum { Red, Green };   export Color;   open Color;
```

`expand.ml:1045` then applies `Syntax.publish` (`syntax.ml:574`) to every emitted
binding, flipping `public = true` on each — the `export` included. At a block site,
`expand.ml:569` drops the `export` instead. The macro knows nothing of either.

The alternative is **the macro adapts itself**, given its site:

```
syntax type : Decl { type $(r : List(TokenTree)) => { type_decls($r, $site) } };

pub macro type_decls(ts : List(TokenTree), site : Syntax.Site) : List(Decl) {
  body = …;
  match (site) { InModule => Cons(export_decl, body), InBlock => body }
};
```

`$site` is an ambient hole: the expander fills it from where the form was used, as it
already fills `$r` from what the use captured.

## The ruling this ticket needs

**These are competing answers to one question, not two features.** Adopting `$site`
while `publish` still distributes `pub` leaves `pub type` working through one
mechanism and `type`-in-a-block through the other. So the ticket is:

> If a macro is given its site, does `Syntax.publish` distribution go away — the macro
> emitting `pub` itself when its site is a module and its use said `pub`?

Decided in the affirmative (user, 2026-09-18, in principle): **M1 should accompany the
removal of `publish`.** Not scheduled. What is still open is whether that survives
contact with the cases `publish` currently handles — every binding variant in
`syntax.ml:574` is one a macro would then have to publish for itself.

## Why it was deferred

One client. `type_decls` is the only macro in the prelude and the only `: Decl` syntax
form (`dotnet/std/stage2.fun:174,184`); the conformance cases whose macros emit `pub`
do so directly and are unaffected. A language feature with one caller, costing a new
`Syntax.Site` nominal and a new ambient hole kind — six reflection sites per
`CLAUDE.md`, in both implementations — that buys no cases on its own.

**Trigger to pick this up:** a second declaration macro that needs its site, or any
change to what `publish` distributes.

## What was decided instead (2026-09-18)

The site adapts, and the rule is written down — see
[port-stage2-residue](port-stage2-residue.md) item 1. A **generated** `export` at a
block site is dropped; a **source-written** one is an error. That keeps the silent
no-op out of user code without giving the macro a site.

## Direction (user, 2026-09-18): take this from Klister, generically

`$site` as sketched above is the ad-hoc version — one ambient hole, one enum, one
question ("module or block?"). Klister already has the general form, and this repo
has the material extracted:

```
(define-macro (convert)
  (>>= (which-problem)
    (lambda (problem)
      (case problem
        [(expression fun-type) …]))))
```

`which-problem` is a macro-monad primitive returning **the problem the expander is
currently solving** — declaration, type, expression or pattern — carrying the expected
type where there is one
(`docs/wayfinder/macro-system/extracted/klister/commentary/interleaving.md`;
`examples/which-problem.kl`, noted in `source-notes/implementation-map.md:140`, is one
macro expanding four ways).

Why that is the better target than `$site`:

- **One primitive, not one hole per question.** "Module item or block statement" is a
  single instance of "what is the expander solving right now". A second question
  (expected type, pattern position, whether the use said `pub`) would want a second
  ambient hole under the `$site` design and nothing new under this one.
- **It joins work already done.** Type-aware interleaving is built
  (`Macro_driver`, `docs/wayfinder/macro-system/TYPE_AWARE_INTERLEAVING.md`), and the
  expected type a typed macro already receives is exactly the `(expression fun-type)`
  payload of Klister's `problem`. Two halves of one primitive.
- **It is hardened by an existing design** rather than invented here, which is the
  standing rule for macro-system vocabulary: follow Honu, sets-of-scopes and Klister,
  and treat a deviation as a defect.

Read before designing: `interleaving.md` (the macro monad and the task queue,
including how a task blocks on an unsolved problem), and
`commentary/architecture.md:171` on the signal system.

Still later, not now. The trigger above is unchanged.

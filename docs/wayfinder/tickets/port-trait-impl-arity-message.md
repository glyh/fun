---
title: "Port: the trait/impl arity refusal is a language error"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: the trait/impl arity refusal is a language error

Found by the coverage sweep (2026-09-24), which probed it in both runners:
`{ trait P = sig { … }; 1 }` and multi-parameter spellings are **rejected by the prototype
too** ("trait declaration requires exactly one parameter"). So the port's refusals here are
about the *language*, not about an unported path — and convention 2 says a language error is
a `FunException`, never `NotImplementedException`.

That distinction is load-bearing: today those sites make an `error` case pass for the wrong
reason, and they would also make a *missing feature* look like a language rule to anyone
reading them later.

## What to do

1. Probe each site in **both** runners — a trait/impl spelled with zero, two and three
   parameters — and convert the ones where the prototype refuses too. The sweep probed
   `Reflection.cs:723`, `:731`, `:927`, `:933`; **`:767` it did not**, so treat that one as
   unverified until you construct a program for it.
2. The message should say what the language requires, in the port's own words (the glossary's
   vocabulary, per convention 1), not "not ported yet".
3. Add the ordinary `error` case (both runners refuse, so nothing goes in
   `prototype-divergences.txt`) so the conversion is pinned and cannot drift back.

## Reading

- `dotnet/src/Fun.Compiler/Reflection.cs:723`, `:731`, `:767`, `:927`, `:933`
- the prototype's own arity check for traits/impls (`lib/semantic/typecheck/`)
- [the probed rows' conversions](port-probed-row-conversions.md) — the same pattern, done
  for a different set of sites

# Trait plan

## Goal

Add nominal traits for ad-hoc polymorphism. Traits provide named operation sets such as equality while preserving explicit language semantics: implementations are resolved by the elaborator, passed as hidden runtime dictionaries for generic code, and may be specialized/erased later as an optimization.

Target surface:

```fun
trait Eq(A) = sig
  eq : A -> A -> Bool
end

impl Eq(I64) = module
  fn eq(x, y) -> eq_i64(x, y)
end

same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) -> Eq.eq(x, y)
```

Multi-trait bounds use Rust-like `+` syntax:

```fun
encode_key : [A : Eq + Jsonable] -> A -> String = ...
```

## Decisions

- The feature is called `trait`.
- Traits are nominal declarations, not structural records.
- Trait operation signatures use `sig ... end` or signature-shaped `module ... end` syntax.
- Implementations use `impl Trait(Arg) = module ... end` syntax.
- Trait evidence is not a user-facing value.
- Trait dictionaries are passed hidden at runtime for generic functions.
- Compile-time specialization and dictionary erasure are optimizations, not initial semantics.
- Trait methods are called with qualified syntax such as `Eq.eq(x, y)`.
- Implementations are resolved from lexical/imported scope.
- Multiple matching implementations in scope are an ambiguity error.

## Current prototype status

Implemented:

- `trait` and `impl` tokens, parser forms, and surface AST nodes for expression and struct contexts;
- nominal trait identities tracked by elaboration context;
- operation signature elaboration under trait parameters;
- implementation checking for duplicate, missing, and unknown operation fields;
- dictionary-shaped internal evidence values with hidden implicit binders;
- trait-bound function types for `[A : Eq]` and `[A : Eq + Jsonable]`;
- direct qualified method dispatch through `Eq.eq`;
- generic dispatch where a trait-bound function receives or forwards hidden evidence;
- local implementation lookup and ambiguity errors;
- syntax and semantic tests for declaration shape, implementation shape, bounds, method dispatch, missing implementations, duplicate fields, and missing fields.

Still incomplete:

- qualified trait paths for imported/module traits are parsed but rejected during elaboration;
- public/imported implementations do not yet participate in module visibility rules;
- implementation lookup is intentionally local and lexical;
- record/ADT deriving, associated types, supertraits, default methods, trait objects, and dictionary erasure remain non-goals for this first pass.

## Phase 1: Syntax and AST

Add tokens for `trait` and `impl`.

Surface declarations:

```ocaml
type trait_sig_field = string * t

type trait_bound = {
  trait_path : string list;
  trait_name : string;
}
```

Extend implicit arrow binders so `[A : Eq]` and `[A : Eq + Jsonable]` can represent a type parameter plus trait bounds, distinct from the existing `[A : Type]` ordinary implicit binder.

Add expression forms:

```ocaml
| TraitDef of {
    name : string;
    params : string list;
    fields : trait_sig_field list;
    body : t;
  }
| ImplDef of {
    trait_path : string list;
    trait_name : string;
    args : t list;
    fields : (string * t) list;
    body : t;
  }
```

Parser targets:

```fun
do
  trait Eq(A) = sig eq : A -> A -> Bool end
  impl Eq(I64) = module fn eq(x, y) -> eq_i64(x, y) end
  f : [A : Eq] -> A -> A -> Bool = ...
  g : [A : Eq + Jsonable] -> ... = ...
end
```

Keep this phase syntactic only where possible; do not require full trait resolution before parser tests pass.

## Phase 2: Core representation

Add nominal trait identities and hidden dictionary binders.

Possible core additions:

```ocaml
type trait_id

type trait_bound = {
  trait_id : trait_id;
  trait_name : string;
  args : term list;
}
```

Represent trait dictionaries as internal struct-like runtime values, but mark binders/evidence as hidden so source code cannot name or pass them directly.

For a bound:

```fun
[A : Eq + Jsonable] -> R
```

elaborate conceptually to:

```fun
[A : Type] -> [hidden eq : Eq(A)] -> [hidden jsonable : Jsonable(A)] -> R
```

The exact core encoding can reuse implicit `Pi` initially if that is simpler, plus an internal naming convention/metadata for dictionary binders.

## Phase 3: Trait declaration elaboration

Elaborate:

```fun
trait Eq(A) = sig
  eq : A -> A -> Bool
end
```

into a nominal trait definition with:

- stable trait identity;
- parameter list;
- operation signatures checked under trait parameters;
- a trait type constructor available in the body.

`Eq` should be nominal: another trait with the same fields is not interchangeable.

## Phase 4: Impl declaration elaboration

Elaborate:

```fun
impl Eq(I64) = module
  fn eq(x, y) -> eq_i64(x, y)
end
```

by:

- resolving the trait head nominally;
- checking implementation arguments against trait parameters;
- checking each provided operation against the instantiated trait signature;
- rejecting missing, extra, or duplicate operation implementations;
- registering the implementation in lexical scope for the body.

Implementations should not introduce ordinary user-accessible values unless explicitly exported through the future module/import design.

## Phase 5: Trait-bound function elaboration

For an implicit binder with trait bounds:

```fun
[A : Eq + Jsonable] -> R
```

elaborate as:

1. bind `A : Type`;
2. add hidden dictionary binders for `Eq(A)` and `Jsonable(A)`;
3. make those dictionaries available to trait method lookup in the function body.

For function application, resolve missing hidden dictionaries from lexical/imported impl scope when the trait arguments are concrete. If the callee already has an abstract hidden dictionary requirement, pass the current bound dictionary through.

## Phase 6: Method lookup

Resolve qualified trait calls:

```fun
Eq.eq(x, y)
```

by:

- resolving `Eq` as a trait, not a struct/module value;
- resolving `eq` as an operation of that trait;
- inferring enough argument/type context to determine the required trait instance;
- selecting a hidden in-scope dictionary or concrete impl;
- elaborating the operation call as a projection/application from the dictionary.

Ambiguity rules:

- zero matching impls: elaboration error;
- exactly one matching impl: select it;
- multiple matching impls: ambiguity error.

## Phase 7: Imports and lexical scope

Imported public impls should participate in lookup only when imported/opened according to normal lexical visibility rules.

Initial implementation can restrict impls to local lexical scope if import interaction is too large, but the target behavior is lexical/imported lookup.

## Phase 8: Tests

Syntax tests:

- trait declaration shape;
- impl declaration shape;
- single trait bound `[A : Eq]`;
- multi-trait bound `[A : Eq + Jsonable]`;
- qualified method call `Eq.eq(x, y)`.

Semantic tests:

- trait declaration checks operation signatures;
- impl checks operation bodies;
- missing operation rejects;
- extra operation rejects;
- duplicate impl ambiguity rejects;
- generic function with `[A : Eq]` typechecks;
- `[A : Eq + Jsonable]` typechecks and exposes both trait methods.

Backend tests:

- `Eq.eq(1, 1)` evaluates through `impl Eq(I64)`;
- generic `same : [A : Eq] -> A -> A -> Bool` works for `I64`;
- generic function passes hidden dictionary through another generic function;
- lexical shadowing/ambiguity behavior is enforced;
- imported impl is found after import/open once import support is implemented.

## Non-goals for the first implementation

- Deriving `Eq` for records or ADTs.
- Automatic structural equality for records.
- Orphan/coherence policy beyond lexical ambiguity checks.
- Specialization or dictionary erasure.
- Associated types.
- Supertraits.
- Trait objects or first-class dictionaries.
- Default method implementations.

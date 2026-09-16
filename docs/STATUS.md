# STATUS — canonical current implementation snapshot

This is the **authoritative** status document for the `fun` compiler prototype.
When other docs disagree with this file, STATUS.md wins.

Last updated: after .NET port wave 1, 2026-09-16.

---

## Completed

### .NET port — slice 2a and wave 1 (2026-09-16)

- **89 of 618 conformance cases pass in C#** (73 xUnit). Added since slice 1:
  first-class modules and `open` with open choices; imports of compilation units
  (base-anchored, value transport, cycles, elaborate-once); implicit parameters and
  insertion; structs, record construction, signatures, module-type unification,
  struct methods; `rec` values and groups with the checker budget and lazy delta;
  `match` compiled to decision trees, non-recursive enums as nominals (E11).
- Built by five parallel forks against written conventions (port ticket,
  "Porting conventions"); each wave-1 ticket records its merge and follow-ups.
- The prelude's source is copied to `dotnet/std/stage{1,2}.fun`; the OCaml
  prototype is not maintained once the port is done.
- A bare constructor pattern resolves like any other name (binder or open); the
  prototype's by-name lookup is ticketed and listed as a divergence.

### .NET port — slice 1: reader to evaluator (2026-09-16)

- The C# port lives in `dotnet/` in this repo, so `test/conformance/cases` stays
  one copy. Three projects (`Fun.Kernel`, `Fun.Expand`, `Fun.Compiler`, plus
  `Fun.Cli` and two test projects): the split enforces that `Fun.Expand` cannot
  reference the elaborator, as `core_tt_expand` cannot today. `dune build`
  ignores `dotnet/` (root `dune`, `(dirs :standard \ dotnet)`).
- The whole pipeline runs for the prelude-free subset: a hand-written reader
  (`System.Buffers.SearchValues`), enforestation with blocks read one statement at
  a time, scope-set expansion, bidirectional elaboration with metavariables, and
  the evaluator as an explicit frame-stack machine (a million nested calls run
  without touching the native stack).
- **12 of 604 conformance cases pass**, each for a genuine reason: literals,
  lambdas, `let` with shadowing and annotations, and three elaboration cases.
  41 xUnit cases cover the reader, hygiene, the machine, unification and
  kernel equality.
- Unported forms raise `NotImplementedException`, which the runner counts as a
  failure and never as a passing `error` case; so does a name the base context
  lacks inside the prelude open, and (until `std`'s syntax roles are ported) any
  enforest error.
- The domain model is the port's specification. Two prototype defects are
  ticketed and fixed in C# only: a meta's solution under a spine is renamed
  without lifting under binders, and checking a lambda ignored its written
  parameter type. The shared suite states the specified behaviour: cases the
  prototype gets wrong are listed in `test/conformance/prototype-divergences.txt`,
  which the OCaml runner expects to fail (`elab-049` is now `error`; three cases).
- Kernel records hold sequences as `EquatableArray<T>`: `ImmutableArray<T>`
  compares by reference, which made structurally equal records unequal.
- Decisions and the slice plan: `docs/wayfinder/tickets/port-core-tt-to-dotnet.md`.


### Stage 11 increment 2 — keyword surface (2026-09-16)

- `then`, `with`, `end`, `else` and `Unit` are no longer keyword tokens: no parser
  rule matched them (a syntax form's rule literals compare by spelling, so the
  prelude's `if` form matches `else` as a plain token; `Unit` had an identical
  `Ident` path in expression and pattern position). They are ordinary identifiers.
- Already library, confirmed by survey: `if`/`else`, `&&`/`||`, arithmetic and
  comparison operators, prefix `not`, `type` (stage-2 std macro).


### Shared conformance suite (2026-09-16)

- `test/conformance/cases/<area>/<name>.fun` + `<name>.expect` is the
  language-behaviour suite both the prototype and the .NET port run; extra units
  are `<name>.unit-<unit>.fun`. 601 cases (`values`, `macros`, `imports`,
  `elaborate`), extracted from the Alcotest binaries. **It is now the only copy:**
  the Alcotest cases it covers are deleted (`test_core` 443 → 172 cases,
  `test_elaborate` 500 → 269), so a language behaviour is tested in exactly one
  place.
- `.expect` is a value, a constructor name, `ok` (elaborates) or `error` (fails).
  Error wording is not pinned - it is implementation-specific.
- Runner: `test/conformance/run_conformance.ml`, `dune test test/conformance`.
  Internal tests stay in Alcotest: shapes, reflection round trips, budget
  accounting, macro nominals plumbing, an exact error constructor
  (`expect_elab_error`/`expect_expand_error`), a type rather than a value
  (`check_type`/`check_conv`), and a program whose expectation lives in the
  caller.


### Staged prelude; `type` is a std macro; `export` (2026-09-16)

- The prelude elaborates once, in two stages: stage 1 declares `Bool`, `Option`,
  `List` and `Syntax` as enums (no elaborator); stage 2 is a driver run against it
  that re-exports it and defines the operators, `Eq` and `type`
  (`Macro_driver.init_ctx`, `std_syntax`, `std_load_syntax`).
- `type` is a std syntax form over the macro `type_decls` expanding to
  `rec … = enum { … } [and …]; export …; open …`; the compiler's type declaration
  is deleted. An `import "std"` delivers roles and compiled macros
  (`Expand_ctx.unit_syntax`).
- `export M` / `export M.{a, b}`: members, enum constructors, named impls, a unit's
  roles and macros; clashes are errors.
- Blocks apply declaration macro calls and read unread items; a block's
  `rec … and …` enum group elaborates; type-case refinement skips names bound
  before its target.
- Known: in a recursive function, `match (l) { …, Cons(m, Nil) => …, Cons(m, rest) => … f(rest) }`
  fails at run time with "match on non-constructor value" (pre-existing; the
  prelude's macro avoids the shape).

### Effects on the arrow; `~>`; bound sets (2026-09-16)

- A row sits on its arrow: `A ->{Log, Exc} B`, open `A ->{Log | e} B`, a variable
  alone `A ->{e} B`, inferred `A ->{_} B` (unsolved at the entry is
  `UnsolvedEffectRow`). A bare arrow is pure. `can` is deleted.
- A row is known effects plus a **set** of row variables (`A ->{Log | e1, e2} B`),
  so a result may unite several callbacks' rows. A tail solved to a row is
  spliced in (`normalize_effect_row_value`), so a union disappears as its
  variables are solved; a union of two unsolved tails against a concrete row is a
  mismatch, not a guess.
- A type that mints its own row (`Callback = Unit ~> I64`) is a function of that
  row; a parameter naming one mints it at the definition that takes it (rank 1),
  so its caller chooses the effects. A binder written out in the annotation stays
  rank 2.
- `~>` (a base role): every function type is read on its own - each parameter's
  type is a signature in its own right, and the chain's **final** arrow carries
  the variables its parameters minted (`Elab_poly_arrows`), so a partial
  application is pure; on a definition the final arrow also infers what the body
  performs. A final `~>` with no parameter to collect from mints its own variable.
- Definitions: `fn(n : I64) : I64 { … }` when pure, `fn(n : I64) ->{Log} I64 { … }`
  or `fn(g : Unit ~> I64) ~> I64 { … }` when not; `pub method m() ->{Exc} I64 { … }`.
  `: T` is the pure member of that family: it states the function's type with an
  empty row (so it needs every parameter's type), and a body that performs under
  it is `EffectsInPureResult`, which names the effects and points at the arrow
  forms. A method with no declared row reads the same way.
- Trait bounds are a set: `[A : {Eq, Show}]` (a single bound may stay bare).
- Row unification sends each side's unnamed effects to the other side's tail.


### `export`; block-level enum groups (2026-09-16)

- `export M` / `export M.{a, b}` re-exports a module's public members or an enum's
  constructors as members of the enclosing module, opening nothing locally;
  clashes are `ExportClash`; a unit's roles re-export with it. A module with
  public impls is `ExportImpls` (undecided).
- A block's `rec A = enum { … } and B = enum { … }` elaborates with the module
  knot; its slots become `Let`s.
- The `type` prelude macro is blocked: the prelude has no elaborator to compile a
  procedural macro, and `std` delivers no macros (see the ADT ticket).

### Enums: ADTs as values (2026-09-16)

- `enum { Red, Some(A) }` is a type value; its constructors are members:
  `Color.Red`, `open Color`, and patterns `Color.Red`. It elaborates as the block
  type declaration it names (`TypeDef`), so `type … = …` shares its path.
- `Option = fn(A : Type) { enum { Some(A), None } }`: `Option.Some` is generic over
  the former's parameters; `Option(I64)` is one type (E11 captures).
- `rec Tree = enum { Leaf, Node(Tree, Tree) }`, `rec L = fn(A : Type) { enum { … L(A) … } }`,
  and a module's `rec A = enum { … } and B = enum { … }`. Block enum groups and
  enum/struct mixes are errors.


### Effects follow-ups (2026-09-16)

- **Tunneling by lexical handler and instance.** A call with an open row is
  `Tunnel { named; handlers }`; a request whose effect instance (family and
  parameters) its row does not name passes the handlers lexically enclosing the
  call (`effect_request.skips`; each `EffectBranch` carries its handler id).
- **E6 through outer references:** storing a closure that names a handled effect
  into a reference not local to the match is `HandledEffectEscapes`; so is the
  shape where another branch fixed the closure's row.
- **Refs:** an unhandled `Mutate` names the reference (`effect Mutate(r)`); a
  `let` or block with private mutation discharges its local heaps; local-heap
  detection is one pass over older metas; `EffectRef` carries the effect's id.

### Macro-system fixes for a `type` macro (2026-09-16)

- **`pub` before a declaration syntax form or macro call** makes every
  declaration it returns public (read items included). `InstantiateBinding` and
  `MacroCallBinding` carry `public`, reflected both ways (`Syntax.publish`).
- **`List(TokenTree)` holes and parameters:** a `: Decl` form's last hole of that
  kind takes the rest of the use as unread token trees (`CapTokens`); a macro
  parameter of that kind takes its argument's tokens. A hole written as a
  macro's whole argument in a replacement takes the captured tokens. Anywhere
  else (not last, not `: Decl`) is an error at the definition.
- **`type` is an ordinary identifier.** The built-in type declaration is the base
  role `TypeDeclaration`, resolved by scope set, so a user form named `type`
  shadows it.

### E11: captures from the enclosing scope; generative modules sealed (2026-09-16)

- A nominal captures the variables its enclosing module or function body names
  (from the first bound variable on), so `Set(I64, less).T ≠ Set(I64, greater).T`.
- A `let` or module member whose value performs something seals its type at the
  binder: a declared type member becomes `st1.Symbol`, distinct from `st2.Symbol`.
  An unnamed effectful module's declared type may not escape a field access
  (`GenerativeTypeEscapes`).
- Run-time stamps: a module's private stamp slot is a fresh cell per evaluation
  of a generative module; type-case compares nominal instances by captures and
  stamp. Sealing picks declared nominals by identity; a sealed type may not leave
  its binder's scope or its module's type.
- A `rec` struct occurrence carries captures and unfolds per instance.
### Nominal identity is applicative (E11, first half, 2026-09-15)

- A nominal's identity is its declaration id plus `captures`, the values of the
  declaration's own free variables, compared by conversion (`VNominal`,
  `NomRef`). `NomRef` builds the nominal directly; constructors come from
  `Core.nominal_decls` over an instance's captures. No environment scan.
- `TypeBind` / `NominalDef` build nominals in their evaluation scope: a type
  declared under a binder evaluates, and `Set(I64, less)` twice shares `T`.
- Module values quote each binding at the depth the earlier bindings push.
- Open: generative nominals under an effectful maker (sealing), and `rec` struct
  identities under binders (see nominal-identity-applicative-by-purity).

### Handlers tunnel callback effects (2026-09-15)

- Handling is lexical (E5): a call whose latent row has an open tail is wrapped
  in `Core.Tunnel`; a request of an effect family the row does not name, which a
  handler lexically enclosing the call in the same function body handles, skips
  those handlers (`effect_request.hops`, decremented by each handler of that
  family). A lambda or method body starts with no enclosing handlers
  (`Ctx.handler_scopes`). `find(user, 1)` answers 999.
- A handled effect may not escape (E6): a match whose result type carries a
  function whose row names a family it handles is `HandledEffectEscapes`. A saved
  continuation may outlive its handler (its row is the residual).
- Not covered: per-instance (vs per-family) routing; an escape through an outer
  ref's type; `resume` returns the scrutinee value without re-running the value
  branch (pre-existing).

### Refs in effect rows (2026-09-15)

- **Using a reference is an effect.** `ref(e)`, `deref(r)` and `r <- e` perform
  `Mutate(h)` on the reference's hidden heap. `Ref : [h : Type] -> Type -> Type`
  takes the heap as an implicit argument, so `Ref(I64)` is unchanged and each use
  gets a fresh one. `VRefTy`/`RefTy` carry (heap, element).
- **A signature names the reference or infers:** `(r : Ref(I64)) -> Unit can
  {Mutate(r)}` (`Mutate : [h] -> [A] -> Ref(h, A) -> Type` maps a reference to its
  heap's effect) or `can _`.
- **Local mutation is pure.** At a function boundary, a heap created while
  elaborating the function that its type (domain, result) does not mention and no
  older heap aliases is dropped (`Elab_effects.discharge_local_heaps`):
  `sum_to : I64 -> I64` with a local accumulator checks and evaluates in a type.
  Returning the reference, or merging its heap with an outer one, keeps the effect.
- **Top-level references work:** the entry's runtime handler
  (`runtime_handled_effects`) discharges `Mutate` on any heap.
- Discharge happens at function boundaries and the entry; a non-function `let` or
  block does not discharge on its own.

### One declaration per primitive; checked I64 arithmetic (2026-09-15)

- `Nbe_prim.declarations` is the one table of primitives (name, type, reducer:
  `Atoms` or `Special`); the elaborator's prim types and the evaluator's reducers
  derive from it. The pure-arrow combinators live in `core.ml`.
- I64 `+ - * /` are checked: overflow (including `min_int / -1`) fails with
  "integer overflow in <op>", as division by zero fails.

### Tuple types; one grammar for types (2026-09-15)

- Tuple types are `Tuple(n, T1, …, Tn)`: a built-in whose type is
  `(n : I64) -> tuple_arity(n)`, the arity computed from `n` (a negative `n` is an
  evaluation error). It reduces to the flat product type; projections are
  unchanged. `*` is only multiplication: `I64 * Bool` as a type is rejected.
- Annotation types (bindings, parameters, fields, signature members, `e : T`) are
  read with the expression grammar; the separate type grammar is deleted. A user
  type operator such as the prelude's `~>` works in every annotation position.

### Methods follow the arrow rule (2026-09-15)

- A method is pure unless it declares a row: `pub method bump() can {Log} { … }`,
  `can _` infers it (`Syntax.MethodBinding.effects`, reflected as `DeclMethod`'s
  row). The row sits on the method's innermost arrow, so a call performs it once
  every argument is supplied; a body performing beyond it is `UnhandledEffects`.
- A trait method signature's arrow carries its row like any arrow, and an impl's
  method is checked against it.

### A bare arrow is pure (2026-09-15)

- `A -> B` is `A -> B can {}` (E3): an omitted row is the closed empty row
  (`Elab_type_expr.elaborate_effect_row`). `A -> B can _` infers the row (a fresh
  meta tail); `_` may also stand for a written row's tail, `can {IO | _}`
  (`Syntax.effect_row.inferred`, reflected as `MkEffectRow`'s third field).
- `A ~> B` is prelude sugar for `A -> B can _` (`order arrow`, right-associative).
  It is read only by the expression grammar: type annotations (`x : T`, binding
  types) go through the separate type grammar, which reads no user operators, so
  there write `can _` (or bind `Callback = Unit ~> I64` first).
- A plain `rec fact : I64 -> I64` is now known pure, so its calls get the lazy
  delta shortcut. A pure call is deferred (`VGlued`) only in checker requests,
  not inside a macro application, whose result is read at once.
- Test helpers that run a program now use `Ctx.run` (unbudgeted), not the
  checker's `Ctx.eval`.

### Recursive records by identity; records only as let bindings (2026-09-15)

- A record type is a value: `P = struct { x : I64 }`, or with parameters
  `Pair = fn[A : Type, B : Type] { struct { fst : A; snd : B } }`.
  `type X = struct { … }` is a parse error naming these forms.
- `rec Numbers = struct { head : I64; tail : Option(Numbers) }` (under any
  parameters) mints a record identity. The body sees `Numbers` as a recursive
  occurrence (`Core.RecOcc` / `VRecOcc`, replacing `SelfTypeRef` / `VSelfType`),
  bound by a core `Let`; the finished value is recorded under the identity
  (`Core.finish_record`). An occurrence unfolds on demand (`Nbe.force_shape` at
  field access, record patterns and match domains); conversion and unification
  compare two occurrences by identity and unfold one against anything else. Two
  same-shape recursive records are distinct; plain records stay structural.
- `rec A = struct { … } and B = struct { … }` is a recursive group
  (`RecGroupBinding` / `LetRecGroup`, reflected `DeclRecGroup` /
  `RawLetRecGroup`); one knot (`elab_rec_group`) serves a lone `rec` struct and a
  group. A group holds struct types or values, not both.
- `rewrite_record_self_refs` (matched by spelling) is deleted. `Self` means only
  the struct being defined. A struct's fields are checked for duplicates.
- Limit: the identity is minted once at elaboration, so a `rec` struct under a
  binder shares one identity across evaluations until E11 lands.
- `rec even : I64 -> Bool = fn(n) { … odd(n - 1) … } and odd : … = …` is a group of
  mutually recursive values (2026-09-15). A fixpoint is a member of a recursive
  group: `Core.Fix { members; index }`, `VFix` over a `fix_closure`; a single
  `rec` is a group of one. Every body is checked once seeing every member at its
  annotated type (or a meta); each member keeps its own name and purity, so
  check-time unfolding under the budget and the pure-call lazy delta work per
  member. A module's group bindings are emitted in member order (the group
  used to come out reversed).

### One-pass effects; unhandled effects at the top are errors (2026-09-15)

- Effects are computed while inferring and checking, not by a second walk:
  each context carries a sink `perform` and a latent row at an application emit
  into; a lambda body, a type, a handled scrutinee and a unit elaborate in a
  fresh one. `elab_effect_collect.ml` (`collect_effects`, `compile_time_safe`)
  and the deferred-output table are deleted.
- A handler discharges what it handles from its scrutinee and its branch bodies
  (deep). A let or module/struct member whose value performs is opaque at check
  time.
- An entry expression and an imported unit's top-level bindings are checked
  against the row the runtime handles (none yet): an unhandled effect is
  `UnhandledEffects` naming it
  ([unhandled-effects-pass-the-checker](wayfinder/tickets/unhandled-effects-pass-the-checker.md)).

### Fresh declaration binders, no string-built ids (2026-09-15)

- Every declaration binder - types, constructors, effects, traits, pattern
  synonyms, module items, macros - gets a fresh resolved name
  (`Expand.bind_declaration`); a declaration exports its label (`Syntax.label`),
  and the elaborator keys its context by resolved name. A macro's `type Tmp`
  no longer takes the caller's `Tmp`.
- `Syntax.new_id`, `var`, `lam`, `let_in`, `seq` and `no_scopes` are deleted: a
  macro gets a name from `quote` or an `Id` parameter. A reflected id's `Scopes`
  carries the resolved name it was minted with, and a reflected `#` name is
  accepted only under that certificate, so a macro cannot forge one. The
  empty-scope and operator spelling fallbacks for macro heads are gone.

### Performance after M9 (2026-09-14)
- M9 run 2 slowed the suites up to ~125x (`test_macro_driver_stage7`
  0.13s → 16s). Two causes, both fixed without semantic change:
  - **Cubic item loop.** A definition context re-applied every active scope to
    *all* its remaining unread items before each item. An item's extent is
    structural (`take_statement`), so only the item being read takes the
    scopes; several scopes are added as one union, not one traversal each.
  - **Context refinement copied the whole context.** `refine_context_type_var`
    (type-case branches) was ~90% of `init_ctx` before M9 too, and grew with
    the larger prelude. The substitution now preserves sharing and memoises
    values and environment tails by physical identity.
- Side by side with `0262a02`: stage7 0.07/0.05s, syntax 0.30/0.34s,
  elaborate 7.5/8.3s, core 6.9/7.5s.

### Regression coverage
- Imports, module files, records, record patterns, methods, `self`/`Self`, qualified
  patterns/constructors, algebraic effects (`perform`, handlers, `resume`) all have
  regression test coverage. See [regression coverage](wayfinder/topics/regression-coverage.md).

### Type-case / generic programming
- Primitive and nominal type-head matching, structural record type reflection,
  open-`Type` fallback, generic equality dispatch.
  See [type-case / generic programming](wayfinder/topics/type-case-generic-programming.md).

### Record type reflection
- `struct … end` type patterns over constructor fields.
  See [record type reflection](wayfinder/topics/record-type-reflection.md).

### Algebraic effects
- Nominal effect families, latent rows with open row tails, `perform`, match-based
  handlers, `resume`. Deep handler semantics, lexical resume in nested lambdas,
  one-shot continuations. See [algebraic effects](wayfinder/topics/algebraic-effects.md).

### References
- `Ref(A)`, `ref(e)`, `deref(r)`, `r <- e`. Opaque mutable cells, aliasing and
  closure-capture semantics preserved; using one performs `Mutate` on its heap
  (see "Refs in effect rows"). See [references](wayfinder/topics/references.md).

### Modules and the strict phase rule
- `open <module-expr>` is an item of a module or struct body, not only a `do`-block
  statement: it scopes over the *subsequent* bindings, exports nothing, and carries
  its runtime scope extension as `Core.OpenBind`. Imported modules are strict about
  prelude **syntax** — `Enforest.parse_module` has no `?open_prelude` flag and the
  loader no longer harvests the prelude for them, so a module that uses `+` writes
  `open (import "std")` itself. Prelude *values* still reach a module through the
  importer's elaboration context; see
  [imported module elaboration context](wayfinder/tickets/imported-module-elaboration-context.md).

### Brace surface syntax (2026-09-14)
- Bodies are brace groups: `fn(x) { … }`, `method m() { … }`, `macro m(x) : K { … }`,
  `infix (op) g ($a, $b) { … }`, blocks `{ … }`, `module { … }`,
  `sig { … }`, `struct { … }`, `multi { … }`, `syntax head { rule => replacement, … }`.
- `if (c) { t } else { e }` (a prelude template, `if ($c) $t else $e`) and
  `match (v) { pattern => result, effect E.op x => result }` (Rust-style arms: a `{ … }` result ends its arm, any other ends at `,`; `|` is pattern union).
- `->` is only the function-type arrow; `=>` separates a pattern from its result
  and is reserved (`infix (=>)` is an error).
- Record types are `type P = struct { x: I64 }`; construction, record patterns and
  `can {…}` rows are unchanged. `module M do … end` is gone (`M = module { … }`).
- Newlines are whitespace; `;` separates. A trailing `;` before `}` discards the
  block's value, and a block statement may be a bare expression (`_ = e`).
- Old `-> body` / `do … end` / `match x do` forms fail with an error naming the
  new form. The reader's keyword-pair grouping helpers are deleted; arms split
  by "a pattern holds no bare `=>`, a result holds no bare `|`".
- A template hole ending a group now extends its capture to the whole group.
- Prelude and every test source migrated mechanically
  ([surface-syntax-braces](wayfinder/tickets/surface-syntax-braces.md)).

### Struct items in source order (2026-09-15)

- A struct's items are one source-ordered list: a field is an item
  (`Syntax.FieldBinding`, reflected `DeclField`; `RawStruct` carries only items).
- A field's type sees the opens and bindings written before it, as a module's
  items do; a later binding is not visible. Its type leaves the struct as a
  value, quoted at the struct's own level.
- A method is checked after the last field, so `self` has every field. A field
  type mentioning an earlier method is `FieldTypeMentionsMethod` (a cycle).
- `Self` in an item other than a field or method is the fields written so far.
- No dependent fields: a field is not a binder, so `struct { n : Type; v : n }`
  leaves `n` unbound. A struct does not see its own name (`C` binds after
  `C = struct { … }`); `C.k` inside it is unbound.

### Decl macro output types; opening a module parameter (2026-09-15)
- `: Decl` is one declaration, `: List(Decl)` any number: the annotation is the
  type the body is checked against where the macro is defined
  (`Syntax.macro_compiled` wraps the body in `Annotated`, `Decl` written as
  `Syntax.Decl` at the annotation's scopes). `quote { … }` checked against
  `Syntax.Decl` must hold exactly one non-hole item (`QuoteNotOneDecl`);
  anywhere else it is the list. The `VU` instantiation workaround is gone.
- A parameter means what the same type means as an output: `(d : Decl)` takes
  a `{ … }` group holding exactly one item (`HoleOneDecl`, captured `CapDecl`,
  the macro sees a `Decl`); `(d : List(Decl))` takes a group of any number
  (`HoleDecl`, a `Decls`). A syntax form's `$(d : Decl)` hole is unchanged.
- `open` binds what the module's **type** lists (I2): `Core.Open`/`OpenBind`
  carry the members (`OpenField name`, `OpenImpl i`), and the evaluator pushes
  each as a projection of the module value. A module parameter (a neutral) opens
  by projection; a module with more members than its signature opens only the
  signature's. A signature names every impl it requires, so a parameter's impl
  opens by projection too.
- Signatures are telescopes (2026-09-15): `sig { T : Type; empty : T }` reads
  `empty : self.T`, so a parameter `s : Stack` has `s.empty : s.T` (abstract) and
  an argument is checked against the member types its own `T` gives
  (`Core.Sig` → `VSig` closure, `Nbe.module_type_of`). An impl a signature
  requires is named, `eq_T : impl Eq(T)`: provided under that name, reached as
  `s.eq_T`, brought into trait resolution by `open s`; an anonymous `impl` in a
  `sig` is a parse error.

### Macro signatures (2026-09-15)
- A macro's type binders, `(x : Expr(T))` parameters and `: Expr(T)` output are
  its **signature**, a pi type elaborated where the macro is defined
  (`Syntax.macro_signature`, carried on `Expand_ctx.macro_entry`). A name in it
  must resolve there and a promised `T` must be a type. The `_ = T` body device is
  deleted; `MacroBinding`/`MacroDef` carry `output`, reflected both ways.
- A macro whose signature promises a type waits for the elaborator, which applies
  it like a function over types (`Elab_resolve.apply_typed_macro`): binders become
  metas, typed arguments are checked at their types, the result meets the
  expected type, every binder must then be solved ("cannot infer A for
  `default`"), the macro runs with them, and its output is checked at the promised
  type ("macro `n` promises Expr(I64) …"). Any number of binders. A macro that
  promises no type still runs during expansion.
- A typed argument elaborates once, at the call. Where the output places it
  unchanged it becomes `Syntax.Elaborated` (internal, reflected as the argument
  itself), and elaborating that reuses the core, weakened past the binders the
  output added (`Elab_defs.shift_term`, which now widens inserted metas' masks).
  A rebuilt argument is new syntax and elaborates normally; an argument whose
  core holds an `open` placed under new binders elaborates again.
- A typed call's effects are its output's: the elaborator records the output by
  the call's node (`Elab_resolve.deferred_outputs`) and the effect pass reads it
  there. Every effect read follows the elaboration it reads — a type is required
  pure after it elaborates and before it is evaluated
  (`Elab_type_expr.require_pure`), so a typed call works in a type position too.
- `open` of a non-module is `NotAModule` in every form (expression, module item,
  struct item, effect pass); an open's entries are its module type's public
  fields and impls, and a value whose entries do not line up with its type is an
  internal invariant failure, never a silently narrower open.
- The elaborator's copied macro table is gone: it asks its macro runtime, so a
  typed macro works inside the unit that defines it and when imported
  ([macro-annotation-constraints-mean-nothing](wayfinder/tickets/macro-annotation-constraints-mean-nothing.md)).

### Order groups and structural hole extents (2026-09-15)
- Precedence is relative: `order g : stronger_than(a) weaker_than(b) assoc(right)`
  declares a group (a binder resolved by scope set, `pub`, delivered by `open` and
  import binders); the order is transitive and a cyclic declaration is an error.
  `infix (op) g`, `prefix (op) g`, `syntax name g { … }` join a group; numeric
  precedence is gone (an error names the new form). Operators with no declared
  order never mix ("no declared order; parenthesise"); a form or operator in no
  group is weaker than every grouped one. The prelude's operators are in
  `disjunction < conjunction < comparison < additive < multiplicative < negation`.
- `assoc(none)`: members of a non-associative group do not chain ("do not chain;
  parenthesise"). `<-` is in the compiler-known group `assignment`, below
  `disjunction` and `assoc(none)`: `r <- x + 1` is `r <- (x + 1)`, `a <- b <- c`
  is an error. A group may be named through a unit: `stronger_than(O.g)`,
  `infix (op) O.g`, where `O` denotes an imported unit.
- A syntax form's hole extent is structural: the hole ending a use reads the
  form's operand at its order; a hole before `,`/`;` reads to it; any other hole
  is one term (a token or one bracket group). Captures are one parse per hole
  (`try_prefixes` deleted). See
  [brackets-decide-grouping](wayfinder/tickets/brackets-decide-grouping.md).

### Macro model M9, run 3 — M9 complete (2026-09-15)
- A macro parameter takes a kind: `macro m(n : Id, p : Pattern, b : Block, d : Decl)`.
  A call's arguments are read as the kinds of the macro its head resolves to,
  local or imported (the loader's caches carry the kinds); a `Decl` argument is a
  brace group of items, unread until spliced, and its value is `Decls`. A wrong
  kind or count is an `Expand_error`.
- A declaration hole `$d` in `quote { … }` takes `Decls` and splices them.
- A `$n` identifier token in `quote { … }` (a generated rule's head) is an `Id`
  hole.
- An import's roles bind in the region of the open or binder that imported it.

### Macro model M9, run 2 (2026-09-14)
- A syntax form is a macro: its rules are reflected data on its role, and a use
  is filled through `Expand.application` (`Instantiate`). The region rule and
  token-level re-enforestation are gone.
- The expander drives the enforester: `{ … }` bodies and unit/module items stay
  unread (`Block`, `Items`) until expansion reaches them, so syntax a form
  generates is usable by the next form. Quoted syntax is read where written.
- A unit's syntax exports come from expanding it.
- `Syntax.tokens(b)` reads a block's token tree; `Syntax.expand_block(b)` expands
  one inside a macro; `Syntax.expand_decls(d)` does the same for a `Decl`
  argument's items, in order (2026-09-15); expansion is idempotent. Resolved
  names are `x#n`.
- A `: Decl` form works as a block statement; binders a macro returns into a
  definition context lose the use-site scope.
- Test changes: units that define forms open `std` themselves (a replacement is
  read at its definition); import cycles are reported by the syntax load that
  reaches them first; the circular-syntax tests use real import cycles.

### Macro model M9, run 1 (2026-09-14)
- Hole kinds are reflection types: `$(x : Expr | Block | Id | Decl | Pattern)`,
  a bare `$v` is `Expr`; `binder`/`ident` are gone (`Id` binds or refers by
  position). A `Pattern` capture splices a use-site pattern.
- `syntax head : Decl { pat => { items } }`; a syntax form is used only in its
  kind's position. `multi` is deleted.
- `quote { items }` quotes declarations; a lone `$d` item is a `Decl` hole.
- The expansion position is the site's: a `Decl` macro works inside an
  expression-level `module { … }`.
- (Run 2 below lands templates as macros, the loop, `Block` and `expand_block`.)

### Checker evaluation budget (2026-09-14)
- Every evaluation the checker asks for spends from one call budget
  (`Eval_budget`, 1,000,000 calls per request, no surface syntax to raise it);
  running out is `ElabError EvaluationBudgetExceeded`, not a hang.
- A fixpoint unfolds at check time on any argument, open or closed (revised
  2026-09-15, see below): `double(n)` converts with `n + n`; a divergent
  unfolding such as `loop(n)` in a type is a budget error naming the call.
- The budget measures work: calls plus every conversion and unification step.
  Two calls of one known-pure fixpoint (closed empty effect row) on convertible
  arguments convert without unfolding: under the checker such a call is a
  deferred `VGlued`, unfolded when inspected (lazy delta).
- **One binder count per core form.** `Core.map_subterms` states how many
  environment entries each immediate subterm sits under (`None` where only
  evaluation knows: an `open`'s body, bindings after an `OpenBind`); the
  closed-term rule, `shift_term`, generalization's closedness check,
  `term_mentions_var` and the recursive-payload closer all read it. This fixed
  generalization under match branches and around `perform`
  ([core-traversals-count-binders-separately](wayfinder/tickets/core-traversals-count-binders-separately.md)).
- Running a program (`Ctx.run`, the REPL) is unbudgeted.
  See [checker-evaluation-budget](wayfinder/tickets/checker-evaluation-budget.md).
- **Macro applications are calls under the same budget** (M5). The depth fuel
  (256, reserve/release) is deleted. A macro application spends one call and
  opens a request that its body evaluation (fresh metas, shared budget,
  `Nbe.apply_macro`) and the expansion of its output spend from, so a nest of
  applications is bounded as a whole: breadth blowup at bounded depth is an
  `Expand_error` `BudgetExceeded` naming the innermost macro and, for a syntax
  operator, its site - the application installs its error on the budget, so
  the overrun is raised with its site and nothing re-catches it. The driver's
  own elaboration requests report overruns through `Elab_entry.reporting_budget`
  like every other checker request
  ([expansion-errors-reach-the-user-raw](wayfinder/tickets/expansion-errors-reach-the-user-raw.md)). A type-aware call's request also
  covers elaborating its output.
- **Expansion failures are error values** (M8): kind mismatch, non-syntax
  result, non-declaration result, self-expansion during definition and a
  missing callback are `Expand_error.Error { error; site }`, with the syntax
  operator's use and declaration spans as the site. No `failwith` remains in
  `expand.ml`, and the catch-all that re-wrapped a macro body's exceptions as
  strings is gone
  ([macro-fuel-is-the-evaluation-budget](wayfinder/tickets/macro-fuel-is-the-evaluation-budget.md)).
  Any evaluation failure inside a macro application (`panic`, division by zero,
  a runtime match failure) is `Expand_error.EvalFailed` with the application's
  site: the evaluator fails through one helper, `Nbe_support.fail`, which raises
  the error of the macro application running under the budget, or `EvalError`
  outside one. Primitive reducers return a failure instead of raising
  ([macro-body-eval-errors-lack-site](wayfinder/tickets/macro-body-eval-errors-lack-site.md)).

### Macro model enforcement and one IR (2026-09-14)
- **One IR.** `Surface.t` and lowering are deleted; the elaborator reads expanded
  `Syntax.t`, so ids, paths and spans reach it
  ([delete-surface-ir](wayfinder/tickets/delete-surface-ir.md)).
- **Hygiene.** Every macro application (untyped, type-aware, decl, operator) goes
  through `Expand.application`: use-site and intro scopes on what it receives,
  intro flipped on what it returns. `quote(…)` builds syntax with
  definition-site scopes and holes typed by position (`Expr`/`Pattern`/`Id`).
  Scope sets are opaque `Scopes` values. Macros no longer capture their
  arguments, and template literals resolve at the definition.
- **Reflection is total.** The prelude's `Syntax` ADTs have one constructor per
  form (one `and` chain), and the round trip is the identity on every field.
- **Name resolution.** Local binders always get fresh resolved names
  (`x__0`). A path's head is an id. A bare name resolves to a binder or to an
  **open choice** (the candidate opens by scope set, then the shadowed binder),
  settled by the elaborator against each open's members; no bare name is found
  by spelling among locals. A path's head (qualified pattern heads, record-pattern
  types, `perform E.op`, effect branches, `impl M.Trait`) resolves the same way,
  and traits and nominal types are located through the entry the head resolves
  to - by trait identity, or by applying a type former - not by a name-keyed
  table or an environment scan. `impl M.Trait(..)` and `[A : M.Trait]` now work
  ([names-resolve-without-spelling](wayfinder/tickets/names-resolve-without-spelling.md)).
  Still by spelling: a constructor label matched inside a scrutinee's known nominal.
- **Macro type binders are explicit.** `macro m[A](x) : Expr(A)` binds `A` (a
  reflected type, `Syntax.R`, unless annotated), so arity is syntactic. (Superseded
  2026-09-15 by macro signatures, above: any number of binders, `T` checked.) The uppercase rule, the
  `Macro_resolver` pass, the parse-time adapter and `: A` binders are deleted
  ([macro-type-binders-should-be-explicit](wayfinder/tickets/macro-type-binders-should-be-explicit.md)).
  `Elab_infer` no longer special-cases `EffectRow` or `stx_` names
  ([elaborator-matches-names-by-spelling](wayfinder/tickets/elaborator-matches-names-by-spelling.md)).
- **Macro bodies** elaborate inside the unit opens around their definition,
  nothing ambient (M3). Units that write macros open the prelude themselves.
- **Types.** `type A = … and B = …` chains are mutually recursive nominals. Nested
  patterns through recursive positions work (they read constructors by nominal id).
- **Syntactic roles (M7).** Raw tokens carry scope sets and every id takes its
  token's. A syntax form or operator resolves by scope set (largest subset,
  ambiguity loud); units, modules, structs and blocks scope their tokens, so
  syntax shadows syntax lexically and a template's replacement sees roles as of
  its definition. Template intro scopes go on replacement tokens: syntax a
  template names itself is invisible to user code, and a hole may name a
  generated declaration (`syntax $n { $n $x => … }`, `infix ($op) …`). A role
  never mixes with another binder of its name: syntax declarations survive as
  `SyntaxBinding` / `SyntaxDef`, imported roles are seeded into the expander,
  and the binder funnel raises `RoleConflict` in either order (application-written
  binders and a fixity attached to its value excepted); an open supplying a
  visible role's name is `OpenSuppliesRole`
  ([template-heads-resolve-by-scope-set](wayfinder/tickets/template-heads-resolve-by-scope-set.md)).
  Enforestation still precedes expansion; the expander-driven loop rides on M9.
- M9 (templates desugar to macros) is complete; see "Macro model M9, run 3".
  The macro model's remaining distances are in the design map.

### Macro system — Stages 0–10
- Stages 0 through 10 are complete: substrate, hygiene, expansion, phase-aware imports,
  enforestation, syntax templates, computed ADT-based syntax API, kind-tagged macros,
  Decl/Pattern ADTs, type-aware macros. See [macro status](wayfinder/macro-system/STATUS.md).
- The Stage 10 annotation-name disambiguation limitation is resolved on the
  semantic driver path: annotations are resolved against the current prior
  type namespace (builtins, user types, value aliases, and qualified
  imported types via `Expr(M.T)`). See
  [type-aware interleaving](wayfinder/macro-system/TYPE_AWARE_INTERLEAVING.md).
- Type-aware interleaving migration Stages 1–9 are done: AST split, static
  list removal, `Macro_driver` skeleton, prelude-type constraint resolution,
  canonical per-binding kind registration via injected callback,
  macro-generated declaration re-entry (generated `MacroBinding` nodes
  compile/register, generated siblings thread scopes), scoped per-binding
  semantic advancement (top-level source-order prior user type/record
  declarations now constrain later macro annotations), recursive-macro
  safety infrastructure (top-level provisional macro registration/rollback plus
  macro expansion guarded by the evaluation budget, shared across copied contexts),
  driver-based import loading (`Macro_driver.visit_macros` compiles imported
  public macros through a full driver run, so their annotations resolve in
  the imported module's own context), and retirement of the old
  `Core_loader.visit_macros` parser-heuristic path. Same-Decl generated
  type→macro interleaving and transformer-level self-recursive macro
  bodies are deferred.

---

## Mostly complete / in progress

### Traits and trait stdlib/pub semantics
- Trait declarations, `impl` declarations, structural dictionary evidence,
  trait-bound implicit parameters, qualified method calls, public module/struct
  impl evidence all implemented.
- Remaining: explicit deriving/fallback behavior as library-level type-case code;
  more protocol-style operations. See [traits](wayfinder/topics/traits.md) and
  [trait module/stdlib](wayfinder/topics/trait-module-stdlib.md).

---

## Active / deferred

### Macro Stages 11–12
- Stage 11 (macro-powered language features) and Stage 12 (macro diagnostics &
  expansion UX) have no specification yet. See [macro status](wayfinder/macro-system/STATUS.md)
  and [macro implementation plan](wayfinder/macro-system/IMPLEMENTATION_PLAN.md).

### Annotation scope disambiguation / type-aware interleaving
- Migration Stages 1–9 are done; semantic annotation resolution is active for
  driver-based module compilation and for imported macro modules. Remaining
  deferred items: same-Decl generated type→macro interleaving,
  transformer-level self-recursive macro bodies, mutually recursive macro
  groups, semantic `resolved_type_ref` constraint identity (constraints are
  still recorded by name and resolved at the use site), and resolved-export
  cache fingerprinting (the macro cache is still keyed by module path). See
  [macro interleaving design](wayfinder/topics/macro-interleaving-design.md),
  [type-aware interleaving](wayfinder/macro-system/TYPE_AWARE_INTERLEAVING.md),
  and the [direction map](wayfinder/fun-design-map.md).

### Private type visibility
- Design-only task using the OCaml/SML model (private types become abstract outside
  their defining module). See [private type visibility](wayfinder/topics/private-type-visibility.md)
  and the [direction map](wayfinder/fun-design-map.md).

### Generated symbol cleanup
- Trait declaration markers, struct nominal hashes, and other compiler-internal
  generated symbols should be replaced with explicit structural representations.
  See [generated symbol cleanup](wayfinder/topics/generated-symbol-cleanup.md).

### Enforester improvements
- Structured errors with spans, fault-tolerant parsing, spec-oriented combinators.
  See [enforester improvements](wayfinder/topics/enforester-improvements.md).

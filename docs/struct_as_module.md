## Struct-as-Module: Design & Implementation Plan

### Design

Inspired by Zig, `struct ... end` is the one universal construct for defining types, ADTs, modules, and namespaces. `{ ... }` is reserved for literal construction. There is no separate module language, no functors, no signatures.

Structs are not first-class values — they are compile-time declarations. `let Name = struct ... end` uses expression syntax, but structs cannot be passed to functions or stored in data structures.

Typeclasses are planned separately for ad-hoc polymorphism.

### Phased approach

**Phase 1 (current):** Simple namespacing. `struct ... end` only contains `let` and `type` bindings with `pub` visibility. No fields, no variants inside struct. Plus `import "file"` for file-level modules.

**Phase 2 (later):** Fuse records and ADTs into struct. Fields, variants, `self` keyword, `Name { ... }` construction syntax.

---

### Phase 1: Namespacing + Imports

#### The `struct ... end` construct (Phase 1)

A `struct ... end` block contains only `let` and `type` definitions, with optional `pub` visibility:

```
let Math = struct
  let helper x = x + x
  pub let double = helper
  pub type color = Red | Green | Blue
end
```

- `pub` controls export from the struct boundary
- Inside a block, all definitions (including private) are visible to later definitions
- The struct's type is `Struct [("double", I64 -> I64); ("Red", color); ...]` (pub values + constructors from pub types)

#### Qualified access

- `M.x` for values — reuses existing `FieldAccess` in expressions
- `M.Red` for constructors in expressions — also `FieldAccess`, constructors are pub struct members
- `M.Red` for constructors in patterns — unified `Tagged of string list * Id.t * t option` in AST, resolved by typechecker
- `M.Type` for types in annotations — `Con` uses `TypeId.t = { path : string list; name : string }`, scoped during typechecking
- Aliasing works: `let N = M in N.Red` resolves through the struct type, not string names

#### Qualified patterns

In expressions, `A.B.C.x` is already nested `FieldAccess` — no change needed.

In patterns, `A.B.C.Red` is `Tagged(["A";"B";"C"], "Red", payload)`. The parser uses a `dotted_ids` rule requiring at least one dot. The typechecker's `resolve_module_path` walks the struct type chain (look up `A` in env → `Struct` → look up `B` in fields → ...) and finds the constructor type. This produces a plain `Tagged` in the typed IR, so match compilation and the evaluator never see paths.

#### Scoped type names

Type names use `TypeId.t = { path : string list; name : string }` instead of flat strings. `type color = ...` at top level gets `{ path = []; name = "color" }`. Inside `struct M`, it's `{ path = ["M"]; name = "color" }`. The typechecker threads a `~scope` parameter and builds `type_defs : TypeDefs.t` (a `TypeId.Map`) during inference. The evaluator receives `type_defs` immutably — no context threading needed.

#### File imports

```
let Vec2 = import "vec2"           (* resolves to ./vec2.fun *)
```

A `.fun` file is parsed as a list of struct definitions (bindings with optional `pub`) via the `module_eof` parser entry point. Import resolution is lazy — the typechecker calls a `~loader` callback when it encounters `Import`. The loader (`lib/loader/loader.ml`) resolves `"name"` → `<base_dir>/name.fun`, parses, typechecks (via callback to `Inference.on_expr`), and caches the fully typechecked result. Circular imports are detected (raises `CircularImport`). The same module imported multiple times is only typechecked once.

---

### Implementation (Phase 1)

#### Step 1: Syntax — Lexer, Parser, AST (done)

**Lexer** (`lib/syntax/lexer.ml`):
- Added keywords: `"pub"` → `PUB`, `"import"` → `IMPORT`, `"struct"` → `STRUCT`
- Added string literal lexing: `'"' ... '"'` → `STRING of string`

**AST** (`lib/syntax/ast.ml` + `ast.mli`):
- Added `visibility = Public | Private`
- Added `Struct_def.t = { vis : visibility; binding : Binding.t }`
- Added to `Expr.t`: `| StructDef of Struct_def.t list` and `| Import of string`

**Parser** (`lib/syntax/parser.mly`):
- Added `%token PUB IMPORT` and `%token <string> STRING`
- Added `struct_def` rule, `STRUCT list(struct_def) END`, `IMPORT STRING`
- Added `module_eof` start symbol for file imports

#### Step 2: Type System — Struct type variant (done)

**`Type.Generic`** (`lib/semantic/type/type.ml` + `type.mli`):
- Added `| Struct of (string * ('var, 'var_set) t) list`
- Updated all match sites: `to_tag`, `Precedence.of_tag`, `pp`, `of_human`, `order_vars`, `map_equal`

**Typecheck helpers** (`lib/semantic/type/typecheck/typecheck.ml`):
- `FreeVariables`, `Substitution`, `Unification` all handle `Struct`

#### Step 3: Typed IR + Typechecker (done)

**Typed IR** (`lib/semantic/typed_ir/typed_ir.ml`):
- Added `| StructDef of { members : Binding.t list; pub_names : string list }`

**Typechecker** (`lib/semantic/type/typecheck/typecheck.ml`):
- `StructDef` case: processes members sequentially, pub type constructors become struct members
- `FieldAccess`: checks `Struct` type first, falls back to `RecordDefs`

#### Step 4: Evaluator (done)

**Runtime value** (`lib/backend/interp/model.ml`):
- Added `| Struct of (string * t) list` with `equal` and `pp`

**Evaluator** (`lib/backend/interp/eval.ml`):
- `StructDef` case: processes members, collects pub values
- `FieldAccess`: handles both `Record` and `Struct`

#### Step 5: Qualified patterns + scoped type names (done)

**AST** (`lib/syntax/ast.ml` + `ast.mli`):
- Unified `Tagged` to `Tagged of string list * Id.t * t option` — path is `[]` for unqualified
- `ModulePath.pp` helper (no type alias, to avoid menhir circular dependency)

**Parser** (`lib/syntax/parser.mly`):
- Added `dotted_ids` rule: left-recursive `ID DOT ID (DOT ID)*`
- Pattern rules for qualified constructors: `qid`, `qid LPAREN p RPAREN`
- `type_` rule uses `Type.TypeId.make` for `Con` construction

**Type system** (`lib/semantic/type/type.ml` + `type.mli`):
- Added `TypeId` module: `type t = { path : string list; name : string }` with `Map`, `equal`, `pp`
- Changed `Con of Id.t * ...` to `Con of TypeId.t * ...`
- Added `TypeDefs` module: `type t = (Id.t * Human.t option) Nonempty_list.t TypeId.Map.t`

**Typechecker** (`lib/semantic/type/typecheck/typecheck.ml`):
- Added `resolve_module_path` with `instantiate` for `Forall`-wrapped struct types
- Threads `~scope : string list` through `constraints_from_expr`
- Builds `type_defs : TypeDefs.t` during inference, returned from `on_expr`
- `Let/TypeDecl` registers ADTs with `TypeId.{ path = scope; name }`
- Struct type fields sorted alphabetically for canonical ordering

**Match compiler** (`lib/semantic/match/match_compile.ml`):
- `type_defs` changed to `Type.TypeDefs.t`, keyed by `TypeId.t`

**Evaluator** (`lib/backend/interp/eval.ml`):
- Removed context threading — `eval` takes `~type_defs` as immutable parameter
- No longer builds `type_defs` during evaluation

#### Step 6: File Imports (done)

**File loader** (`lib/loader/loader.ml`):
- `Loader.create ~base_dir ~typecheck_fn` returns a `string -> result` closure
- Resolves `"name"` → `<base_dir>/name.fun`, parses with `module_eof`
- Caches fully typechecked results (`Typed_ir.Expr.t * Type.TypeDefs.t`)
- Detects circular imports via `In_progress` / `Done` cache states
- Passes itself as `~loader` to `typecheck_fn` for nested imports

**Typechecker** (`lib/semantic/type/typecheck/typecheck.ml`):
- `constraints_from_expr` and `on_expr` accept `?loader` optional parameter
- `Import path` case calls loader, merges imported `type_defs`

**Tests**: 4 import tests (basic value, ADT + pattern match, nested, circular detection)

---

### Phase 2: Fuse Records/ADTs into Struct

#### Parameterized structs

Structs that define types (fields or variants) need type parameters. Syntax: `struct['a, 'b] ... end`.

```
let Pair = struct['a, 'b]
  fst: 'a;
  snd: 'b;
end

let Option = struct['a]
  pub variant Some 'a
  pub variant None
end
```

- `StructDef` in the AST gains `args : string list` (empty for non-parameterized structs/namespaces)
- The struct's nominal type is `Con(TypeId, [args])` when it has fields or variants, `Struct [...]` when it's a pure namespace
- Type parameters are inferred at construction sites: `Pair {fst = 1; snd = true}` infers `Pair[I64, Bool]`
- `type pair['a, 'b] = ...` desugars to `struct['a, 'b] ... end` (step 2.5)

#### Step 2.1: Named record construction `Name { ... }` (done)

Replace anonymous `{x = 1; y = 2}` with `point {x = 1; y = 2}` (and `M.point {x = 1}` for qualified). Same for patterns: `point {x; y}` instead of `{x; y}`. Removes `RecordDefs.by_field` from construction/pattern paths (keep for `FieldAccess` until step 2.6).

**AST** (`lib/syntax/ast.ml` + `ast.mli`):
- `Expr.Record` → `Expr.RecordConstruct of { path : string list; name : string; fields : ... }`
- `Pattern.Record` → `Pattern.RecordConstruct of { path : string list; name : string; fields : ...; partial : bool }`

**Parser** (`lib/syntax/parser.mly`):
- `expr_primary`: `name=ID LBRACE fields RBRACE` for unqualified
- `expr_postfix`: `expr_postfix DOT name=ID LBRACE fields RBRACE` for qualified (collects path by walking `FieldAccess` chain, avoids shift/reduce conflict with `dotted_ids`)
- `pattern`: `name=ID LBRACE entries RBRACE` and `qid=dotted_ids LBRACE entries RBRACE`

**Typechecker**: Resolve path via `resolve_module_path`, look up `name` in `RecordDefs.by_name`.

**Typed IR / Evaluator**: No change — typed IR keeps `Record`, evaluator keeps `Value.Record`.

**Tests**: Updated all record test sources to use named construction.

#### Step 2.2: Field declarations in struct (done)

`name: Type;` inside `struct ... end`. Fields are always public, must come before let/type bindings, and require a trailing semicolon. A struct with fields defines a record type. No `field` keyword needed.

```
let Point = struct
  x: I64;
  y: I64;
end

let Pair = struct['a, 'b]
  fst: 'a;
  snd: 'b;
  pub let make a b = Pair {fst = a; snd = b}
end
```

Construction: `Point {x = 1; y = 2}`, `Pair {fst = 1; snd = true}` (step 2.1 syntax, params inferred).

**AST** (`lib/syntax/ast.ml` + `ast.mli`):
- `StructDef of Struct_def.t list` → `StructDef of { args : string list; fields : (string * Type.Human.t) list; members : Struct_def.t list }`

**Parser** (`lib/syntax/parser.mly`):
- `STRUCT option(LBRACKET args RBRACKET) list(struct_field_decl) list(struct_def) END`
- `struct_field_decl`: `name=ID COLON type_ SEMI` (mandatory trailing `;`)
- `module_eof` returns `string list * (string * Type.Human.t) list * Struct_def.t list` triple

**Typechecker** (`lib/semantic/type/typecheck/typecheck.ml`):
- `StructDef` case: if struct has fields, validates free vars against `args`, registers in `RecordDefs.by_name`
- Struct name extracted from `scope` (set by enclosing `Let` binding)
- Struct value type remains `Struct [pub_types]` (namespace); the nominal `Con(TypeId, params)` type applies to constructed records

**Loader** (`lib/loader/loader.ml`):
- Updated for new `module_eof` return type `(args, fields, defs)` triple

**Tests**: 9 new tests — 2 syntax parsing, 3 typecheck, 4 evaluator (construction, field access, pub let methods, parameterized structs, pattern matching)

#### Step 2.3: Variant declarations in struct (done)

Variants use `| Name payload` syntax (same as current ADT syntax), placed at the struct level. No `variant` keyword.

A struct body is one of three mutually exclusive forms (enforced by `struct_body` sum type in AST):
1. `Fields of (string * Type.Human.t) Nonempty_list.t` — record (step 2.2)
2. `Variants of (string * Type.Human.t option) Nonempty_list.t` — ADT (this step)
3. `Namespace` — pure namespace (phase 1)

Trailing `;` rule: the last variant needs `;` only when followed by bindings. Enforced structurally in the parser via `struct_body_with_defs` rule (4 productions: namespace, fields, variants-only, variants+SEMI+defs).

```
let Color = struct
  | Red
  | Green
  | Blue
end

let Option = struct['a]
  | Some 'a
  | None
end

(* With bindings — last variant needs trailing ; *)
let Option = struct['a]
  | Some 'a
  | None;
  pub let is_some x = match x | Option.Some(_) -> true | Option.None -> false end
end
```

Constructors accessed as `Color.Red`, `Option.Some 42` via struct member access. Type parameters on variants checked against the struct's declared params.

**AST** (`lib/syntax/ast.ml` + `ast.mli`):
- Added `struct_body` sum type with `Fields | Variants | Namespace`
- `StructDef` changed from `{ args; fields; members }` to `{ args; body : struct_body; members }`
- Both `Fields` and `Variants` use `Nonempty_list.t`

**Parser** (`lib/syntax/parser.mly`):
- Added `struct_variant` rule: `PIPE tag=ID payload=option(type_)`
- Added `struct_body_with_defs` rule with 4 productions handling all valid combinations
- Added `nonempty_to_nel` inline helper for `nonempty_list` → `Nonempty_list.t` conversion
- `module_eof` returns `string list * struct_body * Struct_def.t list`

**Typechecker** (`lib/semantic/type/typecheck/typecheck.ml`):
- `StructDef` case: if `Variants`, validates free vars, registers constructors in env and `type_defs`, adds constructor types as pub struct members
- Synthesizes a `TypeDecl` binding in the typed IR so the evaluator needs no changes
- Adds preliminary struct type to env for self-referencing qualified patterns inside struct methods
- `FieldAccess` on `Struct` now calls `instantiate` on the field type (fixes polymorphic constructor access)

**Evaluator**: No changes — synthesized `TypeDecl` handled by existing `process_binding` logic.

**Tests**: 2 syntax parsing, 2 typecheck, 4 evaluator (construction, payload, parameterized, pub let with qualified patterns).

#### Step 2.4: `self` keyword (done)

`self` in type positions inside struct refers to the enclosing struct's type, enabling recursive types. Also works in `type ... = ...` declarations.

```
let List = struct['a]
  | Cons ('a, self)
  | Nil
end
```

**Lexer** (`lib/syntax/lexer.ml`): Added `"self"` → `SELF` keyword.

**Type system** (`lib/semantic/type/type.ml` + `type.mli`): Encoded `self` as a sentinel `Con({path=[]; name="self"}, [])` rather than adding a new variant to `Type.Generic.t`. Added `self_sentinel : TypeId.t` and `self : _ t` helpers. No changes to `Generic.t` itself — avoids failwith branches and keeps the type clean.

**Parser** (`lib/syntax/parser.mly`): Added `%token SELF`, `| SELF { Type.Generic.self }` to `type_` rule.

**Typechecker** (`lib/semantic/type/typecheck/typecheck.ml`): Added `resolve_self ~nominal` function that recursively substitutes `Con(self_sentinel, [])` → the struct's nominal type in `Human.t`. Applied in three places:
- `StructDef` with `Variants` body — resolves `self` in variant payloads
- `StructDef` with `Fields` body — resolves `self` in field types
- `TypeDecl` with `Adt` and `Record` rhs — resolves `self` in `type ... = ...` declarations

**Tests**: 1 syntax parsing (self in variant payload), 1 typecheck (recursive struct type), 1 evaluator (linked list sum with `self`).

#### Step 2.5: User-facing `open` and `export` (done)

`open Name in body` brings all public members of the struct bound to `Name` into scope for `body`. Parsed as `Let { binding = Open name; body }`.

```
let M = struct
  pub let x = 1
  pub let y = 2
end in
open M in
x + y
```

Inside structs, `open Name` as a member brings the struct's public members into scope for subsequent members. Opened names are NOT added to the struct's exports — they stay private.

`export Name` works like `open` but also re-exports the opened members publicly. Used by the desugar pass for `pub type` declarations.

Nullary constructors in patterns require parens: `Red()` is a constructor, `Red` is a variable binding. `()` is lexed as `UNIT`, so the parser uses `| id=ID UNIT` for this.

**AST** (`lib/syntax/ast.ml` + `ast.mli`):
- Added `| Open of string` and `| Export of string` to `Binding.t`

**Lexer** (`lib/syntax/lexer.ml`): `"open"` → `OPEN`, `"export"` → `EXPORT`

**Parser** (`lib/syntax/parser.mly`):
- `expr_stmt`: `| OPEN name=ID IN body=expr`
- `struct_def`: `| OPEN name=ID` (always private), `| EXPORT name=ID` (always private — re-export handled by typechecker)
- Pattern: `| id=ID UNIT { Pattern.Tagged ([], id, None) }` for nullary constructors

**Typed IR** (`lib/semantic/typed_ir/typed_ir.ml`):
- Added `| Open of string` and `| Export of string` to `Binding.t`

**Typechecker** (`lib/semantic/type/typecheck/typecheck.ml`):
- `Open`: extends env with struct members, returns no exports
- `Export`: extends env with struct members, returns members as exports (always public regardless of visibility flag)

**Evaluator** (`lib/backend/interp/eval.ml`):
- `Open` and `Export` both extend env with struct members at runtime

**Tests**: parse `open M in expr`, typecheck open bringing members into scope, evaluator end-to-end, open inside struct.

#### Step 2.6: Desugar `type name = ...` to struct + open (done)

`type color = Red | Green | Blue in body` desugars to `let color = struct | Red | Green | Blue end in open color in body`. Existing syntax keeps working, goes through struct path internally.

**Desugared AST** (`lib/syntax/desugared_ast.ml`, new file):
- Mirrors `Ast` types but `Binding.t` has no `TypeDecl` variant — only `Value | Open | Export`
- Shares `Atom`, `Pattern`, `Param`, `struct_body`, `visibility` from `Ast`

**Desugar pass** (`lib/syntax/desugar.ml`, new file):
- `expr : Ast.Expr.t -> Desugared_ast.Expr.t` — recursive AST transform
- `TypeDecl { name; args; rhs }` → `Value { name; value = StructDef { args; body; members = [] } }` + `Open name` wrapping body
- Inside structs: one `TypeDecl` member → two members: `Value` (same vis) + `Open` (private) or `Export` (for pub types)
- `toplevel` and `binding` helpers for top-level desugaring

**Typechecker** (`lib/semantic/type/typecheck/typecheck.ml`):
- `constraints_from_expr` and `process_binding` now take `Desugared_ast` types
- `TypeDecl` cases removed entirely — handled by struct machinery
- Callers (`bin/main.ml`, test helpers) call `Desugar.expr` before passing to typechecker

**Tests**: all existing `type ... = ...` tests pass through the new desugar path.

#### Step 2.7: Cleanup (done)

- Removed `TypeDecl` from `Typed_ir.Binding.t` — replaced with `body : Ast.struct_body` on `StructDef` node so the evaluator reads variant constructors directly from the struct body
- `Typed_ir.Binding.t` is now just `Value | Open | Export`
- Unified `Value.Record` into `Value.Struct` — single `Struct of (string * t) list` runtime value
- Evaluator's `StructDef` handler uses `register_variants` from `body` field, no synthetic bindings needed

### Verification

After each step: `dune build` + `dune test`. Each step adds its own test cases.

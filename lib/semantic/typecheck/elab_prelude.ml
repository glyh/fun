open Core
open Elab_common

(* Was duplicated verbatim from [Nbe_prim]; kept as an alias so the mapping from
   an atom to its type has one definition. *)
let atom_ty_of_atom = Nbe_prim.atom_ty_of_atom

(* Primitives never mention [Bool]: all predicates return I64 (1/0). The prelude
   wraps them into the library [Bool] ADT via [i64_to_bool]. *)
let prims =
  let arithemetic = VAtomTy Atom_ty.TI64 ^-> AtomTy Atom_ty.TI64 ^->> AtomTy Atom_ty.TI64 in
  let i64_predicate = VAtomTy Atom_ty.TI64 ^-> AtomTy Atom_ty.TI64 ^->> AtomTy Atom_ty.TI64 in
  let char_predicate = VAtomTy Atom_ty.TChar ^-> AtomTy Atom_ty.TChar ^->> AtomTy Atom_ty.TI64 in
  let unit_predicate = VAtomTy Atom_ty.TUnit ^-> AtomTy Atom_ty.TUnit ^->> AtomTy Atom_ty.TI64 in
  let string_predicate = VAtomTy Atom_ty.TString ^-> AtomTy Atom_ty.TString ^->> AtomTy Atom_ty.TI64 in
  [
    ("+", arithemetic);
    ("-", arithemetic);
    ("*", arithemetic);
    ("/", arithemetic);
    ("%", arithemetic);
    ("eq_i64", i64_predicate);
    ("neq_i64", i64_predicate);
    ("eq_char", char_predicate);
    ("neq_char", char_predicate);
    ("eq_unit", unit_predicate);
    ("neq_unit", unit_predicate);
    ("eq_string", string_predicate);
    ("neq_string", string_predicate);
    ("panic", VPi { explicitness = Implicit; domain = VU; effects = pure_effects; codomain = { env = []; body = AtomTy Atom_ty.TString ^->> Var 1 } });
    ("lt_i64", i64_predicate);
    ("gt_i64", i64_predicate);
    ("le_i64", i64_predicate);
    ("ge_i64", i64_predicate);
  ]
  |> NameMap.of_list

(* Primitives that carry a type but deliberately have no entry in
   [Nbe_prim.prim_table]. [panic] is special-cased inside [Nbe.try_prim_reduce]
   because it needs the frame list and raises rather than returning an atom. *)
let prims_without_reducer = [ "panic" ]

(* The one desync that is silent, and so the only one worth a check.
   A name in [prims] but missing from [Nbe_prim.prim_table] type-checks fine and
   then fails to reduce: [try_prim_reduce] falls through to [None], the
   application stays a stuck neutral, and evaluation yields a [VNeutral (HPrim …)]
   where a number was expected - no error, just a wrong value. The other
   direction is already loud, since a name in the prelude source with no type
   fails prelude elaboration and takes every test with it.
   See docs/wayfinder/tickets/unify-primitive-declaration.md. *)
let () =
  let missing =
    NameMap.bindings prims
    |> List.filter_map (fun (name, _) ->
           if List.mem name prims_without_reducer
              || Hashtbl.mem Nbe_prim.prim_table name
           then None
           else Some name)
  in
  if missing <> [] then
    failwith
      ("primitives declared with a type but with no reducer, and not listed in "
      ^ "prims_without_reducer: " ^ String.concat ", " missing)

let syntax_primitive_names = []

let stdlib_source =
  {|
pub type Bool = False | True;
pub syntax if do | if $c do $t else $e end -> match $c do True -> $t | False -> $e end end;
pub infix (&&) 4 Left ($a, $b) -> match $a do True -> $b | False -> False end;
pub infix (||) 3 Left ($a, $b) -> match $a do True -> True | False -> $b end;
pub i64_to_bool = fn(n) -> match n do 0 -> False | _ -> True end;
pub not = fn(b) -> match b do True -> False | False -> True end;
pub (<) = fn(x, y) -> i64_to_bool(lt_i64(x, y));
pub (>) = fn(x, y) -> i64_to_bool(gt_i64(x, y));
pub (<=) = fn(x, y) -> i64_to_bool(le_i64(x, y));
pub (>=) = fn(x, y) -> i64_to_bool(ge_i64(x, y));
pub trait Eq(A) = sig eq : A -> A -> Bool end;
pub impl Eq(I64) = module fn eq(x, y) -> i64_to_bool(eq_i64(x, y)) end;
pub impl Eq(Bool) = module fn eq(x, y) -> match x do True -> y | False -> not(y) end end;
pub impl Eq(Char) = module fn eq(x, y) -> i64_to_bool(eq_char(x, y)) end;
pub impl Eq(Unit) = module fn eq(x, y) -> i64_to_bool(eq_unit(x, y)) end;
pub impl Eq(String) = module fn eq(x, y) -> i64_to_bool(eq_string(x, y)) end;
pub (==) : [A : Eq] -> A -> A -> Bool = fn[A : Type](lhs, rhs) -> Eq.eq(lhs, rhs);
pub (!=) : [A : Eq] -> A -> A -> Bool = fn[A : Type](lhs, rhs) -> not((==)[A](lhs, rhs));
pub infix (==) 5 Left;
pub infix (!=) 5 Left;
pub infix (<) 5 Left;
pub infix (>) 5 Left;
pub infix (<=) 5 Left;
pub infix (>=) 5 Left;
pub infix (+) 10 Left;
pub infix (-) 10 Left;
pub infix (*) 20 Left;
pub infix (/) 20 Left;
pub infix (%) 20 Left;
pub prefix (not) 30;
pub type Option(A) = Some(A) | None;
pub type List(A) = Nil | Cons(A, List(A));
pub module Syntax do
  pub type Explicitness = Explicit | Implicit
  pub type Assoc = Left | Right

  pub type Span = {file: Option(String); start_byte: I64; end_byte: I64; start_line: Option(I64); start_col: Option(I64); end_line: Option(I64); end_col: Option(I64)}

  pub type Id = {name: String; span: Span; scope: I64}

  pub type Param = {name: Id; type_: Option(Type); explicitness: Explicitness}
  pub type AtomVal = I64Atom(I64) | CharAtom(Char) | StringAtom(String) | UnitAtom
  pub type Expr =
    | RawVar(Option(Span), Id)
    | RawAtom(Option(Span), AtomVal)
    | RawAp(Option(Span), Expr, Explicitness, Expr)
    | RawLam(Option(Span), Param, Expr)
    | RawLet(Option(Span), Id, Option(Expr), Expr, Expr, Bool)
  pub pattern Var(name) = RawVar(_, name)
  pub pattern Ap(f, a) = RawAp(_, f, _, a)
  pub pattern Lam(name, body) = RawLam(_, name, body)
  pub pattern Let(name, val, body) = RawLet(_, name, _, val, body, _)
  pub pattern Atom(val) = RawAtom(_, val)
  pub TypeExpr : Type = Type
  pub type Pattern =
    | RawPatWild(Option(Span))
    | RawPatBind(Option(Span), Id)
    | RawPatCon(Option(Span), Id, List(Pattern))
    | RawPatAtom(Option(Span), AtomVal)
    | RawPatProd(Option(Span), List(Pattern))
    | RawPatOr(Option(Span), Pattern, Pattern)
  pub pat_wild = RawPatWild(None)
  pub pat_var = fn(id) -> RawPatBind(None, id)
  pub pat_con = fn(name, args) -> RawPatCon(None, name, args)
  pub pat_atom = fn(val) -> RawPatAtom(None, val)
  pub pat_prod = fn(pats) -> RawPatProd(None, pats)
  pub pat_or = fn(l, r) -> RawPatOr(None, l, r)
  pub pattern PatWild = RawPatWild(_)
  pub pattern PatBind(name) = RawPatBind(_, name)
  pub pattern PatCon(name, args) = RawPatCon(_, name, args)
  pub pattern PatAtom(val) = RawPatAtom(_, val)
  pub pattern PatProd(pats) = RawPatProd(_, pats)
  pub pattern PatOr(l, r) = RawPatOr(_, l, r)
  pub type Decl = DeclLet(Id, Expr, Bool)
  pub Decls = List(Decl)
  pub decl_let = fn(name, val, is_pub) -> DeclLet(name, val, is_pub)
  pub type R = RExpr(Type) | RDecls | RPat
  pub synthetic_span = Span{file = None; start_byte = 0; end_byte = 0; start_line = None; start_col = None; end_line = None; end_col = None}
  pub new_id = fn(name) -> Id{name = name; span = synthetic_span; scope = 0}
  pub atom_val = fn(val) -> RawAtom(None, val)
  pub var = fn(name) -> RawVar(None, new_id(name))
  pub ap = fn(f, a) -> RawAp(None, f, Explicit, a)
  pub lam = fn(name, body) -> RawLam(None, Param{name = new_id(name); type_ = None; explicitness = Explicit}, body)
  pub let_in = fn(name, val, body) -> RawLet(None, new_id(name), None, val, body, False)
  pub i64 = fn(n) -> atom_val(I64Atom(n))
  pub string = fn(s) -> atom_val(StringAtom(s))
  pub char = fn(c) -> atom_val(CharAtom(c))
  pub unit = fn(_) -> atom_val(UnitAtom)
  pub seq = fn(a, b) -> RawLet(None, new_id("_"), None, a, b, False)
  pub id_name = fn(stx) -> match stx do | RawVar(_, id) -> id.name | _ -> panic[String]("expected identifier") end
  pub id_eq = fn(a, b) -> match a do | RawVar(_, ida) -> match b do | RawVar(_, idb) -> i64_to_bool(eq_string(ida.name, idb.name)) | _ -> panic[Bool]("expected identifier") end | _ -> panic[Bool]("expected identifier") end

end
|}

(* The stdlib's public syntax exports (templates like [if]; the arithmetic and
   comparison operators). Under the strict phase rule these are delivered only
   where [std] is opened: the parser resolves them through [load_syntax] on the
   reserved [import "std"] path (see [std_load_syntax] and [Core_loader]), rather
   than through the old [builtin_syntax_hook] inversion ref. Collecting the
   exports uses a non-seeded env, so there is no recursion even though this same
   source is later parsed for elaboration. *)
let stdlib_syntax_exports = lazy (Enforest.parse_public_syntax_exports stdlib_source)

(* A [load_syntax] resolver for parses that have no loader (expression eval,
   the REPL's non-file input): it answers the reserved [import "std"] path with
   the prelude's exports and knows no other module. Compose with the loader's
   own [load_syntax_exports] when files must also resolve. *)
let std_load_syntax path =
  if String.equal path Compiler_names.Module_name.std_import_path
  then Lazy.force stdlib_syntax_exports else []

(* The prelude body itself uses only prim calls and [match] (no infix operators),
   so it parses correctly without any [builtin_syntax] seed. *)
let parsed_stdlib = lazy (Parse_expand.parse_module stdlib_source)

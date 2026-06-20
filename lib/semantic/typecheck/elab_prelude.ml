open Core
open Elab_common

let atom_ty_of_atom = function
  | Atom.I64 _ -> Atom_ty.TI64
  | Bool _ -> Atom_ty.TBool
  | Unit -> Atom_ty.TUnit
  | Char _ -> Atom_ty.TChar
  | String _ -> Atom_ty.TString

let prims =
  let arithemetic = VAtomTy Atom_ty.TI64 ^-> AtomTy Atom_ty.TI64 ^->> AtomTy Atom_ty.TI64 in
  let i64_comparator = VAtomTy Atom_ty.TI64 ^-> AtomTy Atom_ty.TI64 ^->> AtomTy Atom_ty.TBool in
  let bool_comparator = VAtomTy Atom_ty.TBool ^-> AtomTy Atom_ty.TBool ^->> AtomTy Atom_ty.TBool in
  let char_comparator = VAtomTy Atom_ty.TChar ^-> AtomTy Atom_ty.TChar ^->> AtomTy Atom_ty.TBool in
  let unit_comparator = VAtomTy Atom_ty.TUnit ^-> AtomTy Atom_ty.TUnit ^->> AtomTy Atom_ty.TBool in
  let string_comparator = VAtomTy Atom_ty.TString ^-> AtomTy Atom_ty.TString ^->> AtomTy Atom_ty.TBool in
  [
    ("+", arithemetic);
    ("-", arithemetic);
    ("*", arithemetic);
    ("/", arithemetic);
    ("%", arithemetic);
    ("eq_i64", i64_comparator);
    ("neq_i64", i64_comparator);
    ("eq_bool", bool_comparator);
    ("neq_bool", bool_comparator);
    ("eq_char", char_comparator);
    ("neq_char", char_comparator);
    ("eq_unit", unit_comparator);
    ("neq_unit", unit_comparator);
    ("eq_string", string_comparator);
    ("neq_string", string_comparator);
    ("panic", VPi { explicitness = Implicit; domain = VU; effects = pure_effects; codomain = { env = []; body = AtomTy Atom_ty.TString ^->> Var 1 } });
    ("<", i64_comparator);
    (">", i64_comparator);
    ("<=", i64_comparator);
    (">=", i64_comparator);
    ("not", VAtomTy Atom_ty.TBool ^-> AtomTy Atom_ty.TBool);
  ]
  |> NameMap.of_list

let syntax_primitive_names = []

let stdlib_source =
  {|
pub trait Eq(A) = sig eq : A -> A -> Bool end;
pub impl Eq(I64) = module fn eq(x, y) -> eq_i64(x, y) end;
pub impl Eq(Bool) = module fn eq(x, y) -> eq_bool(x, y) end;
pub impl Eq(Char) = module fn eq(x, y) -> eq_char(x, y) end;
pub impl Eq(Unit) = module fn eq(x, y) -> eq_unit(x, y) end;
pub impl Eq(String) = module fn eq(x, y) -> eq_string(x, y) end;
pub (==) : [A : Eq] -> A -> A -> Bool = fn[A : Type](lhs, rhs) -> Eq.eq(lhs, rhs);
pub (!=) : [A : Eq] -> A -> A -> Bool = fn[A : Type](lhs, rhs) -> not((==)[A](lhs, rhs));
pub type Option(A) = Some(A) | None;
pub module Syntax do
  pub type Explicitness = Explicit | Implicit
  pub type Assoc = Left | Right

  pub type Span = {file: Option(String); start_byte: I64; end_byte: I64; start_line: Option(I64); start_col: Option(I64); end_line: Option(I64); end_col: Option(I64)}

  pub type Id = {name: String; span: Span; scope: I64}

  pub type Param = {name: Id; type_: Option(Type); explicitness: Explicitness}
  pub type AtomVal = I64Atom(I64) | BoolAtom(Bool) | CharAtom(Char) | StringAtom(String) | UnitAtom
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
  pub Pattern : Type = Type
  pub Decl : Type = Type
  pub Decls : Type = Type
  pub synthetic_span = Span{file = None; start_byte = 0; end_byte = 0; start_line = None; start_col = None; end_line = None; end_col = None}
  pub new_id = fn(name) -> Id{name = name; span = synthetic_span; scope = 0}
  pub atom_val = fn(val) -> RawAtom(None, val)
  pub var = fn(name) -> RawVar(None, new_id(name))
  pub ap = fn(f, a) -> RawAp(None, f, Explicit, a)
  pub lam = fn(name, body) -> RawLam(None, Param{name = new_id(name); type_ = None; explicitness = Explicit}, body)
  pub let_in = fn(name, val, body) -> RawLet(None, new_id(name), None, val, body, false)
  pub i64 = fn(n) -> atom_val(I64Atom(n))
  pub string = fn(s) -> atom_val(StringAtom(s))
  pub bool = fn(b) -> atom_val(BoolAtom(b))
  pub char = fn(c) -> atom_val(CharAtom(c))
  pub unit = fn(_) -> atom_val(UnitAtom)
  pub seq = fn(a, b) -> RawLet(None, new_id("_"), None, a, b, false)
  pub id_name = fn(stx) -> match stx do | RawVar(_, id) -> id.name | _ -> panic[String]("expected identifier") end
  pub id_eq = fn(a, b) -> match a do | RawVar(_, ida) -> match b do | RawVar(_, idb) -> eq_string(ida.name, idb.name) | _ -> panic[Bool]("expected identifier") end | _ -> panic[Bool]("expected identifier") end

end
|}

let parsed_stdlib = lazy (Parse_expand.parse_module stdlib_source)

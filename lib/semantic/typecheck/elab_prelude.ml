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
    ("stx_make_var", VAtomTy Atom_ty.TString ^-> U);
    ("stx_make_ap", VU ^-> U ^->> U);
    ("stx_make_lam", VAtomTy Atom_ty.TString ^-> U ^->> U);
    ("stx_make_let", VAtomTy Atom_ty.TString ^-> U ^->> U ^->> U);
    ("stx_make_seq", VU ^-> U ^->> U);
    ("stx_make_i64", VAtomTy Atom_ty.TI64 ^-> U);
    ("stx_make_string", VAtomTy Atom_ty.TString ^-> U);
    ("stx_make_bool", VAtomTy Atom_ty.TBool ^-> U);
    ("stx_make_char", VAtomTy Atom_ty.TChar ^-> U);
    ("stx_make_unit", VAtomTy Atom_ty.TUnit ^-> U);
    ("stx_i64_value", VU ^-> AtomTy Atom_ty.TI64);
    ("stx_string_value", VU ^-> AtomTy Atom_ty.TString);
    ("stx_bool_value", VU ^-> AtomTy Atom_ty.TBool);
    ("stx_char_value", VU ^-> AtomTy Atom_ty.TChar);
    ("stx_unit_value", VU ^-> AtomTy Atom_ty.TUnit);
    ("stx_is_ap", VU ^-> AtomTy Atom_ty.TBool);
    ("stx_ap_fn", VU ^-> U);
    ("stx_ap_arg", VU ^-> U);
    ("stx_is_lam", VU ^-> AtomTy Atom_ty.TBool);
    ("stx_lam_name", VU ^-> AtomTy Atom_ty.TString);
    ("stx_lam_body", VU ^-> U);
    ("stx_is_let", VU ^-> AtomTy Atom_ty.TBool);
    ("stx_let_name", VU ^-> AtomTy Atom_ty.TString);
    ("stx_let_value", VU ^-> U);
    ("stx_let_body", VU ^-> U);
    ("stx_kind", VU ^-> AtomTy Atom_ty.TString);
    ("stx_is_var", VU ^-> AtomTy Atom_ty.TBool);
    ("stx_is_atom", VU ^-> AtomTy Atom_ty.TBool);
    ("stx_id_name", VU ^-> AtomTy Atom_ty.TString);
    ("stx_id_eq", VU ^-> U ^->> AtomTy Atom_ty.TBool);
    ("stx_operator_symbol", VU ^-> AtomTy Atom_ty.TString);
    ("stx_operator_fixity", VU ^-> AtomTy Atom_ty.TString);
    ("stx_operator_arity", VU ^-> AtomTy Atom_ty.TI64);
    ("stx_operator_operand", VU ^-> AtomTy Atom_ty.TI64 ^->> U);
  ]
  |> NameMap.of_list

let syntax_primitive_names =
  [ "stx_make_var";
    "stx_make_ap";
    "stx_make_lam";
    "stx_make_let";
    "stx_make_seq";
    "stx_make_i64";
    "stx_make_string";
    "stx_make_bool";
    "stx_make_char";
    "stx_make_unit";
    "stx_i64_value";
    "stx_string_value";
    "stx_bool_value";
    "stx_char_value";
    "stx_unit_value";
    "stx_is_ap";
    "stx_ap_fn";
    "stx_ap_arg";
    "stx_is_lam";
    "stx_lam_name";
    "stx_lam_body";
    "stx_is_let";
    "stx_let_name";
    "stx_let_value";
    "stx_let_body";
    "stx_kind";
    "stx_is_var";
    "stx_is_atom";
    "stx_id_name";
    "stx_id_eq";
    "stx_operator_symbol";
    "stx_operator_fixity";
    "stx_operator_arity";
    "stx_operator_operand" ]

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
pub module Syntax do
  type Option(A) = Some A | None
  type Foo = Bar
  pub Expr : Type = Type
  pub TypeExpr : Type = Type
  pub Pattern : Type = Type
  pub Decl : Type = Type
  pub Decls : Type = Type
  pub var = stx_make_var
  pub ap = stx_make_ap
  pub lam = stx_make_lam
  pub let_in = stx_make_let
  pub seq = stx_make_seq
  pub i64 = stx_make_i64
  pub string = stx_make_string
  pub bool = stx_make_bool
  pub char = stx_make_char
  pub unit = stx_make_unit
  pub i64_value = stx_i64_value
  pub string_value = stx_string_value
  pub bool_value = stx_bool_value
  pub char_value = stx_char_value
  pub unit_value = stx_unit_value
  pub is_ap = stx_is_ap
  pub ap_fn = stx_ap_fn
  pub ap_arg = stx_ap_arg
  pub is_lam = stx_is_lam
  pub lam_name = stx_lam_name
  pub lam_body = stx_lam_body
  pub is_let = stx_is_let
  pub let_name = stx_let_name
  pub let_value = stx_let_value
  pub let_body = stx_let_body
  pub kind = stx_kind
  pub is_var = stx_is_var
  pub is_atom = stx_is_atom
  pub id_name = stx_id_name
  pub id_eq = stx_id_eq
  pub operator_symbol = stx_operator_symbol
  pub operator_fixity = stx_operator_fixity
  pub operator_arity = stx_operator_arity
  pub operator_operand = stx_operator_operand
end
|}

let parsed_stdlib = lazy (Parse_expand.parse_module stdlib_source)

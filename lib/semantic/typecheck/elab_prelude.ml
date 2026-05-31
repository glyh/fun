open Core
open Elab_common

let atom_ty_of_atom = function
  | Atom.I64 _ -> TI64
  | Bool _ -> TBool
  | Unit -> TUnit
  | Char _ -> TChar
  | String _ -> TString

let prims =
  let arithemetic = VAtomTy TI64 ^-> AtomTy TI64 ^->> AtomTy TI64 in
  let i64_comparator = VAtomTy TI64 ^-> AtomTy TI64 ^->> AtomTy TBool in
  let bool_comparator = VAtomTy TBool ^-> AtomTy TBool ^->> AtomTy TBool in
  let char_comparator = VAtomTy TChar ^-> AtomTy TChar ^->> AtomTy TBool in
  let unit_comparator = VAtomTy TUnit ^-> AtomTy TUnit ^->> AtomTy TBool in
  let string_comparator = VAtomTy TString ^-> AtomTy TString ^->> AtomTy TBool in
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
    ("panic", VPi { explicitness = Implicit; domain = VU; effects = pure_effects; codomain = { env = []; body = AtomTy TString ^->> Var 1 } });
    ("<", i64_comparator);
    (">", i64_comparator);
    ("<=", i64_comparator);
    (">=", i64_comparator);
    ("not", VAtomTy TBool ^-> AtomTy TBool);
  ]
  |> NameMap.of_list

let stdlib_source =
  {|
pub trait Eq A = struct
  eq : A -> A -> Bool
end;
pub impl Eq I64 = struct let eq x y = eq_i64 x y end;
pub impl Eq Bool = struct let eq x y = eq_bool x y end;
pub impl Eq Char = struct let eq x y = eq_char x y end;
pub impl Eq Unit = struct let eq x y = eq_unit x y end;
pub impl Eq String = struct let eq x y = eq_string x y end;
pub let (==) : {A : Eq} -> A -> A -> Bool =
  fun {A : Type} lhs rhs -> Eq.eq lhs rhs;
pub let (!=) : {A : Eq} -> A -> A -> Bool =
  fun {A : Type} lhs rhs -> not ((==) {A} lhs rhs)
|}

let parsed_stdlib = lazy (Parse_expand.parse_module stdlib_source)

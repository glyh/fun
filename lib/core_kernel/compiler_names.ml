module Type_name = struct
  let i64 = "I64"
  let bool = "Bool"
  let unit = "Unit"
  let char = "Char"
  let string = "String"
  let absurd = "Absurd"
  let type_ = "Type"
  let effect_row = "EffectRow"
  let ref_ = "Ref"
  let ref_keyword = "ref"

  let builtin_atoms = [ i64; bool; unit; char; string; absurd ]
  let parser_type_keywords = ref_keyword :: effect_row :: type_ :: builtin_atoms

  let macro_annotation_known =
    builtin_atoms
    @ [ type_;
        "Id";
        "Span";
        "Expr";
        "Option";
        "List";
        "AtomVal";
        "Explicitness";
        ref_;
        "Pattern";
        "Decl";
        "Trait" ]
end

module Module_name = struct
  let syntax = "Syntax"
  let stdlib = "stdlib"
end

module Syntax_name = struct
  let r = "R"
end

module Constructor_name = struct
  let r_expr = "RExpr"
  let some = "Some"
  let none = "None"
end

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
end

module Module_name = struct
  let syntax = "Syntax"
  let stdlib = "stdlib"

  (* Reserved import path: [import "std"] resolves to the builtin prelude
     module rather than reading a [std.fun] file from disk. *)
  let std_import_path = "std"
end

module Syntax_name = struct
  let r = "R"
end

module Constructor_name = struct
  let r_expr = "RExpr"
  let some = "Some"
  let none = "None"
end

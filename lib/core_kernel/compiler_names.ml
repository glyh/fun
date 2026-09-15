module Type_name = struct
  let option = "Option"
  let list = "List"
  let i64 = "I64"
  let bool = "Bool"
  let unit = "Unit"
  let char = "Char"
  let string = "String"
  let scopes = "Scopes"
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

  (* The label of an open of [import path] (M: open choice). The same in every
     context, so a context that opens a unit itself - the macro-body context
     opening [std] - answers choices naming it. *)
  let unit_open_label path = "unit:" ^ path
end

module Syntax_name = struct
  let r = "R"

  (* The prelude's [Syntax] types macros read and build (reflection nominals). *)
  let expr = "Expr"
  let explicitness = "Explicitness"
  let atom_val = "AtomVal"
  let decl = "Decl"
  let decls = "Decls"
  let id = "Id"
  let pattern = "Pattern"
  let field = "Field"
  let param = "Param"
  let effect_row = "EffectRow"
  let effect_op = "EffectOp"
  let type_decl = "TypeDecl"
  let ctor = "Ctor"
  let branch = "Branch"
  let pat_field = "PatField"
  let atom_ty = "AtomTy"
  let fixity = "Fixity"
  let macro_ann = "MacroAnn"
  let quote_hole = "QuoteHole"
  let token_tree = "TokenTree"
  let token_kind = "TokenKind"
  let delim = "Delim"
  let assoc = "Assoc"
  let role = "Role"
  let role_meaning = "RoleMeaning"
  let order = "Order"
  let rule = "Rule"
  let rule_part = "RulePart"
  let hole_kind = "HoleKind"
  let replacement = "Replacement"
  let capture = "Capture"
  let captured = "Captured"
end

module Constructor_name = struct
  let r_expr = "RExpr"
  let some = "Some"
  let none = "None"
end

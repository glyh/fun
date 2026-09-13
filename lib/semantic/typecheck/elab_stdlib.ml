open Core
open Elab_error

module Ctx = Elab_ctx.Ctx

let resolve (ctx : Ctx.t) (path : string list) : value =
  let ix, _ty = Ctx.lookup ctx Compiler_names.Module_name.stdlib in
  let stdlib_value = Ctx.eval ctx (Var ix) in
  List.fold_left
    (fun acc field_name ->
      match Nbe.force ctx.metas acc with
      | VModule { entries; _ } ->
          let fields = module_entry_fields entries in
          (match List.find_opt (fun (n, k, _v) -> String.equal n field_name && Nbe_support.visible_kind k) fields with
           | Some (_, _, v) -> v
           | None -> raise (ElabError (UnboundVariable field_name)))
      | VStruct { entries; _ } ->
          let fields = struct_entry_fields entries in
          (match List.find_opt (fun (n, k, _v) -> String.equal n field_name && Nbe_support.visible_kind k) fields with
           | Some (_, _, v) -> v
           | None -> raise (ElabError (UnboundVariable field_name)))
      | _ -> raise (ElabError (UnboundVariable field_name)))
    stdlib_value path

let syntax_nominals ctx : Macro_eval.syntax_nominals =
  { Macro_eval.expr = resolve ctx [Compiler_names.Module_name.syntax; "Expr"];
    explicitness = resolve ctx [Compiler_names.Module_name.syntax; "Explicitness"];
    atom_val = resolve ctx [Compiler_names.Module_name.syntax; "AtomVal"];
    option_ = resolve ctx ["Option"];
    decl = resolve ctx [Compiler_names.Module_name.syntax; "Decl"];
    list = resolve ctx ["List"];
    pat = resolve ctx [Compiler_names.Module_name.syntax; "Pattern"];
    r_ = resolve ctx [Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.r];
    bool = resolve ctx [Compiler_names.Type_name.bool];
    field = resolve ctx [Compiler_names.Module_name.syntax; "Field"];
    param = resolve ctx [Compiler_names.Module_name.syntax; "Param"];
    effect_row = resolve ctx [Compiler_names.Module_name.syntax; "EffectRow"];
    effect_op = resolve ctx [Compiler_names.Module_name.syntax; "EffectOp"];
    type_decl = resolve ctx [Compiler_names.Module_name.syntax; "TypeDecl"];
    ctor = resolve ctx [Compiler_names.Module_name.syntax; "Ctor"];
    branch = resolve ctx [Compiler_names.Module_name.syntax; "Branch"];
    pat_field = resolve ctx [Compiler_names.Module_name.syntax; "PatField"];
    atom_ty = resolve ctx [Compiler_names.Module_name.syntax; "AtomTy"];
    fixity = resolve ctx [Compiler_names.Module_name.syntax; "Fixity"];
    ann_arg = resolve ctx [Compiler_names.Module_name.syntax; "AnnArg"];
    macro_ann = resolve ctx [Compiler_names.Module_name.syntax; "MacroAnn"] }

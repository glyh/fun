open Core
open Elab_error

module Ctx = Elab_ctx.Ctx

let resolve (ctx : Ctx.t) (path : string list) : value =
  let ix, _ty = Ctx.lookup ctx Compiler_names.Module_name.stdlib in
  let stdlib_value = Ctx.eval ctx (Var ix) in
  List.fold_left
    (fun acc field_name ->
      let fields =
        match Nbe.force ctx.metas acc with
        | VModule { entries; _ } -> module_entry_fields entries
        | VStruct { entries; _ } -> struct_entry_fields entries
        | _ -> []
      in
      match find_field_last (fun (n, k, _v) -> String.equal n field_name && Nbe_support.visible_kind k) fields with
      | Some (_, _, v) -> v
      | None -> raise (ElabError (UnboundVariable field_name)))
    stdlib_value path

(* A compiler-known type's nominal, unapplied: a parameterised type's name is its
   former ([fn(A : Type) { enum { … } }]), applied here to placeholders until the
   nominal appears. *)
let rec unapplied_nominal (ctx : Ctx.t) (v : value) : value =
  match Nbe.force ctx.metas v with
  | VNominal n -> VNominal { n with params = [] }
  | VLam _ as former -> unapplied_nominal ctx (Nbe.apply ctx.metas former VU)
  | other -> other

let syntax_nominals ctx : Macro_eval.syntax_nominals =
  let resolve ctx path = unapplied_nominal ctx (resolve ctx path) in
  { Macro_eval.expr = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.expr ];
    explicitness = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.explicitness ];
    atom_val = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.atom_val ];
    option_ = resolve ctx [ Compiler_names.Type_name.option ];
    decl = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.decl ];
    list = resolve ctx [ Compiler_names.Type_name.list ];
    pat = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.pattern ];
    r_ = resolve ctx [Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.r];
    bool = resolve ctx [Compiler_names.Type_name.bool];
    field = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.field ];
    param = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.param ];
    effect_row = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.effect_row ];
    effect_op = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.effect_op ];
    type_decl = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.type_decl ];
    ctor = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.ctor ];
    branch = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.branch ];
    pat_field = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.pat_field ];
    atom_ty = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.atom_ty ];
    fixity = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.fixity ];
    macro_ann = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.macro_ann ];
    quote_hole = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.quote_hole ];
    token_tree = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.token_tree ];
    token_kind = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.token_kind ];
    delim = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.delim ];
    assoc = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.assoc ];
    role = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.role ];
    role_meaning = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.role_meaning ];
    order = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.order ];
    rule = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.rule ];
    rule_part = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.rule_part ];
    hole_kind = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.hole_kind ];
    replacement = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.replacement ];
    capture = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.capture ];
    captured = resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.captured ] }

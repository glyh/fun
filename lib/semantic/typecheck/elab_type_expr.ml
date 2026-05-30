open Core
include Elab_error
open Elab_validate

module Ctx = Elab_ctx.Ctx

open Elab_ops

(** Bidirectional type inference: given a surface expression, produce a
    core term and its type. *)
let type_value_of_expr ops ctx expr =
  match expr with
  | Surface.Module { bindings } ->
      let rec go ctx acc = function
        | [] -> List.rev acc
        | Surface.LetBinding { name; value; _ } :: rest ->
            let value_core, value_ty = ops.infer ctx value in
            let value_val = Ctx.eval ctx value_core in
            check_type_like ctx value_ty value_val;
            go (Ctx.define ctx name VU value_val) ((name, Public, value_val) :: acc) rest
        | _ -> raise (ElabError ApplyingNonFunction)
      in
      let fields = go (Ctx.clear_self_scope ctx) [] bindings in
      check_duplicate_names (List.map (fun (name, _, _) -> name) fields);
      validate_module_fields fields;
      ( Module { bindings = List.map (fun (name, _, value) -> LetBind (name, Public, Ctx.quote ctx value)) fields },
        VU,
        VModule { entries = List.map (fun (name, kind, value) -> ModuleField (name, kind, value)) fields; partial = true } )
  | _ ->
      let core, ty = ops.infer ctx expr in
      let value = Ctx.eval ctx core in
      check_type_like ctx ty value;
      (core, ty, value)

let elaborate_effect_row ops (ctx : Ctx.t) : Surface.effect_row option -> effect_row = function
  | None -> { effects = []; tail = Some (Meta (MetaContext.fresh ctx.Ctx.metas)) }
  | Some (row : Surface.effect_row) ->
      let entries =
        List.map
          (fun eff_expr ->
            let eff_core, eff_ty = ops.infer ctx eff_expr in
            Ctx.unify ctx eff_ty VU;
            let eff_value = Ctx.eval ctx eff_core in
            match Nbe.force ctx.metas eff_value with
            | VEffect _ -> (eff_core, eff_value)
            | _ -> raise (ElabError ExpectedEffect))
          row.effects
      in
      let rec check_unique = function
        | [] -> ()
        | (_, eff_value) :: rest ->
            if List.exists (fun (_, other) -> Ctx.conv ctx eff_value other) rest then
              raise (ElabError DuplicateEffect);
            check_unique rest
      in
      check_unique entries;
      let tail =
        Option.map
          (fun tail_expr ->
            let tail_core, tail_ty = ops.infer ctx tail_expr in
            Ctx.unify ctx tail_ty VEffectRowTy;
            tail_core)
          row.tail
      in
      { effects = List.map fst entries; tail }


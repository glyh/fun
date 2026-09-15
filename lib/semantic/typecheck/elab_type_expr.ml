open Core
include Elab_error
open Elab_validate

module Ctx = Elab_ctx.Ctx

open Elab_ops

(* A type is evaluated at check time, so it must be pure (E4). Its effects are
   read after it elaborates: a typed macro call in it has run by then, and its
   effects are its output's. *)
let require_pure ops ctx (expr : Syntax.t) = Elab_effects.require_empty_effects ctx (ops.collect_effects ctx expr)

(** Bidirectional type inference: given an expanded expression, produce a
    core term and its type. *)
let type_value_of_expr ops ctx (expr : Syntax.t) =
  let core, ty = ops.infer ctx expr in
  require_pure ops ctx expr;
  let value = Ctx.eval ctx core in
  match signature_of_module ctx value with
  | Some signature -> (core, VU, signature)
  | None ->
      check_type_like ctx ty value;
      (core, ty, value)

let elaborate_effect_row ops (ctx : Ctx.t) : Syntax.effect_row option -> effect_row = function
  | None -> { effects = []; tail = Some (Meta (MetaContext.fresh ctx.Ctx.metas)) }
  | Some (row : Syntax.effect_row) ->
      let entries =
        List.map
          (fun eff_expr ->
            let eff_core, eff_ty = ops.infer ctx eff_expr in
            require_pure ops ctx eff_expr;
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
            require_pure ops ctx tail_expr;
            Ctx.unify ctx tail_ty VEffectRowTy;
            tail_core)
          row.tail
      in
      { effects = List.map fst entries; tail }


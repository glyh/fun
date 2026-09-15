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
  (* A module is never a type: only a signature value ([sig { … }]) is. *)
  (match Nbe.force ctx.Ctx.metas value with
   | VModule { partial = false; _ } -> raise (ElabError (NotASignature (Option.map Syntax.label (Syntax.written_name expr))))
   | _ -> ());
  check_type_like ctx ty value;
  (core, ty, value)

(* [sig { x : I64; … }]: a signature value, its own kind of value, distinct from
   a module. Each member is a type, seen by the members after it. *)
let infer_signature ops ctx bindings =
  let rec go ctx acc = function
    | [] -> List.rev acc
    | Syntax.LetBinding { name = { name = key; _ }; value; _ } :: rest ->
        let name = Syntax.label key in
        let value_core, value_ty = ops.infer ctx value in
        require_pure ops ctx value;
        let value_val = Ctx.eval ctx value_core in
        check_type_like ctx value_ty value_val;
        go (Ctx.define ctx key VU value_val) ((name, Public, value_val) :: acc) rest
    | _ -> raise (ElabError ApplyingNonFunction)
  in
  let fields = go (Ctx.clear_self_scope ctx) [] bindings in
  check_duplicate_names (List.map (fun (name, _, _) -> name) fields);
  validate_module_fields fields;
  (Module { bindings = List.map (fun (name, _, value) -> LetBind (name, Public, Ctx.quote ctx value)) fields; signature = true }, VU)

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


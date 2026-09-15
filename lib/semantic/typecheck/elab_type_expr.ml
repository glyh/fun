open Core
include Elab_error
open Elab_validate
open Elab_defs

module Ctx = Elab_ctx.Ctx

open Elab_ops

(* A type is evaluated at check time, so it must be pure (E4). *)
let infer_pure ops ctx (expr : Syntax.t) = Elab_effects.pure ctx (fun ctx -> ops.infer ctx expr)

(** Bidirectional type inference: given an expanded expression, produce a
    core term and its type. *)
let type_value_of_expr ops ctx (expr : Syntax.t) =
  let core, ty = infer_pure ops ctx expr in
  let value = Ctx.eval ctx core in
  (* A module is never a type: only a signature value ([sig { … }]) is. *)
  (match Nbe.force ctx.Ctx.metas value with
   | VModule { partial = false; _ } -> raise (ElabError (NotASignature (Option.map Syntax.label (Syntax.written_name expr))))
   | _ -> ());
  check_type_like ctx ty value;
  (core, ty, value)

(* [sig { T : Type; empty : T; ord_T : impl Ord(T) }]: a signature value, its own
   kind of value, distinct from a module. It is a telescope over the module it
   describes: elaborated under a binder for that module ([self]), each member is
   seen by the members after it as [self.name], so a later type reads an earlier
   member abstractly ([empty : self.T]). An impl member is named and promises the
   dictionary type of its trait. *)
let infer_signature ops ctx bindings =
  let ctx = Ctx.clear_self_scope ctx in
  let self_level = ctx.Ctx.lvl in
  let self = VRigid { lvl = self_level; spine = [] } in
  let ctx = Ctx.bind ctx "sig#self" VU in
  let member ctx key ty = Ctx.define ctx key ty (Nbe_support.dot_value ctx.Ctx.metas self (Syntax.label key)) in
  let rec go ctx acc = function
    | [] -> List.rev acc
    | Syntax.LetBinding { name = { name = key; _ }; value; _ } :: rest ->
        let value_core, value_ty = infer_pure ops ctx value in
        let value_val = Ctx.eval ctx value_core in
        check_type_like ctx value_ty value_val;
        let binding = LetBind (Syntax.label key, Public, Ctx.quote ctx value_val) in
        go (member ctx key value_val) ((Syntax.label key, binding) :: acc) rest
    | Syntax.ImplBinding { name = Some { name = key; _ }; trait; args; fields = []; _ } :: rest ->
        let _, _, _, dict_ty = impl_dict_type ops ctx trait args in
        let binding = ImplBind (Some (Syntax.label key), Public, Ctx.quote ctx dict_ty, VU) in
        go (member ctx key dict_ty) ((Syntax.label key, binding) :: acc) rest
    | _ -> raise (ElabError ApplyingNonFunction)
  in
  let members = go ctx [] bindings in
  check_duplicate_names (List.map fst members);
  (Sig (Module { bindings = List.map snd members; signature = true }), VU)

let elaborate_effect_row ops (ctx : Ctx.t) : Syntax.effect_row option -> effect_row = function
  (* A bare arrow is pure (E3). *)
  | None -> empty_effect_row
  | Some (row : Syntax.effect_row) ->
      (* An entry is an effect, or a row variable ([->{Log, e}]): the row's tail. *)
      let classified =
        List.map
          (fun eff_expr ->
            let eff_core, eff_ty = infer_pure ops ctx eff_expr in
            match Nbe.force ctx.metas eff_ty with
            | VEffectRowTy -> Either.Right eff_core
            | _ -> (
                Ctx.unify ctx eff_ty VU;
                let eff_value = Ctx.eval ctx eff_core in
                match Nbe.force ctx.metas eff_value with
                | VEffect _ -> Either.Left (eff_core, eff_value)
                | _ -> raise (ElabError ExpectedEffect)))
          row.effects
      in
      let entries, row_vars = List.partition_map Fun.id classified in
      let written_tail =
        match row_vars, row.tail with
        | [], _ -> None
        | [ var ], None when not row.inferred -> Some var
        (* ponytail: a row holds one tail (E2); a union of row variables
           ([->{e1, e2}]) needs multi-tail rows. *)
        | _ -> raise (ElabError (UnsupportedRowUnion (List.length row_vars)))
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
        if Option.is_some written_tail then written_tail
        else if row.inferred then Some (Meta (MetaContext.fresh ctx.Ctx.metas))
        else
          Option.map
            (fun tail_expr ->
              let tail_core, tail_ty = infer_pure ops ctx tail_expr in
              Ctx.unify ctx tail_ty VEffectRowTy;
              tail_core)
            row.tail
      in
      { effects = List.map fst entries; tail }


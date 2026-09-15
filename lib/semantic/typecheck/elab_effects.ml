open Core
open Elab_error

module Ctx = Elab_ctx.Ctx

type expr_effect = Elab_ctx.expr_effect = { core : term; value : value }
type expr_effects = Elab_ctx.expr_effects = { effects : expr_effect list; tail : expr_effect option }

let empty_expr_effects = { effects = []; tail = None }
let singleton_expr_effect core value = { effects = [ { core; value } ]; tail = None }
let singleton_expr_effect_tail core value = { effects = []; tail = Some { core; value } }
let is_empty_expr_effects effects = List.is_empty effects.effects && Option.is_none effects.tail

let union_expr_effects ctx lhs rhs =
  let add acc eff =
    if List.exists (fun existing -> Ctx.conv ctx existing.value eff.value) acc then acc else eff :: acc
  in
  let tail =
    match lhs.tail, rhs.tail with
    | None, tail | tail, None -> tail
    | Some lhs_tail, Some rhs_tail when Ctx.conv ctx lhs_tail.value rhs_tail.value -> Some lhs_tail
    | Some lhs_tail, Some rhs_tail ->
        let row = VEffectRow { effect_values = List.map (fun eff -> eff.value) rhs.effects; tail_value = Some rhs_tail.value } in
        Ctx.unify ctx lhs_tail.value row;
        Some lhs_tail
  in
  { effects = List.rev (List.fold_left add (List.rev lhs.effects) rhs.effects); tail }

let union_many_expr_effects ctx effs = List.fold_left (union_expr_effects ctx) empty_expr_effects effs

(* The form being elaborated performs [effects]. *)
let emit ctx effects =
  if not (is_empty_expr_effects effects) then
    ctx.Ctx.sink.performed <- union_expr_effects ctx ctx.Ctx.sink.performed effects

(* [f] elaborates in a fresh sink: its result, and what it performed - which
   does not reach the enclosing form. *)
let collecting (ctx : Ctx.t) f =
  let sink = { Elab_ctx.performed = empty_expr_effects } in
  let result = f { ctx with Ctx.sink } in
  (result, sink.performed)

let unhandled ctx effects =
  ElabError (UnhandledEffects (List.map (fun eff -> Debug.pp_value_short ctx.Ctx.metas eff.value) effects))

let require_empty_effects ctx effects =
  match effects.effects, effects.tail with
  | [], None -> ()
  | [], Some tail -> Ctx.unify ctx tail.value (VEffectRow { effect_values = []; tail_value = None })
  | effs, _ -> raise (unhandled ctx effs)

(* A type is evaluated at check time, so it must be pure (E4). *)
let pure ctx f =
  let result, effects = collecting ctx f in
  require_empty_effects ctx effects;
  result

let effect_row_values ctx row binder =
  Nbe.eval_effect_row_closure ctx.Ctx.metas row binder

let expr_effects_of_row_values ctx row =
  { effects = List.map (fun value -> { core = Ctx.quote ctx value; value }) row.effect_values;
    tail = Option.map (fun value -> { core = Ctx.quote ctx value; value }) row.tail_value }

let check_effect_subset ctx (actual : expr_effects) (expected : effect_row_value) =
  let same_flex lhs rhs =
    match Nbe.force ctx.Ctx.metas lhs, Nbe.force ctx.Ctx.metas rhs with
    | VFlex { id = lhs_id; spine = lhs_spine }, VFlex { id = rhs_id; spine = rhs_spine } ->
        lhs_id = rhs_id && List.length lhs_spine = List.length rhs_spine && List.for_all2 (Ctx.conv ctx) lhs_spine rhs_spine
    | _ -> false
  in
  let rec remove_match eff = function
    | [] -> None
    | candidate :: rest ->
        if Ctx.try_unify ctx eff.value candidate then Some rest
        else Option.map (fun rest -> candidate :: rest) (remove_match eff rest)
  in
  let unmatched =
    List.fold_left
      (fun unmatched eff ->
        match remove_match eff expected.effect_values with
        | Some _ -> unmatched
        | None -> eff :: unmatched)
      [] actual.effects
    |> List.rev
  in
  match unmatched, actual.tail, expected.tail_value with
  | [], None, _ -> ()
  | [], Some actual_tail, Some expected_tail when same_flex actual_tail.value expected_tail || Ctx.conv ctx actual_tail.value expected_tail -> ()
  | [], Some actual_tail, Some expected_tail -> Ctx.unify ctx actual_tail.value expected_tail
  | leftovers, None, Some expected_tail ->
      Ctx.unify ctx expected_tail (VEffectRow { effect_values = List.map (fun eff -> eff.value) leftovers; tail_value = None })
  | leftovers, Some actual_tail, Some expected_tail when same_flex actual_tail.value expected_tail || Ctx.conv ctx actual_tail.value expected_tail ->
      Ctx.unify ctx expected_tail (VEffectRow { effect_values = List.map (fun eff -> eff.value) leftovers; tail_value = Some expected_tail })
  | leftovers, Some actual_tail, Some expected_tail ->
      Ctx.unify ctx expected_tail (VEffectRow { effect_values = List.map (fun eff -> eff.value) leftovers; tail_value = Some actual_tail.value })
  | [], Some actual_tail, None -> Ctx.unify ctx actual_tail.value (VEffectRow { effect_values = []; tail_value = None })
  | leftovers, _, None -> raise (unhandled ctx leftovers)

(* The effects a program's entry may leave unhandled: those the handler the
   runtime wraps around the entry discharges. The runtime handles none yet, so
   an entry's residual row must be empty; a runtime-provided effect joins this
   row rather than an exemption list. *)
let runtime_handled_effects : effect_row_value = { effect_values = []; tail_value = None }

(* A program's top - a unit's bindings, an entry expression - performs only
   what the runtime handles. *)
let require_handled_at_entry ctx effects = check_effect_subset ctx effects runtime_handled_effects

let effect_row_of_expr_effects ctx (effects : expr_effects) : effect_row =
  { effects = List.map (fun eff -> Ctx.quote ctx eff.value) effects.effects;
    tail = Option.map (fun eff -> Ctx.quote ctx eff.value) effects.tail }

let expr_effect_of_value ctx value = { core = Ctx.quote ctx value; value }

let effect_values_match ctx lhs rhs =
  Ctx.conv ctx lhs rhs || Ctx.try_unify ctx lhs rhs

let remove_expr_effect ctx handled effects =
  { effects with effects = List.filter (fun candidate -> not (effect_values_match ctx candidate.value handled.value)) effects.effects }

let effect_instance_ops = function
  | VEffect eff -> List.map (fun (name, _, _) -> name) eff.operations
  | _ -> []

let effect_row_closure_of_expr_effects ctx effects =
  effect_row_closure ctx.Ctx.env (effect_row_of_expr_effects ctx effects)

(* A let's value is known in its body only when evaluating it performs nothing:
   the checker may then evaluate it. *)
let let_body_ctx ctx name ty core value_effects =
  if is_empty_expr_effects value_effects then Ctx.define ctx name ty (Ctx.eval ctx core) else Ctx.bind ctx name ty

(* A handler: the scrutinee's and the branch bodies' effects - a handler is deep,
   so what a branch body performs it handles too - less those it handles. *)
let emit_residual ctx ~residual_of scrutinee_effects body_effects =
  emit ctx (residual_of (union_expr_effects ctx scrutinee_effects body_effects))

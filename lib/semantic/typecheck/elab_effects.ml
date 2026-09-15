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
   does not reach the enclosing form. What it stored into references does. *)
let collecting (ctx : Ctx.t) f =
  let sink = { Elab_ctx.performed = empty_expr_effects; stored = [] } in
  let result = f { ctx with Ctx.sink } in
  ctx.Ctx.sink.stored <- sink.stored @ ctx.Ctx.sink.stored;
  (result, sink.performed)

let record_store ctx heap ty = ctx.Ctx.sink.stored <- (heap, ty) :: ctx.Ctx.sink.stored

(* Using a reference performs [Mutate] on its heap. *)
let mutate_effect ctx heap =
  let value = VEffect { id = mutate_effect_id; name = Compiler_names.Effect_name.mutate; params = [ heap ]; operations = [] } in
  singleton_expr_effect (Ctx.quote ctx value) value

(* The heaps allocated since meta [since] that no older meta has been unified
   with: an older heap solved to a newer id makes that id stand for the older
   heap too, which is not local. One pass over the older metas. *)
let local_heaps ctx ~since =
  let metas = ctx.Ctx.metas in
  let aliased = Hashtbl.create 8 in
  for i = 0 to since - 1 do
    match MetaContext.lookup metas i with
    | Solved _ -> (
        match Nbe.force metas (VFlex { id = i; spine = [] }) with
        | VFlex { id; spine = [] } -> Hashtbl.replace aliased id ()
        | _ -> ())
    | Unsolved -> ()
  done;
  fun heap ->
    match Nbe.force metas heap with
    | VFlex { id = m; spine = [] } when m >= since && not (Hashtbl.mem aliased m) -> Some m
    | _ -> None

(* A heap allocated while elaborating a function (its meta is newer than
   [since]) that nothing [visible] outside mentions - the function's domain and
   result - cannot be observed from outside: its [Mutate] effects are dropped, so
   local mutation is pure. An outer heap, or one merged into an outer heap by
   unification, is never dropped. *)
let discharge_local_heaps ctx ~since ~(visible : term list) effects =
  let metas = ctx.Ctx.metas in
  let local = local_heaps ctx ~since in
  let local_heap eff =
    match Nbe.force metas eff.value with
    | VEffect { id; params = [ heap ]; _ } when id = mutate_effect_id -> local heap
    | _ -> None
  in
  let rec mentions m = function
    | Meta id | InsertedMeta (id, _) when id = m -> true
    | t -> List.exists (fun (_, sub) -> mentions m sub) (subterms t)
  in
  let observable eff = match local_heap eff with Some m -> List.exists (mentions m) visible | None -> true in
  { effects with effects = List.filter observable effects.effects }

(* [f] elaborates a binding form; heaps it allocated that its result (what
   [visible_of] quotes) does not mention are dropped from what it performed, as at
   a function boundary: a [let] or block with private mutation is pure. *)
let discharging ctx ~visible_of f =
  let since = MetaContext.count ctx.Ctx.metas in
  let result, effects = collecting ctx f in
  emit ctx (discharge_local_heaps ctx ~since ~visible:(visible_of result) effects);
  result

(* An effect as an error names it: [Mutate] on a heap names a reference in
   scope on that heap ([effect Mutate(r)]), not the hidden heap. *)
let describe_effect ctx eff =
  let metas = ctx.Ctx.metas in
  let reference_on heap =
    Elab_common.NameMap.fold
      (fun name entry found ->
        match found, Nbe.force metas entry.Elab_common.ty with
        | None, VRefTy (h, _) when Ctx.conv ctx h heap -> Some (Syntax.label name)
        | _ -> found)
      ctx.Ctx.name_table None
  in
  match Nbe.force metas eff.value with
  | VEffect { id; name; params = [ heap ]; _ } when id = mutate_effect_id -> (
      match reference_on heap with
      | Some r -> Printf.sprintf "effect %s(%s)" name r
      | None -> Debug.pp_value_short metas eff.value)
  | _ -> Debug.pp_value_short metas eff.value

let unhandled ctx effects = ElabError (UnhandledEffects (List.map (describe_effect ctx) effects))

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
   runtime wraps around the entry discharges: the heap, so top-level references
   work ([Mutate] on any heap). A runtime-provided effect joins this row rather
   than an exemption list. *)
let runtime_handled_effects ctx : effect_row_value =
  let heap = Ctx.raw_meta ctx in
  { effect_values = [ VEffect { id = mutate_effect_id; name = Compiler_names.Effect_name.mutate; params = [ heap ]; operations = [] } ];
    tail_value = None }

(* A program's top - a unit's bindings, an entry expression - performs only
   what the runtime handles. *)
let require_handled_at_entry ctx effects = check_effect_subset ctx effects (runtime_handled_effects ctx)

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
(* E11: a module whose evaluation performs something is generative - each
   evaluation is a new type - and its binder names it. In the binder's type, each
   nominal the module declares (a type member of its type) becomes that member
   of the binder, so [st1 = SymbolTable(())] gives
   [st1.intern : I64 -> st1.Symbol], shared with no other evaluation. The type is
   read one level deeper, where the binder is. *)
(* [t] with each reference to a generative nominal rewritten by [f], under
   [cutoff] binders. *)
let rec map_generative_refs f cutoff t =
  match t with
  | NomRef { id; name; params; _ } when Hashtbl.mem generative_nominals id -> f cutoff name params
  | _ ->
      map_subterms
        (fun under sub ->
          match under with
          | Some u -> map_generative_refs f (cutoff + u) sub
          | None -> failwith "map_generative_refs: subterm under a binder count known only by evaluation (an open)")
        t

(* The generative nominals [ty] mentions, read at [depth]: member label,
   declaration, arity. *)
let generative_refs mc depth ty =
  let found = ref [] in
  let rec go cutoff t =
    match t with
    | NomRef { id; name; num_params; params; _ } when Hashtbl.mem generative_nominals id ->
        if not (List.mem_assoc name !found) then found := (name, (id, num_params)) :: !found;
        List.iter (go cutoff) params
    | _ -> List.iter (fun (under, sub) -> match under with Some u -> go (cutoff + u) sub | None -> ()) (subterms t)
  in
  go 0 (Nbe.quote mc depth ty);
  List.rev !found

let mentions_generative mc depth ty = Option.map fst (List.nth_opt (generative_refs mc depth ty) 0)

(* E11: a module whose evaluation performs something is generative - each
   evaluation is a new type - and its binder names it. In the binder's type, each
   nominal such a module declares becomes that member of the binder, so
   [st1 = SymbolTable(())] gives [st1.intern : I64 -> st1.Symbol], shared with no
   other evaluation. The type is read one level deeper, where the binder is.
   Returns the sealed type and the nominals it sealed. *)
let seal_generative (ctx : Ctx.t) (ty : value) : value * (string * (nominal_id * int)) list =
  let mc = ctx.Ctx.metas and depth = ctx.Ctx.lvl + 1 in
  match generative_refs mc depth ty with
  | [] -> (ty, [])
  | sealed ->
      let rec seal cutoff name params =
        List.fold_left (fun acc p -> Ap (acc, Explicit, map_generative_refs seal cutoff p)) (Dot (Var cutoff, name)) params
      in
      (Nbe.eval mc (VRigid { lvl = ctx.Ctx.lvl; spine = [] } :: ctx.Ctx.env) (map_generative_refs seal 0 (Nbe.quote mc depth ty)), sealed)

(* [ctx] knows the entry at [lvl] was sealed over [sealed]. *)
let note_sealed (ctx : Ctx.t) lvl sealed = if sealed = [] then ctx else { ctx with Ctx.sealed = (lvl, sealed) :: ctx.Ctx.sealed }

(* A member of a generative module no binder names: its type may not mention a
   type the module declares, for that type would escape the expression. *)
let check_generative_escape (ctx : Ctx.t) ~head_effects member_ty =
  if not (is_empty_expr_effects head_effects) then
    Option.iter (fun name -> raise (ElabError (GenerativeTypeEscapes name))) (mentions_generative ctx.Ctx.metas ctx.Ctx.lvl member_ty)

(* A sealed binder's types may not leave its scope: [ty], read at [depth], names
   no entry at a level from [inner] up to [depth]. *)
let check_sealed_stays mc ~inner ~depth ~name ty =
  let rec go cutoff t =
    match t with
    | Var ix when ix >= cutoff && depth - 1 - (ix - cutoff) >= inner -> raise (ElabError (GenerativeTypeEscapes name))
    | _ -> List.iter (fun (under, sub) -> match under with Some u -> go (cutoff + u) sub | None -> ()) (subterms t)
  in
  go 0 (Nbe.quote mc depth ty)

(* A generative module's members, sealed inside it, may not reach its type. *)
let check_sealed_members_stay (_ctx : Ctx.t) ~inner (end_ctx : Ctx.t) entries =
  List.iter
    (function
      | ModuleField (name, _, ty) -> check_sealed_stays end_ctx.Ctx.metas ~inner ~depth:end_ctx.Ctx.lvl ~name ty
      | ModuleImpl _ -> ())
    entries

let let_body_ctx ctx name ty core value_effects =
  if is_empty_expr_effects value_effects then Ctx.define ctx name ty (Ctx.eval ctx core)
  else
    let sealed_ty, sealed = seal_generative ctx ty in
    note_sealed (Ctx.bind ctx name sealed_ty) ctx.Ctx.lvl sealed

(* A handler: the scrutinee's and the branch bodies' effects - a handler is deep,
   so what a branch body performs it handles too - less those it handles. *)
let emit_residual ctx ~residual_of scrutinee_effects body_effects =
  emit ctx (residual_of (union_expr_effects ctx scrutinee_effects body_effects))

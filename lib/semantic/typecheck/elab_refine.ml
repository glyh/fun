open Core
include Elab_error
open Elab_common

module Ctx = Elab_ctx.Ctx

open Elab_resolve

(* Whether [term] may mention [Var target]; under a binder count only
   evaluation reveals, it may. *)
let rec term_mentions_var target = function
  | Var ix -> ix = target
  | term ->
      List.exists
        (fun (under, sub) -> match under with Some n -> term_mentions_var (target + n) sub | None -> true)
        (subterms term)

let rec subst_value_var (mc : MetaContext.t) (target : lvl) (replacement : value) (v : value) : value =
  match Nbe.force mc v with
  | VRigid { lvl; spine } when lvl = target ->
      List.fold_left (Nbe.apply mc) replacement spine
  | VPi { explicitness; domain; effects; codomain } ->
      let domain = subst_value_var mc target replacement domain in
      let effects = subst_effect_row_closure_var mc target replacement effects in
      VPi { explicitness; domain; effects; codomain = subst_closure_var mc target replacement codomain }
  | VProd elems -> VProd (List.map (subst_value_var mc target replacement) elems)
  | VProdTy elems -> VProdTy (List.map (subst_value_var mc target replacement) elems)
  | VEffectRow row ->
      VEffectRow
        { effect_values = List.map (subst_value_var mc target replacement) row.effect_values;
          tail_value = Option.map (subst_value_var mc target replacement) row.tail_value }
  | VModule { entries; partial } ->
      let entries =
        List.map
          (function
            | ModuleField (name, kind, value) -> ModuleField (name, kind, subst_value_var mc target replacement value)
            | ModuleImpl (name, kind, ty, value) ->
                ModuleImpl (name, kind, subst_value_var mc target replacement ty, subst_value_var mc target replacement value))
          entries
      in
      VModule { entries; partial }
  | VStruct { entries; partial } ->
      let entries =
        List.map
          (function
            | StructField (name, kind, value) -> StructField (name, kind, subst_value_var mc target replacement value)
            | StructImpl (name, kind, ty, value) ->
                StructImpl (name, kind, subst_value_var mc target replacement ty, subst_value_var mc target replacement value))
          entries
      in
      VStruct { entries; partial }
  | VRecord { typ; fields } ->
      VRecord { typ = subst_value_var mc target replacement typ; fields = List.map (fun (name, value) -> (name, subst_value_var mc target replacement value)) fields }
  | VNominal n -> VNominal { n with params = List.map (subst_value_var mc target replacement) n.params }
  | VEffect e -> VEffect { e with params = List.map (subst_value_var mc target replacement) e.params }
  | VTrait _ as v -> v
  | VTraitDict d ->
      VTraitDict
        { d with
          args = List.map (subst_value_var mc target replacement) d.args;
          fields = List.map (fun (name, value) -> (name, subst_value_var mc target replacement value)) d.fields }
  | VSelfType args -> VSelfType (List.map (subst_value_var mc target replacement) args)
  | VRefTy a -> VRefTy (subst_value_var mc target replacement a)
  | VRef _ as v -> v
  | VCon c -> VCon { c with spine = List.map (subst_value_var mc target replacement) c.spine; nominal = subst_value_var mc target replacement c.nominal }
  | VNeutral { ty; neutral } ->
      VNeutral { ty = subst_value_var mc target replacement ty; neutral = subst_neutral_var mc target replacement neutral }
  | VFlex { id; spine } -> VFlex { id; spine = List.map (subst_value_var mc target replacement) spine }
  | VRigid { lvl; spine } -> VRigid { lvl; spine = List.map (subst_value_var mc target replacement) spine }
  | VLam _ | VFix _ | VCont _ | VStx _ | VPatternSyn _ as v -> v
  | VU | VEffectRowTy | VAtom _ | VAtomTy _ as v -> v

and subst_closure_var mc target replacement clo =
  { clo with env = List.map (subst_value_var mc target replacement) clo.env }

and subst_effect_row_closure_var mc target replacement row =
  { row with env = List.map (subst_value_var mc target replacement) row.env }

and subst_neutral_var mc target replacement neutral =
  let frames =
    List.map
      (function
        | FApp value -> FApp (subst_value_var mc target replacement value)
        | FProj _ as frame -> frame
        | FDot _ as frame -> frame
        | FRefGet as frame -> frame
        | FRefSet value -> FRefSet (subst_value_var mc target replacement value)
        | FMatch branches -> FMatch (List.map (fun (pat, clo) -> (pat, subst_closure_var mc target replacement clo)) branches))
      neutral.frames
  in
  { neutral with frames }

let rec branch_type_refinement = function
  | Syntax.PatType atom_ty -> Some (VAtomTy atom_ty)
  | Syntax.PatOr (lhs, rhs) -> (
      match branch_type_refinement lhs with Some _ as found -> found | None -> branch_type_refinement rhs)
  | _ -> None

let refinement_target_of_scrutinee ctx scrut_core =
  match scrut_core with
  | Var ix -> Some (ctx.Ctx.lvl - ix - 1)
  | _ -> None

let refine_context_type_var ctx target replacement =
  let substitute = subst_value_var ctx.Ctx.metas target replacement in
  {
    ctx with
    Ctx.name_table = NameMap.map (fun entry -> { entry with ty = substitute entry.ty }) ctx.Ctx.name_table;
    self_entry = Option.map (fun entry -> { entry with ty = substitute entry.ty }) ctx.Ctx.self_entry;
    resume_entry = Option.map (fun entry -> { entry with ty = substitute entry.ty }) ctx.Ctx.resume_entry;
  }

(* A payload elaborated in a context where a type chain's member names were
   temporarily defined, last member innermost: rewrite each reference to a
   member into a [NomRef] by id, and drop the temporary slots. [members] is
   the chain in declaration order, as [(id, name, num_params)]. *)
let close_recursive_payload_group members =
  let width = List.length members in
  let member_at cutoff ix =
    let rel = ix - cutoff in
    if rel >= 0 && rel < width then Some (List.nth members (width - 1 - rel)) else None
  in
  let rec collect_apps acc = function
    | Ap (f, Explicit, a) -> collect_apps (a :: acc) f
    | f -> (f, acc)
  in
  let rec go cutoff term =
    match collect_apps [] term with
    | Var ix, args
      when (match member_at cutoff ix with Some (_, _, n) -> List.length args = n | None -> false) ->
        let id, name, _ = Option.get (member_at cutoff ix) in
        NomRef { id; name; params = List.map (go cutoff) args }
    | _ -> (
        match term with
        | Var ix when Option.is_some (member_at cutoff ix) ->
            let id, name, num_params = Option.get (member_at cutoff ix) in
            NomRef { id; name; params = List.init num_params (fun i -> Var (num_params - 1 - i)) }
        | Var ix when ix >= cutoff + width -> Var (ix - width)
        | Var ix -> Var ix
        | _ ->
            map_subterms
              (fun under sub ->
                match under with
                | Some n -> go (cutoff + n) sub
                | None -> Elab_defs.reject_unknown_binder_count "close_recursive_payload_group")
              term)
  in
  go 0


let close_recursive_payload_term nominal_id nominal_name num_params =
  close_recursive_payload_group [ (nominal_id, nominal_name, num_params) ]

let rec refinement_for_nominal_head ctx = function
  | Syntax.PatCon (con_path, _) -> (
      match find_nominal_for_pattern_head_opt ctx con_path with
      | Some (VNominal n) -> Some (VNominal { n with params = List.init n.num_params (fun _ -> Ctx.raw_meta ctx) })
      | Some _ | None -> None)
  | Syntax.PatOr (lhs, rhs) -> (
      match refinement_for_nominal_head ctx lhs with
      | Some _ as found -> found
      | None -> refinement_for_nominal_head ctx rhs)
  | _ -> None

let refine_branch_context ctx refinement_target pat =
  match (branch_type_refinement pat, refinement_target) with
  | Some replacement, Some target -> refine_context_type_var ctx target replacement
  | None, Some target -> (
      match refinement_for_nominal_head ctx pat with
      | Some replacement -> refine_context_type_var ctx target replacement
      | None -> ctx)
  | _ -> ctx

let refine_branch_expected ctx refinement_target pat expected =
  match (branch_type_refinement pat, refinement_target) with
  | Some replacement, Some target -> subst_value_var ctx.Ctx.metas target replacement expected
  | None, Some target -> (
      match refinement_for_nominal_head ctx pat with
      | Some replacement -> subst_value_var ctx.Ctx.metas target replacement expected
      | None -> expected)
  | _ -> expected

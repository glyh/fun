open Core

module Ctx = Elab_ctx.Ctx


(** Generalize a let-bound value's type: if the value is a syntactic lambda
    and its type contains unsolved metas, abstract those metas into implicit
    VPi layers. At each use site, Phase 6's insert_implicits auto-instantiates
    with fresh metas. *)
let generalize (ctx : Ctx.t) (val_core : term) (val_ty : value) : term * value =
  (* Only generalize simple closed single-binder lambdas at the top level.
     Adding implicit binders around lambdas with outer captures would require
     shifting those captured de Bruijn indices under the inserted binders. *)
  let rec closed_under depth = function
    | Var ix -> ix < depth
    | term ->
        (* An unknown binder count cannot be KNOWN closed; declining the
           generalization below is always sound. *)
        List.for_all
          (fun (under, sub) -> match under with Some n -> closed_under (depth + n) sub | None -> false)
          (subterms term)
  in
  let has_bound = List.exists (fun bd -> bd = Bound) ctx.bds in
  let eligible = match val_core with
    | Lam body when not has_bound ->
        begin match body with Lam _ -> false | _ -> closed_under 1 body end
    | _ -> false
  in
  if not eligible then (val_core, val_ty)
  else begin
    let seen = ref [] in
    let add id = if not (List.mem id !seen) then seen := id :: !seen in
    let rec collect v =
      match Nbe.force ctx.metas v with
      | VFlex { id; spine = [] } ->
          (match MetaContext.lookup ctx.metas id with Unsolved -> add id | Solved _ -> ())
      | VFlex { spine; _ } -> List.iter collect spine
      | VPi { domain = a; effects; codomain; _ } ->
          collect a;
          let var = VRigid { lvl = ctx.lvl; spine = [] } in
          List.iter (fun eff -> collect (Nbe.eval ctx.metas (var :: effects.env) eff)) effects.effects;
          collect (Nbe.closure_apply ctx.metas codomain var)
      | VRefTy (h, a) -> collect h; collect a
      | VU | VAtom _ | VAtomTy _ | VTrait _ | VTraitDict _ | VRigid _ | VProd _ | VProdTy _ | VCont _ | VRef _ -> ()
      | _ -> ()
    in
    collect val_ty;
    let unsolved = List.rev !seen in
    let n = List.length unsolved in
    if n = 0 then (val_core, val_ty)
    else begin
      (* Solve innermost meta to highest level (deepest), outermost to ctx.lvl.
         All VPis are quoted at depth ctx.lvl + n so VRigid{lvl} → Var(ix) works. *)
      List.iteri (fun i meta_id ->
        MetaContext.solve ctx.metas meta_id
          (VRigid { lvl = ctx.lvl + n - 1 - i; spine = [] })
      ) unsolved;
      let gen_val = List.fold_left (fun acc _ -> Lam acc) val_core unsolved in
      let qdepth = ctx.lvl + n in
      let gen_ty_val =
        List.fold_right (fun _ acc ->
          VPi { explicitness = Implicit; domain = VU;
                effects = effect_row_closure ctx.env empty_effect_row;
                codomain = { env = ctx.env; body = Nbe.quote ctx.metas qdepth acc } })
          unsolved val_ty
      in
      (gen_val, gen_ty_val)
    end
  end

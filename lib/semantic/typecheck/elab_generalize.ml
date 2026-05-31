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
    | Lam body -> closed_under (depth + 1) body
    | Ap (f, _, a) -> closed_under depth f && closed_under depth a
    | Let (ty, def, body) -> closed_under depth ty && closed_under depth def && closed_under (depth + 1) body
    | Pi { domain; effects; codomain; _ } ->
        closed_under depth domain
        && List.for_all (closed_under (depth + 1)) effects.effects
        && Option.fold ~none:true ~some:(closed_under (depth + 1)) effects.tail
        && closed_under (depth + 1) codomain
    | If (cond, then_, else_) -> closed_under depth cond && closed_under depth then_ && closed_under depth else_
    | Prod elems | ProdTy elems -> List.for_all (closed_under depth) elems
    | EffectRowTy -> true
    | EffectRowLit row ->
        List.for_all (closed_under depth) row.effects
        && Option.fold ~none:true ~some:(closed_under depth) row.tail
    | RefTy a | RefNew a | RefGet a -> closed_under depth a
    | RefSet (r, e) -> closed_under depth r && closed_under depth e
    | Proj (e, _) | Dot (e, _) -> closed_under depth e
    | RecordConstruct { typ; fields } -> closed_under depth typ && List.for_all (fun (_, value) -> closed_under depth value) fields
    | Open (s, body) -> closed_under depth s && closed_under depth body
    | Fix body -> closed_under (depth + 1) body
    | NomRef (_, params) | EffectRef (_, params) -> List.for_all (closed_under depth) params
    | TraitDictTy { args; fields; _ } ->
        List.for_all (closed_under depth) args && List.for_all (fun (_, value) -> closed_under depth value) fields
    | SelfTypeRef args -> List.for_all (closed_under depth) args
    | Ctor { spine; nominal_spine; _ } ->
        List.for_all (closed_under depth) spine && List.for_all (closed_under depth) nominal_spine
    | Match (scrut, branches) ->
        closed_under depth scrut
        && List.for_all
             (function
               | ValueBranch (_, body) -> closed_under depth body
               | EffectBranch { body; _ } -> closed_under depth body)
             branches
    | NominalDef { ctors; body; _ } ->
        List.for_all (fun (_, payload) -> Option.fold ~none:true ~some:(closed_under depth) payload) ctors && closed_under depth body
    | EffectDef { ops; body; _ } ->
        List.for_all (fun (_, input, output) -> closed_under depth input && closed_under depth output) ops && closed_under depth body
    | Module { bindings } ->
        List.for_all
          (function
            | LetBind (_, _, value) -> closed_under depth value
            | ImplBind (_, value, _) -> closed_under depth value
            | TypeBind _ | EffectBind _ -> true)
          bindings
    | Struct { con_fields; bindings; _ } ->
        List.for_all (fun (_, ty) -> closed_under depth ty) con_fields
        && List.for_all
             (function
               | LetBind (_, _, value) -> closed_under depth value
               | ImplBind (_, value, _) -> closed_under depth value
               | TypeBind _ | EffectBind _ -> true)
             bindings
    | Atom _ | AtomTy _ | U | Prim _ | Meta _ | InsertedMeta _ | Con _ | TraitRef _ | Perform _ | Stx _ -> true
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
      | VRefTy a -> collect a
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

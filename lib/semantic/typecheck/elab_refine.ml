open Core
include Elab_error
open Elab_common

module Ctx = Elab_ctx.Ctx

open Elab_resolve

(* Whether [term] may mention [Var target]; under a binder count only
   evaluation reveals, it may. *)
let rec term_mentions_var target = function
  | Var ix -> ix = target
  (* An inserted meta is applied to every bound slot, so it reads each one. *)
  | InsertedMeta (_, bds) -> List.nth_opt bds target = Some Bound
  | term ->
      List.exists
        (fun (under, sub) -> match under with Some n -> term_mentions_var (target + n) sub | None -> true)
        (subterms term)

(* Substituting a context variable walks every value in the context, and those
   values share structure heavily: every closure's environment is a suffix of
   the context's, and one module value sits in many environments. So a
   substitution returns its input itself when nothing in it changes, and
   remembers, by physical identity, the values and environment tails it has
   already done. *)
module Phys (T : sig type t end) = Hashtbl.Make (struct
  type t = T.t
  let equal = ( == )
  let hash = Hashtbl.hash
end)

module Value_memo = Phys (struct type t = value end)
module Env_memo = Phys (struct type t = value list end)

let rec map_shared f = function
  | [] as l -> l
  | x :: xs as l ->
      let x' = f x and xs' = map_shared f xs in
      if x' == x && xs' == xs then l else x' :: xs'

(* One substitution of [replacement] for the rigid variable [target], shared
   by every value it is applied to. *)
let value_substituter (mc : MetaContext.t) (target : lvl) (replacement : value) : value -> value =
  let values = Value_memo.create 256 and envs = Env_memo.create 256 in
  let rec sub v =
    match Value_memo.find_opt values v with
    | Some v' -> v'
    | None ->
        let v' = sub_uncached v in
        Value_memo.replace values v v';
        v'
  and env = function
    | [] as l -> l
    | x :: xs as l -> (
        match Env_memo.find_opt envs l with
        | Some l' -> l'
        | None ->
            let x' = sub x and xs' = env xs in
            let l' = if x' == x && xs' == xs then l else x' :: xs' in
            Env_memo.replace envs l l';
            l')
  and subs l = map_shared sub l
  and fields fs = map_shared (fun ((name, value) as f) -> let value' = sub value in if value' == value then f else (name, value')) fs
  and closure (clo : closure) = let e = env clo.env in if e == clo.env then clo else { clo with env = e }
  and row_closure (row : effect_row_closure) = let e = env row.env in if e == row.env then row else { row with env = e }
  and neutral (n : neutral) =
    let frames =
      map_shared
        (function
          | FApp value as f -> let value' = sub value in if value' == value then f else FApp value'
          | (FProj _ | FDot _ | FRefGet) as frame -> frame
          | FRefSet value as f -> let value' = sub value in if value' == value then f else FRefSet value'
          | FMatch branches as f ->
              let branches' = map_shared (fun ((pat, clo) as b) -> let clo' = closure clo in if clo' == clo then b else (pat, clo')) branches in
              if branches' == branches then f else FMatch branches')
        n.frames
    in
    if frames == n.frames then n else { n with frames }
  and sub_uncached v =
    match (match v with VGlued _ -> v | _ -> Nbe.force mc v) with
    (* A deferred call stays deferred: substitute into its argument. *)
    | VGlued { fix; arg; _ } as v ->
        let arg' = sub arg in
        if arg' == arg then v else Nbe.apply mc (VFix fix) arg'
    | VRigid { lvl; spine } when lvl = target -> List.fold_left (Nbe.apply mc) replacement spine
    | VPi ({ domain; effects; codomain; _ } as pi) as v ->
        let domain' = sub domain and effects' = row_closure effects and codomain' = closure codomain in
        if domain' == domain && effects' == effects && codomain' == codomain then v
        else VPi { pi with domain = domain'; effects = effects'; codomain = codomain' }
    | VSig clo as v -> let clo' = closure clo in if clo' == clo then v else VSig clo'
    | VProd elems as v -> let elems' = subs elems in if elems' == elems then v else VProd elems'
    | VProdTy elems as v -> let elems' = subs elems in if elems' == elems then v else VProdTy elems'
    | VEffectRow row as v ->
        let effect_values = subs row.effect_values in
        let tail_value =
          match row.tail_value with
          | Some t -> let t' = sub t in if t' == t then row.tail_value else Some t'
          | None -> None
        in
        if effect_values == row.effect_values && tail_value == row.tail_value then v
        else VEffectRow { effect_values; tail_value }
    | VModule { entries; partial } as v ->
        let entries' =
          map_shared
            (function
              | ModuleField (name, kind, value) as e -> let value' = sub value in if value' == value then e else ModuleField (name, kind, value')
              | ModuleImpl (name, kind, ty, value) as e ->
                  let ty' = sub ty and value' = sub value in
                  if ty' == ty && value' == value then e else ModuleImpl (name, kind, ty', value'))
            entries
        in
        if entries' == entries then v else VModule { entries = entries'; partial }
    | VStruct { entries; partial } as v ->
        let entries' =
          map_shared
            (function
              | StructField (name, kind, value) as e -> let value' = sub value in if value' == value then e else StructField (name, kind, value')
              | StructImpl (name, kind, ty, value) as e ->
                  let ty' = sub ty and value' = sub value in
                  if ty' == ty && value' == value then e else StructImpl (name, kind, ty', value'))
            entries
        in
        if entries' == entries then v else VStruct { entries = entries'; partial }
    | VRecord { typ; fields = fs } as v ->
        let typ' = sub typ and fs' = fields fs in
        if typ' == typ && fs' == fs then v else VRecord { typ = typ'; fields = fs' }
    | VNominal n as v -> let params = subs n.params in if params == n.params then v else VNominal { n with params }
    | VEffect e as v -> let params = subs e.params in if params == e.params then v else VEffect { e with params }
    | VTraitDict d as v ->
        let args = subs d.args and fs = fields d.fields in
        if args == d.args && fs == d.fields then v else VTraitDict { d with args; fields = fs }
    | VRecOcc r as v -> let args' = subs r.args in if args' == r.args then v else VRecOcc { r with args = args' }
    | VRefTy (h, a) as v -> let h' = sub h and a' = sub a in if h' == h && a' == a then v else VRefTy (h', a')
    | VCon c as v ->
        let spine = subs c.spine and nominal = sub c.nominal in
        if spine == c.spine && nominal == c.nominal then v else VCon { c with spine; nominal }
    | VNeutral { ty; neutral = n } as v ->
        let ty' = sub ty and n' = neutral n in
        if ty' == ty && n' == n then v else VNeutral { ty = ty'; neutral = n' }
    | VFlex { id; spine } as v -> let spine' = subs spine in if spine' == spine then v else VFlex { id; spine = spine' }
    | VRigid { lvl; spine } as v -> let spine' = subs spine in if spine' == spine then v else VRigid { lvl; spine = spine' }
    | (VTrait _ | VRef _ | VLam _ | VFix _ | VCont _ | VStx _ | VPatternSyn _) as v -> v
    | (VU | VEffectRowTy | VAtom _ | VAtomTy _) as v -> v
  in
  sub

let subst_value_var mc target replacement v = value_substituter mc target replacement v

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
  let substitute = value_substituter ctx.Ctx.metas target replacement in
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

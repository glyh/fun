open Core

(* The one way the evaluator fails: with the error of the request it serves -
   inside a macro application, that application's error carrying its site;
   otherwise [EvalError]. *)
let fail (mc : MetaContext.t) message =
  match mc.budget.application with
  | Some app -> raise (app.failed message)
  | None -> raise (Nbe_error.EvalError message)

let make_cont resume = VCont { used = false; resume }

let rec bind_result result f =
  match result with
  | Done v -> f v
  | Effect e -> Effect { e with k = (fun v -> bind_result (e.k v) f) }

let visible_kind = function
  | Private | PrivateMethod -> false
  | Field | Public | Method -> true

let dot_value (mc : MetaContext.t) (value : value) (name : string) : value =
  match value with
  | VModule { entries; partial = _ } -> (
      let fields = module_entry_fields entries in
      validate_module_fields fields;
      match
        find_field_last
          (fun (n, k, _) -> String.equal n name && visible_kind k)
          fields
      with
      | Some (_, _, v) -> v
      (* A named impl is a member too: [M.eq_C] denotes the dictionary the
         [impl eq_C : …] binding produced. Anonymous impls stay unreachable by
         name and arrive only through [open].
         See docs/wayfinder/topics/impl-visibility.md. *)
      | None -> (
          match module_impl_value_opt entries name with
          | Some (k, v) when visible_kind k -> v
          | _ -> fail mc "field not found"))
  | VStruct { entries; _ } -> (
      let fields = struct_entry_fields entries in
      match
        find_field_last
          (fun (n, k, _) -> String.equal n name && visible_kind k)
          fields
      with
      | Some (_, _, v) -> v
      | None -> fail mc "field not found")
  | VRecord { fields; _ } -> (
      match List.find_opt (fun (n, _) -> String.equal n name) fields with
      | Some (_, v) -> v
      | None -> fail mc "field not found")
  | VNeutral { neutral; _ } ->
      VNeutral
        { ty = VU;
          neutral = { neutral with frames = neutral.frames @ [ FDot name ] } }
  | VFlex { id; spine = sp } ->
      let frames = List.map (fun v -> FApp v) sp in
      VNeutral
        { ty = VU;
          neutral = { head = HMeta id; frames = frames @ [ FDot name ] } }
  | VRigid { lvl; spine = sp } ->
      let frames = List.map (fun v -> FApp v) sp in
      VNeutral
        { ty = VU;
          neutral = { head = HVar lvl; frames = frames @ [ FDot name ] } }
  | _ -> fail mc "field access on non-struct"

let unhandled_effect_error mc eff op =
  match eff with
  | VEffect e ->
      fail mc
        ("unhandled effect " ^ e.name ^ "." ^ op
       ^ "; handlers are not implemented")
  | _ -> fail mc "perform target is not an effect"

let result_value mc = function
  | Done v -> v
  | Effect { eff; op; _ } -> unhandled_effect_error mc eff op

let env_value_label (v : value) =
  match v with
  | VLam _ -> "VLam"
  | VPi _ -> "VPi"
  | VU -> "VU"
  | VAtom _ -> "VAtom"
  | VAtomTy _ -> "VAtomTy"
  | VProd _ -> "VProd"
  | VProdTy _ -> "VProdTy"
  | VFix _ -> "VFix"
  | VModule _ -> "VModule"
  | VStruct _ -> "VStruct"
  | VRecord _ -> "VRecord"
  | VNominal n -> "VNominal(" ^ n.name ^ ")"
  | VEffect e -> "VEffect(" ^ e.name ^ ")"
  | VTrait t -> "VTrait(" ^ t.trait_name ^ ")"
  | VTraitDict _ -> "VTraitDict"
  | VSelfType _ -> "VSelfType"
  | VRefTy _ -> "VRefTy"
  | VRef _ -> "VRef"
  | VCon { name; _ } -> "VCon(" ^ name ^ ")"
  | VCont _ -> "VCont"
  | VStx _ -> "VStx"
  | VNeutral { neutral = { head = HPrim n; _ }; _ } -> "VNeutral(HPrim " ^ n ^ ")"
  | VNeutral { neutral = { head = HVar lvl; _ }; _ } -> "VNeutral(HVar " ^ string_of_int lvl ^ ")"
  | VNeutral { neutral = { head = HMeta id; _ }; _ } -> "VNeutral(HMeta " ^ string_of_int id ^ ")"
  | VRigid { lvl; _ } -> "VRigid(" ^ string_of_int lvl ^ ")"
  | VFlex { id; _ } -> "VFlex(" ^ string_of_int id ^ ")"
  | VEffectRowTy -> "VEffectRowTy"
  | VEffectRow _ -> "VEffectRow"
  | VPatternSyn _ -> "VPatternSyn"

let dump_env label env =
  Printf.eprintf "--- env dump [%s] (%d entries) ---\n%!" label (List.length env);
  let _ = List.fold_left (fun i v ->
    Printf.eprintf "  [%d] %s\n%!" i (env_value_label v);
    i + 1
  ) 0 env in
  ()

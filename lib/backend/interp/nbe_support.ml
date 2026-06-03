open Core

let make_cont resume = VCont { used = false; resume }

let get_cont = function
  | VCont c -> c
  | _ -> raise (Nbe_error.EvalError "expected VCont")

let rec bind_result result f =
  match result with
  | Done v -> f v
  | Effect e -> Effect { e with k = (fun v -> bind_result (e.k v) f) }

let visible_kind = function
  | Private | PrivateMethod -> false
  | Field | Public | Method -> true

let dot_value (value : value) (name : string) : value =
  match value with
  | VModule { entries; partial = _ } -> (
      let fields = module_entry_fields entries in
      validate_module_fields fields;
      match
        List.find_opt
          (fun (n, k, _) -> String.equal n name && visible_kind k)
          fields
      with
      | Some (_, _, v) -> v
      | None -> raise (Nbe_error.EvalError "field not found"))
  | VStruct { entries; _ } -> (
      let fields = struct_entry_fields entries in
      match
        List.find_opt
          (fun (n, k, _) -> String.equal n name && visible_kind k)
          fields
      with
      | Some (_, _, v) -> v
      | None -> raise (Nbe_error.EvalError "field not found"))
  | VRecord { fields; _ } -> (
      match List.find_opt (fun (n, _) -> String.equal n name) fields with
      | Some (_, v) -> v
      | None -> raise (Nbe_error.EvalError "field not found"))
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
  | _ -> raise (Nbe_error.EvalError "field access on non-struct")

let unhandled_effect_error eff op =
  match eff with
  | VEffect e ->
      raise
        (Nbe_error.EvalError
           ("unhandled effect " ^ e.name ^ "." ^ op
          ^ "; handlers are not implemented"))
  | _ -> raise (Nbe_error.EvalError "perform target is not an effect")

let result_value _mc = function
  | Done v -> v
  | Effect { eff; op; _ } -> unhandled_effect_error eff op

open Core

exception EvalError = Nbe_error.EvalError

let make_cont = Nbe_support.make_cont
let bind_result = Nbe_support.bind_result
let visible_kind = Nbe_support.visible_kind
let dot_value = Nbe_support.dot_value
let fail = Nbe_support.fail
let result_value = Nbe_support.result_value

(* Runtime scope extension for [open]: the values an opened module contributes,
   in entry order, innermost-last. This must stay in lockstep with the
   elaborator's [Elab_resolve.open_module_value], which defines exactly one
   context entry per public field and one per public impl — otherwise the
   de Bruijn indices the elaborator produced for the opened body would not line
   up at runtime. ([Method] is unreachable in a module: module bindings are only
   ever [Public]/[Private]. It is matched here so the filter also reads correctly
   for any module-shaped value quoted from elsewhere.) *)
(* The members an [open] binds, pushed in order: each a projection of the opened
   module, so a module parameter (a neutral) opens as well as a module does. *)
let push_open_members mc env (module_value : value) (members : open_member list) =
  List.fold_left
    (fun e member ->
      match member, module_value with
      | OpenField name, _ -> dot_value mc module_value name :: e
      | OpenImpl i, VModule { entries; _ } -> (
          let impls = List.filter_map (function ModuleImpl (_, Public, _, v) -> Some v | _ -> None) entries in
          match List.nth_opt impls i with Some v -> v :: e | None -> fail mc "open of a missing impl")
      | OpenImpl _, _ -> fail mc "open of an impl of a non-module")
    env members

let rec closure_apply (mc : MetaContext.t) (c : closure) (v : value) : value =
  eval mc (v :: c.env) c.body

and eval (mc : MetaContext.t) (env : env) (t : term) : value =
  result_value mc (eval_result mc env t)

and sequence_values mc env terms k =
  match terms with
  | [] -> k []
  | term :: rest ->
      bind_result (eval_result mc env term) (fun value ->
          sequence_values mc env rest (fun values -> k (value :: values)))

(* The one place a binding list extends a scope. [Core.binding_slots] states
   what each binding contributes and in what order; this pushes exactly that,
   so the evaluator no longer derives the order and count of its own and then
   checks them against the contract. [Module] and [Struct] differ only in which
   entry constructors they build, which is what [field] and [impl] supply.
   See docs/wayfinder/tickets/env-width-contract-is-unnamed.md. *)
and eval_bindings :
      'entry.
      MetaContext.t ->
      env ->
      Core.struct_binding_term list ->
      field:(string -> struct_field_kind -> value -> 'entry) ->
      impl:(string option -> struct_field_kind -> value -> value -> 'entry) ->
      env * 'entry list =
 fun mc env bindings ~field ~impl ->
  let rec go env acc = function
    | [] -> (env, List.rev acc)
    | OpenBind (def, members) :: rest -> go (push_open_members mc env (eval mc env def) members) acc rest
    | b :: rest ->
        let slots =
          match Core.binding_slots b with
          | Some slots -> slots
          | None -> fail mc "binding with no slot list"
        in
        let env, values =
          List.fold_left
            (fun (env, values) (sl : Core.slot) ->
              let v =
                match sl.Core.sl_source with
                | Core.SlotDef t -> eval mc env t
                | Core.SlotValue v -> v
                | Core.SlotPlaceholder -> VAtomTy Atom_ty.TUnit
              in
              (v :: env, v :: values))
            (env, []) slots
        in
        let entries = binding_entries ~field ~impl b slots (List.rev values) in
        go env (List.rev_append entries acc) rest
  in
  go env [] bindings

(* The entries a binding exports, in view order — which is not push order: a
   nominal's own entry precedes its constructors', so that a constructor sharing
   its type's name is the one a path resolves to (see [find_field_last]).
   Unnamed slots — a nominal's parameters — export nothing. *)
and binding_entries :
      'entry.
      field:(string -> struct_field_kind -> value -> 'entry) ->
      impl:(string option -> struct_field_kind -> value -> value -> 'entry) ->
      Core.struct_binding_term ->
      Core.slot list ->
      value list ->
      'entry list =
 fun ~field ~impl b slots values ->
  match b with
  | ImplBind (name, kind, _, ty) -> [ impl name kind ty (List.hd values) ]
  | _ -> (
      let named =
        List.filter_map
          (fun ((sl : Core.slot), v) ->
            Option.map (fun n -> field n sl.Core.sl_kind v) sl.Core.sl_name)
          (List.combine slots values)
      in
      match List.rev named with [] -> [] | last :: earlier -> last :: List.rev earlier)

and eval_result (mc : MetaContext.t) (env : env) (t : term) : result =
  match t with
  | Var ix -> Done (List.nth env ix)
  | Lam body -> Done (VLam { body = { env; body } })
  | Ap (f, _, a) ->
      bind_result (eval_result mc env f) (fun vf ->
          bind_result (eval_result mc env a) (fun va -> apply_result mc vf va))
  | Let (_, def, body) ->
      bind_result (eval_result mc env def) (fun vdef ->
          eval_result mc (vdef :: env) body)
  | Pi { explicitness; domain; effects; codomain } ->
      Done
        (VPi
           {
             explicitness;
             domain = eval mc env domain;
             effects = effect_row_closure env effects;
             codomain = { env; body = codomain };
           })
  | U -> Done VU
  | EffectRowTy -> Done VEffectRowTy
  | EffectRowLit row -> Done (eval_effect_row_literal mc env row)
  | Atom a -> Done (VAtom a)
  | AtomTy t -> Done (VAtomTy t)
  | Stx stx -> Done (VStx (StxExpr stx))
  | Quote { template; holes } ->
      sequence_values mc env (List.map snd holes) (fun values ->
          Done (Quote_holes.fill template (List.combine (List.map fst holes) values)))
  (* Anchor-independent: a unit's value carries its own environment. *)
  | Imported v -> Done v
  | RefTy (h, a) -> bind_result (eval_result mc env h) (fun h -> bind_result (eval_result mc env a) (fun a -> Done (VRefTy (h, a))))
  | RefNew e ->
      bind_result (eval_result mc env e) (fun value -> Done (VRef (ref value)))
  | RefGet r ->
      bind_result (eval_result mc env r) (fun ref_value ->
          match force mc ref_value with
          | VRef cell -> Done !cell
          | VNeutral { ty; neutral } -> (
              match force mc ty with
              | VRefTy (_, elem_ty) ->
                  Done
                    (VNeutral
                       {
                         ty = elem_ty;
                         neutral =
                           {
                             neutral with
                             frames = neutral.frames @ [ FRefGet ];
                           };
                       })
              | _ -> fail mc "deref of non-ref")
          | VFlex { id; spine = sp } ->
              let frames = List.map (fun v -> FApp v) sp in
              Done
                (VNeutral
                   {
                     ty = VU;
                     neutral =
                       { head = HMeta id; frames = frames @ [ FRefGet ] };
                   })
          | VRigid { lvl; spine = sp } ->
              let frames = List.map (fun v -> FApp v) sp in
              Done
                (VNeutral
                   {
                     ty = VU;
                     neutral =
                       { head = HVar lvl; frames = frames @ [ FRefGet ] };
                   })
          | _ -> fail mc "deref of non-ref")
  | RefSet (r, e) ->
      bind_result (eval_result mc env r) (fun ref_value ->
          bind_result (eval_result mc env e) (fun value ->
              match force mc ref_value with
              | VRef cell ->
                  cell := value;
                  Done (VAtom Unit)
              | VNeutral { neutral; _ } ->
                  Done
                    (VNeutral
                       {
                         ty = VAtomTy Atom_ty.TUnit;
                         neutral =
                           {
                             neutral with
                             frames = neutral.frames @ [ FRefSet value ];
                           };
                       })
              | VFlex { id; spine = sp } ->
                  let frames = List.map (fun v -> FApp v) sp in
                  Done
                    (VNeutral
                       {
                         ty = VAtomTy Atom_ty.TUnit;
                         neutral =
                           {
                             head = HMeta id;
                             frames = frames @ [ FRefSet value ];
                           };
                       })
              | VRigid { lvl; spine = sp } ->
                  let frames = List.map (fun v -> FApp v) sp in
                  Done
                    (VNeutral
                       {
                         ty = VAtomTy Atom_ty.TUnit;
                         neutral =
                           {
                             head = HVar lvl;
                             frames = frames @ [ FRefSet value ];
                           };
                       })
              | _ -> fail mc "assignment to non-ref"))
  | Prod elems ->
      sequence_values mc env elems (fun values -> Done (VProd values))
  | ProdTy elems ->
      sequence_values mc env elems (fun values -> Done (VProdTy values))
  | Module { bindings; signature } ->
      let _env, entries =
        eval_bindings mc env bindings
          ~field:(fun name kind v -> ModuleField (name, kind, v))
          ~impl:(fun name kind ty v -> ModuleImpl (name, kind, ty, v))
      in
      (* A signature's impl member evaluates its term to the dictionary type
         (see [Core.Sig]); the entry carries it as both type and payload. *)
      let entries =
        if signature then List.map (function ModuleImpl (n, k, _, ty) -> ModuleImpl (n, k, ty, ty) | e -> e) entries
        else entries
      in
      Done (VModule { entries; partial = signature })
  | Sig body -> Done (VSig { env; body })
  | Struct { con_fields; bindings; partial } ->
      (* con_fields: all at same scope, no sequential dependency *)
      let con_entries =
        List.map
          (fun (name, ty) -> StructField (name, Field, eval mc env ty))
          con_fields
      in
      let _env, bind_entries =
        eval_bindings mc env bindings
          ~field:(fun name kind v -> StructField (name, kind, v))
          ~impl:(fun name kind ty v -> StructImpl (name, kind, ty, v))
      in
      Done (VStruct { entries = con_entries @ bind_entries; partial })
  | RecordConstruct { typ; fields } ->
      bind_result (eval_result mc env typ) (fun typ ->
          let rec go acc = function
            | [] -> Done (VRecord { typ; fields = List.rev acc })
            | (name, value) :: rest ->
                bind_result (eval_result mc env value) (fun value ->
                    go ((name, value) :: acc) rest)
          in
          go [] fields)
  | Proj (e, i) ->
      bind_result (eval_result mc env e) (fun vs ->
          Done
            (match vs with
            | VProd elems -> List.nth elems i
            | VNeutral { neutral; _ } ->
                VNeutral
                  {
                    ty = VU;
                    neutral =
                      { neutral with frames = neutral.frames @ [ FProj i ] };
                  }
            | VFlex { id; spine = sp } ->
                let frames = List.map (fun v -> FApp v) sp in
                VNeutral
                  {
                    ty = VU;
                    neutral = { head = HMeta id; frames = frames @ [ FProj i ] };
                  }
            | VRigid { lvl; spine = sp } ->
                let frames = List.map (fun v -> FApp v) sp in
                VNeutral
                  {
                    ty = VU;
                    neutral = { head = HVar lvl; frames = frames @ [ FProj i ] };
                  }
            | _ -> fail mc "projection of non-product"))
  | Dot (e, name) ->
      bind_result (eval_result mc env e) (fun value ->
          match value with
          (* [v.m] on a record with no field [m]: a method of its type, applied to it. *)
          | VRecord { typ; fields } when not (List.mem_assoc name fields) ->
              let typ = match typ with
                | VRecOcc { id; captures; args; _ } -> (
                    match Hashtbl.find_opt Core.finished_records id with
                    | Some r -> List.fold_left (apply mc) (eval mc (Core.record_instance_env r captures) r.record_body) args
                    | None -> typ)
                | _ -> typ
              in
              apply_result mc (dot_value mc typ name) value
          | _ -> Done (dot_value mc value name))
  | Open (s, members, body) ->
      bind_result (eval_result mc env s) (fun vs -> eval_result mc (push_open_members mc env vs members) body)
  | Fix { members; index } -> Done (VFix { fix_members = members; fix_env = env; fix_index = index })
  | NomRef { id; name; num_params; captures; params } ->
      sequence_values mc env captures (fun captures ->
          sequence_values mc env params (fun params ->
              Done (VNominal { id; name; num_params; captures; params })))
  | EffectRef (name, params) -> (
      match eval_eff env name with
      | VEffect _ as eff ->
          sequence_values mc env params (fun param_vals ->
              Done (List.fold_left (fun acc v -> apply mc acc v) eff param_vals))
      | _ -> fail mc ("EffectRef is not VEffect: " ^ name))
  | TraitRef { trait_id; trait_name } -> Done (VTrait { trait_id; trait_name })
  | TraitDictTy { trait_id; trait_name; args; fields } ->
      sequence_values mc env args (fun arg_vals ->
          let rec eval_fields acc = function
            | [] ->
                Done
                  (VTraitDict
                     {
                       trait_id;
                       trait_name;
                       args = arg_vals;
                       fields = List.rev acc;
                     })
            | (name, field) :: rest ->
                bind_result (eval_result mc env field) (fun value ->
                    eval_fields ((name, value) :: acc) rest)
          in
          eval_fields [] fields)
  | RecOcc { id; name; captures; args } ->
      sequence_values mc env captures (fun captures ->
          sequence_values mc env args (fun arg_vals -> Done (VRecOcc { id; name; captures; args = arg_vals })))
  | Ctor { name; spine; nominal_spine; nominal; _ } ->
      sequence_values mc env spine (fun spine_vals ->
          sequence_values mc env nominal_spine (fun nom_spine_vals ->
              match eval mc env nominal with
              | VNominal n ->
                  Done
                    (VCon
                       {
                         name;
                         spine = spine_vals;
                         nominal = VNominal { n with params = nom_spine_vals };
                       })
              | _ ->
                  fail mc "Ctor nominal is not VNominal"))
  | Prim name ->
      Done (VNeutral { ty = VU; neutral = { head = HPrim name; frames = [] } })
  | Meta id -> Done (eval_meta mc id)
  | InsertedMeta (id, bds) -> Done (eval_inserted_meta mc env id bds)
  | NominalDef { id; name; num_params; captures; ctors; body } ->
      let env0 = env in
      sequence_values mc env captures (fun capture_vals ->
          let nominal = VNominal { id; name; num_params; captures = capture_vals; params = [] } in
          (* Rigid stand-ins for the type params (the body expects them in scope),
             the nominal, and for a parameterised type its type-name entry. *)
          let depth = List.length env in
          let env = List.rev (List.init num_params (fun i -> VRigid { lvl = depth + i; spine = [] })) @ env in
          let env = nominal :: env in
          (* A parameterised type's name is its former, [fn(params) { NomRef }]. *)
          let former () =
            eval mc env0
              (List.fold_right (fun _ acc -> Lam acc) (List.init num_params Fun.id)
                 (NomRef { id; name; num_params; captures = List.map (shift_capture num_params) captures;
                           params = List.init num_params (fun i -> Var (num_params - 1 - i)) }))
          in
          let env = if num_params > 0 then former () :: env else env in
          (* Each constructor chain reads the nominal it sits over: [i] entries in. *)
          let env =
            List.fold_left
              (fun env (i, (cname, payloads)) ->
                let ctor =
                  ctor_term ~nominal:(Var (if num_params > 0 then i + 1 else i)) ~name:cname ~nominal_name:name ~num_params ~payload_count:(List.length payloads)
                in
                eval mc env ctor :: env)
              env (List.mapi (fun i c -> (i, c)) ctors)
          in
          eval_result mc env body)
  | EffectDef { id; name; ops; body; _ } ->
      let elaborated_ops =
        List.map
          (fun (op_name, input, output) ->
            (op_name, { env; body = input }, { env; body = output }))
          ops
      in
      let eff =
        VEffect { id; name; params = []; operations = elaborated_ops }
      in
      eval_result mc (eff :: env) body
  | Match (scrut, branches) -> eval_match_result mc env scrut branches
  | Tunnel (skips, body) ->
      let rec tunnel = function
        | Done v -> Done v
        | Effect request ->
            let hops =
              match force mc request.eff with
              | VEffect { id; _ } -> Option.value (List.assoc_opt id skips) ~default:request.hops
              | _ -> request.hops
            in
            Effect { request with hops; k = (fun v -> tunnel (request.k v)) }
      in
      tunnel (eval_result mc env body)
  | Perform { eff; op; arg } ->
      bind_result (eval_result mc env eff) (fun eff ->
          bind_result (eval_result mc env arg) (fun arg ->
              match force mc eff with
              | VEffect _ as eff ->
                  Effect { eff; op; arg; hops = 0; k = (fun v -> Done v) }
              | _ -> fail mc "perform target is not an effect"))

and try_prim_reduce (mc : MetaContext.t) (head : head) (frames : frame list) : value option =
  match head with
  | HPrim "panic" when List.length frames >= 2 ->
      let msg =
        match List.nth frames 1 with
        | FApp (VAtom (String s)) -> s
        | _ -> "panic"
      in
      fail mc msg
  (* [expand_block(b)] and [expand_decls(d)]: the running macro application
     expands its argument (M9). On an open argument - the macro body being
     checked - the call stays stuck, like any primitive. *)
  | HPrim ("expand_block" | "expand_decls" as prim) when List.length frames >= 2 -> (
      match (mc.budget.application, List.nth frames 1) with
      | _, FApp (VRigid _ | VFlex _ | VNeutral _) -> None
      | Some app, FApp stx -> Some (if String.equal prim "expand_block" then app.expand stx else app.expand_decls stx)
      | _ -> fail mc (prim ^ " runs only inside a macro application"))
  (* [tuple_arity(n)]: [Type] after no more arguments, else [Type -> tuple_arity(n - 1)]. *)
  | HPrim name when String.equal name Compiler_names.Type_name.tuple_arity -> (
      match frames with
      | [ FApp (VAtom (I64 n)) ] when n < 0L -> fail mc "Tuple: the number of components is negative"
      | [ FApp (VAtom (I64 0L)) ] -> Some VU
      | [ FApp (VAtom (I64 n)) ] ->
          Some
            (VPi
               { explicitness = Explicit; domain = VU; effects = effect_row_closure [] empty_effect_row;
                 codomain = { env = [ VAtom (I64 (Int64.pred n)) ]; body = Ap (Prim name, Explicit, Var 1) } })
      | _ -> None)
  (* [Tuple(n, T1, …, Tn)]: the flat product of its [n] component types. *)
  | HPrim name when String.equal name Compiler_names.Type_name.tuple -> (
      match frames with
      | FApp (VAtom (I64 n)) :: components -> (
          let types = List.filter_map (function FApp v -> Some v | _ -> None) components in
          if List.length types = List.length components && Int64.equal n (Int64.of_int (List.length types))
          then Some (VProdTy types) else None)
      | _ -> None)
  | HPrim name -> (
      let atoms =
        List.filter_map (function FApp (VAtom a) -> Some a | _ -> None) frames
      in
      if List.length atoms = List.length frames then
        match Hashtbl.find_opt Nbe_prim.atom_reducers name with
        | Some f -> (
            match f atoms with
            | Nbe_prim.Prim.Reduced a -> Some (VAtom a)
            | Stuck -> None
            | Failed message -> fail mc message)
        | None -> None
      else None)
  | _ -> None

and eval_effect_row_literal (mc : MetaContext.t) (env : env) (row : effect_row) : value =
  let effects = List.map (eval mc env) row.effects in
  let tail = Option.map (eval mc env) row.tail in
  VEffectRow { effect_values = effects; tail_value = tail }

and apply_result (mc : MetaContext.t) (vf : value) (va : value) : result =
  match vf with
  | VLam { body = clo; _ } ->
      spend_call mc clo;
      eval_result mc (va :: clo.env) clo.body
  | VFix fc ->
      (* A fixpoint unfolds on any argument, open or closed; under the checker a
         divergent unfolding runs out of budget (an error). Unfolding is charged
         too: a fixpoint that unfolds to another fixpoint would otherwise loop
         without ever making a call. Under the checker a pure call is deferred
         until something inspects it, so conversion can compare two calls of
         the same fixpoint by their arguments first. *)
      let { fix_name; fix_pure; fix_body } = fix_member fc in
      let unfold () =
        mc.MetaContext.budget.calling <- Some fix_name;
        spend_call mc { env = fc.fix_env; body = fix_body };
        match eval mc (fix_body_env fc) fix_body with
        | VLam { body = lam } -> eval_result mc (va :: lam.env) lam.body
        | unfolded -> apply_result mc unfolded va
      in
      (* A macro application runs its body like a program: its result is read
         at once, so nothing is deferred there. *)
      let budget = mc.MetaContext.budget in
      if fix_pure && Option.is_some budget.limit && Option.is_none budget.application then
        Done (VGlued { fix = fc; arg = va; unfolded = lazy (result_value mc (unfold ())) })
      else unfold ()
  | VGlued _ -> apply_result mc (force mc vf) va
  | VCont c ->
      let cont = c in
      if cont.used then fail mc "continuation already used";
      cont.used <- true;
      cont.resume va
  | VNeutral { ty; neutral = neu } ->
      let cod = apply_ty mc ty va in
      let frames = neu.frames @ [ FApp va ] in
      Done
        (match try_prim_reduce mc neu.head frames with
        | Some v -> v
        | None -> VNeutral { ty = cod; neutral = { head = neu.head; frames } })
  | VFlex { id; spine = sp } -> Done (VFlex { id; spine = sp @ [ va ] })
  | VRigid { lvl; spine = sp } -> Done (VRigid { lvl; spine = sp @ [ va ] })
  | VNominal n -> Done (VNominal { n with params = n.params @ [ va ] })
  | VEffect e -> Done (VEffect { e with params = e.params @ [ va ] })
  | VTraitDict d -> Done (VTraitDict { d with args = d.args @ [ va ] })
  | VCon c -> Done (VCon { c with spine = c.spine @ [ va ] })
  | _ -> fail mc "applying non-function"

and spend_call (mc : MetaContext.t) (clo : closure) =
  Eval_budget.spend mc.MetaContext.budget ~call:(fun () ->
      match mc.MetaContext.budget.calling with
      | Some name -> name
      | None ->
      let body = Debug.pp_term clo.body in
      if String.length body <= 120 then body else String.sub body 0 120 ^ "…")

and apply (mc : MetaContext.t) (vf : value) (va : value) : value =
  result_value mc (apply_result mc vf va)

and apply_ty (mc : MetaContext.t) (ty : value) (va : value) : value =
  match ty with
  | VPi { codomain = clo; _ } -> eval mc (va :: clo.env) clo.body
  | _ -> VU

and eval_effect_row_closure (mc : MetaContext.t) (row : effect_row_closure)
    (binder : value) : effect_row_value =
  let env = binder :: row.env in
  let row_value =
    { effect_values = List.map (eval mc env) row.effects;
      tail_value = Option.map (eval mc env) row.tail }
  in
  normalize_effect_row_value mc row_value

and normalize_effect_row_value (mc : MetaContext.t) (row : effect_row_value) : effect_row_value =
  match Option.map (force mc) row.tail_value with
  | Some (VEffectRow tail_row) ->
      let tail_row = normalize_effect_row_value mc tail_row in
      { effect_values = row.effect_values @ tail_row.effect_values;
        tail_value = tail_row.tail_value }
  | tail_value -> { row with tail_value }

(* A match on a value with an unknown head: the match waits as its last frame. *)
and stuck_match env head frames branches : result =
  let branches = List.map (fun (p, body) -> (p, { env; body })) branches in
  Done (VNeutral { ty = VU; neutral = { head; frames = frames @ [ FMatch branches ] } })

and eval_meta (mc : MetaContext.t) (id : meta_id) : value =
  match MetaContext.lookup mc id with
  | Solved v -> v
  | Unsolved -> VFlex { id; spine = [] }
  | exception Invalid_argument _ -> VFlex { id; spine = [] }

(* Apply an InsertedMeta to the bound variables in scope, skipping defined ones *)
and eval_inserted_meta (mc : MetaContext.t) (env : env) (id : meta_id)
    (bds : bd list) : value =
  let base = eval_meta mc id in
  let rec go v e bds =
    match (e, bds) with
    | [], [] -> v
    | val_ :: env_rest, bd :: bds_rest -> (
        match bd with
        | Bound -> go (apply mc v val_) env_rest bds_rest
        | Defined -> go v env_rest bds_rest)
    | _ -> fail mc "bd mask length mismatch"
  in
  go base (List.rev env) (List.rev bds)

and eval_eff (env : env) (name : string) : value =
  let rec go = function
    | [] -> raise (EvalError ("unbound eff: " ^ name))
    | VEffect e :: _ when String.equal e.name name -> VEffect e
    | _ :: rest -> go rest
  in
  go env

and eval_match_result (mc : MetaContext.t) (env : env) (scrutinee : term)
    (branches : match_branch list) : result =
  let value_branches, effect_branches = close_match_branches env branches in
  let rec handle_scrutinee = function
    | Done v ->
        (* Always evaluate the selected branch body effect-awarely, even with no
           effect branches: a [perform] in a branch body must propagate to an
           outer handler (this path also covers [if], now desugared to [match]). *)
        handle_body (eval_match_result_value mc env v value_branches)
    | Effect request -> handle_effect handle_scrutinee request
  and handle_body = function
    | Done v -> Done v
    | Effect request -> handle_effect handle_body request
  and handle_effect resume_with request =
    (* A tunneled request skips this handler when it handles the request's
       effect family at all (the count was taken per family). *)
    let handles_family =
      request.hops > 0
      && List.exists (fun (branch_eff, _, _, _) -> same_effect_family mc branch_eff request.eff) effect_branches
    in
    if handles_family then
      Effect { request with hops = request.hops - 1; k = (fun resume -> resume_with (request.k resume)) }
    else
    match
      find_effect_branch mc effect_branches request.eff request.op request.arg
    with
    | Some (arg_bindings, body) ->
        (* Deep (E8): resuming re-enters this handler, so the resumed
           computation's result passes through its value branch. *)
        let k = make_cont (fun resume -> resume_with (request.k resume)) in
        handle_body
          (eval_result mc
             (k :: List.rev_append arg_bindings body.env)
             body.body)
    | None ->
        Effect
          { request with k = (fun resume -> resume_with (request.k resume)) }
  in
  handle_scrutinee (eval_result mc env scrutinee)

and close_match_branches env branches =
  let value_branches, effect_branches =
    List.fold_right
      (fun branch (values, effects) ->
        match branch with
        | ValueBranch (pat, body) -> ((pat, body) :: values, effects)
        | EffectBranch { eff; op; arg_pat; body } ->
            (values, (eff, op, arg_pat, { env; body }) :: effects))
      branches ([], [])
  in
  (value_branches, effect_branches)

and find_effect_branch mc branches eff op arg =
  List.find_map
    (fun (branch_eff, branch_op, arg_pat, body) ->
      let branch_eff = force mc branch_eff in
      if String.equal op branch_op && runtime_value_equal mc eff branch_eff then
        Option.map
          (fun bindings -> (bindings, body))
          (match_core_pat mc body.env arg_pat arg)
      else None)
    branches

and same_effect_family mc lhs rhs =
  match (force mc lhs, force mc rhs) with
  | VEffect e1, VEffect e2 -> e1.id = e2.id
  | _ -> false

and runtime_value_equal mc lhs rhs =
  lhs == rhs ||
  match (force mc lhs, force mc rhs) with
  | VRef a, VRef b -> a == b
  | VEffect e1, VEffect e2 ->
      e1.id = e2.id
      && List.length e1.params = List.length e2.params
      && List.for_all2 (runtime_value_equal mc) e1.params e2.params
  | VNominal n1, VNominal n2 ->
      n1.id = n2.id
      && List.length n1.captures = List.length n2.captures
      && List.for_all2 (runtime_value_equal mc) n1.captures n2.captures
      && List.length n1.params = List.length n2.params
      && List.for_all2 (runtime_value_equal mc) n1.params n2.params
  | VAtom a, VAtom b -> Atom.equal a b
  | VAtomTy a, VAtomTy b -> Atom_ty.equal a b
  | VU, VU -> true
  | _ -> false

and core_pat_contains_struct_type = function
  | CPatStructType _ -> true
  | CPatProd pats -> List.exists core_pat_contains_struct_type pats
  | CPatOr (lhs, rhs) ->
      core_pat_contains_struct_type lhs || core_pat_contains_struct_type rhs
  | CPatRecord { fields; _ } ->
      List.exists (fun (_, pat) -> core_pat_contains_struct_type pat) fields
  | CPatCon (_, _, pats) -> List.exists core_pat_contains_struct_type pats
  | CPatNominalHead { param_pats; _ } ->
      List.exists core_pat_contains_struct_type param_pats
  | CPatWild | CPatBind | CPatAtom _ | CPatType _ | CPatSyn _ -> false

and core_pat_contains_nominal_head = function
  | CPatNominalHead _ -> true
  | CPatProd pats -> List.exists core_pat_contains_nominal_head pats
  | CPatOr (lhs, rhs) ->
      core_pat_contains_nominal_head lhs || core_pat_contains_nominal_head rhs
  | CPatRecord { fields; _ } ->
      List.exists (fun (_, pat) -> core_pat_contains_nominal_head pat) fields
  | CPatCon (_, _, pats) -> List.exists core_pat_contains_nominal_head pats
  | CPatStructType { fields; _ } ->
      List.exists (fun (_, pat) -> core_pat_contains_nominal_head pat) fields
  | CPatWild | CPatBind | CPatAtom _ | CPatType _ | CPatSyn _ -> false

and struct_type_fields fields =
  List.filter_map
    (fun (name, kind, ty) -> if kind = Field then Some (name, ty) else None)
    fields

and match_core_pat mc env pat value =
  match (pat, force mc value) with
  | CPatWild, _ -> Some []
  | CPatBind, v -> Some [ v ]
  | CPatAtom expected, VAtom actual when Atom.equal expected actual -> Some []
  | CPatType expected, VAtomTy actual when Atom_ty.equal expected actual ->
      Some []
  | CPatProd pats, VProd values when List.length pats = List.length values ->
      match_core_pats mc env pats values
  | CPatSyn { rhs; _ }, v -> match_core_pat mc env rhs v
  | CPatCon (name, num_type_params, sub_pats), VCon { name = actual; spine; _ }
    when String.equal name actual ->
      let payload = List.drop num_type_params spine in
      if List.length sub_pats = List.length payload then
        match_core_pats mc env sub_pats payload
      else None
  | CPatNominalHead { id; head; param_pats; _ }, VNominal n when n.id = id && same_instance mc env head n.id n.captures n.params ->
      if List.length param_pats = List.length n.params then
        match_core_pats mc env param_pats n.params
      else None
  | CPatRecord { fields; _ }, VRecord { fields = values; _ } ->
      let rec go acc = function
        | [] -> Some (List.rev acc)
        | (name, pat) :: rest -> (
            match List.assoc_opt name values with
            | Some value -> (
                match match_core_pat mc env pat value with
                | Some bindings -> go (List.rev_append bindings acc) rest
                | None -> None)
            | None -> None)
      in
      go [] fields
  | CPatStructType { fields; partial }, VStruct { entries = struct_entries; _ }
    ->
      let struct_fields =
        struct_type_fields (struct_entry_fields struct_entries)
      in
      if (not partial) && List.length fields <> List.length struct_fields then
        None
      else
        let rec go acc = function
          | [] -> Some (List.rev acc)
          | (name, pat) :: rest -> (
              match List.assoc_opt name struct_fields with
              | Some field_ty -> (
                  match match_core_pat mc env pat field_ty with
                  | Some bindings -> go (List.rev_append bindings acc) rest
                  | None -> None)
              | None -> None)
        in
        go [] fields
  | CPatOr (lhs, rhs), v -> (
      match match_core_pat mc env lhs v with
      | Some _ as matched -> matched
      | None -> match_core_pat mc env rhs v)
  | _ -> None

(* A written nominal head, read in the match's scope, is the same instance as
   [n]: its declaration evaluated over the same captures (E11). A type former is
   applied to [n]'s params first. *)
and same_instance mc env head id captures params =
  match head with
  | None -> true
  | Some term -> (
      let written = force mc (eval mc env term) in
      let written = match written with VNominal _ -> written | _ -> force mc (List.fold_left (apply mc) written params) in
      match written with
      | VNominal h ->
          h.id = id && List.length h.captures = List.length captures && List.for_all2 (runtime_value_equal mc) h.captures captures
      | _ -> false)

and match_core_pats mc env pats values =
  let rec go acc pats values =
    match (pats, values) with
    | [], [] -> Some (List.rev acc)
    | pat :: pats, value :: values -> (
        match match_core_pat mc env pat value with
        | Some bindings -> go (List.rev_append bindings acc) pats values
        | None -> None)
    | _ -> None
  in
  go [] pats values

(* Pattern matching: compile to decision tree, then interpret.
   For a stuck scrutinee, accumulate an FMatch frame. *)
and eval_match_result_value (mc : MetaContext.t) (env : env) (scrutinee : value)
    (branches : (core_pat * term) list) : result =
  let scrutinee = force mc scrutinee in
  if
    List.exists
      (fun (pat, _) ->
        core_pat_contains_struct_type pat || core_pat_contains_nominal_head pat)
      branches
  then eval_match_direct_result mc env scrutinee branches
  else
    match scrutinee with
    (* Only a value whose head is unknown makes the match stuck. Every other
       value - constructors, atoms, types, products, records, closures - is
       matched by the decision tree; a shape no pattern inspects has the
       [Unknown] domain, so a variable or wildcard binds it. *)
    | VNeutral { neutral; _ } -> stuck_match env neutral.head neutral.frames branches
    | VFlex { id; spine } -> stuck_match env (HMeta id) (List.map (fun v -> FApp v) spine) branches
    | VRigid { lvl; spine } -> stuck_match env (HVar lvl) (List.map (fun v -> FApp v) spine) branches
    | _ ->
        let domain_of_occurrence occ =
          match resolve_occurrence_opt mc scrutinee occ with
          | Some (VCon { nominal; _ }) -> Core_match_compile.Nominal (nominal_constructors mc nominal)
          | Some (VAtom atom) -> Core_match_compile.Atom (Nbe_prim.atom_ty_of_atom atom)
          | Some (VAtomTy _) | Some (VNominal _) -> Type
          | Some (VProd elems) -> Product (List.length elems)
          | Some (VRecord { typ = VStruct { entries; _ }; _ }) ->
              Record
                (List.filter_map
                   (fun (n, k, _) -> if k = Field then Some n else None)
                   (struct_entry_fields entries))
          | _ -> Unknown
        in
        let dt = Core_match_compile.compile_with_domains ~domain_of_occurrence (List.map fst branches) in
        eval_decision_tree_result mc env scrutinee branches dt

and eval_match_direct (mc : MetaContext.t) (env : env) (scrutinee : value)
    (branches : (core_pat * term) list) : value =
  result_value mc (eval_match_direct_result mc env scrutinee branches)

and eval_match_direct_result (mc : MetaContext.t) (env : env)
    (scrutinee : value) (branches : (core_pat * term) list) : result =
  match branches with
  | [] -> fail mc "non-exhaustive match at runtime"
  | (pat, body) :: rest -> (
      match match_core_pat mc env pat scrutinee with
      | Some bindings -> eval_result mc (List.rev_append bindings env) body
      | None -> eval_match_direct_result mc env scrutinee rest)

and nominal_constructors (mc : MetaContext.t) (nom : value) :
    (string * int * int) list =
  match force mc nom with
  | VNominal { id; params; captures; _ } ->
      let ntp = List.length params in
      List.map
        (fun (name, payloads) -> (name, ntp, List.length payloads))
        (Core.nominal_constructors id captures)
  | _ -> fail mc "match scrutinee type is not a nominal"

and eval_decision_tree_result (mc : MetaContext.t) (env : env) (root : value)
    (branches : (core_pat * term) list) (dt : Core_decision_tree.t) : result =
  match dt.content with
  | Leaf { branch; bindings } ->
      let env' =
        List.fold_left
          (fun e occ -> resolve_occurrence mc root occ :: e)
          env bindings
      in
      let _, body = List.nth branches branch in
      eval_result mc env' body
  | Destruct { occurrence; cases; default } -> (
      let v = resolve_occurrence mc root occurrence |> force mc in
      match v with
      | VCon { name; _ } -> (
          match
            List.find_opt (fun (cn, _, _) -> String.equal cn name) cases
          with
          | Some (_, _, sub) ->
              eval_decision_tree_result mc env root branches sub
          | None -> (
              match default with
              | Some d -> eval_decision_tree_result mc env root branches d
              | None -> fail mc "non-exhaustive match at runtime"))
      | _ -> (
          match default with
          | Some d -> eval_decision_tree_result mc env root branches d
          | None -> fail mc "match on non-constructor value"))
  | Switch { cases; default; occurrence } -> (
      let v = resolve_occurrence mc root occurrence |> force mc in
      (match v with
       | VAtom atom ->
           (match List.find_opt (fun (case_key, _) -> Core_decision_tree.switch_key_equal case_key (Core_decision_tree.KAtom atom)) cases with
            | Some (_, sub) -> eval_decision_tree_result mc env root branches sub
            | None -> eval_decision_tree_result mc env root branches default)
       | VAtomTy atom_ty ->
           (match List.find_opt (fun (case_key, _) -> Core_decision_tree.switch_key_equal case_key (KType atom_ty)) cases with
            | Some (_, sub) -> eval_decision_tree_result mc env root branches sub
            | None -> eval_decision_tree_result mc env root branches default)
       | VNominal n ->
           (match List.find_opt (fun (case_key, _) -> Core_decision_tree.switch_key_equal case_key (KNominal n.id)) cases with
            | Some (_, sub) -> eval_decision_tree_result mc env root branches sub
            | None -> eval_decision_tree_result mc env root branches default)
       | _ -> eval_decision_tree_result mc env root branches default))

and resolve_occurrence (mc : MetaContext.t) (root : value)
    (occ : Core_decision_tree.occurrence) : value =
  match resolve_occurrence_opt mc root occ with
  | Some v -> v
  | None -> fail mc "resolve_occurrence: invalid occurrence"

and resolve_occurrence_opt (mc : MetaContext.t) (root : value)
    (occ : Core_decision_tree.occurrence) : value option =
  match occ with
  | OBase -> Some root
  | OChild { parent; index } -> (
      match Option.map (force mc) (resolve_occurrence_opt mc root parent) with
      | Some (VCon { spine; _ }) -> List.nth_opt spine index
      | Some (VNominal { params; _ }) -> List.nth_opt params index
      | Some (VProd elems) -> List.nth_opt elems index
      | _ -> None)
  | OField { parent; name } -> (
      match Option.map (force mc) (resolve_occurrence_opt mc root parent) with
      | Some (VRecord { fields; _ }) -> List.assoc_opt name fields
      | _ -> None)

and force (mc : MetaContext.t) (v : value) : value =
  match v with
  | VGlued { unfolded; _ } -> force mc (Lazy.force unfolded)
  | VFlex { id; spine = sp } -> (
      match MetaContext.lookup mc id with
      | Solved v ->
          let applied = List.fold_left (fun f a -> apply mc f a) v sp in
          force mc applied
      | Unsolved -> VFlex { id; spine = sp }
      | exception Invalid_argument _ -> VFlex { id; spine = sp })
  | _ -> v

let quote_ops : Nbe_quote.ops =
  { force;
    closure_apply;
    eval_effect_row_closure;
    eval;
    apply }

let lvl_to_ix = Nbe_quote.lvl_to_ix
let conv_pat = Nbe_quote.conv_pat
let fix_bodies mc depth fc = Nbe_quote.fix_bodies_at quote_ops mc depth fc

(* The checker's entry points. Each call from outside the evaluator is one
   evaluation under the budget (see [Eval_budget]); the evaluator's own
   recursion above binds the unwrapped functions, so re-entry spends from the
   same request. [run] is the one entry that runs a program, with no limit. *)
let request ~demand mc f = Eval_budget.request ~demand mc.MetaContext.budget f
let run mc env t = Eval_budget.run mc.MetaContext.budget (fun () -> eval mc env t)
let eval mc env t = request ~demand:"an evaluation" mc (fun () -> eval mc env t)
let apply mc f a = request ~demand:"an application" mc (fun () -> apply mc f a)
let closure_apply mc c v = request ~demand:"an application" mc (fun () -> closure_apply mc c v)
let eval_effect_row_closure mc row binder = request ~demand:"an effect row" mc (fun () -> eval_effect_row_closure mc row binder)
let force mc v =
  match v with
  | VFlex _ -> request ~demand:"forcing a metavariable" mc (fun () -> force mc v)
  | VGlued _ -> request ~demand:"an evaluation" mc (fun () -> force mc v)
  | _ -> v
(* A type as the shape it has: metavariables solved, a recursive occurrence
   unfolded to its struct type. *)
let force_shape mc v =
  match force mc v with
  | VRecOcc _ as occ -> (match request ~demand:"unfolding a recursive type" mc (fun () -> Nbe_quote.unfold_rec quote_ops mc occ) with VRecOcc _ as v -> v | v -> force mc v)
  | v -> v
(* A module's type as its member types: a signature instantiated with the module
   it describes ([module_value]), so a member reads earlier ones through it. *)
let module_type_of mc ty module_value =
  match force_shape mc ty with
  | VSig clo -> force mc (closure_apply mc clo module_value)
  | v -> v
let quote mc depth value = request ~demand:"a normalisation" mc (fun () -> Nbe_quote.quote quote_ops mc depth value)
let conv mc depth lhs rhs = request ~demand:"a conversion" mc (fun () -> Nbe_quote.conv quote_ops mc depth lhs rhs)

(* How a macro is applied (the expander's [eval_and_apply]): with fresh metas,
   since an application solves nothing its caller needs, under the budget of the
   expansion it belongs to, so its body spends from that one request (M5). *)
let apply_macro budget f a = apply (MetaContext.create ~budget ()) f a

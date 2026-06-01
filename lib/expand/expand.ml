open Syntax

let id_name (id : Syntax.id) : string = id.name

let param_name (param : Syntax.param) : string = id_name param.name

let add_id_scope (scope : Scope_set.t) (id : Syntax.id) : Syntax.id =
  { name = id.name; span = id.span; scope = Scope_set.union id.scope scope }

let add_id_scopes scopes id =
  List.fold_left (fun id scope -> add_id_scope scope id) id scopes

let bind_id scope resolved_name (id : Syntax.id) : Syntax.id =
  { name = resolved_name; span = id.span; scope = Scope_set.union id.scope scope }

let map_param (scope : Scope_set.t) (f : Syntax.t -> Syntax.t) (param : Syntax.param) : Syntax.param =
  { name = add_id_scope scope param.name;
    type_ = Option.map f param.type_;
    trait_bounds = param.trait_bounds;
    explicitness = param.explicitness }

(** Add a scope mark to every variable occurrence scope set. *)
let rec add_scope (s : Scope_set.t) (stx : t) : t =
  { stx with kind = go_kind s stx.kind }

and go_kind (s : Scope_set.t) (k : kind) : kind =
  let go = add_scope s in
  match k with
  | Var id -> Var { id with scope = Scope_set.union id.scope s }
  | Atom _ -> k
  | Self -> k
  | SelfType -> k
  | Ap (f, e, a) -> Ap (go f, e, go a)
  | Lam (p, body) ->
    Lam (map_param s go p, go body)
  | Let { name; type_; value; body; recursive } ->
    Let { name = add_id_scope s name;
          type_ = Option.map go type_;
          value = go value;
          body = go body;
          recursive }
  | If { cond; then_; else_ } ->
    If { cond = go cond; then_ = go then_; else_ = go else_ }
  | Annotated { inner; typ } ->
    Annotated { inner = go inner; typ = go typ }
  | Prod xs -> Prod (List.map go xs)
  | ProdTy xs -> ProdTy (List.map go xs)
  | Arrow (expl, name, dom, eff, cod) ->
    Arrow (expl, Option.map (add_id_scope s) name, go dom, Option.map (fun e -> { effects = List.map go e.effects; tail = Option.map go e.tail }) eff, go cod)
  | FieldAccess (e, n) -> FieldAccess (go e, n)
  | Proj (e, n) -> Proj (go e, n)
  | RecordConstruct { typ; fields } ->
    RecordConstruct { typ = go typ; fields = List.map (fun (n, e) -> (n, go e)) fields }
  | Struct { con_fields; bindings } ->
    Struct { con_fields = List.map (fun (n, e) -> (n, go e)) con_fields;
             bindings = List.map (go_struct_binding s) bindings }
  | Module { bindings } ->
    Module { bindings = List.map (go_struct_binding s) bindings }
  | Import _ -> k
  | Open (m, body) -> Open (add_id_scope s m, go body)
  | RecordTypeDef { name; params; fields; body } ->
    RecordTypeDef { name = add_id_scope s name; params; fields = List.map (fun (n, e) -> (n, go e)) fields; body = go body }
  | TypeDef { name; params; ctors; body } ->
    TypeDef { name = add_id_scope s name; params; ctors = List.map (fun (n, p) -> (add_id_scope s n, Option.map go p)) ctors; body = go body }
  | EffectDef { name; params; ops; body } ->
    EffectDef { name = add_id_scope s name; params; ops = List.map (fun op -> { op with input = go op.input; output = go op.output }) ops; body = go body }
  | TraitDef { name; params; fields; body } ->
    TraitDef { name = add_id_scope s name; params; fields = List.map (fun (n, e) -> (n, go e)) fields; body = go body }
  | ImplDef { trait_path; trait_name; args; fields; body } ->
    ImplDef { trait_path; trait_name; args = List.map go args; fields = List.map (fun (n, e) -> (n, go e)) fields; body = go body }
  | Perform { effect_path; op; arg } ->
    Perform { effect_path; op; arg = go arg }
  | Resume e -> Resume (go e)
  | RefNew e -> RefNew (go e)
  | RefGet e -> RefGet (go e)
  | RefSet (l, r) -> RefSet (go l, go r)
  | Match (scrut, brs) ->
    Match (go scrut, List.map (go_match_branch s) brs)
  | MacroDef { name; value; body } ->
    MacroDef { name = add_id_scope s name; value = go value; body = go body }
  | MacroCall (f, a) ->
    MacroCall (go f, go a)

and go_struct_binding (s : Scope_set.t) (binding : Syntax.struct_binding) : Syntax.struct_binding =
  match binding with
  | LetBinding { name; value; public } ->
    LetBinding { name = add_id_scope s name;
                 value = add_scope s value; public }
  | MethodBinding { name; params; body; public } ->
    let new_name = add_id_scope s name in
    let new_params = List.map (map_param s (add_scope s)) params in
    MethodBinding { name = new_name; params = new_params;
                    body = add_scope s body; public }
  | TypeBinding { name; params; ctors; public } ->
    TypeBinding { name = add_id_scope s name;
                  params; ctors = List.map (fun (n, p) -> (n, Option.map (add_scope s) p)) ctors; public }
  | RecordTypeBinding { name; params; fields; public } ->
    RecordTypeBinding { name = add_id_scope s name;
                        params; fields = List.map (fun (n, e) -> (n, add_scope s e)) fields; public }
  | EffectBinding { name; params; ops; public } ->
    EffectBinding { name = add_id_scope s name;
                    params; ops = List.map (fun op -> { op with input = add_scope s op.input; output = add_scope s op.output }) ops; public }
  | TraitBinding { name; params; fields; public } ->
    TraitBinding { name = add_id_scope s name;
                   params; fields = List.map (fun (n, e) -> (n, add_scope s e)) fields; public }
  | ImplBinding { trait_path; trait_name; args; fields; public } ->
    ImplBinding { trait_path; trait_name; args = List.map (add_scope s) args;
                  fields = List.map (fun (n, e) -> (n, add_scope s e)) fields; public }
  | MacroBinding { name; value; public } ->
    MacroBinding { name = add_id_scope s name; value = add_scope s value; public }

and go_match_branch s = function
  | ValueBranch (p, body) -> ValueBranch (go_pat s p, add_scope s body)
  | EffectBranch { effect_path; op; arg_pat; body } ->
    EffectBranch { effect_path; op; arg_pat = go_pat s arg_pat; body = add_scope s body }

and go_pat s k = match k with
  | PatCon (path, name, ps) -> PatCon (path, name, List.map (go_pat s) ps)
  | PatRecord { typ_path; typ; fields; partial } ->
    PatRecord { typ_path; typ;
                fields = List.map (fun (n, p) -> (n, Option.map (go_pat s) p)) fields;
                partial }
  | PatStructType { fields; partial } ->
    PatStructType { fields = List.map (fun (n, p) -> (n, go_pat s p)) fields; partial }
  | PatOr (l, r) -> PatOr (go_pat s l, go_pat s r)
  | PatProd ps -> PatProd (List.map (go_pat s) ps)
  | PatAtom _ -> k
  | PatType _ -> k
  | PatWild -> k
  | PatBind id -> PatBind { id with scope = Scope_set.union id.scope s }

let add_param_scope (scope : Scope_set.t) (param : Syntax.param) : Syntax.param =
  { param with name = add_id_scope scope param.name; type_ = Option.map (add_scope scope) param.type_ }

let add_scopes (scopes : Scope_set.t list) (stx : Syntax.t) : Syntax.t =
  List.fold_left (fun acc scope -> add_scope scope acc) stx scopes

let add_pat_scope (scope : Scope_set.t) pat = go_pat scope pat

let add_pat_scopes (scopes : Scope_set.t list) pat =
  List.fold_left (fun acc scope -> add_pat_scope scope acc) pat scopes

let add_struct_binding_scopes scopes binding =
  List.fold_left
    (fun binding scope -> go_struct_binding scope binding)
    binding scopes

let expand_id_params (ctx : Expand_ctx.t) scopes params =
  let rec go active_scopes param_scopes acc = function
    | [] -> (List.rev acc, List.rev param_scopes)
    | param :: rest ->
      let param = add_id_scopes active_scopes param in
      let scope, resolved_name =
        Expand_ctx.extend_at_fresh ctx ~name:param.name ~base_scope:param.scope
      in
      go (active_scopes @ [ scope ]) (scope :: param_scopes)
        (bind_id scope resolved_name param :: acc) rest
  in
  go scopes [] [] params

(** The main expander: walks the syntax tree, allocates fresh scopes for
    each binder, adds those scopes to identifier occurrences in the binder's
    body. This implements hygienic lexical scoping. *)
let rec expand (ctx : Expand_ctx.t) (stx : t) : t =
  match stx.kind with
  | Var id ->
    begin match Expand_ctx.resolve ctx id with
    | Some info -> { stx with kind = Var { id with name = info.resolved_name } }
    | None -> stx
    end
  | Atom _ | Self | SelfType -> stx
  | Import path ->
    Option.iter (fun f -> f ctx path) ctx.Expand_ctx.load_macros;
    stx
  | Lam (param, body) ->
    let pname = param_name param in
    let scope, resolved_name = Expand_ctx.extend_at_fresh ctx ~name:pname ~base_scope:param.name.scope in
    let body = expand ctx (add_scope scope body) in
    let param = { param with name = bind_id scope resolved_name param.name; type_ = Option.map (expand ctx) param.type_ } in
    { stx with kind = Lam (param, body) }
  | Let { name; type_; value; body; recursive } ->
    let binding_name = id_name name in
    let scope, resolved_name = Expand_ctx.extend_at_fresh ctx ~name:binding_name ~base_scope:name.scope in
    let value = if recursive then expand ctx (add_scope scope value) else expand ctx value in
    let body = expand ctx (add_scope scope body) in
    let name = bind_id scope resolved_name name in
    { stx with kind = Let { name; type_ = Option.map (expand ctx) type_; value; body; recursive } }
  | Ap (f, e, a) ->
    { stx with kind = Ap (expand ctx f, e, expand ctx a) }
  | If { cond; then_; else_ } ->
    { stx with kind = If { cond = expand ctx cond; then_ = expand ctx then_; else_ = expand ctx else_ } }
  | Annotated { inner; typ } ->
    { stx with kind = Annotated { inner = expand ctx inner; typ = expand ctx typ } }
  | Prod xs -> { stx with kind = Prod (List.map (expand ctx) xs) }
  | ProdTy xs -> { stx with kind = ProdTy (List.map (expand ctx) xs) }
  | Arrow (expl, Some name, dom, eff, cod) ->
    let dom = expand ctx dom in
    let scope, resolved_name = Expand_ctx.extend_at_fresh ctx ~name:name.name ~base_scope:name.scope in
    let name = bind_id scope resolved_name name in
    let expand_scoped e = expand ctx (add_scope scope e) in
    let eff = Option.map (fun e -> { effects = List.map expand_scoped e.effects; tail = Option.map expand_scoped e.tail }) eff in
    { stx with kind = Arrow (expl, Some name, dom, eff, expand_scoped cod) }
  | Arrow (expl, None, dom, eff, cod) ->
    { stx with kind = Arrow (expl, None, expand ctx dom, Option.map (fun e -> { effects = List.map (expand ctx) e.effects; tail = Option.map (expand ctx) e.tail }) eff, expand ctx cod) }
  | FieldAccess (e, n) -> { stx with kind = FieldAccess (expand ctx e, n) }
  | Proj (e, n) -> { stx with kind = Proj (expand ctx e, n) }
  | RecordConstruct { typ; fields } ->
    { stx with kind = RecordConstruct { typ = expand ctx typ; fields = List.map (fun (n, e) -> (n, expand ctx e)) fields } }
  | Struct { con_fields; bindings } ->
    { stx with kind = Struct { con_fields = List.map (fun (n, e) -> (n, expand ctx e)) con_fields;
                               bindings = expand_struct_bindings ctx bindings } }
  | Module { bindings } ->
    { stx with kind = Module { bindings = expand_struct_bindings ctx bindings } }
  | Open (m, body) ->
    let m =
      match Expand_ctx.resolve ctx m with
      | Some info -> { m with name = info.resolved_name }
      | None -> m
    in
    { stx with kind = Open (m, expand ctx body) }
  | RecordTypeDef { name; params; fields; body } ->
    let scope = Expand_ctx.extend_at ctx ~name:name.name ~base_scope:name.scope ~resolved_name:name.name in
    let name = add_id_scope scope name in
    let params, param_scopes = expand_id_params ctx [] params in
    { stx with kind = RecordTypeDef { name; params; fields = List.map (fun (n, e) -> (n, expand ctx (add_scopes param_scopes e))) fields; body = expand ctx (add_scope scope body) } }
  | TypeDef { name; params; ctors; body } ->
    let scope = Expand_ctx.extend_at ctx ~name:name.name ~base_scope:name.scope ~resolved_name:name.name in
    let name = add_id_scope scope name in
    let params, param_scopes = expand_id_params ctx [] params in
    let ctors, ctor_scopes =
      List.split
        (List.map
           (fun ((cname : Syntax.id), payload) ->
             let ctor_scope = Expand_ctx.extend_at ctx ~name:cname.name ~base_scope:cname.scope ~resolved_name:cname.name in
             ((add_id_scope ctor_scope cname, payload), ctor_scope))
           ctors)
    in
    { stx with kind = TypeDef { name; params; ctors = List.map (fun (n, p) -> (n, Option.map (fun p -> expand ctx (add_scopes (scope :: param_scopes) p)) p)) ctors; body = expand ctx (add_scopes (scope :: ctor_scopes) body) } }
  | EffectDef { name; params; ops; body } ->
    let scope = Expand_ctx.extend_at ctx ~name:name.name ~base_scope:name.scope ~resolved_name:name.name in
    let name = add_id_scope scope name in
    let params, param_scopes = expand_id_params ctx [] params in
    { stx with kind = EffectDef { name; params; ops = List.map (fun op -> { op with input = expand ctx (add_scopes param_scopes op.input); output = expand ctx (add_scopes param_scopes op.output) }) ops; body = expand ctx (add_scope scope body) } }
  | TraitDef { name; params; fields; body } ->
    let scope = Expand_ctx.extend_at ctx ~name:name.name ~base_scope:name.scope ~resolved_name:name.name in
    let name = add_id_scope scope name in
    let params, param_scopes = expand_id_params ctx [] params in
    { stx with kind = TraitDef { name; params; fields = List.map (fun (n, e) -> (n, expand ctx (add_scopes param_scopes e))) fields; body = expand ctx (add_scope scope body) } }
  | ImplDef { trait_path; trait_name; args; fields; body } ->
    { stx with kind = ImplDef { trait_path; trait_name; args = List.map (expand ctx) args;
                                fields = List.map (fun (n, e) -> (n, expand ctx e)) fields;
                                body = expand ctx body } }
  | Perform { effect_path; op; arg } ->
    { stx with kind = Perform { effect_path; op; arg = expand ctx arg } }
  | Resume e -> { stx with kind = Resume (expand ctx e) }
  | RefNew e -> { stx with kind = RefNew (expand ctx e) }
  | RefGet e -> { stx with kind = RefGet (expand ctx e) }
  | RefSet (l, r) -> { stx with kind = RefSet (expand ctx l, expand ctx r) }
  | Match (scrut, brs) ->
    { stx with kind =
        Match (expand ctx scrut,
               List.map (expand_match_branch ctx) brs) }
  | MacroDef { name; value; body } ->
    begin match ctx.Expand_ctx.elaborate with
    | Some elab ->
      let lowered = Lower_surface.lower_expr value in
      let macro_fn = elab lowered in
      Expand_ctx.register_macro ctx ~name:name.name ~value:macro_fn;
      expand ctx body
    | None ->
      failwith "macro definition requires an elaboration callback in expand context"
    end
  | MacroCall (f, a) ->
    begin match f.kind with
    | Var id ->
      begin match Expand_ctx.lookup_macro ctx id.name with
      | Some macro_fn ->
        begin match ctx.Expand_ctx.eval_and_apply with
        | Some apply_fn ->
          let arg_stx = Macro_eval.wrap_stx a in
          let result = apply_fn macro_fn arg_stx in
          begin match Macro_eval.unwrap_stx result with
          | Some expanded -> expand ctx expanded
          | None -> failwith "macro did not return a syntax value"
          end
        | None -> failwith "macro call requires an apply callback in expand context"
        end
      | None -> { stx with kind = MacroCall (expand ctx f, expand ctx a) }
      end
    | _ -> { stx with kind = MacroCall (expand ctx f, expand ctx a) }
    end

and expand_struct_bindings (ctx : Expand_ctx.t) bindings =
  let rec go active_scopes acc = function
    | [] -> List.rev acc
    | binding :: rest ->
      let binding = add_struct_binding_scopes active_scopes binding in
      let binding, introduced_scopes = expand_struct_binding ctx binding in
      let acc = match binding with
        | Syntax.MacroBinding _ -> acc
        | _ -> binding :: acc
      in
      go (active_scopes @ introduced_scopes) acc rest
  in
  go [] [] bindings

and expand_method_params_body ctx params body =
  let rec go active_scopes param_scopes acc = function
    | [] ->
      (List.rev acc, expand ctx (add_scopes param_scopes body))
    | param :: rest ->
      let param = List.fold_left (fun param scope -> add_param_scope scope param) param active_scopes in
      let pname = param_name param in
      let scope, resolved_name = Expand_ctx.extend_at_fresh ctx ~name:pname ~base_scope:param.name.scope in
      let param = { param with name = bind_id scope resolved_name param.name; type_ = Option.map (expand ctx) param.type_ } in
      go (active_scopes @ [ scope ]) (param_scopes @ [ scope ]) (param :: acc) rest
  in
  go [] [] [] params

and expand_struct_binding (ctx : Expand_ctx.t) (binding : Syntax.struct_binding) : Syntax.struct_binding * Scope_set.t list =
  match binding with
  | LetBinding { name; value; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    (LetBinding { name = add_id_scope scope name;
                  value = expand ctx value; public },
     [ scope ])
  | MethodBinding { name; params; body; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    let params, body = expand_method_params_body ctx params body in
    (MethodBinding { name = add_id_scope scope name; params; body; public }, [ scope ])
  | TypeBinding { name; params; ctors; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    let params, param_scopes = expand_id_params ctx [] params in
    let ctors, ctor_scopes =
      List.split
        (List.map
           (fun ((cname : Syntax.id), payload) ->
             let ctor_scope = Expand_ctx.extend_at ctx ~name:cname.name ~base_scope:cname.scope ~resolved_name:cname.name in
             ((add_id_scope ctor_scope cname, payload), ctor_scope))
           ctors)
    in
    (TypeBinding { name = add_id_scope scope name;
                   params; ctors = List.map (fun (n, p) -> (n, Option.map (fun p -> expand ctx (add_scopes (scope :: param_scopes) p)) p)) ctors; public },
     scope :: ctor_scopes)
  | RecordTypeBinding { name; params; fields; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    let params, param_scopes = expand_id_params ctx [] params in
    (RecordTypeBinding { name = add_id_scope scope name;
                         params; fields = List.map (fun (n, e) -> (n, expand ctx (add_scopes param_scopes e))) fields; public },
     [ scope ])
  | EffectBinding { name; params; ops; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    let params, param_scopes = expand_id_params ctx [] params in
    (EffectBinding { name = add_id_scope scope name;
                     params; ops = List.map (fun op -> { op with input = expand ctx (add_scopes param_scopes op.input); output = expand ctx (add_scopes param_scopes op.output) }) ops; public },
     [ scope ])
  | TraitBinding { name; params; fields; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    let params, param_scopes = expand_id_params ctx [] params in
    (TraitBinding { name = add_id_scope scope name;
                    params; fields = List.map (fun (n, e) -> (n, expand ctx (add_scopes param_scopes e))) fields; public },
     [ scope ])
  | ImplBinding { trait_path; trait_name; args; fields; public } ->
    (ImplBinding { trait_path; trait_name; args = List.map (expand ctx) args;
                   fields = List.map (fun (n, e) -> (n, expand ctx e)) fields; public },
     [])
  | MacroBinding { name; value; public } ->
    begin match ctx.Expand_ctx.elaborate with
    | Some elab ->
      let lowered = Lower_surface.lower_expr value in
      let macro_fn = elab lowered in
      let binding_name = id_name name in
      let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
      Expand_ctx.register_macro ctx ~name:binding_name ~value:macro_fn;
      (MacroBinding { name = add_id_scope scope name; value = expand ctx value; public },
       [ scope ])
    | None ->
      (MacroBinding { name; value = expand ctx value; public }, [])
    end

and expand_match_branch ctx = function
  | ValueBranch (p, body) ->
    let ctx' = Expand_ctx.copy ctx in
    let binder_scopes = expand_pat_binders ctx' p in
    ValueBranch (expand_pat ctx' (add_pat_scopes binder_scopes p), expand ctx' (add_scopes binder_scopes body))
  | EffectBranch { effect_path; op; arg_pat; body } ->
    let ctx' = Expand_ctx.copy ctx in
    let binder_scopes = expand_pat_binders ctx' arg_pat in
    EffectBranch { effect_path; op; arg_pat = expand_pat ctx' (add_pat_scopes binder_scopes arg_pat); body = expand ctx' (add_scopes binder_scopes body) }

and expand_pat_binders ctx pat =
  let rec collect (acc : Syntax.id list) = function
    | PatBind id ->
      if List.exists (fun (existing : Syntax.id) -> String.equal existing.name id.name) acc then acc else id :: acc
    | PatCon (_, _, ps) | PatProd ps -> List.fold_left collect acc ps
    | PatRecord { fields; _ } ->
      List.fold_left (fun acc (_, p) -> match p with Some p -> collect acc p | None -> acc) acc fields
    | PatStructType { fields; _ } ->
      List.fold_left (fun acc (_, p) -> collect acc p) acc fields
    | PatOr (l, r) -> collect (collect acc l) r
    | PatAtom _ | PatType _ | PatWild -> acc
  in
  collect [] pat
  |> List.rev
  |> List.map (fun (id : Syntax.id) ->
      let scope, _resolved_name = Expand_ctx.extend_at_fresh ctx ~name:id.name ~base_scope:id.scope in
      scope)

and expand_pat ctx k = match k with
  | PatBind id ->
    let info = Expand_ctx.resolve ctx id in
    let resolved = match info with Some i -> i.Binding.resolved_name | None -> id.name in
    PatBind { id with name = resolved }
  | PatCon (path, name, ps) -> PatCon (path, name, List.map (expand_pat ctx) ps)
  | PatRecord { typ_path; typ; fields; partial } ->
    PatRecord { typ_path; typ; fields = List.map (fun (n, p) -> (n, Option.map (expand_pat ctx) p)) fields; partial }
  | PatStructType { fields; partial } ->
    PatStructType { fields = List.map (fun (n, p) -> (n, expand_pat ctx p)) fields; partial }
  | PatOr (l, r) -> PatOr (expand_pat ctx l, expand_pat ctx r)
  | PatProd ps -> PatProd (List.map (expand_pat ctx) ps)
  | PatAtom _ -> k
  | PatType _ -> k
  | PatWild -> k

let expand_expr ?loader stx =
  let ctx = Expand_ctx.create ?loader () in
  expand ctx stx

let expand_module ?loader stx =
  let ctx = Expand_ctx.create ?loader () in
  expand ctx stx

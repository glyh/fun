open Syntax

let id_name (id : Syntax.id) : string = id.name

let param_name (param : Syntax.param) : string = id_name param.name

let add_id_scope (scope : Scope_set.t) (id : Syntax.id) : Syntax.id =
  { name = id.name; span = id.span; scope = Scope_set.union id.scope scope }

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
    Arrow (expl, name, go dom, Option.map (fun e -> { effects = List.map go e.effects; tail = Option.map go e.tail }) eff, go cod)
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
  | Open (m, body) -> Open (m, go body)
  | RecordTypeDef { name; params; fields; body } ->
    RecordTypeDef { name; params; fields = List.map (fun (n, e) -> (n, go e)) fields; body = go body }
  | TypeDef { name; params; ctors; body } ->
    TypeDef { name; params; ctors = List.map (fun (n, p) -> (n, Option.map go p)) ctors; body = go body }
  | EffectDef { name; params; ops; body } ->
    EffectDef { name; params; ops = List.map (fun op -> { op with input = go op.input; output = go op.output }) ops; body = go body }
  | TraitDef { name; params; fields; body } ->
    TraitDef { name; params; fields = List.map (fun (n, e) -> (n, go e)) fields; body = go body }
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
  | Atom _ | Self | SelfType | Import _ -> stx
  | Lam (param, body) ->
    let pname = param_name param in
    let scope = Expand_ctx.extend_at ctx ~name:pname ~base_scope:param.name.scope ~resolved_name:pname in
    let body = expand ctx (add_scope scope body) in
    { stx with kind = Lam (map_param scope (expand ctx) param, body) }
  | Let { name; type_; value; body; recursive } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    let value = if recursive then expand ctx (add_scope scope value) else expand ctx value in
    let body = expand ctx (add_scope scope body) in
    let name = add_id_scope scope name in
    { stx with kind = Let { name; type_ = Option.map (expand ctx) type_; value; body; recursive } }
  | Ap (f, e, a) ->
    { stx with kind = Ap (expand ctx f, e, expand ctx a) }
  | If { cond; then_; else_ } ->
    { stx with kind = If { cond = expand ctx cond; then_ = expand ctx then_; else_ = expand ctx else_ } }
  | Annotated { inner; typ } ->
    { stx with kind = Annotated { inner = expand ctx inner; typ = expand ctx typ } }
  | Prod xs -> { stx with kind = Prod (List.map (expand ctx) xs) }
  | ProdTy xs -> { stx with kind = ProdTy (List.map (expand ctx) xs) }
  | Arrow (expl, name, dom, eff, cod) ->
    { stx with kind = Arrow (expl, name, expand ctx dom, Option.map (fun e -> { effects = List.map (expand ctx) e.effects; tail = Option.map (expand ctx) e.tail }) eff, expand ctx cod) }
  | FieldAccess (e, n) -> { stx with kind = FieldAccess (expand ctx e, n) }
  | Proj (e, n) -> { stx with kind = Proj (expand ctx e, n) }
  | RecordConstruct { typ; fields } ->
    { stx with kind = RecordConstruct { typ = expand ctx typ; fields = List.map (fun (n, e) -> (n, expand ctx e)) fields } }
  | Struct { con_fields; bindings } ->
    { stx with kind = Struct { con_fields = List.map (fun (n, e) -> (n, expand ctx e)) con_fields;
                               bindings = List.map (expand_struct_binding ctx) bindings } }
  | Module { bindings } ->
    { stx with kind = Module { bindings = List.map (expand_struct_binding ctx) bindings } }
  | Open (m, body) ->
    { stx with kind = Open (m, expand ctx body) }
  | RecordTypeDef { name; params; fields; body } ->
    let scope = Expand_ctx.extend ctx ~name ~resolved_name:name in
    let body = expand ctx body in
    { stx with kind = RecordTypeDef { name; params; fields = List.map (fun (n, e) -> (n, expand ctx e)) fields; body = add_scope scope body } }
  | TypeDef { name; params; ctors; body } ->
    let scope = Expand_ctx.extend ctx ~name ~resolved_name:name in
    List.iter (fun (cname, _) -> ignore (Expand_ctx.extend ctx ~name:cname ~resolved_name:cname)) ctors;
    let body = expand ctx body in
    { stx with kind = TypeDef { name; params; ctors = List.map (fun (n, p) -> (n, Option.map (expand ctx) p)) ctors; body = add_scope scope body } }
  | EffectDef { name; params; ops; body } ->
    let scope = Expand_ctx.extend ctx ~name ~resolved_name:name in
    let body = expand ctx body in
    { stx with kind = EffectDef { name; params; ops = List.map (fun op -> { op with input = expand ctx op.input; output = expand ctx op.output }) ops; body = add_scope scope body } }
  | TraitDef { name; params; fields; body } ->
    let scope = Expand_ctx.extend ctx ~name ~resolved_name:name in
    let body = expand ctx body in
    { stx with kind = TraitDef { name; params; fields = List.map (fun (n, e) -> (n, expand ctx e)) fields; body = add_scope scope body } }
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

and expand_struct_binding (ctx : Expand_ctx.t) (binding : Syntax.struct_binding) : Syntax.struct_binding =
  match binding with
  | LetBinding { name; value; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    LetBinding { name = add_id_scope scope name;
                 value = expand ctx value; public }
  | MethodBinding { name; params; body; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    MethodBinding { name = add_id_scope scope name;
                    params = List.map (fun p ->
                        let pname = param_name p in
                        let ps = Expand_ctx.extend_at ctx ~name:pname ~base_scope:p.name.scope ~resolved_name:pname in
                        map_param ps (expand ctx) p) params;
                    body = expand ctx body; public }
  | TypeBinding { name; params; ctors; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    List.iter (fun (cname, _) -> ignore (Expand_ctx.extend ctx ~name:cname ~resolved_name:cname)) ctors;
    TypeBinding { name = add_id_scope scope name;
                  params; ctors = List.map (fun (n, p) -> (n, Option.map (expand ctx) p)) ctors; public }
  | RecordTypeBinding { name; params; fields; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    RecordTypeBinding { name = add_id_scope scope name;
                        params; fields = List.map (fun (n, e) -> (n, expand ctx e)) fields; public }
  | EffectBinding { name; params; ops; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    EffectBinding { name = add_id_scope scope name;
                    params; ops = List.map (fun op -> { op with input = expand ctx op.input; output = expand ctx op.output }) ops; public }
  | TraitBinding { name; params; fields; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    TraitBinding { name = add_id_scope scope name;
                   params; fields = List.map (fun (n, e) -> (n, expand ctx e)) fields; public }
  | ImplBinding { trait_path; trait_name; args; fields; public } ->
    ImplBinding { trait_path; trait_name; args = List.map (expand ctx) args;
                  fields = List.map (fun (n, e) -> (n, expand ctx e)) fields; public }

and expand_match_branch ctx = function
  | ValueBranch (p, body) ->
    let ctx' = Expand_ctx.copy ctx in
    expand_pat_binders ctx' p;
    ValueBranch (expand_pat ctx' p, expand ctx' body)
  | EffectBranch { effect_path; op; arg_pat; body } ->
    let ctx' = Expand_ctx.copy ctx in
    expand_pat_binders ctx' arg_pat;
    EffectBranch { effect_path; op; arg_pat = expand_pat ctx' arg_pat; body = expand ctx' body }

and expand_pat_binders ctx = function
  | PatBind id ->
    let binding_name = id.name in
    ignore (Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:id.scope ~resolved_name:binding_name)
  | PatCon (_, _, ps) -> List.iter (expand_pat_binders ctx) ps
  | PatRecord { fields; _ } ->
    List.iter (fun (_, p) -> Option.iter (expand_pat_binders ctx) p) fields
  | PatStructType { fields; _ } ->
    List.iter (fun (_, p) -> expand_pat_binders ctx p) fields
  | PatOr (l, r) -> expand_pat_binders ctx l; expand_pat_binders ctx r
  | PatProd ps -> List.iter (expand_pat_binders ctx) ps
  | PatAtom _ | PatType _ | PatWild -> ()

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

include Elab_error

let rewrite_record_self_refs record_name params expr =
  let invalid () =
    raise
      (ElabError
         (InvalidRecursiveRecord
            ("recursive record references must be same-instantiation uses of " ^ record_name)))
  in
  let rec collect_apps acc = function
    | Surface.Ap (f, Explicitness.Explicit, a) -> collect_apps (a :: acc) f
    | f -> (f, acc)
  in
  let shadowed bound name = List.exists (String.equal name) bound in
  let is_decl_param bound param arg =
    match arg with Surface.Var name -> String.equal name param && not (shadowed bound name) | _ -> false
  in
  let param_names params = List.map (fun (param : Surface.param) -> param.name) params in
  let rec go bound expr =
    match collect_apps [] expr with
    | Surface.Var name, args when String.equal name record_name && not (shadowed bound name) ->
        if List.length args = List.length params && List.for_all2 (is_decl_param bound) params args then
          Surface.SelfType
        else invalid ()
    | _ -> (
        match expr with
        | Surface.Ap (f, expl, a) -> Surface.Ap (go bound f, expl, go bound a)
        | Surface.Lam (param, body) -> Surface.Lam (param, go (param.name :: bound) body)
        | Surface.Let { name; type_; value; body; recursive } ->
            Surface.Let
              { name;
                type_ = Option.map (go bound) type_;
                value = go bound value;
                body = go (name :: bound) body;
                recursive }
        | Surface.If { cond; then_; else_ } ->
            Surface.If { cond = go bound cond; then_ = go bound then_; else_ = go bound else_ }
        | Surface.Annotated { inner; typ } -> Surface.Annotated { inner = go bound inner; typ = go bound typ }
        | Surface.Prod elems -> Surface.Prod (List.map (go bound) elems)
        | Surface.ProdTy elems -> Surface.ProdTy (List.map (go bound) elems)
        | Surface.Arrow (expl, name, a, effects, b) ->
            let bound' = match name with Some name -> name :: bound | None -> bound in
            let effects =
              Option.map
                (fun (row : Surface.effect_row) -> { Surface.effects = List.map (go bound') row.effects; tail = Option.map (go bound') row.tail })
                effects
            in
            Surface.Arrow (expl, name, go bound a, effects, go bound' b)
        | Surface.FieldAccess (e, name) -> Surface.FieldAccess (go bound e, name)
        | Surface.Proj (e, i) -> Surface.Proj (go bound e, i)
        | Surface.RecordConstruct { typ; fields } ->
            Surface.RecordConstruct
              { typ = go bound typ; fields = List.map (fun (name, value) -> (name, go bound value)) fields }
        | Surface.Module { bindings } ->
            let binding = function
              | Surface.LetBinding { name; value; public } ->
                  Surface.LetBinding { name; value = go bound value; public }
              | Surface.MethodBinding _ -> failwith "module binding cannot be method"
              | Surface.TypeBinding { name; params; ctors; public } ->
                  Surface.TypeBinding
                    { name; params;
                      ctors = List.map (fun (ctor, payloads) -> (ctor, List.map (go (params @ bound)) payloads)) ctors;
                      public }
              | Surface.RecordTypeBinding { name; params; fields; public } ->
                  Surface.RecordTypeBinding
                    { name; params;
                      fields = List.map (fun (field, ty) -> (field, go (params @ bound) ty)) fields;
                      public }
              | Surface.EffectBinding { name; params; ops; public } ->
                  Surface.EffectBinding
                    { name; params;
                      ops = List.map
                        (fun (op : Surface.effect_op) ->
                          { op with input = go (params @ bound) op.input; output = go (params @ bound) op.output })
                        ops;
                      public }
              | Surface.TraitBinding { name; params; fields; public } ->
                  Surface.TraitBinding
                    { name; params;
                      fields = List.map (fun (field, ty) -> (field, go (params @ bound) ty)) fields;
                      public }
                | Surface.ImplBinding { trait_path; trait_name; args; fields; public } ->
                    Surface.ImplBinding
                      { trait_path; trait_name;
                        args = List.map (go bound) args;
                        fields = List.map (fun (field, value) -> (field, go bound value)) fields;
                        public }
                | Surface.MacroBinding { name; value; public } ->
                    Surface.MacroBinding { name; value = go bound value; public }
            in
            Surface.Module { bindings = List.map binding bindings }
        | Surface.Struct { con_fields; bindings } ->
            let binding = function
              | Surface.LetBinding { name; value; public } ->
                  Surface.LetBinding { name; value = go bound value; public }
              | Surface.MethodBinding { name; params; body; public } ->
                  Surface.MethodBinding { name; params; body = go (param_names params @ bound) body; public }
              | Surface.TypeBinding { name; params; ctors; public } ->
                  Surface.TypeBinding
                    { name; params;
                      ctors = List.map (fun (ctor, payloads) -> (ctor, List.map (go (params @ bound)) payloads)) ctors;
                      public }
              | Surface.RecordTypeBinding { name; params; fields; public } ->
                  Surface.RecordTypeBinding
                    { name; params;
                      fields = List.map (fun (field, ty) -> (field, go (params @ bound) ty)) fields;
                      public }
              | Surface.EffectBinding { name; params; ops; public } ->
                  Surface.EffectBinding
                    { name; params;
                      ops = List.map
                        (fun (op : Surface.effect_op) ->
                          { op with input = go (params @ bound) op.input; output = go (params @ bound) op.output })
                        ops;
                      public }
              | Surface.TraitBinding { name; params; fields; public } ->
                  Surface.TraitBinding
                    { name; params;
                      fields = List.map (fun (field, ty) -> (field, go (params @ bound) ty)) fields;
                      public }
                | Surface.ImplBinding { trait_path; trait_name; args; fields; public } ->
                    Surface.ImplBinding
                      { trait_path; trait_name;
                        args = List.map (go bound) args;
                        fields = List.map (fun (field, value) -> (field, go bound value)) fields;
                        public }
                | Surface.MacroBinding { name; value; public } ->
                    Surface.MacroBinding { name; value = go bound value; public }
            in
            Surface.Struct
              { con_fields = List.map (fun (name, ty) -> (name, go bound ty)) con_fields;
                bindings = List.map binding bindings }
        | Surface.Import _ -> expr
        | Surface.Perform { effect_path; op; arg } -> Surface.Perform { effect_path; op; arg = go bound arg }
        | Surface.Resume arg -> Surface.Resume (go bound arg)
        | Surface.RefNew e -> Surface.RefNew (go bound e)
        | Surface.RefGet e -> Surface.RefGet (go bound e)
        | Surface.RefSet (r, e) -> Surface.RefSet (go bound r, go bound e)
        | Surface.Open (name, body) -> Surface.Open (name, go bound body)
        | Surface.RecordTypeDef { name; params; fields; body } ->
            Surface.RecordTypeDef
              { name; params;
                fields = List.map (fun (field, ty) -> (field, go (params @ bound) ty)) fields;
                body = go (name :: bound) body }
        | Surface.TypeDef { name; params; ctors; body } ->
            Surface.TypeDef
              { name; params;
                ctors = List.map (fun (ctor, payloads) -> (ctor, List.map (go (params @ bound)) payloads)) ctors;
                body = go (name :: bound) body }
        | Surface.EffectDef { name; params; ops; body } ->
            Surface.EffectDef
              { name; params;
                ops = List.map
                  (fun (op : Surface.effect_op) ->
                    { op with input = go (params @ bound) op.input; output = go (params @ bound) op.output })
                  ops;
                body = go (name :: bound) body }
        | Surface.TraitDef { name; params; fields; body } ->
            Surface.TraitDef
              { name; params;
                fields = List.map (fun (field, ty) -> (field, go (params @ bound) ty)) fields;
                body = go (name :: bound) body }
        | Surface.ImplDef { trait_path; trait_name; args; fields; body } ->
            Surface.ImplDef
              { trait_path; trait_name;
                args = List.map (go bound) args;
                fields = List.map (fun (field, value) -> (field, go bound value)) fields;
                body = go bound body }
        | Surface.Match (scrutinee, branches) ->
            let go_branch = function
              | Surface.ValueBranch (pat, body) -> Surface.ValueBranch (pat, go bound body)
              | Surface.EffectBranch { effect_path; op; arg_pat; body } ->
                  Surface.EffectBranch { effect_path; op; arg_pat; body = go bound body }
            in
            Surface.Match (go bound scrutinee, List.map go_branch branches)
        | Atom _ | Var _ | Self | SelfType -> expr
        | MacroDef _ | MacroCall _ | SyntaxOperatorUse _ -> failwith "macro-only syntax should not reach elaboration")
  in
  go [] expr

let rec surface_trait_bound_names = function
  | Surface.Var name -> Some [ name ]
  | Surface.Ap (Surface.Ap (Surface.Var "+", Explicitness.Explicit, lhs), Explicitness.Explicit, rhs) -> (
      match surface_trait_bound_names lhs, surface_trait_bound_names rhs with
      | Some lhs, Some rhs -> Some (lhs @ rhs)
      | _ -> None)
  | _ -> None

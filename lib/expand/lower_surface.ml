(** Lower a Syntax.id to a plain string. After expansion, the id's name has
    already been alpha-resolved; we can use it directly as the Var name. *)
let lower_id (id : Syntax.id) : string = id.name

let lower_trait_bound (b : Trait_bound.t) : Trait_bound.t =
  { trait_path = b.trait_path; trait_name = b.trait_name }

let rec lower_param (p : Syntax.param) : Surface.param =
  { name = lower_id p.name;
    type_ = Option.map lower_expr p.type_;
    trait_bounds = List.map lower_trait_bound p.trait_bounds;
    explicitness = p.explicitness }

and lower_effect_row (eff : Syntax.effect_row) : Surface.effect_row =
  { effects = List.map lower_expr eff.effects; tail = Option.map lower_expr eff.tail }

and lower_effect_op (op : Syntax.effect_op) : Surface.effect_op =
  { name = op.name; input = lower_expr op.input; output = lower_expr op.output }

and lower_expr (stx : Syntax.t) : Surface.t =
  match stx.kind with
  | Syntax.Atom a -> Surface.Atom a
  | Syntax.Var id -> Surface.Var (lower_id id)
  | Syntax.Self -> Surface.Self
  | Syntax.SelfType -> Surface.SelfType
  | Syntax.Ap (f, e, a) -> Surface.Ap (lower_expr f, e, lower_expr a)
  | Syntax.Lam (p, body) -> Surface.Lam (lower_param p, lower_expr body)
  | Syntax.Let { name; type_; value; body; recursive } ->
    Surface.Let { name = lower_id name;
                  type_ = Option.map lower_expr type_;
                  value = lower_expr value;
                  body = lower_expr body;
                  recursive }
  | Syntax.If { cond; then_; else_ } ->
    Surface.If { cond = lower_expr cond; then_ = lower_expr then_; else_ = lower_expr else_ }
  | Syntax.Annotated { inner; typ } ->
    Surface.Annotated { inner = lower_expr inner; typ = lower_expr typ }
  | Syntax.Prod xs -> Surface.Prod (List.map lower_expr xs)
  | Syntax.ProdTy xs -> Surface.ProdTy (List.map lower_expr xs)
  | Syntax.Arrow (expl, name, dom, eff, cod) ->
    Surface.Arrow (expl, Option.map lower_id name, lower_expr dom,
                   Option.map lower_effect_row eff,
                   lower_expr cod)
  | Syntax.FieldAccess (e, n) -> Surface.FieldAccess (lower_expr e, n)
  | Syntax.Proj (e, n) -> Surface.Proj (lower_expr e, n)
  | Syntax.RecordConstruct { typ; fields } ->
    Surface.RecordConstruct { typ = lower_expr typ; fields = List.map (fun (n, e) -> (n, lower_expr e)) fields }
  | Syntax.Struct { con_fields; bindings } ->
    Surface.Struct { con_fields = List.map (fun (n, e) -> (n, lower_expr e)) con_fields;
                     bindings = List.map lower_struct_binding bindings }
  | Syntax.Module { bindings } ->
    Surface.Module { bindings = List.map lower_struct_binding bindings }
  | Syntax.Import s -> Surface.Import s
  | Syntax.Open (m, body) -> Surface.Open (lower_id m, lower_expr body)
  | Syntax.RecordTypeDef { name; params; fields; body } ->
    Surface.RecordTypeDef { name = lower_id name; params = List.map lower_id params; fields = List.map (fun (n, e) -> (n, lower_expr e)) fields; body = lower_expr body }
  | Syntax.TypeDef { name; params; ctors; body } ->
    Surface.TypeDef { name = lower_id name; params = List.map lower_id params; ctors = List.map (fun (n, ps) -> (lower_id n, List.map lower_expr ps)) ctors; body = lower_expr body }
  | Syntax.EffectDef { name; params; ops; body } ->
    Surface.EffectDef { name = lower_id name; params = List.map lower_id params; ops = List.map lower_effect_op ops; body = lower_expr body }
  | Syntax.TraitDef { name; params; fields; body } ->
    Surface.TraitDef { name = lower_id name; params = List.map lower_id params; fields = List.map (fun (n, e) -> (n, lower_expr e)) fields; body = lower_expr body }
  | Syntax.ImplDef { trait_path; trait_name; args; fields; body } ->
    Surface.ImplDef { trait_path; trait_name; args = List.map lower_expr args;
                      fields = List.map (fun (n, e) -> (n, lower_expr e)) fields; body = lower_expr body }
  | Syntax.Perform { effect_path; op; arg } ->
    Surface.Perform { effect_path; op; arg = lower_expr arg }
  | Syntax.Resume e -> Surface.Resume (lower_expr e)
  | Syntax.RefNew e -> Surface.RefNew (lower_expr e)
  | Syntax.RefGet e -> Surface.RefGet (lower_expr e)
  | Syntax.RefSet (l, r) -> Surface.RefSet (lower_expr l, lower_expr r)
  | Syntax.Match (scrut, brs) ->
    Surface.Match (lower_expr scrut, List.map lower_match_branch brs)
  | Syntax.MacroDef _ | Syntax.MacroCall _ | Syntax.SyntaxOperatorUse _ ->
    failwith "macro-only syntax should have been expanded away before lowering"

and lower_struct_binding = function
  | Syntax.LetBinding { name; value; public } ->
    Surface.LetBinding { name = lower_id name; value = lower_expr value; public }
  | Syntax.MethodBinding { name; params; body; public } ->
    Surface.MethodBinding { name = lower_id name; params = List.map lower_param params; body = lower_expr body; public }
  | Syntax.TypeBinding { name; params; ctors; public } ->
    Surface.TypeBinding { name = lower_id name; params = List.map lower_id params; ctors = List.map (fun (n, ps) -> (lower_id n, List.map lower_expr ps)) ctors; public }
  | Syntax.RecordTypeBinding { name; params; fields; public } ->
    Surface.RecordTypeBinding { name = lower_id name; params = List.map lower_id params; fields = List.map (fun (n, e) -> (n, lower_expr e)) fields; public }
  | Syntax.EffectBinding { name; params; ops; public } ->
    Surface.EffectBinding { name = lower_id name; params = List.map lower_id params; ops = List.map lower_effect_op ops; public }
  | Syntax.TraitBinding { name; params; fields; public } ->
    Surface.TraitBinding { name = lower_id name; params = List.map lower_id params; fields = List.map (fun (n, e) -> (n, lower_expr e)) fields; public }
  | Syntax.ImplBinding { trait_path; trait_name; args; fields; public } ->
    Surface.ImplBinding { trait_path; trait_name; args = List.map lower_expr args;
                          fields = List.map (fun (n, e) -> (n, lower_expr e)) fields; public }
  | Syntax.MacroBinding { name; value; public } ->
    Surface.MacroBinding { name = lower_id name; value = lower_expr value; public }

and lower_match_branch = function
  | Syntax.ValueBranch (p, body) -> Surface.ValueBranch (lower_pat p, lower_expr body)
  | Syntax.EffectBranch { effect_path; op; arg_pat; body } ->
    Surface.EffectBranch { effect_path; op; arg_pat = lower_pat arg_pat; body = lower_expr body }

and lower_pat = function
  | Syntax.PatCon (path, name, ps) -> Surface.PatCon (path, name, List.map lower_pat ps)
  | Syntax.PatRecord { typ_path; typ; fields; partial } ->
    Surface.PatRecord { typ_path; typ; fields = List.map (fun (n, p) -> (n, Option.map lower_pat p)) fields; partial }
  | Syntax.PatStructType { fields; partial } ->
    Surface.PatStructType { fields = List.map (fun (n, p) -> (n, lower_pat p)) fields; partial }
  | Syntax.PatOr (l, r) -> Surface.PatOr (lower_pat l, lower_pat r)
  | Syntax.PatProd ps -> Surface.PatProd (List.map lower_pat ps)
  | Syntax.PatAtom a -> Surface.PatAtom a
  | Syntax.PatType ty -> Surface.PatType ty
  | Syntax.PatWild -> Surface.PatWild
  | Syntax.PatBind id -> Surface.PatBind (lower_id id)

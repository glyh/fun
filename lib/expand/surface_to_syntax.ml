let span = Source_span.synthetic

let id name = Syntax.fresh_id name

let rec explicitness (e : Explicitness.t) = e

and trait_bound (b : Trait_bound.t) : Trait_bound.t =
  { trait_path = b.trait_path; trait_name = b.trait_name }

and param (p : Surface.param) : Syntax.param =
  { name = id p.name;
    type_ = Option.map expr p.type_;
    trait_bounds = List.map trait_bound p.trait_bounds;
    explicitness = explicitness p.explicitness }

and effect_op (op : Surface.effect_op) : Syntax.effect_op =
  { name = op.name; input = expr op.input; output = expr op.output }

and effect_row (row : Surface.effect_row) : Syntax.effect_row =
  { effects = List.map expr row.effects; tail = Option.map expr row.tail }

and expr (e : Surface.t) : Syntax.t =
  let kind =
    match e with
    | Atom a -> Syntax.Atom a
    | Var name -> Syntax.Var (id name)
    | Self -> Syntax.Self
    | SelfType -> Syntax.SelfType
    | Ap (f, expl, a) -> Syntax.Ap (expr f, explicitness expl, expr a)
    | Lam (p, body) -> Syntax.Lam (param p, expr body)
    | Let { name; type_; value; body; recursive } ->
      Syntax.Let { name = id name; type_ = Option.map expr type_; value = expr value; body = expr body; recursive }
    | If { cond; then_; else_ } ->
      Syntax.If { cond = expr cond; then_ = expr then_; else_ = expr else_ }
    | Annotated { inner; typ } ->
      Syntax.Annotated { inner = expr inner; typ = expr typ }
    | Prod xs -> Syntax.Prod (List.map expr xs)
    | ProdTy xs -> Syntax.ProdTy (List.map expr xs)
    | Arrow (expl, name, domain, effects, codomain) ->
      Syntax.Arrow (explicitness expl, Option.map id name, expr domain, Option.map effect_row effects, expr codomain)
    | FieldAccess (inner, field) -> Syntax.FieldAccess (expr inner, field)
    | Proj (inner, index) -> Syntax.Proj (expr inner, index)
    | RecordConstruct { typ; fields } ->
      Syntax.RecordConstruct { typ = expr typ; fields = List.map (fun (name, value) -> (name, expr value)) fields }
    | Struct { con_fields; bindings } ->
      Syntax.Struct { con_fields = List.map (fun (name, typ) -> (name, expr typ)) con_fields;
                      bindings = List.map struct_binding bindings }
    | Module { bindings } ->
      Syntax.Module { bindings = List.map struct_binding bindings }
    | Import path -> Syntax.Import path
    | Open (name, body) -> Syntax.Open (id name, expr body)
    | RecordTypeDef { name; params; fields; body } ->
      Syntax.RecordTypeDef { name = id name; params = List.map id params; fields = List.map (fun (name, typ) -> (name, expr typ)) fields; body = expr body }
    | TypeDef { name; params; ctors; body } ->
      Syntax.TypeDef { name = id name; params = List.map id params; ctors = List.map (fun (name, payloads) -> (id name, List.map expr payloads)) ctors; body = expr body }
    | EffectDef { name; params; ops; body } ->
      Syntax.EffectDef { name = id name; params = List.map id params; ops = List.map effect_op ops; body = expr body }
    | TraitDef { name; params; fields; body } ->
      Syntax.TraitDef { name = id name; params = List.map id params; fields = List.map (fun (name, typ) -> (name, expr typ)) fields; body = expr body }
    | ImplDef { trait_path; trait_name; args; fields; body } ->
      Syntax.ImplDef { trait_path; trait_name; args = List.map expr args;
                       fields = List.map (fun (name, value) -> (name, expr value)) fields; body = expr body }
    | Perform { effect_path; op; arg } ->
      Syntax.Perform { effect_path; op; arg = expr arg }
    | Resume inner -> Syntax.Resume (expr inner)
    | RefNew inner -> Syntax.RefNew (expr inner)
    | RefGet inner -> Syntax.RefGet (expr inner)
    | RefSet (lhs, rhs) -> Syntax.RefSet (expr lhs, expr rhs)
    | Match (scrut, branches) ->
      Syntax.Match (expr scrut, List.map match_branch branches)
    | MacroDef { name; value; body; _ } ->
      Syntax.MacroDef { name = id name; value = expr value; body = expr body; kind = None }
    | MacroCall (f, a) ->
      Syntax.MacroCall (expr f, List.map (expr) a)
    | SyntaxOperatorUse { operator; fixity; operands; declaration_span; use_span } ->
      let fixity = match fixity with Surface.PrefixOp -> Syntax.PrefixOp | Surface.InfixOp -> Syntax.InfixOp in
      Syntax.SyntaxOperatorUse { operator = id operator; fixity; operands = List.map expr operands; declaration_span; use_span }
  in
  { Syntax.kind; span }

and struct_binding = function
  | Surface.LetBinding { name; value; public; recursive } ->
    Syntax.LetBinding { name = id name; value = expr value; public; recursive }
  | Surface.MethodBinding { name; params; body; public } ->
    Syntax.MethodBinding { name = id name; params = List.map param params; body = expr body; public }
  | Surface.TypeBinding { name; params; ctors; public } ->
    Syntax.TypeBinding { name = id name; params = List.map id params; ctors = List.map (fun (name, payloads) -> (id name, List.map expr payloads)) ctors; public }
  | Surface.RecordTypeBinding { name; params; fields; public } ->
    Syntax.RecordTypeBinding { name = id name; params = List.map id params; fields = List.map (fun (name, typ) -> (name, expr typ)) fields; public }
  | Surface.EffectBinding { name; params; ops; public } ->
    Syntax.EffectBinding { name = id name; params = List.map id params; ops = List.map effect_op ops; public }
  | Surface.TraitBinding { name; params; fields; public } ->
    Syntax.TraitBinding { name = id name; params = List.map id params; fields = List.map (fun (name, typ) -> (name, expr typ)) fields; public }
  | Surface.ImplBinding { trait_path; trait_name; args; fields; public } ->
    Syntax.ImplBinding { trait_path; trait_name; args = List.map expr args;
                         fields = List.map (fun (name, value) -> (name, expr value)) fields; public }
  | Surface.MacroBinding { name; value; public; _ } ->
    Syntax.MacroBinding { name = id name; value = expr value; public; kind = None }
  | Surface.MacroCallBinding { f; args } ->
    Syntax.MacroCallBinding { f = expr f; args = List.map expr args }
  | Surface.PatternSynBinding { name; params; rhs; public } ->
    Syntax.PatternSynBinding { name = id name; params = List.map id params;
                               rhs = pat_ rhs; public }

and match_branch = function
  | Surface.ValueBranch (pat, body) -> Syntax.ValueBranch (pat_ pat, expr body)
  | Surface.EffectBranch { effect_path; op; arg_pat; body } ->
    Syntax.EffectBranch { effect_path; op; arg_pat = pat_ arg_pat; body = expr body }

and pat_ = function
  | Surface.PatCon (path, name, args) -> Syntax.PatCon (path, name, List.map pat_ args)
  | Surface.PatRecord { typ_path; typ; fields; partial } ->
    Syntax.PatRecord { typ_path; typ; fields = List.map (fun (name, pat) -> (name, Option.map pat_ pat)) fields; partial }
  | Surface.PatStructType { fields; partial } ->
    Syntax.PatStructType { fields = List.map (fun (name, pat) -> (name, pat_ pat)) fields; partial }
  | Surface.PatOr (lhs, rhs) -> Syntax.PatOr (pat_ lhs, pat_ rhs)
  | Surface.PatProd pats -> Syntax.PatProd (List.map pat_ pats)
  | Surface.PatAtom atom -> Syntax.PatAtom atom
  | Surface.PatType atom_ty -> Syntax.PatType atom_ty
  | Surface.PatWild -> Syntax.PatWild
  | Surface.PatBind name -> Syntax.PatBind (id name)

(** Stage 4: Semantic macro annotation resolver.

    Resolves [Syntax.MacroAnnotation.t] to [Syntax.MacroKind.t] by
    consulting the elaboration context's type namespace. Stage 7 adds
    per-binding semantic advancement so the caller can update the
    context between bindings.

    The resolver replaces the parser's [Syntax.MacroAnnotationAdapter] for
    the driver path: instead of hardcoded uppercase → binder heuristics,
    it checks whether a name resolves in the current context. If the name
    is already in scope (any kind — type, record, effect, etc.), it is
    treated as a constraint; unbound uppercase names become binders. *)

(** Build a [Syntax.R] type reference, used as the type annotation on
    synthesized implicit binder params. *)
let r_type () : Syntax.t =
  let syntax_var =
    { Syntax.kind = Var { name = Compiler_names.Module_name.syntax;
                          span = Source_span.synthetic;
                          scope = Scope_set.empty };
      span = Source_span.synthetic }
  in
  { Syntax.kind = FieldAccess (syntax_var, Compiler_names.Syntax_name.r);
    span = Source_span.synthetic }

(** Synthesize an implicit [Syntax.R]-typed binder parameter for the given
    [name]. *)
let synthesize_binder_param (name : string) : Syntax.param option =
  let tp = { Syntax.name; span = Source_span.synthetic; scope = Scope_set.empty } in
  let type_ty = r_type () in
  Some { Syntax.name = tp; explicitness = Explicitness.Implicit;
         type_ = Some type_ty; trait_bounds = [] }

(** Check whether [name] is bound to a type-like value in [ctx].
    Resolves the name, evaluates it, and checks with
    [Elab_validate.is_type_like_value].  This correctly: includes
    ADT types ([VNominal]), record types ([VStruct]/[VPi]), and
    built-in types ([VAtomTy]); excludes value bindings (functions,
    constructors like [Some] / [None] / [Cons], etc.).
    [is_type_like_value] also matches [VEffect] — obscure but
    harmless: you wouldn't normally constrain a macro by an effect
    type. *)
let is_known_type (ctx : Elab_ctx.Ctx.t) (name : string) : bool =
  match Elab_resolve.resolve_path_value_opt ctx [] name with
  | Some (v, _ty) ->
      Elab_validate.is_type_like_value ctx v
  | None -> false

(** Semantic resolution of a macro annotation.

    Returns [(MacroKind.t, param option)] where:
    - [MacroKind.Decl] with no param for [Decl]
    - [MacroKind.Expr (None, Some name)] with no param for a constraint
      (name resolves to a type in the semantic context)
    - [MacroKind.Expr (Some name, None)] with synthesized implicit param
      for an unresolved leading-uppercase name (binder mode)
    - [MacroKind.Expr (None, None)] with no param for [_], lowercase,
      or unannotated annotations

    The implicit param (when returned) must be used to construct the macro
    lambda value; the caller is responsible for NOT double-inserting it if
    the parser already synthesized one. *)
let resolve_kind (ctx : Elab_ctx.Ctx.t) (ann : Syntax.MacroAnnotation.t)
    : Syntax.MacroKind.t * Syntax.param option =
  let is_upper c = c >= 'A' && c <= 'Z' in
  let is_binder_name n = String.length n > 0 && is_upper n.[0] in
  match ann with
  | Syntax.MacroAnnotation.Decl ->
      (Syntax.MacroKind.Decl, None)
  | Syntax.MacroAnnotation.LegacyExprBinder name ->
      (Syntax.MacroKind.(Expr (Some name, None)), synthesize_binder_param name)
  | Syntax.MacroAnnotation.Expr (Some arg) -> (
      let name = Syntax.MacroAnnotation.arg_name arg in
      if String.equal name "_" then
        (Syntax.MacroKind.(Expr (None, None)), None)
      else if is_known_type ctx name then
        (Syntax.MacroKind.(Expr (None, Some name)), None)
      else if is_binder_name name then
        (Syntax.MacroKind.(Expr (Some name, None)), synthesize_binder_param name)
      else
        (Syntax.MacroKind.(Expr (None, None)), None))
  | Syntax.MacroAnnotation.Expr None ->
      (Syntax.MacroKind.(Expr (None, None)), None)

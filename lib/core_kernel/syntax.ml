module MacroKind = struct
  type t = Expr of string option * string option | Decl
  let default = Expr (None, None)
  let to_string = function Expr _ -> "Expr" | Decl -> "Decl"
  let of_string = function "Decl" -> Some Decl | "Expr" -> Some (Expr (None, None)) | _ -> None
  let has_type_binding = function Expr (Some _, _) -> true | _ -> false
  let type_binding_name = function Expr (Some n, _) -> Some n | _ -> None
  let type_constraint_name = function Expr (_, Some n) -> Some n | _ -> None
end

(** Unresolved macro annotation as parsed from source syntax.
    Records what was written, not semantic meaning. *)
module MacroAnnotation = struct
  type arg = Wildcard | Named of string
  type t = Expr of arg option | LegacyExprBinder of string | Decl
  let default = Expr None
  let to_string = function Expr _ | LegacyExprBinder _ -> "Expr" | Decl -> "Decl"
  let of_string = function "Decl" -> Some Decl | "Expr" -> Some (Expr None) | _ -> None
  let arg_name = function Wildcard -> "_" | Named s -> s
  let is_decl = function Decl -> true | Expr _ | LegacyExprBinder _ -> false
  let is_expr = function Expr _ | LegacyExprBinder _ -> true | Decl -> false
end



type id = {
  name : string;
  span : Source_span.t;
  scope : Scope_set.t;
}

type param = {
  name : id;
  type_ : t option;
  trait_bounds : Trait_bound.t list;
  explicitness : Explicitness.t;
}

and effect_op = { name : string; input : t; output : t }

and effect_row = { effects : t list; tail : t option }

and struct_binding =
  | LetBinding of { name : id; value : t; public : bool; recursive : bool }
  | MethodBinding of { name : id; params : param list; body : t; public : bool }
  | TypeBinding of {
      name : id;
      params : id list;
      ctors : (id * t list) list;
      public : bool;
    }
  | RecordTypeBinding of {
      name : id;
      params : id list;
      fields : (string * t) list;
      public : bool;
    }
  | EffectBinding of {
      name : id;
      params : id list;
      ops : effect_op list;
      public : bool;
    }
  | TraitBinding of {
      name : id;
      params : id list;
      fields : (string * t) list;
      public : bool;
    }
  | ImplBinding of {
      trait_path : string list;
      trait_name : string;
      args : t list;
      fields : (string * t) list;
      public : bool;
    }
  | MacroBinding of { name : id; value : t; public : bool; kind : MacroAnnotation.t option }
  | MacroCallBinding of { f : t; args : t list }
  | PatternSynBinding of { name : id; params : id list; rhs : pat; public : bool }

and t = {
  kind : kind;
  span : Source_span.t;
}

and kind =
  | Atom of Atom.t
  | Var of id
  | Self
  | SelfType
  | Ap of t * Explicitness.t * t
  | Lam of param * t
  | Let of { name : id; type_ : t option; value : t; body : t; recursive : bool }
  | If of { cond : t; then_ : t; else_ : t }
  | Annotated of { inner : t; typ : t }
  | Prod of t list
  | ProdTy of t list
  | Arrow of Explicitness.t * id option * t * effect_row option * t
  | FieldAccess of t * string
  | Proj of t * int
  | RecordConstruct of { typ : t; fields : (string * t) list }
  | Struct of {
      con_fields : (string * t) list;
      bindings : struct_binding list;
    }
  | Module of { bindings : struct_binding list }
  | Import of string
  | Open of id * t
  | RecordTypeDef of {
      name : id;
      params : id list;
      fields : (string * t) list;
      body : t;
    }
  | TypeDef of {
      name : id;
      params : id list;
      ctors : (id * t list) list;
      body : t;
    }
  | EffectDef of {
      name : id;
      params : id list;
      ops : effect_op list;
      body : t;
    }
  | TraitDef of {
      name : id;
      params : id list;
      fields : (string * t) list;
      body : t;
    }
  | ImplDef of {
      trait_path : string list;
      trait_name : string;
      args : t list;
      fields : (string * t) list;
      body : t;
    }
  | Perform of { effect_path : string list; op : string; arg : t }
  | Resume of t
  | RefNew of t
  | RefGet of t
  | RefSet of t * t
  | Match of t * match_branch list
  | Stx of t  (* opaque syntax wrapper *)
  | MacroDef of { name : id; value : t; body : t; kind : MacroAnnotation.t option }
  | MacroCall of t * t list
  | SyntaxOperatorUse of {
      operator : id;
      fixity : operator_fixity;
      operands : t list;
      declaration_span : Source_span.t;
      use_span : Source_span.t;
    }

and operator_fixity = PrefixOp | InfixOp

and match_branch =
  | ValueBranch of pat * t
  | EffectBranch of {
      effect_path : string list;
      op : string;
      arg_pat : pat;
      body : t;
    }

and pat =
  | PatCon of string list * string * pat list
  | PatRecord of { typ_path : string list; typ : string; fields : (string * pat option) list; partial : bool }
  | PatStructType of { fields : (string * pat) list; partial : bool }
  | PatOr of pat * pat
  | PatProd of pat list
  | PatAtom of Atom.t
  | PatType of Atom_ty.t
  | PatWild
  | PatBind of id

let fresh_id ?(span = Source_span.synthetic) ?(scope = Scope_set.empty) name =
  { name; span; scope }

(** STAGE 2: Uniform binder-only resolution. All leading-uppercase names
    produce [Expr(Some name, None)] + synthesized implicit param. No
    type-constraint path exists yet — that is deferred to Stage 4 (semantic
    driver with type-namespace resolution).

    Contract:
    - [_] (Wildcard) → unconstrained Expr, no binder
    - Leading-uppercase name → binder with synthesized implicit param
    - Lowercase/non-binder name → unconstrained Expr, no binder
    - [LegacyExprBinder] → unchanged (pre-Stage-1 compat)
    - [Decl] → unchanged *)
module MacroAnnotationAdapter = struct
  let r_type () =
    let syntax_var =
      { kind = Var { name = Compiler_names.Module_name.syntax; span = Source_span.synthetic; scope = Scope_set.empty };
        span = Source_span.synthetic }
    in
    { kind = FieldAccess (syntax_var, Compiler_names.Syntax_name.r);
      span = Source_span.synthetic }

  let synthesize_binder_param name =
    let tp = { name; span = Source_span.synthetic; scope = Scope_set.empty } in
    let type_ty = r_type () in
    Some { name = tp; explicitness = Explicitness.Implicit; type_ = Some type_ty; trait_bounds = [] }

  let resolve (ann : MacroAnnotation.t) : MacroKind.t * param option =
    let is_upper c = c >= 'A' && c <= 'Z' in
    let is_binder_name n = String.length n > 0 && is_upper n.[0] in
    match ann with
    | MacroAnnotation.Decl -> (MacroKind.Decl, None)
    | MacroAnnotation.LegacyExprBinder name ->
        (MacroKind.(Expr (Some name, None)), synthesize_binder_param name)
    | MacroAnnotation.Expr (Some arg) -> (
        let name = MacroAnnotation.arg_name arg in
        if String.equal name "_" then
          (MacroKind.(Expr (None, None)), None)
        else if is_binder_name name then
          (MacroKind.(Expr (Some name, None)), synthesize_binder_param name)
        else
          (MacroKind.(Expr (None, None)), None))
    | MacroAnnotation.Expr None ->
        (MacroKind.Expr (None, None), None)

  let resolve_kind_only ann = fst (resolve ann)
  let resolve_param ann = snd (resolve ann)
end

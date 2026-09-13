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
  type arg = Wildcard | Named of string | Qualified of string list * string
  type t = Expr of arg option | LegacyExprBinder of string | Decl
  let default = Expr None
  let to_string = function Expr _ | LegacyExprBinder _ -> "Expr" | Decl -> "Decl"
  let of_string = function "Decl" -> Some Decl | "Expr" -> Some (Expr None) | _ -> None
  let arg_name = function
    | Wildcard -> "_"
    | Named s -> s
    | Qualified (path, last) -> String.concat "." (path @ [ last ])
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
  trait_bounds : path list;
  explicitness : Explicitness.t;
}

and effect_op = { name : string; input : t; output : t }

(** A dotted name, [M.N.x]: its head is a bare name - an id, resolved by scope
    set and renamed like any other occurrence - and the rest are member labels,
    resolved by their container. A single name is a path with no members. *)
and path = { head : id; members : string list }

and type_decl = { name : id; params : id list; ctors : (id * t list) list }

and effect_row = { effects : t list; tail : t option }

and struct_binding =
  | LetBinding of { name : id; value : t; public : bool; recursive : bool }
  | MethodBinding of { name : id; params : param list; body : t; public : bool }
  | TypeBinding of { members : type_decl list; public : bool }
      (** [type A = … and B = …]: one binding per chain, its members mutually
          recursive; a single declaration is the one-member chain. *)
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
      name : id option;   (* [impl NAME : Trait(Args) = …] - see impl-visibility *)
      trait : path;
      args : t list;
      fields : (string * t) list;
      public : bool;
    }
  | MacroBinding of { name : id; value : t; public : bool; kind : MacroAnnotation.t option }
  | MacroCallBinding of { f : t; args : t list }
  | PatternSynBinding of { name : id; params : id list; rhs : pat; public : bool }
  | OpenBinding of t * string
      (** [open <module-expr>] at module/struct top level — the binding-list
          counterpart of the expression-level [Open]. Scopes over the subsequent
          bindings only. The string is the open's label (see [Open]). *)

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
  | Open of t * t * string
      (** [open m; body]. The string labels this open, so an open choice can
          name it: [""] until expansion assigns one - ["unit:p"] for an open of
          [import "p"], ["open:n"] otherwise. *)
  | OpenChoice of { name : id; opens : string list; fallback : string option }
      (** Produced only by expansion: a bare name some open may supply. [opens]
          are the candidate opens' labels, innermost first; [fallback] is the
          resolved name of the binder they shadow, if any. With no opens and no
          binder it names only the base context. *)
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
      name : id option;
      trait : path;
      args : t list;
      fields : (string * t) list;
      body : t;
    }
  | Perform of { op : path; arg : t }
  | Resume of t
  | RefNew of t
  | RefGet of t
  | RefSet of t * t
  | Match of t * match_branch list
  | Stx of t  (* opaque syntax wrapper *)
  | Quote of { template : t; holes : (string * t) list }
      (** [quote(…)]: syntax written literally in a macro body. Each hole [$x]
          stands in [template] as an id spelled ["$x"] - [$] cannot begin a
          source identifier - and in [holes] as the ordinary reference [x]. *)
  | MacroDef of { name : id; value : t; body : t; kind : MacroAnnotation.t option }
  | MacroCall of t * t list
  | SyntaxOperatorUse of {
      operator : id;
      fixity : operator_fixity;
      operands : t list;
      declaration_span : Source_span.t;
      use_span : Source_span.t;
      (* The unit that supplied this operator, so its macro body is looked up in
         the same declaration the fixity came from. [None] when declared here. *)
      unit : string option;
    }

and operator_fixity = PrefixOp | InfixOp

and match_branch =
  | ValueBranch of pat * t
  | EffectBranch of {
      op : path;
      arg_pat : pat;
      body : t;
    }

and pat =
  | PatCon of path * pat list
  | PatRecord of { typ : path; fields : (string * pat option) list; partial : bool }
  | PatStructType of { fields : (string * pat) list; partial : bool }
  | PatOr of pat * pat
  | PatProd of pat list
  | PatAtom of Atom.t
  | PatType of Atom_ty.t
  | PatWild
  | PatBind of id

let fresh_id ?(span = Source_span.synthetic) ?(scope = Scope_set.empty) name =
  { name; span; scope }

let path_of_id head = { head; members = [] }

(* [(M.N, x)] for [M.N.x]: the prefix and the last segment, as the string lists
   everything after lowering speaks. *)
let path_split (p : path) : string list * string =
  match List.rev (p.head.name :: p.members) with
  | last :: rev_prefix -> (List.rev rev_prefix, last)
  | [] -> assert false

let path_of_segments ?span = function
  | head :: members -> { head = fresh_id ?span head; members }
  | [] -> invalid_arg "path_of_segments: empty path"

let path_last (p : path) = snd (path_split p)

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
    | MacroAnnotation.Expr (Some (MacroAnnotation.Qualified _ as arg)) ->
        (* Qualified names cannot bind; without a semantic context assume
           a constraint and let the driver resolver refine it. *)
        (MacroKind.(Expr (None, Some (MacroAnnotation.arg_name arg))), None)
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

(* A form the compiler writes itself, with no source position. *)
let synth kind = { kind; span = Source_span.synthetic }

let names (ids : id list) = List.map (fun (i : id) -> i.name) ids

(* The name a bare-name form was written with, whether expansion resolved it to
   a binder or left it an open choice. For the lookups still keyed by spelling:
   traits, and the trait-bound sugar [A : Eq + Show]. *)
let written_name (stx : t) =
  match stx.kind with
  | Var id -> Some id.name
  | OpenChoice { name; _ } -> Some name.name
  | _ -> None

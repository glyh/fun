(* The syntax tests' assertion view: an expanded program with spans, scope sets
   and resolution erased - the shape a parse produced, by name. It is the tree
   the elaborator used to consume before it read [Syntax.t] directly
   (delete-surface-ir); it survives only here, where naming the shape is the
   point of the test. *)


type param = {
  name : string;
  type_ : t option;
  trait_bounds : Trait_bound.t list;
  explicitness : Explicitness.t;
}

and effect_op = { name : string; input : t; output : t }

and type_decl = { name : string; params : string list; ctors : (string * t list) list }

and effect_row = { effects : t list; tail : t option }

and struct_binding =
  | LetBinding of { name : string; value : t; public : bool; recursive : bool }
  | MethodBinding of { name : string; params : param list; body : t; public : bool }
  | TypeBinding of { members : type_decl list; public : bool }
  | RecordTypeBinding of {
      name : string;
      params : string list;
      fields : (string * t) list;
      public : bool;
    }
  | EffectBinding of {
      name : string;
      params : string list;
      ops : effect_op list;
      public : bool;
    }
  | TraitBinding of {
      name : string;
      params : string list;
      fields : (string * t) list;
      public : bool;
    }
  | ImplBinding of {
      name : string option;
      trait_path : string list;
      trait_name : string;
      args : t list;
      fields : (string * t) list;
      public : bool;
    }
  | MacroBinding of { name : string; value : t; public : bool; kind : Syntax.MacroAnnotation.t option }
  | MacroCallBinding of { f : t; args : t list }
  | PatternSynBinding of { name : string; params : string list; rhs : pat; public : bool }
  | SyntaxBinding of { name : string; attaches : bool }
  | HoleBinding of string
  | OpenBinding of t * string
      (** [open <module-expr>] at module/struct top level. Brings the module's
          public fields into scope for the *subsequent* bindings only (statement
          order), and contributes no field of its own. In a [Struct] it scopes
          over later bindings but not over [con_fields]: record field types are
          elaborated as a group before the binding list. *)

and t =
  | Atom of Atom.t
  | Var of string
  | Self
  | SelfType
  | Ap of t * Explicitness.t * t
  | Lam of param * t
  | Let of { name : string; type_ : t option; value : t; body : t; recursive : bool }
  | Annotated of { inner : t; typ : t }
  | Prod of t list
  | ProdTy of t list
  | Arrow of Explicitness.t * string option * t * effect_row option * t
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
  | OpenChoice of { name : string; opens : string list; fallback : string option }
  | RecordTypeDef of {
      name : string;
      params : string list;
      fields : (string * t) list;
      body : t;
    }
  | TypeDef of {
      name : string;
      params : string list;
      ctors : (string * t list) list;  (* (ctor_name, payload_types) *)
      body : t;
    }
  | EffectDef of {
      name : string;
      params : string list;
      ops : effect_op list;
      body : t;
    }
  | TraitDef of {
      name : string;
      params : string list;
      fields : (string * t) list;
      body : t;
    }
  | ImplDef of {
      name : string option;
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
  | StxExpr of Syntax.t
  | Quote of { template : Syntax.t; holes : (string * t) list } (* opaque syntax wrapper — survives lowering intact *)
  | QuoteDecls of { items : Syntax.struct_binding list; holes : (string * t) list }
  | Match of t * match_branch list  (* match scrutinee | pat -> body ... end *)
  | MacroDef of { name : string; value : t; body : t; kind : Syntax.MacroAnnotation.t option }
  | SyntaxDef of { name : string; attaches : bool; body : t }
  | MacroCall of t * t list
  | SyntaxOperatorUse of {
      operator : string;
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
  | PatCon of string list * string * pat list   (* path, constructor, subpatterns *)
  | PatRecord of { typ_path : string list; typ : string; fields : (string * pat option) list; partial : bool }
  | PatStructType of { fields : (string * pat) list; partial : bool }
  | PatOr of pat * pat
  | PatProd of pat list
  | PatAtom of Atom.t
  | PatType of Atom_ty.t
  | PatWild                       (* _ *)
  | PatBind of string             (* variable binding *)

(* The name a bare-name form was written with, whether expansion resolved it to
   a binder or left it an open choice. For the lookups still keyed by spelling:
   traits, and the trait-bound sugar [A : Eq + Show]. *)
let written_name = function
  | Var n -> Some n
  | OpenChoice { name; _ } -> Some name
  | _ -> None

(* ---- from Syntax ---- *)

(** Lower a Syntax.id to a plain string. After expansion, the id's name has
    already been alpha-resolved; we can use it directly as the Var name. *)
let lower_id (id : Syntax.id) : string = id.name

let lower_trait_bound (b : Syntax.path) : Trait_bound.t =
  let trait_path, trait_name = Syntax.path_split b in
  { trait_path; trait_name }

let rec lower_param (p : Syntax.param) : param =
  { name = lower_id p.name;
    type_ = Option.map lower_expr p.type_;
    trait_bounds = List.map lower_trait_bound p.trait_bounds;
    explicitness = p.explicitness }

and lower_effect_row (eff : Syntax.effect_row) : effect_row =
  { effects = List.map lower_expr eff.effects; tail = Option.map lower_expr eff.tail }

and lower_effect_op (op : Syntax.effect_op) : effect_op =
  { name = op.name; input = lower_expr op.input; output = lower_expr op.output }

and lower_capture = function
  | Syntax.CapExpr e -> lower_expr e
  | CapBlock _ | CapId _ | CapPattern _ | CapDecls _ -> invalid_arg "lower_capture: an argument not read as an Expr"

and lower_expr (stx : Syntax.t) : t =
  match stx.kind with
  | Syntax.Stx s -> StxExpr s
  | Syntax.Quote { template; holes } ->
    Quote { template; holes = List.map (fun (n, h) -> (n, lower_expr h)) holes }
  | Syntax.QuoteDecls { items; holes } ->
    QuoteDecls { items; holes = List.map (fun (n, h) -> (n, lower_expr h)) holes }
  | Syntax.Atom a -> Atom a
  | Syntax.Var id -> Var (lower_id id)
  | Syntax.Self -> Self
  | Syntax.SelfType -> SelfType
  | Syntax.Ap (f, e, a) -> Ap (lower_expr f, e, lower_expr a)
  | Syntax.Lam (p, body) -> Lam (lower_param p, lower_expr body)
  | Syntax.Let { name; type_; value; body; recursive } ->
    Let { name = lower_id name;
                  type_ = Option.map lower_expr type_;
                  value = lower_expr value;
                  body = lower_expr body;
                  recursive }
  | Syntax.Annotated { inner; typ } ->
    Annotated { inner = lower_expr inner; typ = lower_expr typ }
  | Syntax.Prod xs -> Prod (List.map lower_expr xs)
  | Syntax.ProdTy xs -> ProdTy (List.map lower_expr xs)
  | Syntax.Arrow (expl, name, dom, eff, cod) ->
    Arrow (expl, Option.map lower_id name, lower_expr dom,
                   Option.map lower_effect_row eff,
                   lower_expr cod)
  | Syntax.FieldAccess (e, n) -> FieldAccess (lower_expr e, n)
  | Syntax.Proj (e, n) -> Proj (lower_expr e, n)
  | Syntax.RecordConstruct { typ; fields } ->
    RecordConstruct { typ = lower_expr typ; fields = List.map (fun (n, e) -> (n, lower_expr e)) fields }
  | Syntax.Struct { con_fields; bindings } ->
    Struct { con_fields = List.map (fun (n, e) -> (n, lower_expr e)) con_fields;
                     bindings = List.map lower_struct_binding bindings }
  | Syntax.Module { bindings } ->
    Module { bindings = List.map lower_struct_binding bindings }
  | Syntax.Import s -> Import s
  | Syntax.Open (m, body, label) -> Open (lower_expr m, lower_expr body, label)
  | Syntax.OpenChoice { name; opens; fallback } -> OpenChoice { name = name.name; opens; fallback }
  | Syntax.RecordTypeDef { name; params; fields; body } ->
    RecordTypeDef { name = lower_id name; params = List.map lower_id params; fields = List.map (fun (n, e) -> (n, lower_expr e)) fields; body = lower_expr body }
  | Syntax.TypeDef { name; params; ctors; body } ->
    TypeDef { name = lower_id name; params = List.map lower_id params; ctors = List.map (fun (n, ps) -> (lower_id n, List.map lower_expr ps)) ctors; body = lower_expr body }
  | Syntax.EffectDef { name; params; ops; body } ->
    EffectDef { name = lower_id name; params = List.map lower_id params; ops = List.map lower_effect_op ops; body = lower_expr body }
  | Syntax.TraitDef { name; params; fields; body } ->
    TraitDef { name = lower_id name; params = List.map lower_id params; fields = List.map (fun (n, e) -> (n, lower_expr e)) fields; body = lower_expr body }
  | Syntax.ImplDef { name; trait; args; fields; body } ->
    let trait_path, trait_name = Syntax.path_split trait in
    ImplDef { name = Option.map (fun (i : Syntax.id) -> i.name) name;
                      trait_path; trait_name; args = List.map lower_expr args;
                      fields = List.map (fun (n, e) -> (n, lower_expr e)) fields; body = lower_expr body }
  | Syntax.Perform { op; arg } ->
    let effect_path, op = Syntax.path_split op in
    Perform { effect_path; op; arg = lower_expr arg }
  | Syntax.Resume e -> Resume (lower_expr e)
  | Syntax.RefNew e -> RefNew (lower_expr e)
  | Syntax.RefGet e -> RefGet (lower_expr e)
  | Syntax.RefSet (l, r) -> RefSet (lower_expr l, lower_expr r)
  | Syntax.Match (scrut, brs) ->
    Match (lower_expr scrut, List.map lower_match_branch brs)
  | Syntax.MacroDef { name; value; body; _ } ->
    (* Deliberately drop annotation when lowering after macro registration:
       the resolved kind is carried by the macro registry/table, not this
       syntax node. *)
    MacroDef { name = lower_id name; value = lower_expr value; body = lower_expr body; kind = None }
  | Syntax.SyntaxDef { name; role; body } -> SyntaxDef { name = lower_id name; attaches = Syntax.attaches role; body = lower_expr body }
  | Syntax.Block _ | Syntax.Instantiate _ -> invalid_arg "lower_expr: unexpanded syntax"
  | Syntax.MacroCall (f, a) -> MacroCall (lower_expr f, List.map lower_capture a)
  | Syntax.SyntaxOperatorUse { operator; fixity; operands; declaration_span; use_span; unit = _ } ->
    let fixity = match fixity with Syntax.PrefixOp -> PrefixOp | Syntax.InfixOp -> InfixOp in
    SyntaxOperatorUse { operator = lower_id operator; fixity; operands = List.map lower_expr operands; declaration_span; use_span }

and lower_struct_binding = function
  | Syntax.LetBinding { name; value; public; recursive } ->
    LetBinding { name = lower_id name; value = lower_expr value; public; recursive }
  | Syntax.MethodBinding { name; params; body; public } ->
    MethodBinding { name = lower_id name; params = List.map lower_param params; body = lower_expr body; public }
  | Syntax.TypeBinding { members; public } ->
    TypeBinding
      { members =
          List.map
            (fun ({ name; params; ctors } : Syntax.type_decl) ->
              { name = lower_id name; params = List.map lower_id params;
                ctors = List.map (fun (n, ps) -> (lower_id n, List.map lower_expr ps)) ctors })
            members;
        public }
  | Syntax.RecordTypeBinding { name; params; fields; public } ->
    RecordTypeBinding { name = lower_id name; params = List.map lower_id params; fields = List.map (fun (n, e) -> (n, lower_expr e)) fields; public }
  | Syntax.EffectBinding { name; params; ops; public } ->
    EffectBinding { name = lower_id name; params = List.map lower_id params; ops = List.map lower_effect_op ops; public }
  | Syntax.TraitBinding { name; params; fields; public } ->
    TraitBinding { name = lower_id name; params = List.map lower_id params; fields = List.map (fun (n, e) -> (n, lower_expr e)) fields; public }
  | Syntax.ImplBinding { name; trait; args; fields; public } ->
    let trait_path, trait_name = Syntax.path_split trait in
    ImplBinding { name = Option.map (fun (i : Syntax.id) -> i.name) name;
                          trait_path; trait_name; args = List.map lower_expr args;
                          fields = List.map (fun (n, e) -> (n, lower_expr e)) fields; public }
  | Syntax.MacroBinding { name; value; public; kind } ->
    MacroBinding { name = lower_id name; value = lower_expr value; public; kind }
  | Syntax.MacroCallBinding { f; args } ->
    MacroCallBinding { f = lower_expr f; args = List.map lower_capture args }
  | Syntax.PatternSynBinding { name; params; rhs; public } ->
    PatternSynBinding { name = lower_id name;
                                params = List.map lower_id params;
                                rhs = lower_pat rhs; public }
  | Syntax.OpenBinding (m, label) -> OpenBinding (lower_expr m, label)
  | Syntax.SyntaxBinding { name; role; _ } -> SyntaxBinding { name = lower_id name; attaches = Syntax.attaches role }
  | Syntax.Items _ | Syntax.InstantiateBinding _ -> invalid_arg "lower_struct_binding: unexpanded syntax"
  | Syntax.HoleBinding id -> HoleBinding (lower_id id)

and lower_match_branch = function
  | Syntax.ValueBranch (p, body) -> ValueBranch (lower_pat p, lower_expr body)
  | Syntax.EffectBranch { op; arg_pat; body } ->
    let effect_path, op = Syntax.path_split op in
    EffectBranch { effect_path; op; arg_pat = lower_pat arg_pat; body = lower_expr body }

and lower_pat = function
  | Syntax.PatCon (path, ps) ->
    let path, name = Syntax.path_split path in
    PatCon (path, name, List.map lower_pat ps)
  | Syntax.PatRecord { typ; fields; partial } ->
    let typ_path, typ = Syntax.path_split typ in
    PatRecord { typ_path; typ; fields = List.map (fun (n, p) -> (n, Option.map lower_pat p)) fields; partial }
  | Syntax.PatStructType { fields; partial } ->
    PatStructType { fields = List.map (fun (n, p) -> (n, lower_pat p)) fields; partial }
  | Syntax.PatOr (l, r) -> PatOr (lower_pat l, lower_pat r)
  | Syntax.PatProd ps -> PatProd (List.map lower_pat ps)
  | Syntax.PatAtom a -> PatAtom a
  | Syntax.PatType ty -> PatType ty
  | Syntax.PatWild -> PatWild
  | Syntax.PatBind id -> PatBind (lower_id id)

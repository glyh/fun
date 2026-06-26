
type param = {
  name : string;
  type_ : t option;
  trait_bounds : Trait_bound.t list;
  explicitness : Explicitness.t;
}

and effect_op = { name : string; input : t; output : t }

and effect_row = { effects : t list; tail : t option }

and struct_binding =
  | LetBinding of { name : string; value : t; public : bool; recursive : bool }
  | MethodBinding of { name : string; params : param list; body : t; public : bool }
  | TypeBinding of {
      name : string;
      params : string list;
      ctors : (string * t list) list;  (* (ctor_name, payload_types) *)
      public : bool;
    }
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
      trait_path : string list;
      trait_name : string;
      args : t list;
      fields : (string * t) list;
      public : bool;
    }
  | MacroBinding of { name : string; value : t; public : bool; kind : Syntax.MacroKind.t option }
  | MacroCallBinding of { f : t; args : t list }
  | PatternSynBinding of { name : string; params : string list; rhs : pat; public : bool }

and t =
  | Atom of Atom.t
  | Var of string
  | Self
  | SelfType
  | Ap of t * Explicitness.t * t
  | Lam of param * t
  | Let of { name : string; type_ : t option; value : t; body : t; recursive : bool }
  | If of { cond : t; then_ : t; else_ : t }
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
  | Open of string * t
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
  | Match of t * match_branch list  (* match scrutinee | pat -> body ... end *)
  | MacroDef of { name : string; value : t; body : t; kind : Syntax.MacroKind.t option }
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

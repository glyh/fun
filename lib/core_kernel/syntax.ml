module MacroKind = struct
  type t = Expr | Decl
  let default = Expr
  let to_string = function Expr -> "Expr" | Decl -> "Decl"
  let of_string = function "Decl" -> Some Decl | "Expr" -> Some Expr | _ -> None
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
  | MacroBinding of { name : id; value : t; public : bool; kind : MacroKind.t option }
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
  | MacroDef of { name : id; value : t; body : t; kind : MacroKind.t option }
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

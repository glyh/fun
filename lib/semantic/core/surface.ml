type explicitness = Implicit | Explicit

type param = { name : string; type_ : t option; explicitness : explicitness }

and struct_binding =
  | LetBinding of { name : string; value : t; public : bool }
  | MethodBinding of { name : string; params : param list; body : t; public : bool }
  | TypeBinding of {
      name : string;
      params : string list;
      ctors : (string * t option) list;  (* (ctor_name, payload_type option) *)
      public : bool;
    }

and t =
  | Atom of Syntax.Ast.Atom.t
  | Var of string
  | Self
  | SelfType
  | Ap of t * explicitness * t
  | Lam of param * t
  | Let of { name : string; type_ : t option; value : t; body : t; recursive : bool }
  | If of { cond : t; then_ : t; else_ : t }
  | Annotated of { inner : t; typ : t }
  | Prod of t list
  | ProdTy of t list
  | Arrow of explicitness * string option * t * t
  | FieldAccess of t * string
  | Proj of t * int
  | RecordConstruct of { typ : t; fields : (string * t) list }
  | Struct of {
      con_fields : (string * t) list;
      bindings : struct_binding list;
    }
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
      ctors : (string * t option) list;  (* (ctor_name, payload_type option) *)
      body : t;
    }
  | Match of t * (pat * t) list  (* match scrutinee | pat -> body ... end *)

and pat =
  | PatCon of string list * string * pat list   (* path, constructor, subpatterns *)
  | PatRecord of { typ_path : string list; typ : string; fields : (string * pat option) list; partial : bool }
  | PatOr of pat * pat
  | PatProd of pat list
  | PatAtom of Syntax.Ast.Atom.t
  | PatType of Core.atom_ty
  | PatWild                       (* _ *)
  | PatBind of string             (* variable binding *)

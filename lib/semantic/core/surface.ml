type param = { name : string; type_ : t option }

and struct_binding =
  | LetBinding of { name : string; value : t; public : bool }
  | TypeBinding of {
      name : string;
      params : string list;
      ctors : (string * t option) list;  (* (ctor_name, payload_type option) *)
      public : bool;
    }

and t =
  | Atom of Syntax.Ast.Atom.t
  | Var of string
  | Ap of t * t
  | Lam of param * t
  | Let of { name : string; type_ : t option; value : t; body : t }
  | If of { cond : t; then_ : t; else_ : t }
  | Annotated of { inner : t; typ : t }
  | Prod of t list
  | Arrow of t * t
  | FieldAccess of t * string
  | Proj of t * int
  | Struct of {
      con_fields : (string * t) list;
      bindings : struct_binding list;
    }
  | Open of string * t
  | TypeDef of {
      name : string;
      params : string list;
      ctors : (string * t option) list;  (* (ctor_name, payload_type option) *)
      body : t;
    }
  | Match of t * (pat * t) list  (* match scrutinee | pat -> body ... end *)

and pat =
  | PatCon of string * pat list   (* Constructor(sub, patterns) *)
  | PatWild                       (* _ *)
  | PatBind of string             (* variable binding *)

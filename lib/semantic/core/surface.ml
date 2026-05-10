type param = { name : string; type_ : t option }
and struct_binding =
  | LetBinding of { name : string; value : t; public : bool }
  | TypeBinding of { name : string; ctors : string list; public : bool }

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
  | TypeDef of string * string list * t  (* type name + constructor names + body *)

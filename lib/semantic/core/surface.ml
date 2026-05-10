type param = { name : string; type_ : t option }
and struct_binding = { name : string; value : t }

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
  | Struct of struct_binding list
  | Open of string * t

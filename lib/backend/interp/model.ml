open Syntax

module rec Env : sig
  type t = Value.t Type.Id.Map.t
end = struct
  type t = Value.t Type.Id.Map.t
end

and Value : sig
  type t = Norm of Ast.Atom.t | Closure of Env.t * Type.Id.t * Ast.Expr.t
end = struct
  type t = Norm of Ast.Atom.t | Closure of Env.t * Type.Id.t * Ast.Expr.t
end

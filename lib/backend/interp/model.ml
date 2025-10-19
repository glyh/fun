open Syntax

module rec Env : sig
  type t = Value.t Type.Id.Map.t
end = struct
  type t = Value.t Type.Id.Map.t
end

and Value : sig
  type t = Norm of Ast.Atom.t | Closure of (t -> t)

  exception CantCompare of (t * t)

  val equal : t -> t -> bool
  val pp : t -> string
end = struct
  type t = Norm of Ast.Atom.t | Closure of (t -> t)

  exception CantCompare of (t * t)

  let equal lhs rhs =
    match (lhs, rhs) with
    | Norm lhs, Norm rhs -> Ast.Atom.equal lhs rhs
    | lhs, rhs -> raise (CantCompare (lhs, rhs))

  let pp = function Norm a -> Ast.Atom.pp a | Closure _ -> "<closure>"
end

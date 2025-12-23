open Syntax

module rec Env : sig
  type t = Value.t Type.Id.Map.t
end = struct
  type t = Value.t Type.Id.Map.t
end

and Value : sig
  type t =
    | Norm of Ast.Atom.t
    | Closure of (t -> t)
    | Tagged of { tag : string; inner : t option }

  exception CantCompare of (t * t)

  val equal : t -> t -> bool
  val pp : t -> string
end = struct
  type t =
    | Norm of Ast.Atom.t
    | Closure of (t -> t)
    | Tagged of { tag : string; inner : t option }

  exception CantCompare of (t * t)

  let rec equal lhs rhs =
    match (lhs, rhs) with
    | Norm lhs, Norm rhs -> Ast.Atom.equal lhs rhs
    | ( Tagged { tag = lhs_tag; inner = None },
        Tagged { tag = rhs_tag; inner = None } ) ->
        String.equal lhs_tag rhs_tag
    | ( Tagged { tag = lhs_tag; inner = Some lhs },
        Tagged { tag = rhs_tag; inner = Some rhs } ) ->
        String.equal lhs_tag rhs_tag && equal lhs rhs
    | lhs, rhs -> raise (CantCompare (lhs, rhs))

  let rec pp = function
    | Norm a -> Ast.Atom.pp a
    | Closure _ -> "<closure>"
    | Tagged { tag; inner = None } -> tag
    | Tagged { tag; inner = Some inner } ->
        Printf.sprintf "%s (%s)" tag @@ pp inner
end

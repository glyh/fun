open Syntax

module rec Env : sig
  type t = Value.t Type.Id.Map.t
end = struct
  type t = Value.t Type.Id.Map.t
end

and Value : sig
  type t =
    | Atom of Ast.Atom.t
    | Closure of (t -> t)
    | Tagged of { tag : string; inner : t option }
    | Prod of t Std.Nonempty_list.t

  exception CantCompare of (t * t)

  val equal : t -> t -> bool
  val pp : t -> string
end = struct
  type t =
    | Atom of Ast.Atom.t
    | Closure of (t -> t)
    | Tagged of { tag : string; inner : t option }
    | Prod of t Std.Nonempty_list.t

  exception CantCompare of (t * t)

  let rec equal lhs rhs =
    match (lhs, rhs) with
    | Atom lhs, Atom rhs -> Ast.Atom.equal lhs rhs
    | ( Tagged { tag = lhs_tag; inner = None },
        Tagged { tag = rhs_tag; inner = None } ) ->
        String.equal lhs_tag rhs_tag
    | ( Tagged { tag = lhs_tag; inner = Some lhs },
        Tagged { tag = rhs_tag; inner = Some rhs } ) ->
        String.equal lhs_tag rhs_tag && equal lhs rhs
    | Prod elems1, Prod elems2 -> Std.Nonempty_list.equal equal elems1 elems2
    | lhs, rhs -> raise (CantCompare (lhs, rhs))

  let rec pp = function
    | Atom a -> Ast.Atom.pp a
    | Closure _ -> "<closure>"
    | Tagged { tag; inner = None } -> tag
    | Tagged { tag; inner = Some inner } ->
        Printf.sprintf "%s (%s)" tag @@ pp inner
    | Prod elems ->
        Std.Nonempty_list.(map pp elems |> to_list)
        |> String.concat ", " |> Printf.sprintf "(%s)"
end

module Id = Type.Id

module Param = struct
  type t = { name : Id.t; type_ : Type.T.t option } [@@deriving eq]
end

let pp_type_annotated = function
  | None -> ""
  | Some typ -> Printf.sprintf ": %s " (Type.T.pp typ)

module Atom = struct
  type t = Unit | I64 of int64 | Bool of bool [@@deriving eq]

  let pp = function
    | Unit -> "()"
    | I64 i -> Int64.to_string i
    | Bool true -> "true"
    | Bool false -> "false"
end

module rec Expr : sig
  type t =
    | Atom of Atom.t
    | Var of Id.t
    | Ap of t * t
    | Let of { binding : Binding.t; body : t }
    | If of { cond : t; then_ : t; else_ : t }
    | Lam of Param.t * t
    | Annotated of { inner : t; typ : Type.T.t }
  [@@deriving eq]

  val pp : t -> string
end = struct
  type t =
    | Atom of Atom.t
    | Var of Id.t
    | Ap of t * t
    | Let of { binding : Binding.t; body : t }
    | If of { cond : t; then_ : t; else_ : t }
    | Lam of Param.t * t
    | Annotated of { inner : t; typ : Type.T.t }
  [@@deriving eq]

  let rec pp = function
    | Atom a -> Atom.pp a
    | Var id -> id
    | Ap (lhs, rhs) -> pp lhs ^ "(" ^ pp rhs ^ ")"
    | Let { binding; body } ->
        Printf.sprintf "%s in %s" (Binding.pp binding) (pp body)
    | If { cond; then_; else_ } ->
        Printf.sprintf "if (%s) then (%s) else (%s)" (pp cond) (pp then_)
          (pp else_)
    | Lam ({ name; type_ }, body) ->
        Printf.sprintf "fun %s%s -> (%s)" name (pp_type_annotated type_)
          (pp body)
    | Annotated { inner; typ } ->
        Printf.sprintf "(%s : %s)" (pp inner) (Type.T.pp typ)
end

and Binding : sig
  type t = {
    recursive : bool;
    name : Id.t;
    type_ : Type.T.t option;
    value : Expr.t;
  }
  [@@deriving eq]

  val pp : t -> string
end = struct
  type t = {
    recursive : bool;
    name : Id.t;
    type_ : Type.T.t option;
    value : Expr.t;
  }
  [@@deriving eq]

  let pp { recursive; name; type_; value } =
    let rec_str = if recursive then "rec " else "" in
    Printf.sprintf "let %s%s %s= (%s)" rec_str name (pp_type_annotated type_)
      (Expr.pp value)
end

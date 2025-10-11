module Param : sig
  type t = { name : Type.Id.t; type_ : Type.T.t option } [@@deriving eq]
end

module rec Expr : sig
  type t =
    | Unit
    | Num of int
    | Var of Type.Id.t
    | Ap of t * t
    | Let of { binding : Binding.t; body : t }
    | If of { cond : t; then_ : t; else_ : t }
    | Lam of Param.t * t
    | Annotated of { inner : t; typ : Type.T.t }
  [@@deriving eq]

  val pp : t -> string
end

and Binding : sig
  type t = {
    recursive : bool;
    name : Type.Id.t;
    type_ : Type.T.t option;
    value : Expr.t;
  }
  [@@deriving eq]

  val pp : t -> string
end

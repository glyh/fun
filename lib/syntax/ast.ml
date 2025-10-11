module Id = Type.Id

module Param = struct
  type t = { name : Id.t; type_ : Type.T.t option } [@@deriving eq]
end

module rec Expr : sig
  type t =
    | Unit
    | Num of int
    | Var of Id.t
    | Ap of t * t
    | Let of { binding : Binding.t; body : t }
    | If of { cond : t; then_ : t; else_ : t }
    | Lam of Param.t * t
    | Annotated of { inner : t; typ : Type.T.t }
  [@@deriving eq]
end = struct
  type t =
    | Unit
    | Num of int
    | Var of Id.t
    | Ap of t * t
    | Let of { binding : Binding.t; body : t }
    | If of { cond : t; then_ : t; else_ : t }
    | Lam of Param.t * t
    | Annotated of { inner : t; typ : Type.T.t }
  [@@deriving eq]
end

and Binding : sig
  type t = {
    recursive : bool;
    name : Id.t;
    type_ : Type.T.t option;
    value : Expr.t;
  }
  [@@deriving eq]
end = struct
  type t = {
    recursive : bool;
    name : Id.t;
    type_ : Type.T.t option;
    value : Expr.t;
  }
  [@@deriving eq]
end

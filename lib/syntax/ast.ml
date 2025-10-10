open Core

module Id = struct
  type t = string [@@deriving yojson, eq]
end

module TypeAnnotated = struct
  type t = string option [@@deriving yojson, eq]
end

module Param = struct
  type t = { name : Id.t; type_ : TypeAnnotated.t } [@@deriving yojson, eq]
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
  [@@deriving yojson, eq]
end = struct
  type t =
    | Unit
    | Num of int
    | Var of Id.t
    | Ap of t * t
    | Let of { binding : Binding.t; body : t }
    | If of { cond : t; then_ : t; else_ : t }
    | Lam of Param.t * t
  [@@deriving yojson, eq]
end

and Binding : sig
  type t = {
    recursive : bool;
    name : Id.t;
    type_ : TypeAnnotated.t;
    value : Expr.t;
  }
  [@@deriving yojson, eq]
end = struct
  type t = {
    recursive : bool;
    name : Id.t;
    type_ : TypeAnnotated.t;
    value : Expr.t;
  }
  [@@deriving yojson, eq]
end

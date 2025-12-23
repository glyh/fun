module Param : sig
  type t = { name : Type.Id.t; type_ : Type.Human.t option } [@@deriving eq]
end

module Atom : sig
  type t = Unit | I64 of int64 | Bool of bool [@@deriving eq]

  val pp : t -> string
end

module rec Expr : sig
  type t =
    | Atom of Atom.t
    | Var of Type.Id.t
    | Ap of t * t
    | Let of { binding : Binding.t; body : t }
    | If of { cond : t; then_ : t; else_ : t }
    | Lam of Param.t * t
    | Annotated of { inner : t; typ : Type.Human.t }
    | Fix of t
  [@@deriving eq]

  val pp : t -> string
end

and Binding : sig
  type t =
    | Value of { name : Type.Id.t; type_ : Type.Human.t option; value : Expr.t }
    | TypeDecl of {
        name : string;
        args : string list;
        rhs : (string * Type.Human.t option) Std.Nonempty_list.t;
      }
  [@@deriving eq]

  val pp : t -> string
end

module Param : sig
  type t = { name : Type.Id.t; type_ : Type.Human.t option } [@@deriving eq]
end

module Atom : sig
  type t = Unit | I64 of int64 | Bool of bool [@@deriving eq, hash]

  val pp : t -> string
end

module Pattern : sig
  type t =
    | Bind of Type.Id.t
    | Just of Atom.t
    | Prod of t Std.Nonempty_list.t
    | Tagged of Type.Id.t * t option
    | Union of t * t
    | Any
    | Record of (string * t option) Std.Nonempty_list.t
  [@@deriving eq]

  val pp : t -> string
end

type field_accessor = string [@@deriving eq]

type type_rhs =
  | Adt of (string * Type.Human.t option) Std.Nonempty_list.t
  | Record of (string * Type.Human.t) Std.Nonempty_list.t
[@@deriving eq]

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
    | Prod of t Std.Nonempty_list.t
    | Match of { matched : t; branches : (Pattern.t * t) Std.Nonempty_list.t }
    | Record of (field_accessor * t) Std.Nonempty_list.t
    | FieldAccess of t * field_accessor
  [@@deriving eq]

  val pp : t -> string
end

and Binding : sig
  type t =
    | Value of { name : Type.Id.t; type_ : Type.Human.t option; value : Expr.t }
    | TypeDecl of {
        name : string;
        args : string list;
        rhs : type_rhs;
      }
  [@@deriving eq]

  val pp : t -> string
end

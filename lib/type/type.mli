module Var : sig
  type t [@@deriving eq]

  module Set : Set.S with type elt = t
end

module T : sig
  type t =
    | Forall of Var.Set.t * t
    | Var of Var.t
    | Con of string
    | Arrow of t * t
  [@@deriving eq]
end

module Builtin : sig
  open T

  val unit : t
  val int : t
  val bool : t
  val char : t
end

module Env : sig
  type t

  val create : unit -> t
  val generate : t -> ?tag:string -> unit -> Var.t
  val find : t -> Var.t -> T.t option
end

module Id : sig
  type t = string [@@deriving eq]
  val pp: t -> string
  module Map : Map.S with type key = t
end

module Var : sig
  (* NOTE: Abstract the implementation ensuring no one can duplicate it *)
  type t [@@deriving eq]

  val pp: t -> string
  val generate : ?tag:string -> unit -> t
  (** [inherit_ var] Create a new var that is supposed to replace [var] in some expression *)
  val inherit_ : t -> t

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

module Exceptions : sig
  exception Rank2TypeUnsupported
  exception UnusedVars of Var.Set.t
  exception MismatchForallVars of (Var.t Seq.t * Var.t Seq.t)
end

module Human : sig
  type t =
    | Forall of string list * t
    | Var of string
    | Con of string
    | Arrow of t * t
end

module T : sig
  type t =
    | Forall of Var.Set.t * t
    | Var of Var.t
    | Con of Id.t
    | Arrow of t * t
  [@@deriving eq]

  val pp: t -> string

  val of_human: Human.t -> t
end

module Builtin : sig
  open T

  val unit : t
  val i64 : t
  val bool : t
  val char : t
end

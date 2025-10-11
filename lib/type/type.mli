module Id : sig
  type t = string [@@deriving eq]
  module Map : Map.S with type key = t
end

module Var : sig
  (* NOTE: Abstract the implementation ensuring no one can duplicate it *)
  type t [@@deriving eq]

  val generate : ?tag:string -> unit -> t
  (** [inherit_ var] Create a new var that is supposed to replace [var] in some expression *)
  val inherit_ : t -> t

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

module T : sig
  type t =
    | Forall of Var.Set.t * t
    | Var of Var.t
    | Con of string
    | Arrow of t * t
  [@@deriving eq]

  val pp: t -> string
end

module Builtin : sig
  open T

  val unit : t
  val int : t
  val bool : t
  val char : t
end

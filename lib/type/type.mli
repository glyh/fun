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

module Generic : sig
  type ('var, 'var_set) t =
    | Forall of 'var_set * ('var, 'var_set) t
    | Var of 'var
    | Con of Id.t * ('var, 'var_set) t list
    | Prod of ('var, 'var_set) t * ('var, 'var_set) t
    | Arrow of ('var, 'var_set) t * ('var, 'var_set) t
  [@@deriving eq]

  val con_0 : string -> _ t

  val unit : _ t
  val i64 : _ t
  val bool : _ t
  val char : _ t
end

module Human : sig
  type t = (string, string list) Generic.t [@@deriving eq]

  val pp: t -> string

end

module T : sig
  type t = (Var.t, Var.Set.t) Generic.t [@@deriving eq]

  val pp: t -> string

  val of_human: Human.t -> t
end


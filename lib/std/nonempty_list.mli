type 'a t [@@deriving eq]

val init : 'a -> 'a list -> 'a t
val uncons : 'a t -> 'a * 'a list
val cons : 'a -> 'a t -> 'a t
val first : 'a t -> 'a
val map : ('a -> 'b) -> 'a t -> 'b t
val to_list : 'a t -> 'a list

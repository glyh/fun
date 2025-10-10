type _ t

val init : 'a -> 'a list -> 'a t
val uncons : 'a t -> 'a * 'a list
val cons : 'a -> 'a t -> 'a t
val first : 'a t -> 'a

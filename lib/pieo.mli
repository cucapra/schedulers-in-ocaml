type 'a t

val create : ('a -> 'a -> int) -> 'a t
val push : 'a t -> 'a -> 'a t
val peek : 'a t -> 'a option
val top_exn : 'a t -> 'a
val pop : 'a t -> ('a * 'a t) option
val pop_exn : 'a t -> 'a * 'a t
val pop_if : 'a t -> ('a -> bool) -> ('a * 'a t) option
val pop_pred : 'a t -> ('a -> bool) -> ('a * 'a t) option
val is_empty : 'a t -> bool
val length : 'a t -> int
val of_list : 'a list -> ('a -> 'a -> int) -> 'a t
val count : 'a t -> ('a -> bool) -> int
val filter : 'a t -> ('a -> bool) -> 'a t
val flush : 'a t -> 'a list
val flush_pred : 'a t -> ('a -> bool) -> 'a list

type t

val pop : t -> Time.t -> (Packet.t * t) option
val push : t -> Time.t -> Packet.t -> Path.t -> t
val size : t -> Time.t -> int
val well_formed : t -> Time.t -> bool
val flush : t -> Time.t -> Packet.t list
val create : Topo.t -> t

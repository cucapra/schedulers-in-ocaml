type t = 
  { 
    s : State.t; 
    q : Pieotree.t; 
    z_in : State.t -> Packet.t -> Path.t * State.t * Time.t;
    z_out : State.t -> Packet.t -> State.t
  }

val simulate : float -> float -> float -> Packet.t list -> t -> Packet.t list

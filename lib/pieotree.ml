let ( let* ) = Option.bind

type t =
  | Leaf of (Packet.t * Rank.t * Time.t) Pieo.t
  | Internal of t list * (int * Rank.t * Time.t) Pieo.t

let predicate now (_, _, ts) = ts <= now

let rec pop t now =
  match t with
  | Leaf p ->
      let* (pkt, _, _), p' = Pieo.pop_pred p (predicate now) in
      Some (pkt, Leaf p')
  | Internal (qs, p) ->
      let* (i, _, _), p' = Pieo.pop_pred p (predicate now) in
      let* pkt, q' = pop (List.nth qs i) now in
      Some (pkt, Internal (Util.replace_nth qs i q', p'))

let rec push t ts pkt path =
  match (t, path) with
  | Leaf p, [ (_, r) ] -> Leaf (Pieo.push p (pkt, r, ts))
  | Internal (qs, p), (i, r) :: pt ->
      let p' = Pieo.push p (i, r, ts) in
      let q' = push (List.nth qs i) ts pkt pt in
      Internal (Util.replace_nth qs i q', p')
  | _ -> failwith "Push: invalid path"

let rec size t now =
  (* The size of a PIFO tree is the number of ready packets in its leaves. 
     Recall that a packet or reference is _ready_ if its time stamp is <= [now]. 
  *)
  match t with
  | Leaf p -> Pieo.count p (predicate now)
  | Internal (qs, _) -> List.fold_left (fun acc q -> acc + size q now) 0 qs

let rec well_formed t now =
  (* A leaf is well-formed.
      An internal node is well-formed if:
       - each of its child trees is well-formed
       - the number of ready packets in the ith child-tree is equal to the number
         of ready i's in _its own_ PIEO. 
  *)
  let pieo_count_occ p ele = 
    Pieo.count p (fun (v, _, ts) -> v = ele && ts <= now) 
  in
  (* Counts how many times ele occurs as a value in PIFO p. *)
  match t with
  | Leaf _ -> true
  | Internal (qs, p) ->
      List.fold_left ( && ) true
        (List.mapi (fun i q -> well_formed q now && pieo_count_occ p i = size q now) qs)

let rec flush t now =
  if size t now > 0 then
    match pop t now with
    | None -> failwith "Flush: malformed tree."
    | Some (pkt, q') -> flush q' now @ [ pkt ]
  else []

let rec create (topo : Topo.t) =
  match topo with
  | Star -> Leaf (Pieo.create (fun (_, a, _) (_, b, _) -> Rank.cmp a b))
  | Node topos ->
      let qs = List.map create topos in
      let p = Pieo.create (fun (_, a, _) (_, b, _) -> Rank.cmp a b) in
      Internal (qs, p)

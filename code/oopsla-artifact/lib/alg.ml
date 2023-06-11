let poprate = 0.25 (* Four packets per second. *)

let find_flow p =
  (* In pcap_gen.py, we create packets with sources based on their MAC addresses.
     After going through our parser, those packets' sources get converted into
     inscrutable integers.
     This little function converts those integers back into human-readable strings.
  *)
  match Packet.src p with
  | 17661175009296 -> "A" (* Used to be address 10:10:10:10:10:10. *)
  | 35322350018592 -> "B" (* 20...*)
  | 52983525027888 -> "C" (* 30...*)
  | 70644700037184 -> "D" (* 40...*)
  | 88305875046480 -> "E" (* 50...*)
  | 105967050055776 -> "F" (* 60...*)
  | 123628225065072 -> "G" (* 70...*)
  | n -> failwith Printf.(sprintf "Unknown source address: %d." n)

module FCFS_Ternary = struct
  let scheduling_transaction (s : State.t) pkt time =
    match find_flow pkt with
    | "A" -> ([ (0, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s)
    | "B" -> ([ (1, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s)
    | "C" -> ([ (2, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s)
    (* Put flow A into leaf 0, flow B into leaf 1, and flow C into leaf 2.
       The ranks at the root are straightforward: nothing fancy to do with
       the float portion proper, but we do register the time of the packet's
       scheduling. This means that FCFS prevails overall.
       Doing the same thing at the leaves means that FCFS prevails there too.

       Recall that the fist element of the foot of a path is ignored.
       | "A" -> ([ (0, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s)
                                              ^^^
                                            ignored
    *)
    | n -> failwith Printf.(sprintf "Don't know how to route flow %s." n)

  let control : Control.t =
    {
      s = State.create 1;
      q = Pifotree.create Topo.one_level_ternary;
      z = scheduling_transaction;
    }

  let simulate end_time pkts =
    Control.simulate end_time 0.001 poprate pkts control
end

module Strict_Ternary = struct
  let scheduling_transaction (s : State.t) pkt time =
    match find_flow pkt with
    | "A" -> ([ (0, Rank.create 2.0 time); (0, Rank.create 0.0 time) ], s)
    | "B" -> ([ (1, Rank.create 1.0 time); (0, Rank.create 0.0 time) ], s)
    | "C" -> ([ (2, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s)
    (* Put flow A into leaf 0, flow B into leaf 1, and flow C into leaf 2.
       The ranks at the root are set up to prefer C to B, and B to A.
       At the leaves, we let FCFS prevail.
    *)
    | n -> failwith Printf.(sprintf "Don't know how to route flow %s." n)

  let control : Control.t =
    {
      s = State.create 1;
      q = Pifotree.create Topo.one_level_ternary;
      z = scheduling_transaction;
    }

  let simulate end_time pkts =
    Control.simulate end_time 0.001 poprate pkts control
end

module RRobin_Ternary = struct
  let scheduling_transaction s pkt time =
    let flow = find_flow pkt in
    let var_last_finish = Printf.sprintf "%s_last_finish" flow in
    (* We will use this variable to read/write to state.
       There are three flows, so we will end up creating three variables
       over the course of the simulation.
    *)
    let rank_for_root =
      if State.isdefined var_last_finish s then
        max (Time.to_float time) (State.lookup var_last_finish s)
      else Time.to_float time
    in
    let s' =
      State.rebind var_last_finish (rank_for_root +. (100.0 /. 0.33)) s
    in
    let rank_for_root = Rank.create rank_for_root time in
    match flow with
    | "A" -> ([ (0, rank_for_root); (0, Rank.create 0.0 time) ], s')
    | "B" -> ([ (1, rank_for_root); (0, Rank.create 0.0 time) ], s')
    | "C" -> ([ (2, rank_for_root); (0, Rank.create 0.0 time) ], s')
    (* Put flow A into leaf 0, flow B into leaf 1, and flow C into leaf 2.
       The ranks at the root are as computed just above.
       At the leaves, we let FCFS prevail.
    *)
    | n -> failwith Printf.(sprintf "Don't know how to route flow %s." n)

  let control : Control.t =
    {
      s = State.create 3;
      q = Pifotree.create Topo.one_level_ternary;
      z = scheduling_transaction;
    }

  let simulate end_time pkts =
    Control.simulate end_time 0.001 poprate pkts control
end

let wfq_helper s weight var_last_finish pkt_len time : Rank.t * State.t =
  (* The WFQ-style algorithms have a common pattern, so we lift it. *)
  let rank =
    if State.isdefined var_last_finish s then
      max (Time.to_float time) (State.lookup var_last_finish s)
    else Time.to_float time
  in
  let s' = State.rebind var_last_finish (rank +. (pkt_len /. weight)) s in
  (Rank.create rank time, s')

module WFQ_Ternary = struct
  let scheduling_transaction s pkt time =
    let flow = find_flow pkt in
    let var_last_finish = Printf.sprintf "%s_last_finish" flow in
    let var_weight = Printf.sprintf "%s_weight" flow in
    let weight = State.lookup var_weight s in
    let rank_for_root, s' =
      wfq_helper s weight var_last_finish (Packet.len pkt) time
    in
    match flow with
    | "A" -> ([ (0, rank_for_root); (0, Rank.create 0.0 time) ], s')
    | "B" -> ([ (1, rank_for_root); (0, Rank.create 0.0 time) ], s')
    | "C" -> ([ (2, rank_for_root); (0, Rank.create 0.0 time) ], s')
    (* Put flow A into leaf 0, flow B into leaf 1, and flow C into leaf 2.
       The ranks at the root are as computed just above.
       At the leaves, we let FCFS prevail.
    *)
    | n -> failwith Printf.(sprintf "Don't know how to route flow %s." n)

  let control : Control.t =
    {
      s =
        State.create 6
        |> State.rebind "A_weight" 0.1
        |> State.rebind "B_weight" 0.2
        |> State.rebind "C_weight" 0.3;
      q = Pifotree.create Topo.one_level_ternary;
      z = scheduling_transaction;
    }

  let simulate end_time pkts =
    Control.simulate end_time 0.001 poprate pkts control
end

module HPFQ_Binary = struct
  let scheduling_transaction s pkt time =
    let flow = find_flow pkt in
    (* this is either A, B, or C.
       When computing ranks for the root, we arbitrate between {AB} or {C}.
       When computing ranks for the left node, we arbitrate between {A} or {B}.
    *)
    match flow with
    | "A" ->
        let rank_for_root, s' =
          wfq_helper s
            (State.lookup "AB_weight" s)
            "AB_last_finish" (Packet.len pkt) time
        in
        let rank_for_left_node, s'' =
          wfq_helper s'
            (State.lookup "A_weight" s')
            "A_last_finish" (Packet.len pkt) time
        in
        ( [
            (0, rank_for_root);
            (0, rank_for_left_node);
            (0, Rank.create 0.0 time);
          ],
          s'' )
    | "B" ->
        let rank_for_root, s' =
          wfq_helper s
            (State.lookup "AB_weight" s)
            "AB_last_finish" (Packet.len pkt) time
        in
        let rank_for_left_node, s'' =
          wfq_helper s'
            (State.lookup "B_weight" s')
            "B_last_finish" (Packet.len pkt) time
        in
        ( [
            (0, rank_for_root);
            (1, rank_for_left_node);
            (0, Rank.create 0.0 time);
          ],
          s'' )
    | "C" ->
        let rank_for_root, s' =
          wfq_helper s
            (State.lookup "C_weight" s)
            "C_last_finish" (Packet.len pkt) time
        in
        ([ (1, rank_for_root); (0, Rank.create 0.0 time) ], s')
    | n -> failwith Printf.(sprintf "Don't know how to route flow %s." n)

  let control : Control.t =
    {
      s =
        State.create 8
        |> State.rebind "AB_weight" 0.8
        |> State.rebind "A_weight" 0.75
        |> State.rebind "B_weight" 0.25
        |> State.rebind "C_weight" 0.2;
      q = Pifotree.create Topo.binary_three_leaves;
      z = scheduling_transaction;
    }

  let simulate end_time pkts =
    Control.simulate end_time 0.001 poprate pkts control
end

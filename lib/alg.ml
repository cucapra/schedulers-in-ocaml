let poprate = 0.25 (* Four packets per second. *)

module type Alg_t = sig
  val topology : Topo.t
  val control : Control.t
  val simulate : float -> Packet.t list -> Packet.t list
end

module FCFS_Ternary : Alg_t = struct
  let scheduling_transaction (s : State.t) pkt =
    let time = Packet.time pkt in
    match Packet.find_flow pkt with
    | A -> ([ (0, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s, Time.epoch)
    | B -> ([ (1, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s, Time.epoch)
    | C -> ([ (2, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s, Time.epoch)
    (* Put flow A into leaf 0, flow B into leaf 1, and flow C into leaf 2.
       The ranks at the root are straightforward: nothing fancy to do with
       the float portion proper, but we do register the time of the packet's
       scheduling.
       Since the float portion of the rank is always a tie, the time is used to break ties.
       This means that FCFS prevails overall.
       Doing the same thing at the leaves means that FCFS prevails there too.
       Going forward, we will frequently give the the leaves FCFS scheduling in this way.

       Recall that the fist element of the foot of a path is ignored.
       | "A" -> ([ (0, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s)
                                              ^^^
                                            ignored
    *)
    | n -> failwith Printf.(sprintf "Don't know how to route flow %s." (Flow.to_string n))

  let topology = Topo.one_level_ternary

  let control : Control.t =
    {
      s = State.create 1;
      q = Pieotree.create topology;
      z_in = scheduling_transaction;
      z_out = fun x -> fun _ -> x;
    }

  let simulate sim_length pkts =
    Control.simulate sim_length 0.001 poprate pkts control
end

module Strict_Ternary : Alg_t = struct
  let scheduling_transaction (s : State.t) pkt =
    let time = Packet.time pkt in
    let int_for_root, rank_for_root =
      (* Put flow A into leaf 0, flow B into leaf 1, and flow C into leaf 2.
         The ranks at the root are set up to prefer C to B, and B to A.
      *)
      match Packet.find_flow pkt with
      | A -> (0, Rank.create 2.0 time)
      | B-> (1, Rank.create 1.0 time)
      | C -> (2, Rank.create 0.0 time)
      | n -> failwith Printf.(sprintf "Don't know how to route flow %s." (Flow.to_string n))
    in
    ([ (int_for_root, rank_for_root); (0, Rank.create 0.0 time) ], s, Time.epoch)

  let topology = Topo.one_level_ternary

  let control : Control.t =
    {
      s = State.create 1;
      q = Pieotree.create topology;
      z_in = scheduling_transaction;
      z_out = fun x -> fun _ -> x;
    }

  let simulate sim_length pkts =
    Control.simulate sim_length 0.001 poprate pkts control
end

let rrobin n = (
  module struct
    (* n-flow Round-Robin *)
    let _ = assert (n <= 7 && n > 0)

    let who_skip pop turn = 
      let rec who_skip_aux t acc = 
        if t = pop then 
          acc
        else
          who_skip_aux ((t + 1) mod n) (t :: acc)
      in
      who_skip_aux turn []

    let pkt_to_int pkt =
      match Packet.find_flow pkt with
      | A -> 0
      | B -> 1
      | C -> 2 
      | D -> 3 
      | E -> 4 
      | F -> 5 
      | G -> 6

    let z_in s pkt = 
      let time = Packet.time pkt in
      let int_for_root = pkt_to_int pkt in
      let r_i = "r_" ^ (string_of_int int_for_root) in
      let rank_for_root = State.lookup r_i s in
      let s' = State.rebind r_i (rank_for_root +. (float_of_int n)) s in
      let rank_for_root = Rank.create rank_for_root time in
      ([ (int_for_root, rank_for_root); (0, Rank.create 0.0 time) ], s', Time.epoch)

    let z_out s pkt = 
      let turn = State.lookup "turn" s in
      let turn' = ((pkt_to_int pkt) + 1) mod n |> float_of_int in 
      let s' = State.rebind "turn" turn' s in
      let skipped = who_skip (pkt_to_int pkt) (int_of_float turn) in
      let f s i = 
        let r_i = "r_" ^ (string_of_int i) in
        State.rebind r_i (State.lookup r_i s +. (2.0 *. (float_of_int n))) s
      in
      List.fold_left f s' skipped

    let init_state = 
      let zero_to_n = List.init n Fun.id in
      let s = State.create (n + 1) |> State.rebind "turn" 0.0 in
      let f s x = State.rebind ("r_" ^ (string_of_int x)) (float_of_int x) s in
      List.fold_left f s zero_to_n

    let topology = Topo.one_level_n_ary n

    let control : Control.t =
      {
        s = init_state;
        q = Pieotree.create topology;
        z_in = z_in;
        z_out = z_out;
      }

    let simulate sim_length pkts = 
      Control.simulate sim_length 0.001 poprate pkts control
  end : Alg_t
)

module RRobin_Ternary = (val rrobin 3)

let wfq_helper s weight var_last_finish pkt_len time : Rank.t * State.t =
  (* The WFQ-style algorithms have a common pattern,
      so we lift it into this helper method.
  *)
  let rank =
    if State.isdefined var_last_finish s then
      max (Time.to_float time) (State.lookup var_last_finish s)
    else Time.to_float time
  in
  let s' = State.rebind var_last_finish (rank +. (pkt_len /. weight)) s in
  (Rank.create rank time, s')

module WFQ_Ternary : Alg_t = struct
  let scheduling_transaction s pkt =
    let time = Packet.time pkt in
    let flow = Packet.find_flow pkt in
    let var_last_finish = Printf.sprintf "%s_last_finish" (Flow.to_string flow) in
    let var_weight = Printf.sprintf "%s_weight" (Flow.to_string flow) in
    let weight = State.lookup var_weight s in
    let rank_for_root, s' =
      wfq_helper s weight var_last_finish (Packet.len pkt) time
    in
    let int_for_root =
      (* Put flow A into leaf 0, flow B into leaf 1, and flow C into leaf 2. *)
      match flow with
      | A -> 0
      | B -> 1
      | C -> 2
      | n -> failwith Printf.(sprintf "Don't know how to route flow %s." (Flow.to_string n))
    in
    ([ (int_for_root, rank_for_root); (0, Rank.create 0.0 time) ], s', Time.epoch)

  let topology = Topo.one_level_ternary

  let init_state =
    State.create 6
    |> State.rebind "A_weight" 0.1
    |> State.rebind "B_weight" 0.2
    |> State.rebind "C_weight" 0.3

  let control : Control.t =
    { 
      s = init_state; 
      q = Pieotree.create topology; 
      z_in = scheduling_transaction; 
      z_out = fun x -> fun _ -> x;
    }

  let simulate sim_length pkts =
    Control.simulate sim_length 0.001 poprate pkts control
end

module HPFQ_Binary : Alg_t = struct
  let scheduling_transaction s pkt =
    let time = Packet.time pkt in
    let flow = Packet.find_flow pkt in
    (* This is either A, B, or C.
       When computing ranks for the root, we arbitrate between AB or C.
       When computing ranks for the left node, we arbitrate between A or B.
    *)
    match flow with
    | A ->
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
          s'', 
          Time.epoch )
    | B ->
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
          s'',
          Time.epoch )
    | C ->
        let rank_for_root, s' =
          wfq_helper s
            (State.lookup "C_weight" s)
            "C_last_finish" (Packet.len pkt) time
        in
        ([ (1, rank_for_root); (0, Rank.create 0.0 time) ], s', Time.epoch)
    | n -> failwith Printf.(sprintf "Don't know how to route flow %s." (Flow.to_string n))

  let topology = Topo.two_level_binary

  let control : Control.t =
    {
      s =
        State.create 8
        |> State.rebind "AB_weight" 0.8
        |> State.rebind "A_weight" 0.75
        |> State.rebind "B_weight" 0.25
        |> State.rebind "C_weight" 0.2;
      q = Pieotree.create topology;
      z_in = scheduling_transaction;
      z_out = fun x -> fun _ -> x;
    }

  let simulate sim_length pkts =
    Control.simulate sim_length 0.001 poprate pkts control
end

module TwoPol_Ternary : Alg_t = struct
  let scheduling_transaction s pkt =
    let time = Packet.time pkt in
    let flow = Packet.find_flow pkt in
    (* This is either A, B, C, D, or E.
       When computing ranks for the root, we arbitrate between A, B, or CDE.
       When computing ranks for the right node, we arbitrate between C, D, or E.
    *)
    match flow with
    | A ->
        let rank_for_root, s' =
          wfq_helper s
            (State.lookup "A_weight" s)
            "A_last_finish" (Packet.len pkt) time
        in
        ([ (0, rank_for_root); (0, Rank.create 0.0 time) ], s', Time.epoch)
    | B ->
        let rank_for_root, s' =
          wfq_helper s
            (State.lookup "B_weight" s)
            "B_last_finish" (Packet.len pkt) time
        in
        ([ (1, rank_for_root); (0, Rank.create 0.0 time) ], s', Time.epoch)
    | C | D | E ->
        let rank_for_root, s' =
          wfq_helper s
            (State.lookup "CDE_weight" s)
            "CDE_last_finish" (Packet.len pkt) time
        in
        let int_for_right, rank_for_right =
          match flow with
          (* We want C to go to the right node's 0th child,
             D to the 1st child, and E to the 2nd child.
             Futher, we want to prioritize E over D and D over C.
          *)
          | C -> (0, 2.0)
          | D -> (1, 1.0)
          | E -> (2, 0.0)
          | _ -> failwith "Impossible."
        in
        ( [
            (2, rank_for_root);
            (int_for_right, Rank.create rank_for_right time);
            (0, Rank.create 0.0 time);
          ],
          s', 
          Time.epoch )
    | n -> failwith Printf.(sprintf "Don't know how to route flow %s." (Flow.to_string n))

  let topology = Topo.two_level_ternary

  let control : Control.t =
    {
      s =
        State.create 6
        |> State.rebind "A_weight" 0.1
        |> State.rebind "B_weight" 0.1
        |> State.rebind "CDE_weight" 0.8;
      q = Pieotree.create topology;
      z_in = scheduling_transaction;
      z_out = fun x -> fun _ -> x;
    }

  let simulate sim_length pkts =
    Control.simulate sim_length 0.001 poprate pkts control
end

module ThreePol_Ternary : Alg_t = struct
  let scheduling_transaction s pkt =
    let time = Packet.time pkt in
    let flow = Packet.find_flow pkt in
    (* This is either A, B, C, D, E, F, or G.
       When computing ranks for the root, we arbitrate between A, B, or CDEFG.
       When computing ranks for the right node, we arbitrate between C, D, or EFG.
       When computing ranks for the right node's right node, we arbitrate between E, F, or G.
    *)
    match flow with
    | A ->
        let rank_for_root, s' =
          wfq_helper s
            (State.lookup "A_weight" s)
            "A_last_finish" (Packet.len pkt) time
        in
        ([ (0, rank_for_root); (0, Rank.create 0.0 time) ], s', Time.epoch)
    | B ->
        let rank_for_root, s' =
          wfq_helper s
            (State.lookup "B_weight" s)
            "B_last_finish" (Packet.len pkt) time
        in
        ([ (1, rank_for_root); (0, Rank.create 0.0 time) ], s', Time.epoch)
    (* In addition to WFQ at the root,
       we must, at the right node, do round-robin between C, D, and EFG. *)
    | C ->
        let rank_for_root, s' =
          wfq_helper s
            (State.lookup "CDEFG_weight" s)
            "CDEFG_last_finish" (Packet.len pkt) time
        in
        let rank_for_right, s'' =
          let r =
            if State.isdefined "C_last_finish" s' then
              max (Time.to_float time) (State.lookup "C_last_finish" s)
            else Time.to_float time
          in
          let new_state =
            State.rebind "C_last_finish" (r +. (100.0 /. 0.33)) s'
          in
          (Rank.create r time, new_state)
        in
        ( [ (2, rank_for_root); (0, rank_for_right); (0, Rank.create 0.0 time) ],
          s'',
          Time.epoch )
    | D ->
        let rank_for_root, s' =
          wfq_helper s
            (State.lookup "CDEFG_weight" s)
            "CDEFG_last_finish" (Packet.len pkt) time
        in
        let rank_for_right, s'' =
          let r =
            if State.isdefined "D_last_finish" s' then
              max (Time.to_float time) (State.lookup "D_last_finish" s)
            else Time.to_float time
          in
          let new_state =
            State.rebind "D_last_finish" (r +. (100.0 /. 0.33)) s'
          in
          (Rank.create r time, new_state)
        in
        ( [ (2, rank_for_root); (1, rank_for_right); (0, Rank.create 0.0 time) ],
          s'',
          Time.epoch )
    | E | F | G ->
        (* In addition to WFQ at the root and round-robin at the right node,
           we must do WFQ between E, F, and G at the right node's right node. *)
        let rank_for_root, s' =
          wfq_helper s
            (State.lookup "CDEFG_weight" s)
            "CDEFG_last_finish" (Packet.len pkt) time
        in
        let rank_for_right, s'' =
          let r =
            if State.isdefined "EFG_last_finish" s' then
              max (Time.to_float time) (State.lookup "EFG_last_finish" s)
            else Time.to_float time
          in
          let new_state =
            State.rebind "EFG_last_finish" (r +. (100.0 /. 0.33)) s'
          in
          (Rank.create r time, new_state)
        in
        let rank_for_right_right, s''' =
          wfq_helper s''
            (State.lookup (Printf.sprintf "%s_weight" (Flow.to_string flow)) s'')
            (Printf.sprintf "%s_last_finish" (Flow.to_string flow))
            (Packet.len pkt) time
        in
        let int_for_right_right =
          match flow with
          | E -> 0
          | F -> 1
          | G -> 2
          | _ -> failwith "Impossible."
        in
        ( [
            (2, rank_for_root);
            (2, rank_for_right);
            (int_for_right_right, rank_for_right_right);
            (0, Rank.create 0.0 time);
          ],
          s''',
          Time.epoch )

  let topology = Topo.three_level_ternary

  let control : Control.t =
    {
      s =
        State.create 6
        |> State.rebind "A_weight" 0.4
        |> State.rebind "B_weight" 0.4
        |> State.rebind "CDEFG_weight" 0.2
        |> State.rebind "E_weight" 0.1
        |> State.rebind "F_weight" 0.4
        |> State.rebind "G_weight" 0.5;
      q = Pieotree.create topology;
      z_in = scheduling_transaction;
      z_out = fun x -> fun _ -> x;
    }

  let simulate sim_length pkts =
    Control.simulate sim_length 0.001 poprate pkts control
end

module Alg2B (Alg : Alg_t) : Alg_t = struct
  (* We are given an algorithm of type Alg_t that is runs on a heterogenous tree.
     We will compile it to run on a binary tree.

     The following things about the original Alg_t are exposed:
     - topology, the bare tree that it builds a PIFO tree on
     - control, consisting of:
       + the initial state s
       + the PIFO tree q that is built from the topology
       + the scheduling transaction z.
         Given some state s and some packet pkt, z returns a pair of
         * a path pt
         * a new state s'
       - simulate, which we will not use.

     We proceed as follows:
     - We build a new binary topology that can accommodate the original ternary topology.
     - We build the embedding map f that maps addresses over the ternary topology those over the binary topology.
     - We lift f to get a map f-tilde, which maps paths over the ternary tree to paths over the binary tree.
     - From the scheduling transaction z we get a new scheduling transaction z':
       Given some state s and a packet pkt,
       z' returns pair of
       + a path: f-tilde pt
       + a new state: s'
       where pt and s' are gotten by running z s pkt.
  *)
  let topology, f = Topo.build_binary Alg.topology
  let f_tilde = Topo.lift_tilde f Alg.topology

  let z_in' s pkt =
    let pt, s', ts = Alg.control.z_in s pkt in
    (f_tilde pt, s', ts)

  let control : Control.t =
    { 
      s = Alg.control.s; 
      q = Pieotree.create topology; 
      z_in = z_in'; 
      z_out = Alg.control.z_out;
    }

  let simulate sim_length pkts =
    Control.simulate sim_length 0.001 poprate pkts control
end

module FCFS_Ternary_Bin = Alg2B (FCFS_Ternary)
module Strict_Ternary_Bin = Alg2B (Strict_Ternary)
module RRobin_Ternary_Bin = Alg2B (RRobin_Ternary)
module WFQ_Ternary_Bin = Alg2B (WFQ_Ternary)
module TwoPol_Ternary_Bin = Alg2B (TwoPol_Ternary)
module ThreePol_Ternary_Bin = Alg2B (ThreePol_Ternary)

(*************)
(* EXTENSION *)
(*************)

module Extension_Flat : Alg_t = struct
  (* This is a simple modification of FCFS_Ternary.
     We will mark the changes that we have made with comments.
     There are only two.
  *)
  let scheduling_transaction (s : State.t) pkt =
    let time = Packet.time pkt in
    match Packet.find_flow pkt with
    | A -> ([ (0, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s, Time.epoch)
    | B -> ([ (1, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s, Time.epoch)
    | C -> ([ (2, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s, Time.epoch)
    | D -> ([ (3, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s, Time.epoch) (* new *)
    | n -> failwith Printf.(sprintf "Don't know how to route flow %s." (Flow.to_string n))

  let topology = Topo.flat_four (* changed *)

  let control : Control.t =
    {
      s = State.create 1;
      q = Pieotree.create topology;
      z_in = scheduling_transaction;
      z_out = fun x -> fun _ -> x;
    }

  let simulate sim_length pkts =
    Control.simulate sim_length 0.001 poprate pkts control
end

module Alg2T (Alg : Alg_t) : Alg_t = struct
  (* We are given an algorithm Alg that runs on a heterogenous tree.
     We will compile it to run on a ternary tree.
     The process is really very similar to Alg2B above.
  *)

  let topology, f = Topo.build_ternary Alg.topology
  (*                ^^^^^^^^^^^^^^^^^^
                     The only change!
  *)
  let f_tilde = Topo.lift_tilde f Alg.topology

  let z_in' s pkt =
    let pt, s', ts = Alg.control.z_in s pkt in
    (f_tilde pt, s', ts)

  let control : Control.t =
    { 
      s = Alg.control.s; 
      q = Pieotree.create topology; 
      z_in = z_in';
      z_out = Alg.control.z_out;
    }

  let simulate sim_length pkts =
    Control.simulate sim_length 0.001 poprate pkts control
end

module Extension_Ternary = Alg2T (Extension_Flat)

(*************)
(*    NWC    *)
(*************)

module Shifted_FCFS_Ternary : Alg_t = struct
  (* FCFS but every packet is allowed to be popped 5 seconds after arrival. *)
  let scheduling_transaction (s : State.t) pkt =
    let time = Packet.time pkt in
    let shift = Time.add_float time 5.0 in
    match Packet.find_flow pkt with
    | A -> ([ (0, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s, shift)
    | B -> ([ (1, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s, shift)
    | C -> ([ (2, Rank.create 0.0 time); (0, Rank.create 0.0 time) ], s, shift)
    | n -> failwith Printf.(sprintf "Don't know how to route flow %s." (Flow.to_string n))

  let topology = Topo.one_level_ternary

  let control : Control.t =
    {
      s = State.create 1;
      q = Pieotree.create topology;
      z_in = scheduling_transaction;
      z_out = fun x -> fun _ -> x;
    }

  let simulate sim_length pkts =
    Control.simulate sim_length 0.001 poprate pkts control
end

module Rate_Limit_WFQ_Quaternary : Alg_t = struct
  (* WFQ but flow C is rate limited to 78 bytes/s. *)
  let scheduling_transaction s pkt =
    let time = Packet.time pkt in
    let flow = Packet.find_flow pkt in
    let var_last_finish = Printf.sprintf "%s_last_finish" (Flow.to_string flow) in
    let var_weight = Printf.sprintf "%s_weight" (Flow.to_string flow) in
    let weight = State.lookup var_weight s in
    let rank_for_root, s' =
      wfq_helper s weight var_last_finish (Packet.len pkt) time
    in
    let ts, s'' = 
      if flow <> C then 
        Time.epoch, s'
      else
        let c_ts = State.lookup "C_next_ts" s' in
        let throttle = State.lookup "C_throttle" s' in
        Time.of_float c_ts, 
        State.rebind "C_next_ts" (Float.ceil (Packet.len pkt /. throttle) +. c_ts) s'
    in 
    let int_for_root =
      match flow with
      | A -> 0
      | B -> 1
      | C -> 2
      | D -> 3
      | n -> failwith Printf.(sprintf "Don't know how to route flow %s." (Flow.to_string n))
    in
    ([ (int_for_root, rank_for_root); (0, Rank.create 0.0 time) ], s'', ts)

  let topology = Topo.one_level_quaternary

  let init_state =
    State.create 6
    |> State.rebind "A_weight" 0.1
    |> State.rebind "B_weight" 0.2
    |> State.rebind "C_weight" 0.3
    |> State.rebind "D_weight" 0.4
    |> State.rebind "C_next_ts" 0.0
    |> State.rebind "C_throttle" 78.0

  let control : Control.t =
    { 
      s = init_state; 
      q = Pieotree.create topology; 
      z_in = scheduling_transaction;
      z_out = fun x -> fun _ -> x;
    }

  let simulate sim_length pkts =
    Control.simulate sim_length 0.001 poprate pkts control
end

module Shifted_FCFS_Ternary_Bin = Alg2B (Shifted_FCFS_Ternary) 
module Rate_Limit_WFQ_Quaternary_Bin = Alg2B (Rate_Limit_WFQ_Quaternary)
module Rate_Limit_WFQ_Quaternary_Ternary = Alg2T (Rate_Limit_WFQ_Quaternary)

open Pifotrees_lib
open Alg
open Run

let simulate_handwritten () =
  run FCFS_Ternary.simulate fcfs_flow "fcfs";
  run Shifted_FCFS_Ternary.simulate fcfs_flow "shifted_fcfs";
  run Strict_Ternary.simulate strict_flow "strict";
  run RRobin_Ternary.simulate two_then_three "rr";
  run WFQ_Ternary.simulate wfq_flow "wfq";
  run HPFQ_Binary.simulate two_then_three "hpfq";
  run TwoPol_Ternary.simulate five_flows "twopol";
  run ThreePol_Ternary.simulate seven_flows "threepol";
  run Rate_Limit_WFQ_Quaternary.simulate four_flows_diff_size "rate_limit_wfq"


let _ = simulate_handwritten ()
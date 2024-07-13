open Pifotrees_lib
open Alg
open Run

let simulate_ternary () =
  run 
    Rate_Limit_WFQ_Quaternary_Ternary.simulate 
    four_flows_diff_size 
    "rate_limit_wfq_ternary"

let _ = simulate_ternary ()


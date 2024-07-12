type 'a t = { heap : 'a Fheap.t; cmp : 'a -> 'a -> int }

let (let*) = Option.bind

let wrap cmp heap = { heap; cmp }
let tuple_wrap cmp (a, heap) = (a, wrap cmp heap)
let tuple_wrap_opt cmp = function
  | Some tup -> Some (tuple_wrap cmp tup)
  | None -> None

let create cmp = (Fheap.create ~compare:cmp) |> wrap cmp
let push t v = Fheap.add t.heap v |> wrap t.cmp
let peek t = Fheap.top t.heap
let top_exn t = Fheap.top_exn t.heap
let pop t = Fheap.pop t.heap |> tuple_wrap_opt t.cmp

let pop_if t f = Fheap.pop_if t.heap f |> tuple_wrap_opt t.cmp
let pop_exn t = Fheap.pop_exn t.heap |> tuple_wrap t.cmp
let is_empty t = Fheap.is_empty t.heap
let length t = Fheap.length t.heap
let of_list l cmp = wrap cmp (Fheap.of_list l ~compare:cmp)
let count t f = Fheap.count t.heap ~f

let filter t f = 
  (* Filter out elements in the PIFO that fail to satisfy f. *)
  Fheap.to_list t.heap |> List.filter f |> fun x -> of_list x t.cmp

let flush t =
  (* Pop the PIFO repeatedly until it is empty.
     Return a list of its elements in the order they were popped. 
  *)
  let rec helper acc =
    match peek t with
    | None -> List.rev acc
    | Some x ->
        ignore (pop_exn t);
        helper (x :: acc)
  in
  helper []

let pop_pred t f = 
  (* Pop the least element in the PIFO that satisfies f. *)
  let* v = Fheap.find t.heap ~f in
  let init = Fheap.create ~compare:t.cmp in
  let f acc x = if x = v then acc else Fheap.add acc x in
  Some (v, (Fheap.fold t.heap ~init ~f) |> wrap t.cmp)

let flush_pred t f = 
  (* Pop [filter f t] repeatedly until it is empty.
     Return a list of its elements in the order they were popped. 
  *)
  (filter t f) |> flush 

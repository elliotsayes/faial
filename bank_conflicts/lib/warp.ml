open Protocols
module IntMap = Stage0.Common.IntMap
module Task = Transaction.Task

module TransactionMap = struct

  type t = Transaction.t IntMap.t

  let max (m:t) : Transaction.t =
    IntMap.fold
      (fun _ tsx1 tsx2 -> Transaction.max tsx1 tsx2) m Transaction.zero

  let count (w:t) : int =
    IntMap.cardinal w

  let max_transaction_count (w:t) : int =
    max w |> Transaction.count

  let to_string (w:t) : string =
    max w |> Transaction.to_string

  let make
    (to_transaction_id: Task.t -> int)
    (indices:int array)
    (enabled:bool array)
    (tids:Dim3.t array)
  :
    t
  =
    let tid_idx = Array.combine tids indices in
    Array.combine tid_idx enabled
    |> Array.fold_left (fun res ((id, index), enabled) ->
        if enabled then
          let task = Task.{id; index} in
          let bid = to_transaction_id task in
          IntMap.update bid
            (fun o ->
              Some (
                let m =
                  match o with
                  | Some m -> m
                  | None -> Transaction.make bid
                in
                Transaction.add task m
              )
            )
            res
        else
          res
      ) IntMap.empty

end

let bank_conflicts
  (bank_count:int)
  (indices:int array)
  (enabled:bool array)
  (tids:Dim3.t array)
:
  Cost.t
=
  let to_bid (tsk:Task.t) : int =
    Stage0.Common.modulo tsk.index bank_count
  in
  let w = TransactionMap.make to_bid indices enabled tids in
  let state = TransactionMap.max w in
  Cost.make ((Transaction.count state) - 1) state

let uncoalesced
  (indices:int array)
  (enabled:bool array)
  (tids:Dim3.t array)
:
  Cost.t
=
  let warp_count = Array.length tids in
  let min_index =
    Array.combine indices enabled
    |> Array.fold_left (fun (min_idx:int) (index, enabled) ->
        if enabled && index >= 0 then
          min min_idx index
        else
          min_idx
      ) 0
  in
  let tsx_map =
    let to_tsx_id (tsk:Task.t) : int =
      (tsk.index - min_index) / warp_count
    in
    TransactionMap.make to_tsx_id indices enabled tids
  in
  let state =
    tsx_map
    |> IntMap.bindings
    |> List.map (fun (_, e) -> Transaction.choose e)
    |> Transaction.from_list 0
  in
  Cost.make (IntMap.cardinal tsx_map) state

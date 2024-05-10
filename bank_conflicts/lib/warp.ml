open Protocols
module IntMap = Stage0.Common.IntMap
module Task = Transaction.Task

type t = Transaction.t IntMap.t

let max (m:t) : Transaction.t =
  IntMap.fold
    (fun _ tsx1 tsx2 -> Transaction.max tsx1 tsx2) m Transaction.zero

let transaction_count (w:t) : int =
  max w |> Transaction.count

let to_string (w:t) : string =
  max w |> Transaction.to_string

let make
  (bank_count:int)
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
        let bid = Stage0.Common.modulo index bank_count in
        let task = Task.{id; index} in
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

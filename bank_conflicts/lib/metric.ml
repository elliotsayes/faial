type t =
  | BankConflicts
  | UncoalescedAccesses
  | CountAccesses

let to_string : t -> string =
  function
  | BankConflicts -> "bc"
  | UncoalescedAccesses -> "ua"
  | CountAccesses -> "count"

let values : t list =
  [ BankConflicts; UncoalescedAccesses; CountAccesses ]

let choices : (string * t) list =
  values
  |> List.map (fun x -> (to_string x, x))

let min_uncoalesced_accesses : int =
  1

let min_bank_conflicts : int = 0

let max_count_accesses : int = 1

let max_uncoalesced_accesses ~thread_count : int =
  assert (thread_count >= 0);
  thread_count

let max_bank_conflicts ~thread_count ~bank_count : int =
  assert (thread_count >= 0 && bank_count >= 0);
  (* calculate the maximum number of transactions *)
  let max_transactions = min thread_count bank_count in
  (* don't return negative numbers *)
  max (max_transactions - 1) 0

let max_cost ~thread_count ~bank_count : t -> int =
  function
  | BankConflicts ->
    max_bank_conflicts ~thread_count ~bank_count
  | UncoalescedAccesses ->
    max_uncoalesced_accesses ~thread_count
  | CountAccesses -> max_count_accesses

let max_cost_from (cfg:Config.t) : t -> int =
  max_cost ~thread_count:cfg.threads_per_warp ~bank_count:cfg.bank_count

let min_cost (m:t) : int =
  (match m with
  | BankConflicts -> min_bank_conflicts
  | UncoalescedAccesses -> min_uncoalesced_accesses
  | CountAccesses -> 1)

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

let max_uncoalesced_accesses (cfg:Config.t) : int =
  cfg.threads_per_warp

let max_bank_conflicts (cfg:Config.t) : int =
  (min cfg.threads_per_warp cfg.bank_count) - 1

let max_cost (cfg:Config.t) (m:t) : int =
  (match m with
  | BankConflicts -> max_bank_conflicts cfg
  | UncoalescedAccesses -> max_uncoalesced_accesses cfg
  | CountAccesses -> 1)

let min_cost (m:t) : int =
  (match m with
  | BankConflicts -> 0
  | UncoalescedAccesses -> 1
  | CountAccesses -> 1)

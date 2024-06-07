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

let max_cost (cfg:Config.t) (m:t) : Cost.t =
  (match m with
  | BankConflicts -> (min cfg.threads_per_warp cfg.bank_count) - 1
  | UncoalescedAccesses -> cfg.threads_per_warp
  | CountAccesses -> 1)
  |> Cost.from_int

let min_cost (m:t) : Cost.t =
  (match m with
  | BankConflicts -> 0
  | UncoalescedAccesses -> 4
  | CountAccesses -> 1)
  |> Cost.from_int

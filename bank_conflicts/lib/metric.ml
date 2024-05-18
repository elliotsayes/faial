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
  | BankConflicts -> min cfg.warp_count cfg.bank_count
  | UncoalescedAccesses -> cfg.warp_count
  | CountAccesses -> 1)
  |> Cost.from_int

let min_cost (m:t) : Cost.t =
  (match m with
  | BankConflicts -> 0
  | UncoalescedAccesses -> 4
  | CountAccesses -> 1)
  |> Cost.from_int

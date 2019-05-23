open Proto

let rec free_names_nexp e fns =
  match e with
  | Num _ -> fns
  | Var x -> x::fns
  | Bin (_, e1, e2) ->
    free_names_nexp e1 fns |> free_names_nexp e2

let rec free_names_bexp e fns =
  match e with
  | Bool _ -> fns
  | NRel (_, n1, n2) -> free_names_nexp n1 fns |> free_names_nexp n2
  | BRel (_, b1, b2) -> free_names_bexp b1 fns |> free_names_bexp b2
  | BNot b -> free_names_bexp b fns

let free_names_set s fns =
  free_names_nexp s.set_elem fns
  |> free_names_nexp s.set_upper_bound
  |> free_names_bexp s.set_cond

let free_names_access a = free_names_set a.access_set

let free_names_timed t fns : string list =
  free_names_nexp t.timed_phase fns |> free_names_access t.timed_data

let free_names_owned o = free_names_timed o.owned_data

let free_names_list_owned  (l: access_t list) : string list =
  List.fold_right free_names_owned l []

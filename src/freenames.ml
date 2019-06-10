open Proto
open Common

let rec free_names_nexp e (fns:StringSet.t) =
  match e with
  | Num _ -> fns
  | Proj (_, n) -> free_names_nexp n fns
  | Var x -> StringSet.add x fns
  | Bin (_, e1, e2) ->
    free_names_nexp e1 fns |> free_names_nexp e2

let rec free_names_bexp e fns =
  match e with
  | Pred (_, x) -> StringSet.add x fns
  | Bool _ -> fns
  | NRel (_, n1, n2) -> free_names_nexp n1 fns |> free_names_nexp n2
  | BRel (_, b1, b2) -> free_names_bexp b1 fns |> free_names_bexp b2
  | BNot b -> free_names_bexp b fns

let free_names_range r = free_names_nexp r.range_upper_bound

let free_names_access a fns =
  free_names_nexp a.access_index fns |> free_names_bexp a.access_cond

let free_names_timed t fns : StringSet.t =
  free_names_nexp t.timed_phase fns |> free_names_access t.timed_data

let free_names_list f l fns =
  List.fold_right f l fns

let rec free_names_proto p fns =
  match p with
  | Sync
  | Skip -> fns
  | Assert b -> free_names_bexp b fns
  | Acc (_, a) ->
    free_names_nexp a.access_index fns
      |> free_names_bexp a.access_cond
  | Seq (p1, p2) ->
    free_names_proto p1 fns
      |> free_names_proto p2
  | Loop (r, p) ->
    free_names_proto p fns
      |> StringSet.remove r.range_var
      |> StringSet.union (free_names_nexp r.range_upper_bound fns)

let rec free_locs_proto p fns =
  match p with
  | Assert _
  | Sync
  | Skip -> fns
  | Acc (x, _) -> StringSet.add x fns
  | Seq (p1, p2) ->
    free_locs_proto p1 fns
      |> free_locs_proto p2
  | Loop (_, p) -> free_locs_proto p fns

open Proto
open Common

let rec fold_nexp f e a =
  match e with
  | Num _ -> a
  | Proj (_, n) -> fold_nexp f n a
  | Var x -> f x a
  | Bin (_, e1, e2) -> fold_nexp f e1 a |> fold_nexp f e2

let rec fold_bexp f e a =
  match e with
  | Pred (_, x) -> f x a
  | Bool _ -> a
  | NRel (_, n1, n2) -> fold_nexp f n1 a |> fold_nexp f n2
  | BRel (_, b1, b2) -> fold_bexp f b1 a |> fold_bexp f b2
  | BNot b -> fold_bexp f b a

let free_names_nexp e (fns:VarSet.t) = fold_nexp VarSet.add e fns

let free_names_bexp e fns = fold_bexp VarSet.add e fns

let free_names_range r = free_names_nexp r.range_upper_bound

let free_names_access a fns =
  List.fold_right free_names_nexp a.access_index fns
    |> free_names_bexp a.access_cond

let free_names_timed t fns : VarSet.t =
  free_names_nexp t.timed_phase fns |> free_names_access t.timed_data

let free_names_list f l fns =
  List.fold_right f l fns

let rec free_names_proto p fns =
  match p with
  | Sync
  | Skip -> fns
  | Assert b -> free_names_bexp b fns
  | Acc (_, a) -> free_names_access a fns
  | Seq (p1, p2) ->
    free_names_proto p1 fns
      |> free_names_proto p2
  | Loop (r, p) ->
    free_names_proto p fns
      |> VarSet.remove r.range_var
      |> VarSet.union (free_names_nexp r.range_upper_bound fns)

let rec free_locs_proto p fns =
  match p with
  | Assert _
  | Sync
  | Skip -> fns
  | Acc (x, _) -> VarSet.add x fns
  | Seq (p1, p2) ->
    free_locs_proto p1 fns
      |> free_locs_proto p2
  | Loop (_, p) -> free_locs_proto p fns

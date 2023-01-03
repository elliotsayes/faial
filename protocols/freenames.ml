open Exp

let rec fold_nexp f e a =
  match e with
  | Num _ -> a
  | Proj (_, x) -> f x a
  | Var x -> f x a
  | Bin (_, e1, e2) -> fold_nexp f e1 a |> fold_nexp f e2
  | NIf (b, e1, e2) -> fold_bexp f b a |> fold_nexp f e1 |> fold_nexp f e2
  | NCall (_, e) -> fold_nexp f e a

and fold_bexp f e a =
  match e with
  | Pred (_, n) -> fold_nexp f n a
  | Bool _ -> a
  | NRel (_, n1, n2) -> fold_nexp f n1 a |> fold_nexp f n2
  | BRel (_, b1, b2) -> fold_bexp f b1 a |> fold_bexp f b2
  | BNot b -> fold_bexp f b a

let free_names_nexp e (fns:Variable.Set.t) = fold_nexp Variable.Set.add e fns

let is_closed_nexp e : bool =
  free_names_nexp e Variable.Set.empty
  |> Variable.Set.is_empty

let free_names_bexp e fns = fold_bexp Variable.Set.add e fns

let free_names_range (r:Range.t) (fns:Variable.Set.t) : Variable.Set.t =
  free_names_nexp r.lower_bound fns |> free_names_nexp r.upper_bound

let free_names_access a fns =
  let open Access in
  List.fold_right free_names_nexp a.index fns

let free_names_list f l fns =
  List.fold_right f l fns

let rec free_names_inst (i:Proto.inst) (fns:Variable.Set.t) : Variable.Set.t =
  let open Proto in
  match i with
  | Sync -> fns
  | Acc (_, a) -> free_names_access a fns
  | Cond (b, p1) ->
    free_names_bexp b fns
    |> free_names_proto p1
  | Loop (r, p) ->
    free_names_proto p fns
    |> Variable.Set.remove r.var
    |> Variable.Set.union (free_names_nexp r.upper_bound fns)

and free_names_proto (p:Proto.prog) (fns:Variable.Set.t) : Variable.Set.t =
  free_names_list free_names_inst p fns

let rec free_locs_inst (i:Proto.inst) (fns:Variable.Set.t) =
  let open Proto in
  match i with
  | Sync
    -> fns
  | Acc (x, _) -> Variable.Set.add x fns
  | Cond (_, p)
  | Loop (_, p)
    -> free_locs_proto p fns

and free_locs_proto p (fns:Variable.Set.t) =
  free_names_list free_locs_inst p fns

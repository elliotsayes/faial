open Exp

let rec fold_nexp f e a =
  match e with
  | Num _ -> a
  | Var x -> f x a
  | Bin (_, e1, e2) -> fold_nexp f e1 a |> fold_nexp f e2
  | NIf (b, e1, e2) -> fold_bexp f b a |> fold_nexp f e1 |> fold_nexp f e2
  | NCall (_, e) -> fold_nexp f e a

and fold_bexp f e a =
  match e with
  | ThreadEqual n | Pred (_, n) -> fold_nexp f n a
  | Bool _ -> a
  | NRel (_, n1, n2) -> fold_nexp f n1 a |> fold_nexp f n2
  | BRel (_, b1, b2) -> fold_bexp f b1 a |> fold_bexp f b2
  | BNot b -> fold_bexp f b a

let free_names_nexp e (fns:Variable.Set.t) = fold_nexp Variable.Set.add e fns


let is_closed_nexp e : bool =
  free_names_nexp e Variable.Set.empty
  |> Variable.Set.is_empty

let mem_nexp (x:Variable.t) (e:nexp) : bool =
  free_names_nexp e Variable.Set.empty
  |> Variable.Set.mem x

let free_names_bexp e fns = fold_bexp Variable.Set.add e fns

let contains_tid_bexp e : bool =
  free_names_bexp e Variable.Set.empty
  |> Variable.contains_tids

let contains_tid_nexp e : bool =
  free_names_nexp e Variable.Set.empty
  |> Variable.contains_tids

let free_names_range (r:Range.t) (fns:Variable.Set.t) : Variable.Set.t =
  free_names_nexp r.lower_bound fns |> free_names_nexp r.upper_bound

let contains_tid_range e : bool =
  free_names_range e Variable.Set.empty
  |> Variable.contains_tids

let free_names_access a fns =
  let open Access in
  List.fold_right free_names_nexp a.index fns

let free_names_list f l fns =
  List.fold_right f l fns

let rec free_names_proto (i:Proto.Code.t) (fns:Variable.Set.t) : Variable.Set.t =
  match i with
  | Skip
  | Sync _ -> fns
  | Acc (_, a) -> free_names_access a fns
  | Cond (b, p1) ->
    free_names_bexp b fns
    |> free_names_proto p1
  | Decl (x, p) ->
    free_names_proto p fns
    |> Variable.Set.remove x
  | Loop (r, p) ->
    free_names_proto p fns
    |> Variable.Set.remove r.var
    |> Variable.Set.union (free_names_nexp r.upper_bound fns)
  | Seq (p, q) ->
    free_names_proto p fns |> free_names_proto q


let rec free_locs_proto (i:Proto.Code.t) (fns:Variable.Set.t) =
  match i with
  | Skip
  | Sync _
    -> fns
  | Acc (x, _) -> Variable.Set.add x fns
  | Decl (_, p)
  | Cond (_, p)
  | Loop (_, p)
    -> free_locs_proto p fns
  | Seq (p, q) ->
    free_locs_proto p fns |> free_locs_proto q

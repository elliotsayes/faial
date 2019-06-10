open Proto
open Common

let tid1 = ".1."
let tid2 = ".2."

let project prefix x = prefix ^ x

let tid1_s = project tid1 tid
let tid2_s = project tid2 tid

let tid1_t = Var tid1_s
let tid2_t = Var tid2_s

let do_project locals prefix =
  fun x ->
    if StringSet.mem x locals
    then Some (Var (project prefix x))
    else None

let project_access locals (t:access timed) : (access timed) * (access timed) =
  match t with
  | {timed_phase=n; timed_data=a} ->
    let mk ti =
      let si = do_project locals ti in
      {
        timed_phase=Subst.n_subst si n;
        timed_data={
          access_index = Subst.n_subst si a.access_index;
          access_mode = a.access_mode;
          access_cond = Subst.b_subst si a.access_cond;
        }
      }
    in
    mk tid1, mk tid2

(** Given an expression apply all projections in a boolean expression *)

let apply_proj locals (b:bexp) =
  let rec do_n_project (n:nexp) =
    let do_proj ti n =
      let n = do_n_project n in
      Subst.n_subst (do_project locals ti) n
    in
    match n with
    | Var _
    | Num _ -> n
    | Bin (o, n1, n2) -> Bin (o, do_n_project n1, do_n_project n2)
    | Proj (Task1, n) -> do_proj tid1 n
    | Proj (Task2, n) -> do_proj tid2 n
  in

  let rec do_b_project (b:bexp) =
    match b with
    | Pred _
    | Bool _ -> b
    | NRel (o, n1, n2) -> NRel (o, do_n_project n1, do_n_project n2)
    | BRel (o, b1, b2) -> BRel (o, do_b_project b1, do_b_project b2)
    | BNot b -> BNot (do_b_project b)
  in

  do_b_project b

let project_condition locals (b:bexp) =
  let b = apply_proj locals b in
  let locs_in_b = Freenames.free_names_bexp b StringSet.empty
    |> StringSet.inter locals
  in
  if StringSet.is_empty locs_in_b then
    [b]
  else
    let do_subst ti = Subst.b_subst (do_project locals ti) in
    [do_subst tid1 b; do_subst tid2 b]

type stream = (string * access timed) list

let project_stream locals (l:stream) : stream * stream =
  let on_elem ((l1:stream),(l2:stream)) ((x:string), (a:access timed)) : stream * stream =
    let (a1, a2) = project_access locals a in
    (x,a1)::l1, (x, a2)::l2
  in
  List.fold_left on_elem ([],[]) l

type access_2d = access_t list * access_t list

type proj_kernel = {
  proj_kernel_pre: bexp list;
  proj_kernel_vars: StringSet.t;
  proj_kernel_steps: (string, access_2d) Hashtbl.t
}

let group_assoc l =
  let groups = Hashtbl.create 0 in
  let rec iter l =
    match l with
    | (x, a)::l -> begin
      let elems =
        match Hashtbl.find_opt groups x with
        | Some elems -> elems
        | None -> []
      in
      Hashtbl.replace groups x (a::elems);
      iter l
      end
    | [] -> ()
  in
  iter l;
  groups

let project_kernel (k:Loops.flat_kernel) : proj_kernel =
  (* 1. Variables *)
  let locals = k.flat_kernel_multi_vars in
  (* 2. Pre-conditions *)
  let pre = k.flat_kernel_pre
    |> List.map (project_condition locals)
    |> List.flatten
    |> List.filter (fun x -> match x with | Bool true -> false | _ -> true)
  in
  (* 3. Steps *)
  let steps1, steps2 = project_stream locals k.flat_kernel_steps in
  let steps1, steps2 = Constfold.stream_opt steps1, Constfold.stream_opt steps2 in
  let group1 = group_assoc steps1 in
  let group2 = group_assoc steps2 in
  let result = Hashtbl.create (Hashtbl.length group1) in
  let find_or tb k d =
    match Hashtbl.find_opt tb k with
      | Some v -> v
      | None -> d
  in
  let add_result x steps1 steps2 =
    Hashtbl.add result x (steps1, steps2)
  in
  Hashtbl.iter (fun x steps1 ->
    let steps2 = find_or group2 x [] in
    Hashtbl.remove group2 x;
    add_result x steps1 steps2
  ) group1;
  Hashtbl.iter (fun x steps2 ->
    add_result x (find_or group1 x []) steps2
  ) group2;
  (* 4. Split locals *)
  let locals =
    StringSet.union
      (StringSet.map (project tid1) locals)
      (StringSet.map (project tid2) locals)
  in
  {
    proj_kernel_pre = pre;
    proj_kernel_vars = StringSet.union locals k.flat_kernel_single_vars;
    proj_kernel_steps = result;
  }



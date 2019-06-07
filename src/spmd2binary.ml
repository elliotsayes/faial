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

let project_condition locals (b:bexp) =
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

type location = {
  location_pre: bexp list;
  location_fns: StringSet.t;
  location_steps: access_t list * access_t list
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

let extract_free_names h
  : (string, StringSet.t) Hashtbl.t =
  let result = Hashtbl.create 0 in
  Hashtbl.iter (fun x elems ->
    Freenames.free_names_list Freenames.free_names_timed elems StringSet.empty |> Hashtbl.add result x
  ) h;
  result

let restrict_bexp (fns:StringSet.t) (b:bexp) : bexp =
  let simpl b =
    let new_fn = Freenames.free_names_bexp b StringSet.empty in
    if StringSet.is_empty (StringSet.inter fns new_fn) then Bool true
    else b
  in
  let rec iter b =
    match b with
    | Pred _
    | Bool _ -> b
    | BNot b -> BNot (simpl b)
    | NRel (_, _, _) -> simpl b
    | BRel (o, b1, b2) -> begin
        let b1 = iter b1 |> simpl in
        let b2 = iter b2 |> simpl in
        match b1, b2 with
        | Bool true, b | b, Bool true -> b
        | _, _ -> BRel (o, b1, b2)
      end
  in
  iter b |> Constfold.b_opt

let project_kernel (k:Loops.flat_kernel) =
  (* 3. Make the owner of each access explicit *)
  let locals = k.flat_kernel_multi_vars in
  let steps1, steps2 = project_stream locals k.flat_kernel_steps in
  let steps1, steps2 = Constfold.stream_opt steps1, Constfold.stream_opt steps2 in
  let pre = k.flat_kernel_pre
    |> List.map (project_condition locals)
    |> List.flatten
    |> List.filter (fun x -> match x with | Bool true -> false | _ -> true)
  in
  let group1 = group_assoc steps1 in
  let group2 = group_assoc steps2 in
  let fns1 = extract_free_names group1 in
  let fns2 = extract_free_names group2 in
  let result = Hashtbl.create (Hashtbl.length group1) in
  let find_or tb k d =
    match Hashtbl.find_opt tb k with
      | Some v -> v
      | None -> d
  in
  let get_fns x =
    StringSet.union
      (find_or fns1 x StringSet.empty)
      (find_or fns2 x StringSet.empty)
  in
  let add_result x steps1 steps2 =
    let fns = get_fns x in
    let pre = List.map (restrict_bexp fns) pre in
    Hashtbl.add result x {
      location_fns = List.fold_left (fun new_fns b -> Freenames.free_names_bexp b new_fns) fns pre;
      location_pre = pre;
      location_steps = (steps1, steps2)
    }
  in
  Hashtbl.iter (fun x steps1 ->
    let steps2 = find_or group2 x [] in
    Hashtbl.remove group2 x;
    add_result x steps1 steps2
  ) group1;
  Hashtbl.iter (fun x steps2 ->
    add_result x (find_or group1 x []) steps2
  ) group2;
  result


(** Groups two streams together in a single data structure *)


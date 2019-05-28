open Proto
open Sexplib
open Common

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
    Freenames.free_names_steps elems |> StringSet.of_list |> Hashtbl.add result x
  ) h;
  result

let restrict_bexp (b:bexp) (fns:StringSet.t) : bexp =
  let simpl b =
    let new_fn = Freenames.free_names_bexp b [] |> StringSet.of_list in
    if StringSet.is_empty (StringSet.inter fns new_fn) then Bool true
    else b
  in
  let rec iter b =
    match b with
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

let check (k:kernel) =
  (* 0. Make sure each loop has a synchronization step going on *)
  (* XXX *)
  (* 1. Make sure each loops as a unique variable *)
  let p = Loops.normalize_variables k.kernel_code in
  let single_vars = Loops.single_loop_variables p StringSet.empty in
  let single_vars = StringSet.union single_vars (StringSet.of_list k.kernel_variables) in
  let c = Loops.get_constraints p in
  (* 2. Flatten outer loops *)
  let steps = Loops.remove_loops p in
  (* 3. Get the local variables defined in steps *)
  let locals = List.fold_right (fun (_, t) fns -> Freenames.free_names_timed t fns) steps [] in
  let locals = locals |> StringSet.of_list in
  let locals = StringSet.diff locals single_vars in
  (* 3. Make the owner of each access explicit *)
  let steps1, steps2 = Spmd2binary.project_stream locals steps in
  let steps1, steps2 = Constfold.stream_opt steps1, Constfold.stream_opt steps2 in
  (* 4. Perform a constant-fold optimization, to reduce the problem space *)
  let c1, c2 = Spmd2binary.project_condition locals c in
  let pre_and_ x = BRel (BAnd, x, k.kernel_pre) in
  (pre_and_ c1, steps1), (pre_and_ c2, steps2)

type merged = {
  merged_pre: bexp;
  merged_fns: StringSet.t;
  merged_steps: access_t list * access_t list
}

let merge (c1, (steps1: (string * access_t) list)) (c2, steps2) =
  let pre = b_and c1 c2 in
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
    Hashtbl.add result x {
      merged_fns = fns;
      merged_pre = restrict_bexp pre fns;
      merged_steps = (steps1, steps2)
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


let join sep elems =
  let on_elem accum x =
    if String.equal accum ""
    then x
    else x ^ sep ^ accum
  in
  List.fold_left on_elem "" elems

let () =
  let print_data data =
    Hashtbl.iter (fun x m ->
      print_string "Location: ";
      print_endline x;
      (* Print the locals of each location *)
      print_string "Vars: ";
      join ", " (StringSet.elements m.merged_fns) |> print_endline;
      (* Print the pre-conditions of each location *)
      print_string ("Pre: ");
      Serialize.b_ser m.merged_pre |> Sexp.to_string_hum |> print_endline;
      (* Print the various accesses *)
      print_endline ("T1:");
      List.iter (fun o ->
        Serialize.t_ser o |> Sexp.to_string_hum |> print_endline
      ) (fst m.merged_steps);
      print_endline ("T2:");
      List.iter (fun o ->
        Serialize.t_ser o |> Sexp.to_string_hum |> print_endline
      ) (snd m.merged_steps);
      print_endline "";
    ) data;
  in
  let s : Sexp.t = Sexp.input_sexp stdin in
    try
      let d1, d2 = Parse.parse_kernel.run s |> check in
      print_data (merge d1 d2);
    with
    | Parse.ParseError l ->
      List.iter (fun x ->
        print_endline x
      ) l

open Proto
open Sexplib

module StringOT = struct
  type t = string
  let compare = Pervasives.compare
end

module StringSet = Set.Make(StringOT)

type list_owned = access timed owned list

let group_steps (l: (string *access timed owned) list) : (string, list_owned) Hashtbl.t =
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

let as_set (l:string list) : StringSet.t =
  List.fold_right StringSet.add l StringSet.empty

let extract_free_names (h: (string, list_owned) Hashtbl.t)
  : (string, StringSet.t) Hashtbl.t =
  let result = Hashtbl.create 0 in
  Hashtbl.iter (fun x elems ->
    as_set (Freenames.free_names_list_owned elems) |> Hashtbl.add result x
  ) h;
  result

let restrict_bexp (b:bexp) (fns:StringSet.t) : bexp =
  let simpl b =
    let new_fn = Freenames.free_names_bexp b [] |> as_set in
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

let check (p:proto) =
  (* 0. Make sure each loop has a synchronization step going on *)
  (* XXX *)
  (* 1. Make sure each loops as a unique variable *)
  let p = Loops.normalize_variables p in
  let c = Loops.get_constraints p in
  (* 2. Flatten outer loops *)
  let steps = Loops.remove_loops p in
  (* 3. Make the owner of each access explicit *)
  let steps = Spmd2binary.split steps in
  (* 4. Perform a constant-fold optimization, to reduce the problem space *)
  let owned_steps = Constfold.stream_opt steps in
  (* 5. Convert a flat owned-accs-program into a Hoare Logic program,
     yielding a formula *)
  (* 6. Flatten out our theory into First-Order Logic with numbers. *)
  (* 7. Generate Z3 *)
  Constfold.b_opt c, owned_steps

let join sep elems =
  let on_elem accum x =
    if String.equal accum ""
    then x
    else x ^ sep ^ accum
  in
  List.fold_left on_elem "" elems

let () =
  let s : Sexp.t = Sexp.input_sexp stdin in
    try
      let (c, elems) = Parse.parse_proto.run s |> check in
      let groups = group_steps elems in
      let fns = extract_free_names groups in
      Hashtbl.iter (fun x elems ->
        print_string "Location: ";
        print_endline x;
        (* Print the locals of each location *)
        print_string "Vars: ";
        let x_fns = Hashtbl.find fns x in
        join ", " (StringSet.elements x_fns) |> print_endline;
        (* Print the pre-conditions of each location *)
        print_string "Pre: ";
        restrict_bexp c x_fns |> Serialize.b_ser |> Sexp.to_string_hum |> print_endline;
        (* Print the various accesses *)
        List.iter (fun o ->
          Serialize.o_ser o |> Sexp.to_string_hum |> print_endline
        ) elems;
        print_endline "";
      ) groups;
    with
    | Parse.ParseError l ->
      List.iter (fun x ->
        print_endline x
      ) l

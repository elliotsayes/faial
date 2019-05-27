open Proto
open Common

let fresh_name x xs =
  let rec do_fresh_name x n =
    let name = (x ^ string_of_int n) in
    if List.mem name xs
    then do_fresh_name x (n + 1)
    else name
  in
  if List.mem x xs then do_fresh_name x 1 else x

(** Loop normalization: Makes all loop variables distinct. *)
let normalize_variables (p:proto) =
  let rec norm e xs =
    match e with
    | Loop ({range_var=x; range_upper_bound=ub}, e) ->
      if List.mem x xs then (
        let new_x = fresh_name x xs in
        let new_xs = new_x::xs in
        let do_subst = Subst.replace_by (x, Var new_x) in
        let (e, new_xs) = norm (Subst.p_subst do_subst e) new_xs in
        Loop ({range_var=new_x; range_upper_bound=ub}, e), new_xs
      ) else (
        let (e, new_xs) = norm e (x::xs) in
        Loop ({range_var=x;range_upper_bound=ub}, e), new_xs
      )
    | Seq (e1, e2) ->
      let (e1, xs) = norm e1 xs in
      let (e2, xs) = norm e2 xs in
      Seq (e1, e2), xs
    | Acc (_, _)
    | Skip
    | Sync -> e, xs
  in
  norm p [] |> fst

(** Extracts every variable declaration and how to restrict each variable.*)
let get_constraints (p:proto) : bexp =
  let rec iter p b =
    match p with
    | Skip
    | Sync
    | Acc _ -> b
    | Loop (r, p) ->
      let b2 = (NRel (NLt, Var r.range_var, r.range_upper_bound)) in
      iter p (BRel (BAnd, b, b2))
    | Seq (p1, p2) ->
      iter p2 (iter p1 b)
  in
  iter p (Bool true)

let rec does_sync (p:proto) : bool =
  match p with
  | Skip
  | Loop _
  | Acc _
    -> false
  | Sync -> true
  | Seq (p1, p2) -> does_sync p1 || does_sync p2

let rec single_loop_variables (p:proto) (s:StringSet.t) : StringSet.t =
  match p with
  | Acc _
  | Sync
  | Skip -> s
  | Loop (r, p) ->
    let s = if does_sync p then StringSet.add r.range_var s else s in
    single_loop_variables p s
  | Seq (p1, p2) ->
    single_loop_variables p1 s |> single_loop_variables p2

let pexp_to_nexp (ubs:(string,nexp) Hashtbl.t) (e:Phaseord.exp) : Proto.nexp =
  let rec trans e =
    match e with
    | Phaseord.Num n -> Proto.Num n
    | Phaseord.Add (e1, e2) -> Proto.Bin (Proto.Plus, trans e1, trans e2)
    | Phaseord.Mult (e1, e2) -> Proto.Bin (Proto.Mult, trans e1, trans e2)
    | Phaseord.Var x -> begin
      match Hashtbl.find_opt ubs x with
      | Some n -> n
      | None -> Proto.Var x
    end
  in
  trans e

(** Flatten out the loops of a protocol. *)
let remove_loops (e:Proto.proto) : (string * access timed) list =
  let ids : (string,nexp) Hashtbl.t = Hashtbl.create 0 in
  let gen_id e () =
    let key = "$ub" ^ string_of_int (Hashtbl.length ids) in
    Hashtbl.add ids key e;
    key
  in
  let rec trans e =
    match e with
    | Proto.Skip ->
      Phaseord.Skip
    | Proto.Seq (e1, e2) ->
      Phaseord.Seq (trans e1, trans e2)
    | Proto.Sync ->
      Phaseord.Sync
    | Proto.Acc (x,a) -> Phaseord.Step (x,a)
    | Proto.Loop ({range_var = var; range_upper_bound = ub}, e) ->
      let new_ub = gen_id ub () in
      Phaseord.Loop (var, Var new_ub, trans e)
  in
  let steps = trans e |> Phaseord.extract_steps in
  (* Each step pairs a phase of type Phase.exp with accesses *)
  (* We now need to convert each Phase.exp into a Proto.nexp *)
  let mk_timed (n, (x, y)) =
    (x, {timed_phase=pexp_to_nexp ids n;timed_data=y})
  in
  List.map mk_timed steps

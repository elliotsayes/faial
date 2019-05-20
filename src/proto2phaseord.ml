open Proto
open Phaseord

type acc = Proto.variable * Proto.access

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

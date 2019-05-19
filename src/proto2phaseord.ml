open Proto
open Phaseord

type acc = Proto.variable * Proto.access
type lang = acc L0.t


let proto_to_phaseord (e:Proto.proto) =
  let ids = Hashtbl.create 0 in
  let to_list h =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []
  in
  let gen_id e () =
    let key = "$ub" ^ string_of_int (Hashtbl.length ids) in
    Hashtbl.add ids key e;
    key
  in
  let rec trans e =
    match e with
    | Proto.Skip ->
      L0.Skip
    | Proto.Seq (e1, e2) ->
      L0.Seq (trans e1, trans e2)
    | Proto.Sync ->
      L0.Sync
    | Proto.Acc (x,a) -> L0.Step (x,a)
    | Proto.Loop ({range_var = var; range_upper_bound = ub}, e) ->
      let new_ub = gen_id ub () in
      L0.Loop (var, Var new_ub, trans e)
  in
  to_list ids, trans e |> Phaseord.flatten


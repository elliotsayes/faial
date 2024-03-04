type t = C_type.t Variable.Map.t

let empty = Variable.Map.empty
let union_left = Variable.MapUtil.union_left
let union_right = Variable.MapUtil.union_right
let remove_all (s:Variable.Set.t) (m:t) : t =
  Variable.Set.fold Variable.Map.remove s m

let add = Variable.Map.add

let from_set (ty:C_type.t) (s:Variable.Set.t) : t =
  s
  |> Variable.Set.to_seq
  |> Seq.map (fun x -> (x, ty))
  |> Variable.Map.of_seq

let mem = Variable.Map.mem

let from_list (l:(Variable.t * C_type.t) list) : t =
  Variable.MapUtil.from_list l

let to_set (m:t) : Variable.Set.t =
  Variable.MapSetUtil.map_to_set m

let to_string (m:t) : string =
  m
  |> Variable.Map.bindings
  |> List.sort (fun k1 k2 -> Variable.compare (fst k1) (fst k2))
  |> List.map (fun (k, v) -> C_type.to_string v ^ " " ^ Variable.name k)
  |> String.concat ", "
  |> fun l -> "[" ^ l ^ "]"

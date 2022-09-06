type t = {name: string; location: Location.t option}

let make ~name ~location : t = {name=name; location=Some location}

let from_name (name:string) : t = {name=name; location=None}

let set_name (name:string) (v:t) =
  { v with name = name }

let set_location (location:Location.t) (v:t) : t =
  { v with location = Some location}

let clear_location (v:t) = {name = v.name; location = None}

let name (x:t) : string = x.name

let location_opt (x:t) : Location.t option = x.location

let location (x:t) : Location.t =
  location_opt x |> Option.value ~default:Location.empty

let equal (x:t) (y:t) = String.equal x.name y.name

let repr (x:t) : string =
  let l = Option.map Location.repr x.location |> Option.value ~default:"null" in
  "{name=\"" ^ x.name ^ "\", location="^ l ^ "}"

module OT = struct
  type t' = t
  type t = t'
  let compare = fun x y -> Stdlib.compare x.name y.name
end

module Set = Set.Make(OT)
module Map = Map.Make(OT)
module MapUtil = Common.MapUtil(Map)
module MapSetUtil = Common.MapSetUtil (Set) (Map)

(*
let var_set_to_map (s:VarSet.t) (f:variable -> 'a option) : 'a VarMap.t =
  VarSet.fold (fun k m ->
    match f k with
    | Some v -> VarMap.add k v m
    | None -> m
  ) s VarMap.empty
let var_map_to_set (m:'a VarMap.t) : VarSet.t =
  VarMap.bindings m |> List.map fst |> VarSet.of_list

let list_to_var_map (l:(variable * 'a) list) : 'a VarMap.t =
  List.fold_left (fun m (k,v) ->
    VarMap.add k v m
  ) VarMap.empty l
*)

open Stage0

module Hierarchy = struct
  type t =
    | SharedMemory
    | GlobalMemory

  let to_string : t -> string =
    function
    | SharedMemory -> "shared"
    | GlobalMemory -> "global"
end

type t = {
  hierarchy: Hierarchy.t;
  size: int list; (* Empty means unknown *)
  data_type: string list; (* Empty means unknown *)
}

let is_global (x:t) : bool =
  x.hierarchy = Hierarchy.GlobalMemory

let is_shared (x:t) : bool =
  x.hierarchy = Hierarchy.SharedMemory

let make (h:Hierarchy.t) : t = {
  hierarchy = h;
  size = [];
  data_type = [];
}

let from_type (h:Hierarchy.t) (ty:C_type.t) : t =
  {
    hierarchy = h;
    size = C_type.get_array_length ty;
    data_type = C_type.get_array_type ty;
  }

let make_map (h:Hierarchy.t) (vs:Variable.t list) : t Variable.Map.t =
  vs
  |> List.map (fun x -> (x, make h))
  |> Variable.MapUtil.from_list


let to_string (a:t) : string =
  let ty = a.data_type |> Common.join " " in
  let ty = if ty = "" then "" else ty ^ "  "
  in
  let size =
    List.map string_of_int a.size
    |> Common.join ", "
  in
  let h = a.hierarchy |> Hierarchy.to_string in
  h ^ " " ^ ty ^ "[" ^ size ^ "]"

let map_to_string (vs:t Variable.Map.t) : string =
  Variable.Map.bindings vs
  |> List.map (fun (k,v) -> Variable.name k ^ ": " ^ to_string v)
  |> Common.join ", "

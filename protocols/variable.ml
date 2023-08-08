open Stage0 (* Loads Location.t *)

type t = {name: string; label: string option; location: Location.t option}

let make ~name ~location : t = {name=name; label=None; location=Some location}

let from_name (name:string) : t = {name=name; label=None; location=None}

let label (x:t) =
  match x.label with
  | Some l -> l
  | None -> x.name

let label_opt (x:t) = x.label

let tidx : t = from_name "threadIdx.x"

let tidy : t = from_name "threadIdx.y"

let tidz : t = from_name "threadIdx.z"

let update_name (f: string -> string) (v:t) : t =
  { v with name = f v.name }

let set_name (name:string) : t -> t =
  update_name (fun _ -> name)

let set_location (location:Location.t) (v:t) : t =
  { v with location = Some location}

let clear_location (v:t) = { v with location = None}

let name (x:t) : string = x.name

let location_opt (x:t) : Location.t option = x.location

let location (x:t) : Location.t =
  location_opt x |> Option.value ~default:Location.empty

let equal (x:t) (y:t) = String.equal x.name y.name

let name_line (x:t) : string =
  let l : string = match location_opt x with
  | Some l -> ":" ^ (l |> Location.line |> Index.to_base1 |> string_of_int)
  | None -> ""
  in
  name x ^ l

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

let list_to_string (vs: t list) : string =
  vs
  |> List.map name
  |> Common.join ", "

let set_to_string (vs:Set.t) : string =
  Set.elements vs |> list_to_string

(** Given a variable and a set of known variables, returns
    a fresh variable name. *)

let fresh (xs:Set.t) (x:t) : t =
  let rec do_fresh_name x n =
    let new_x = set_name (x.name ^ string_of_int n) x in
    if Set.mem new_x xs
    then do_fresh_name x (n + 1)
    else new_x
  in
  if Set.mem x xs
  then do_fresh_name x 1
  else x

let is_tid (x:t) : bool =
  equal x tidx || equal x tidy || equal x tidz

let tid_var_list : t list = [tidx; tidy; tidz]

let tid_var_set : Set.t = Set.of_list tid_var_list

let contains_tids (vs:Set.t) : bool =
  Set.mem tidx vs ||
  Set.mem tidy vs ||
  Set.mem tidz vs


module StringOT = struct
  type t = string
  let compare = Stdlib.compare
end

module StringSet = Set.Make(StringOT)

let join sep elems =
  let on_elem accum x =
    if String.equal accum ""
    then x
    else x ^ sep ^ accum
  in
  List.fold_left on_elem "" elems

let enumerate l =
  let rec iter idx l =
    match l with
    | h::t -> (idx,h) :: iter (idx + 1) t
    | _ -> []
  in
  iter 0 l

let opt_to_list = function
  | Some x -> [x]
  | None -> []

let flatten_opt l =
  List.map opt_to_list l |> List.flatten

let map_opt f l =
  List.map f l |> flatten_opt

let ends_with s suffix =
  let suffix_len = String.length suffix in
  let s_len = String.length s in
  (suffix_len = 0) ||
  (s_len >= suffix_len && String.sub s (s_len - suffix_len) suffix_len = suffix)

let hashtbl_elements t =
  Hashtbl.fold (fun k v accum ->
    (k,v)::accum
  ) t []

let hashtbl_update ht kvs =
  List.iter (fun (k,v) -> Hashtbl.add ht k v) kvs

let hashtbl_from_list kvs =
  let ht = Hashtbl.create (List.length kvs) in
  hashtbl_update ht kvs;
  ht

let rec zip l1 l2 =
  match l1, l2 with
  | [], _ | _, [] -> []
  | x::l1, y::l2 -> (x,y) :: (zip l1 l2)

module StringOT = struct
  type t = string
  let compare = Pervasives.compare
end

module StringSet = Set.Make(StringOT)

let join sep elems =
  let on_elem accum x =
    if String.equal accum ""
    then x
    else x ^ sep ^ accum
  in
  List.fold_left on_elem "" elems

let hashtbl_elements t =
  Hashtbl.fold (fun k v accum ->
    (k,v)::accum
  ) t []

let rec zip l1 l2 =
  match l1, l2 with
  | [], _ | _, [] -> []
  | x::l1, y::l2 -> (x,y) :: (zip l1 l2)


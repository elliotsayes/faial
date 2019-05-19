open Proto

let tid1 = "$TID1"
let tid2 = "$TID2"


let as_binary (t:timed_access) : timed_access * timed_access =
  match t with
  | TAcc (n, {access_set = s; access_mode = m}) ->
    let mk si = TAcc (
      Subst.n_subst si n,
      {access_set = Subst.s_subst si s; access_mode = m})
    in
    let s1 = (tid, Var tid1) in
    let s2 = (tid, Var tid2) in
    mk s1, mk s2

let split (l:(string * timed_access) list) : (string * timed_access) list =
  let on_elem (x, t) =
    let a1, a2 = as_binary t in
    [(x, a1); (x, a2)]
  in
  List.flatten (List.map on_elem l)

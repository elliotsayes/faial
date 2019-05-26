open Proto

let tid1 = "$TID1"
let tid2 = "$TID2"


let as_binary (t:access timed) : (access timed) owned * (access timed) owned =
  match t with
  | {timed_phase=n; timed_data=a} ->
    let mk ti =
      let si = Subst.replace_by (tid, Var ti) in
      {
        owned_tid = ti;
        owned_data = {
          timed_phase=Subst.n_subst si n;
          timed_data={
            access_index = Subst.n_subst si a.access_index;
            access_mode = a.access_mode;
            access_cond = Subst.b_subst si a.access_cond;
          };
        }
      }
    in
    mk tid1, mk tid2

let split (l:(string * access timed) list)
    : (string * (access timed) owned) list =
  let on_elem (x, t) =
    let a1, a2 = as_binary t in
    [(x, a1); (x, a2)]
  in
  List.flatten (List.map on_elem l)

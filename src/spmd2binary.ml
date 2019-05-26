open Proto
open Common

let tid1 = "1:"
let tid2 = "2:"

let project prefix x = prefix ^ x

let do_project locals prefix =
  fun x ->
    if StringSet.mem x locals
    then Some (Var (project prefix x))
    else None

let project_access locals (t:access timed) : (access timed) owned * (access timed) owned =
  match t with
  | {timed_phase=n; timed_data=a} ->
    let mk ti =
      let si = do_project locals ti in
      {
        owned_tid = project ti tid;
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

let project_condition locals (b:bexp) =
  let do_subst ti = Subst.b_subst (do_project locals ti) in
  do_subst tid1 b, do_subst tid2 b

let project_stream locals (l:(string * access timed) list)
    : (string * (access timed) owned) list =
  let on_elem (x, t) =
    let a1, a2 = project_access locals t in
    [(x, a1); (x, a2)]
  in
  List.flatten (List.map on_elem l)

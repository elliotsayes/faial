open Proto
open Common

let tid1 = ".1."
let tid2 = ".2."

let project prefix x = prefix ^ x

let tid1_s = project tid1 tid
let tid2_s = project tid2 tid

let tid1_t = Var tid1_s
let tid2_t = Var tid2_s

let do_project locals prefix =
  fun x ->
    if StringSet.mem x locals
    then Some (Var (project prefix x))
    else None

let project_access locals (t:access timed) : (access timed) * (access timed) =
  match t with
  | {timed_phase=n; timed_data=a} ->
    let mk ti =
      let si = do_project locals ti in
      {
        timed_phase=Subst.n_subst si n;
        timed_data={
          access_index = Subst.n_subst si a.access_index;
          access_mode = a.access_mode;
          access_cond = Subst.b_subst si a.access_cond;
        }
      }
    in
    mk tid1, mk tid2

let project_condition locals (b:bexp) =
  let do_subst ti = Subst.b_subst (do_project locals ti) in
  b_and (do_subst tid1 b) (do_subst tid2 b)

type stream = (string * access timed) list

let project_stream locals (l:stream) : stream * stream =
  let on_elem ((l1:stream),(l2:stream)) ((x:string), (a:access timed)) : stream * stream =
    let (a1, a2) = project_access locals a in
    (x,a1)::l1, (x, a2)::l2
  in
  List.fold_left on_elem ([],[]) l

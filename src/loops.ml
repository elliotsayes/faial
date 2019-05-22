open Proto

(** Loop normalization: Makes all loop variables distinct. *)

let fresh_name x xs =
  let rec do_fresh_name x n =
    let name = (x ^ string_of_int n) in
    if List.mem name xs
    then do_fresh_name x (n + 1)
    else name
  in
  if List.mem x xs then do_fresh_name x 1 else x

(** Makes all loop variables distinct. *)
let normalize_variables (p:proto) : (proto * string list) =
  let rec norm e xs =
    match e with
    | Loop ({range_var=x; range_upper_bound=ub}, e) ->
      if List.mem x xs then (
        let new_x = fresh_name x xs in
        let new_xs = new_x::xs in
        let (e, new_xs) = norm (Subst.p_subst (x, Var new_x) e) new_xs in
        Loop ({range_var=new_x; range_upper_bound=ub}, e), new_xs
      ) else (
        let (e, new_xs) = norm e (x::xs) in
        Loop ({range_var=x;range_upper_bound=ub}, e), new_xs
      )
    | Seq (e1, e2) ->
      let (e1, xs) = norm e1 xs in
      let (e2, xs) = norm e2 xs in
      Seq (e1, e2), xs
    | Skip
    | Sync
    | Acc _ -> e, xs
  in
  norm p []

let get_declarations (p:proto) : (string,nexp) Hashtbl.t =
  let decls = Hashtbl.create 0 in
  let rec iter p =
    match p with
    | Skip
    | Sync
    | Acc _ -> ()
    | Loop (r, p) ->
      Hashtbl.add decls r.range_var r.range_upper_bound;
      iter p
    | Seq (p1, p2) ->
      iter p1;
      iter p2
  in
  iter p;
  decls

open Proto

let n_subst f n : nexp =
  let rec subst n =
    match n with
    | Var x ->
      begin
        match f x with
        | Some v -> v
        | _ -> n
      end
    | Num _ -> n
    | Proj (t, n) -> Proj (t, subst n)
    | Bin (o, n1, n2) -> Bin (o, subst n1, subst n2)
  in
  subst n

let b_subst f b : bexp =
  let rec subst b =
    match b with
      | Pred (p,x) ->
        begin match f x with
        | Some (Var v) -> Pred (p, v)
        | _ -> raise (Failure "Subsitution inside predicate returned a non-variable.")
        end
      | Bool _ -> b
      | NRel (o, n1, n2) -> NRel (o, n_subst f n1, n_subst f n2)
      | BRel (o, b1, b2) -> BRel (o, subst b1, subst b2)
      | BNot b -> BNot (subst b)
  in
  subst b

let a_subst f a : access =
  {
    access_index = List.map (n_subst f) a.access_index;
    access_mode = a.access_mode;
    access_cond = b_subst f a.access_cond;
  }

let p_subst f p =
  let rec subst p =
    match p with
    | Skip -> Skip
    | Sync -> Sync
    | Goal b -> Goal (b_subst f b)
    | Assert b -> Assert (b_subst f b)
    | Acc (x, a) -> Acc (x, a_subst f a)
    | Seq (p1, p2) -> Seq (subst p1, subst p2)
    | Loop (r, p) ->
      let shadows =
        match f r.range_var with
        | Some _ -> true
        | None -> false
      in
        Loop (
          {
            range_var = r.range_var;
            range_upper_bound = n_subst f r.range_upper_bound
          },
          if shadows then p else subst p
        )
  in
  subst p

let replace_by (x, v) y = if var_equal x y then Some v else None

(** Substitute using an association list. *)

let assoc_replace kvs : variable -> nexp option =
  let do_find = Hashtbl.find_opt (Common.hashtbl_from_list kvs) in
  (fun k -> do_find k.var_name)

(** Replace variables by constants. *)

let replace_constants (kvs:(string*int) list) k : kernel =
  let kvs = List.map (fun (x,n) -> x, Num n) kvs in
  let keys = List.split kvs |> fst |> List.map var_make |> VarSet.of_list in
  { k with
    kernel_code = p_subst (assoc_replace kvs) k.kernel_code;
    kernel_global_variables = VarSet.diff k.kernel_global_variables keys;
    kernel_local_variables = VarSet.diff k.kernel_local_variables keys;
  }
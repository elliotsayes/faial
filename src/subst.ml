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
    | Bin (o, n1, n2) -> Bin (o, subst n1, subst n2)
  in
  subst n

let b_subst f b : bexp =
  let rec subst b =
    match b with
      | Bool _ -> b
      | NRel (o, n1, n2) -> NRel (o, n_subst f n1, n_subst f n2)
      | BRel (o, b1, b2) -> BRel (o, subst b1, subst b2)
      | BNot b -> BNot (subst b)
  in
  subst b

let a_subst f a : access =
  {
    access_index = n_subst f a.access_index;
    access_mode = a.access_mode;
    access_cond = b_subst f a.access_cond;
  }

let p_subst f p =
  let rec subst p =
    match p with
    | Skip -> Skip
    | Sync -> Sync
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

let replace_by (x, v) y = if String.equal x y then Some v else None

open Proto

let n_subst (x,v) n : nexp =
  let rec subst n =
    match n with
    | Var y -> if String.equal x y then v else n
    | Num _ -> n
    | Bin (o, n1, n2) -> Bin (o, subst n1, subst n2)
  in
  subst n

let b_subst (x,v) b : bexp =
  let rec subst b =
    match b with
      | Bool _ -> b
      | NRel (o, n1, n2) -> NRel (o, n_subst (x,v) n1, n_subst (x,v) n2)
      | BRel (o, b1, b2) -> BRel (o, subst b1, subst b2)
      | BNot b -> BNot (subst b)
  in
  subst b

let a_subst s a : access =
  {
    access_index = n_subst s a.access_index;
    access_mode = a.access_mode;
    access_cond = b_subst s a.access_cond;
  }

let p_subst (x,v) p =
  let rec subst p =
    match p with
    | Skip -> Skip
    | Sync -> Sync
    | Acc (x, a) -> Acc (x, a_subst (x,v) a)
    | Seq (p1, p2) -> Seq (subst p1, subst p2)
    | Loop ({range_var=y;range_upper_bound=ub}, p) ->
      Loop (
        {range_var=y;range_upper_bound=n_subst (x,v) ub},
        if String.equal x y then p else subst p
      )
  in
  subst p

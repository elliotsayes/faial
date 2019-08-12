open Proto

module type SUBST =
  sig
    type t
    (* Given a substitution map and a variable perform the substitution if possible. *)
    val find: t -> variable -> nexp option
    (* Removes a variable from the current substitution map *)
    val remove: t -> variable -> t option
  end

module Make (S:SUBST) = struct
  let shadows (s:S.t) x =
    match S.find s x with
    | Some _ -> true
    | None -> false

  let add (s:S.t) x cont =
    if shadows s x then
      match S.remove s x with
      | Some s -> cont (Some s)
      | None -> cont None
    else
      cont (Some s)

  let n_subst (s:S.t) n : nexp =
    let rec subst n =
      match n with
      | Var x ->
        begin
          match S.find s x with
          | Some v -> v
          | _ -> n
        end
      | Num _ -> n
      | Proj (t, n) -> Proj (t, subst n)
      | Bin (o, n1, n2) -> n_bin o (subst n1) (subst n2)
    in
    subst n

  let b_subst (s:S.t) b : bexp =
    let rec subst b =
      match b with
        | Pred (p,x) ->
          begin match S.find s x with
          | Some (Var v) -> Pred (p, v)
          | _ -> raise (Failure "Subsitution inside predicate returned a non-variable.")
          end
        | Bool _ -> b
        | NRel (o, n1, n2) -> n_rel o (n_subst s n1) (n_subst s n2)
        | BRel (o, b1, b2) -> b_rel o (subst b1) (subst b2)
        | BNot b -> b_not (subst b)
    in
    subst b

  let a_subst (s:S.t) a : access =
    {
      access_index = List.map (n_subst s) a.access_index;
      access_mode = a.access_mode;
      access_cond = b_subst s a.access_cond;
    }

  let p_subst f p =
    let rec subst f p =
      match p with
      | Skip -> Skip
      | Sync -> Sync
      | Goal b -> Goal (b_subst f b)
      | Assert b -> Assert (b_subst f b)
      | Acc (x, a) -> Acc (x, a_subst f a)
      | Seq (p1, p2) -> Seq (subst f p1, subst f p2)
      | Loop (r, p) ->
        let r = { r with range_upper_bound = n_subst f r.range_upper_bound } in
        add f r.range_var (function
          | Some f -> Loop(r, subst f p)
          | None -> Loop (r, p)
        )
    in
    subst f p
end

module SubstPair =
  struct
    type t = (variable * nexp)
    let make (x, v) : t = (x, v)
    let find (x, v) y = if var_equal x y then Some v else None
    let remove (x, v) y = if var_equal x y then None else Some (x, v)
  end

module ReplacePair = Make(SubstPair)

(** Substitute using an association list. *)

module SubstAssoc =
  struct
    type t = (string, nexp) Hashtbl.t
    let make kvs = Common.hashtbl_from_list kvs
    let find ht k = Hashtbl.find_opt ht k.var_name
    let remove ht k =
      let ht = Hashtbl.copy ht in
      Hashtbl.remove ht k.var_name;
      if Hashtbl.length ht = 0 then None
      else Some ht
  end
module ReplaceAssoc =  Make(SubstAssoc)

(** Replace variables by constants. *)

let replace_constants (kvs:(string*int) list) k : kernel =
  let kvs = List.map (fun (x,n) -> x, Num n) kvs in
  let keys = List.split kvs |> fst |> List.map var_make |> VarSet.of_list in
  let kvs = SubstAssoc.make kvs in
  { k with
    kernel_code = ReplaceAssoc.p_subst kvs k.kernel_code;
    kernel_global_variables = VarSet.diff k.kernel_global_variables keys;
    kernel_local_variables = VarSet.diff k.kernel_local_variables keys;
  }
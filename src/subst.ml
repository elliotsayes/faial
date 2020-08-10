open Exp
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
  let shadows (s:S.t) (x:variable) : bool =
    match S.find s x with
    | Some _ -> true
    | None -> false

  let add (s:S.t) (x:variable) (cont: S.t option -> 'a) : 'a  =
    if shadows s x then
      match S.remove s x with
      | Some s -> cont (Some s)
      | None -> cont None
    else
      cont (Some s)

  let n_subst (s:S.t) (n:nexp) : nexp =
    let rec subst n =
      match n with
      | Var x ->
        begin
          match S.find s x with
          | Some v -> v
          | None -> n
        end
      | Num _ -> n
      | Proj (t, x) ->
        begin
          match S.find s x with
          | Some (Var y) -> Proj (t, y)
          | Some n -> failwith (
              "Expecting substitution of variable '" ^ x.var_name ^
              "' by another variable in projection, but got something else"
            )
          | None -> Proj (t, x)
        end
      | Bin (o, n1, n2) -> n_bin o (subst n1) (subst n2)
    in
    subst n

  let b_subst (s:S.t) (b:bexp) : bexp =
    let rec subst b =
      match b with
        | Pred (p,v) -> Pred (p, n_subst s v)
        | Bool _ -> b
        | NRel (o, n1, n2) -> n_rel o (n_subst s n1) (n_subst s n2)
        | BRel (o, b1, b2) -> b_rel o (subst b1) (subst b2)
        | BNot b -> b_not (subst b)
    in
    subst b

  let a_subst (s:S.t) (a:access) : access =
    { a with
      access_index = List.map (n_subst s) a.access_index
    }

  let r_subst (s:S.t) (r:range) : range =
    { r with
      range_lower_bound = n_subst s r.range_lower_bound;
      range_upper_bound = n_subst s r.range_upper_bound
    }

  let rec i_subst (f:'a -> 'a) (s:S.t) (i:'a  base_inst) : 'a  base_inst =
    match i with
    | Base b -> Base (f b)
    | Cond (b, p1) -> Cond (
        b_subst s b,
        i_list_subst f s p1
      )
    | Loop (r, p) ->
      let r = r_subst s r in
      add s r.range_var (function
        | Some s -> Loop (r, i_list_subst f s p)
        | None -> Loop (r, p)
      )
  and i_list_subst (f:'a -> 'a) (s:S.t) : ('a  base_inst) list -> ('a  base_inst) list =
    List.map (i_subst f s)

  let acc_inst_subst (s:S.t) : acc_inst -> acc_inst =
    function
      | Goal b -> Goal (b_subst s b)
      | Acc (x, a) -> Acc (x, a_subst s a)

  let p_subst (s:S.t) : prog -> prog =
    i_list_subst (function
      | Unsync e -> Unsync (acc_inst_subst s e)
      | Sync -> Sync
    ) s
(*
  let u_subst (s:S.t) : u_prog -> u_prog =
    i_list_subst (acc_inst_subst s) s

  let s_subst (s:S.t) : s_prog -> s_prog =
    i_list_subst (u_subst s) s*)
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

let replace_constants (kvs:(string*int) list) (k:prog kernel) : prog kernel =
  let kvs = List.map (fun (x,n) -> x, Num n) kvs in
  let keys = List.split kvs |> fst |> List.map var_make |> VarSet.of_list in
  let kvs = SubstAssoc.make kvs in
  { k with
    kernel_code = ReplaceAssoc.p_subst kvs k.kernel_code;
    kernel_global_variables = VarSet.diff k.kernel_global_variables keys;
    kernel_local_variables = VarSet.diff k.kernel_local_variables keys;
  }
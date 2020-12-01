open Exp
open Hash_rt
open Ppx_compare_lib.Builtin

(* The source instruction uses the base defined above *)
type inst =
  | Acc of acc_expr
  | Sync
  | Cond of bexp * inst list
  | Loop of range * inst list
  [@@deriving hash, compare]

(* The source program *)
type prog = inst list [@@deriving hash, compare]

type 'a kernel = {
  (* The shared locations that can be accessed in the kernel. *)
  kernel_locations: VarSet.t;
  (* The internal variables are used in the code of the kernel.  *)
  kernel_global_variables: VarSet.t;
  (* The internal variables are used in the code of the kernel.  *)
  kernel_local_variables: VarSet.t;
  (* A thread-local pre-condition that is true on all phases. *)
  kernel_pre: bexp;
  (* The code of a kernel performs the actual memory accesses. *)
  kernel_code: 'a;
}


let kernel_constants (k:prog kernel) =
  let rec constants (b: bexp) (kvs:(string*int) list) : (string*int) list =
    match b with
    | Bool _ -> kvs
    | NRel (NEq, Var x, Num n)
    | NRel (NEq, Num n, Var x) ->
      (x.var_name, n) :: kvs
    | BRel (BAnd, b1, b2) ->
      constants b1 kvs |> constants b2
    | BNot _
    | Pred _
    | NRel _
    | BRel _ -> kvs
  in
  constants k.kernel_pre []

(** Replace variables by constants. *)

module Make (S:Subst.SUBST) = struct
  module M = Subst.Make(S)
  let p_subst: S.t -> prog -> prog =
    let rec i_subst (s:S.t) (i:inst) : inst =
      match i with
      | Acc (x, e) -> Acc (x, M.a_subst s e)
      | Sync -> Sync
      | Cond (b, p) -> Cond (
          M.b_subst s b,
          p_subst s p
        )
      | Loop (r, p) ->
        let r = M.r_subst s r in
        M.add s r.range_var (function
          | Some s -> Loop (r, p_subst s p)
          | None -> Loop (r, p)
        )
    and p_subst (s:S.t) : prog -> prog =
      List.map (i_subst s)
    in
    p_subst

end

module PSubstAssoc = Make(Subst.SubstAssoc)
module PSubstPair = Make(Subst.SubstPair)

let p_opt (p:prog) : prog =
  let rec opt_i : inst -> prog =
    function
    | Acc e -> [Acc e]
    | Sync -> [Sync]
    | Cond(b, p) ->
      begin
        let b = Constfold.b_opt b in
        let p = opt_p p in
        match b, p with
        | Bool true, _ -> p
        | Bool false, _ -> []
        | _, [] -> []
        | _, _ -> [Cond (b, p)]
      end
    | Loop (r, p) ->
      begin
        let r = Constfold.r_opt r in
        let p = opt_p p in
        match r.range_lower_bound, r.range_upper_bound, p with
        | _, _, [] -> []
        | Num lb, Num ub, _ ->
          if lb >= ub then
            []
          else if lb + 1 = ub then
            PSubstPair.p_subst (r.range_var, Num lb) p
          else if lb + 2 = ub then
            PSubstPair.p_subst (r.range_var, Num lb) p
            @
            PSubstPair.p_subst (r.range_var, Num (lb + 1)) p
          else
            [Loop (r, p)]
        | _, _, _ -> [Loop (r, p)]
      end
  and opt_p (p:prog) =
    List.concat_map opt_i p
  in
  opt_p p

let replace_constants (kvs:(string*int) list) (k:prog kernel) : prog kernel =
  if Common.list_is_empty kvs then k else
  let kvs = List.map (fun (x,n) -> x, Num n) kvs in
  let keys = List.split kvs |> fst |> List.map var_make |> VarSet.of_list in
  let kvs = Subst.SubstAssoc.make kvs in
  {
    kernel_locations = k.kernel_locations;
    kernel_pre = PSubstAssoc.M.b_subst kvs k.kernel_pre |> Constfold.b_opt;
    kernel_code = PSubstAssoc.p_subst kvs k.kernel_code |> p_opt;
    kernel_global_variables = VarSet.diff k.kernel_global_variables keys;
    kernel_local_variables = VarSet.diff k.kernel_local_variables keys;
  }


let vars_distinct (p:prog)  (known:VarSet.t) : prog =
  let open Bindings in
  let rec uniq_i (i:inst) (xs:VarSet.t) : inst * VarSet.t =
    match i with
    | Acc _
    | Sync
      -> (i, xs)
    | Cond (b, p) ->
      let (p, xs) = uniq_p p xs in
      (Cond (b, p), xs)
    | Loop (r, p) ->
      let x = r.range_var in
      if VarSet.mem x xs then (
        let new_x : variable = generate_fresh_name x xs in
        let new_xs = VarSet.add new_x xs in
        let s = Subst.SubstPair.make (x, Var new_x) in
        let new_p = PSubstPair.p_subst s p in
        let (p, new_xs) = uniq_p new_p new_xs in
        Loop ({ r with range_var = new_x }, p), new_xs
      ) else (
        let (p, new_xs) = uniq_p p (VarSet.add x xs) in
        Loop (r, p), new_xs
      )
  and uniq_p (p:prog) (xs:VarSet.t) : prog * VarSet.t =
    match p with
    | [] -> ([], xs)
    | i::p ->
      let (i, xs) = uniq_i i xs in
      let (p, xs) = uniq_p p xs in
      (i::p, xs)
  in
  uniq_p p known |> fst

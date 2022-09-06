open Exp
open Serialize

module type SUBST =
  sig
    type t
    (* Given a substitution map and a variable perform the substitution if possible. *)
    val find: t -> Variable.t -> nexp option
    (* Removes a variable from the current substitution map; return None if the map became empty *)
    val remove: t -> Variable.t -> t option
    (* Renders as a string *)
    val to_string: t -> string
    (* Tests if the map is empty *)
    val is_empty: t -> bool
  end

module Make (S:SUBST) = struct
  let shadows (s:S.t) (x:Variable.t) : bool =
    match S.find s x with
    | Some _ -> true
    | None -> false

  let add (s:S.t) (x:Variable.t) (cont: S.t option -> 'a) : 'a  =
    if shadows s x then
      match S.remove s x with
      | Some s -> cont (Some s)
      | None -> cont None
    else
      cont (Some s)

  let rec n_subst (s:S.t) (n:nexp) : nexp =
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
          | Some _ ->
            let exp = PPrint.n_to_s n in
            let repl = S.to_string s in
            failwith (
              "Error: cannot replace thread-local variable " ^ Variable.name x ^
              " by constant\n" ^
              "substitution(expression=" ^ exp ^ ", replacement=" ^ repl ^ ")"
            )
          | None -> Proj (t, x)
        end
      | Bin (o, n1, n2) -> n_bin o (subst n1) (subst n2)
      | NIf (b, n1, n2) -> n_if (b_subst s b) (subst n1) (subst n2)
      | NCall (x, a) -> NCall (x, n_subst s a)
    in
    subst n

  and b_subst (s:S.t) (b:bexp) : bexp =
    let rec subst: bexp -> bexp = function
      | Pred (n, v) -> Pred (n, n_subst s v)
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

  let s_subst (s:S.t) (se:step_expr) : step_expr =
    match se with
    | Default n -> Default (n_subst s n)
    | StepName _ -> se

  let r_subst (s:S.t) (r:range) : range =
    { r with
      range_step = s_subst s r.range_step;
      range_lower_bound = n_subst s r.range_lower_bound;
      range_upper_bound = n_subst s r.range_upper_bound
    }

  let acc_expr_subst (s:S.t) ((x,e):acc_expr) : acc_expr =
    (x, a_subst s e)

end

module SubstPair =
  struct
    type t = (Variable.t * nexp)
    let make (x, v) : t = (x, v)
    let find (x, v) y = if Variable.equal x y then Some v else None
    let remove (x, v) y = if Variable.equal x y then None else Some (x, v)
    let to_string (x, v) = "[" ^ Variable.name x ^ "=" ^ PPrint.n_to_s v ^ "]"
    let is_empty (_, _) = false
  end

module ReplacePair = Make(SubstPair)

(** Substitute using an association list. *)

module SubstAssoc =
  struct
    type t = (string, nexp) Hashtbl.t

    let make kvs = Common.hashtbl_from_list kvs

    let find ht k = Hashtbl.find_opt ht (Variable.name k)

    let put_mut (ht:t) (k:Variable.t) (n:nexp) : unit =
      Hashtbl.replace ht (Variable.name k) n
      
    let is_empty (ht:t) : bool = Hashtbl.length ht = 0

    let put (ht:t) (k:Variable.t) (n:nexp) : t =
      let ht = Hashtbl.copy ht in
      put_mut ht k n;
      ht

    let del ht k =
      let ht = Hashtbl.copy ht in
      Hashtbl.remove ht (Variable.name k);
      ht

    let remove ht k =
      let ht = del ht k in
      if Hashtbl.length ht = 0 then None
      else Some ht

    let to_string ht =
      Common.hashtbl_elements ht
      |> List.map (fun (k, v) -> k ^ "=" ^ PPrint.n_to_s v)
      |> Common.join ", "
      |> fun x -> "[" ^ x ^ "]"
  end
module ReplaceAssoc = Make(SubstAssoc)

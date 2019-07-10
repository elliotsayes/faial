open Proto

type pred = {
  pred_name: string;
  pred_arg: string;
  pred_body: bexp;
}

let mk_pred (name:string) (body:nexp -> bexp) : pred =
  { pred_name = name; pred_arg = "x"; pred_body = body (Var (var_make "x")) }

type proof = {
  proof_preds: pred list;
  proof_decls: string list;
  proof_pre: bexp list;
  proof_goal: bexp;
}

let mk_proof preds decls pre goal =
  {
    proof_preds = preds;
    proof_decls = decls;
    proof_pre = pre;
    proof_goal = goal;
  }

let mode_to_nexp m =
  Num (match m with
  | R -> 0
  | W -> 1)

let access_list_to_bexp elems time idx mode other_mode =
  List.map (fun elem ->
    let result = [
      n_eq time elem.timed_phase;
      n_eq mode (elem.timed_data.access_mode |> mode_to_nexp);
      elem.timed_data.access_cond
    ] @
    List.map (fun (i, j) -> n_eq i j) (Common.zip idx elem.timed_data.access_index);

    in
    (if elem.timed_data.access_mode = R
    then (n_eq other_mode (mode_to_nexp W))::result
    else result) |> b_and_ex
  ) elems |> b_or_ex

let steps_to_bexp (step1, step2) (time1, idx1, mode1) (time2, idx2, mode2) =
  b_and_ex (
    [
      access_list_to_bexp step1 time1 idx1 mode1 mode2;
      access_list_to_bexp step2 time2 idx2 mode2 mode1;
      n_eq time1 time2;
    ]
    @
    List.map (fun (i,j) -> n_eq i j) (List.combine idx1 idx2)
  )

let range i j =
  let rec iter n acc =
    if n < i then acc else iter (n-1) (n :: acc)
  in
  iter j []

let is_even n =
  n mod 2 = 0

let pow base exponent =
  if exponent < 0 then invalid_arg "exponent can not be negative" else
  let rec aux accumulator base = function
    | 0 -> accumulator
    | 1 -> base * accumulator
    | e when is_even e -> aux accumulator (base * base) (e / 2)
    | e -> aux (base * accumulator) (base * base) ((e - 1) / 2) in
  aux 1 base exponent

let eq_nums x l : bexp =
  List.map (fun i -> n_eq x (Num i)) l
  |> b_or_ex

let gen_pow base x : bexp =
  let ub = 0xFFFFFFFF in
  let rec pows n : int list =
    let x = pow base n in
    if x > ub then []
    else if x == ub then [x]
    else x :: pows (n + 1)
  in
  pows 0 |> eq_nums x

let predicates =
  List.map (fun x ->
    mk_pred ("pow" ^ string_of_int x) (gen_pow x)
  ) (range 2 4)
  @
  [
    mk_pred "uint32" (fun x -> n_le x (Num 0xFFFFFFFF));
    mk_pred "uint16" (fun x -> n_le x (Num 0xFFFF));
    mk_pred "uint8" (fun x -> n_le x (Num 0xFF));
  ]

let generate_kernel k =
  let open Taskproj in
  let mk_var x = Var (var_make x) in
  let time1 = mk_var (tid1 ^ "time$") in
  let time2 = mk_var (tid2 ^ "time$") in
  let mode1 = mk_var (tid1 ^ "mode$") in
  let mode2 = mk_var (tid2 ^ "mode$") in
  let dims = ["x"; "y"; "z"; "w"] in
  let idx1 = List.map (fun d -> mk_var (tid1 ^ "idx" ^ d ^ "$")) dims in
  let idx2 = List.map (fun d -> mk_var (tid2 ^ "idx" ^ d ^ "$")) dims in
  let generate_vars =
    let vars = k.proj_kernel_vars in
    let more_vars =
      List.map
        (fun x -> match x with | Var x -> x | _ -> failwith "")
        ([time1; time2; mode1; mode2] @ idx1 @ idx2)
    in
    VarSet.elements vars @ more_vars |> List.map (fun x -> x.var_name)
  in
  let gen_steps ss =
    steps_to_bexp ss (time1, idx1, mode1) (time2, idx2, mode2)
  in
  (generate_vars, gen_steps)

let kernel_to_proofs prove_drf proof_obl k : proof list =
  let decls, gen_steps = generate_kernel k in
  let mk_p (pre, b) = mk_proof predicates decls pre b in
  let split_pre l : bexp list * bexp =
    let last = (List.length l) - 1 in
    List.mapi (fun idx e -> (idx, e)) l
    |> List.partition (fun (idx, _) -> idx < last)
    |> fun (l1, l2) -> List.map snd l1, List.nth (List.map snd l2) 0
  in
  List.flatten
  [
    if proof_obl then
      List.map (fun l ->
        split_pre l |> mk_p
      ) k.proj_kernel_proofs
    else [];
    if prove_drf then
      Common.hashtbl_elements k.proj_kernel_steps
      |> List.map (fun (x, s) ->
        mk_p (k.proj_kernel_pre, gen_steps s)
      )
    else []
  ]
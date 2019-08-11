open Proto

type var_type = Location | Index

type locality = Global | Local

type access_expr = {access_index: nexp list; access_mode: mode}

type instruction =
| ISync
| IGoal of bexp
| IAssert of bexp
| IAcc of variable * access

type step_kind = Default | Pred of string

type range_expr = {range_expr_start: nexp; range_expr_stop: nexp; range_expr_step: nexp; range_expr_kind: step_kind}

type program =
| Inst of instruction
| Block of (program list)
| Decl of (variable * locality * nexp option)
| If of (bexp * program * program)
| For of (variable * range_expr * program)

type p_kernel = {
  (* The shared locations that can be accessed in the kernel. *)
  p_kernel_locations: VarSet.t;
  (* The internal variables are used in the code of the kernel.  *)
  p_kernel_params: VarSet.t;
  (* The code of a kernel performs the actual memory accesses. *)
  p_kernel_code: program;
}

let unblock p =
  match p with
  | Block l -> l
  | _ -> raise (Failure "!!")

(** Variable normalization: Makes all variable declarations distinct. *)

let on_subst f o =
  match o with
  | Some n -> Some (Subst.n_subst f n)
  | None -> None

let p_subst f p =
  let shadows x =
    match f x with
    | Some _ -> true
    | None -> false
  in
  let rec subst p =
    match p with
    | Inst ISync -> Inst ISync
    | Inst (IGoal b) -> Inst (IGoal (Subst.b_subst f b))
    | Inst (IAssert b) -> Inst (IAssert (Subst.b_subst f b))
    | Inst (IAcc (x, a)) -> Inst (IAcc (x, Subst.a_subst f a))
    | Block (p::l) ->
      begin match p with
        | Decl (x,v, o) when shadows x ->
            (* When there is a shadowing we stop replacing the rest of the block *)
            Block (Decl (x,v, on_subst f o)::l)
        | _ ->
          let head = subst p in
          Block (head::(unblock (subst (Block l))))
      end
    | Block [] -> Block []
    | Decl (x,v,o) -> Decl (x,v, on_subst f o)
    | If (b, p1, p2) -> If (Subst.b_subst f b, subst p1, subst p2)
    | For (x, r, p) ->
        For (x,
          {
            range_expr_start = Subst.n_subst f r.range_expr_start;
            range_expr_step = Subst.n_subst f r.range_expr_step;
            range_expr_stop = Subst.n_subst f r.range_expr_stop;
            range_expr_kind = r.range_expr_kind;
          },
          if shadows x then p else subst p
        )
  in
  subst p

let normalize_variables (p:program) xs =
  let rec norm p xs : program * VarSet.t =
    let do_subst x (do_cont:variable -> VarSet.t -> (variable -> nexp option) -> (program -> program * VarSet.t) -> program * VarSet.t) : program * VarSet.t =
      if VarSet.mem x xs then (
        let new_x : variable = Loops.generate_fresh_name x xs in
        let new_xs = VarSet.add new_x xs in
        let subst = Subst.replace_by (x, Var new_x) in
        do_cont new_x new_xs subst (fun p -> norm (p_subst subst p) new_xs)
      ) else (
        let new_xs = VarSet.add x xs in
        do_cont x new_xs (fun x -> None) (fun p -> norm p new_xs)
      )
    in
    match p with
    | Inst _ -> (p, xs)
    | Block (p :: l) ->
      begin match p with
      | Decl (x,v,n) ->
        do_subst x (fun new_x new_xs subst do_rec ->
          let p, new_xs = do_rec (Block l) in
          Block (Decl (new_x,v, n) :: unblock p), new_xs
        )
      | _ ->
        let rest, xs = norm (Block l) xs in
        Block (p :: unblock rest), xs
      end
    | Block [] -> Block [], xs
    | Decl (x,v, n) -> do_subst x (fun new_x new_xs subst kont ->
        Decl (new_x, v, n), new_xs
      )
    | If (b, p1, p2) ->
      let p1, xs = norm p1 xs in
      let p2, xs = norm p2 xs in
      If (b, p1, p2), xs
    | For (x, r, p) ->
      do_subst x (fun new_x new_xs subst kont ->
        let p, xs = kont p in
        For (x, r, p), xs
      )
  in
  norm p xs |> fst

let rec reify (p:program) : proto =
  (**
    Breaks down syntactic sugar:
    1. Converts declarations into asserts
    2. Converts blocks into seqs
    3. Convert structured-loops into protocol Foreach
  *)
  match p with
  | Decl (x,_,None) -> Skip
  | Decl (x,_,Some n) -> Assert (n_eq (Var x) n)
  | Inst ISync -> Sync
  | Inst (IGoal b) -> Goal b
  | Inst (IAssert b) -> Assert b
  | Inst (IAcc (x,y)) -> Acc (x,y)
  | Block l -> proto_block (List.map reify l)
  | If _ -> raise (Failure "Call remove_if first!")
  | For (x, r, p) ->
    let index = Var x in
    let p = reify p in
    let body = begin match r.range_expr_kind with
      | Default ->
        proto_block [
          (* assert index >= INIT; *)
          Assert (n_ge index r.range_expr_start);
          (*  assert (index - INIT) % STRIDE == 0; *)
          Assert (n_eq (n_mod (n_minus index r.range_expr_start) r.range_expr_step) (Num 0));
          (* assert STRIDE > 0; *)
          Assert (n_gt r.range_expr_step (Num 0));
          (* Ensure the lower bound is smaller than the upper bound *)
          Assert (n_le r.range_expr_start r.range_expr_stop);
          (* The rest of the body *)
          p
        ]
      | Pred name ->
        proto_block [
          Assert (n_ge index r.range_expr_start);
          Assert (Pred (name, x));
          (* Ensure the lower bound is smaller than the upper bound *)
          Assert (n_le r.range_expr_start r.range_expr_stop);
          p
        ]
    end in
    Loop ({range_var = x; range_upper_bound = r.range_expr_stop}, body)

let remove_if (p:program) : program =
  let i_remove_if (cnd:bexp) (i:instruction) =
    match i with
    | ISync
    | IGoal _
    | IAssert _ -> i
    | IAcc (x, a) -> IAcc (x, {a with access_cond = b_and cnd a.access_cond})
  in
  let rec iter (cnd:bexp) (p:program) =
    if cnd = Bool false then Block []
    else
      match p with
      | Inst i -> Inst (i_remove_if cnd i)
      | Block l -> Block (List.map (iter cnd) l)
      | Decl _ -> p
      | If (b, p1, p2) -> Block [iter (b_and cnd b) p1; iter (b_and cnd (b_not b)) p2]
      | For (x, r, p) -> For (x, r, iter cnd p)
  in
  iter (Bool true) p

let rec get_variable_decls (p:program) (locals,globals:VarSet.t * VarSet.t) : VarSet.t * VarSet.t =
  match p with
  | Inst _ -> (locals,globals)
  | Block l -> List.fold_right get_variable_decls l (locals,globals)
  | Decl (x, Local, _) -> VarSet.add x locals, globals
  | Decl (x, Global, _) -> locals, VarSet.add x globals
  | If (_, p1, p2) -> get_variable_decls p1 (locals,globals) |> get_variable_decls p2
  | For (_, _, p) -> get_variable_decls p (locals,globals)

let compile (k:p_kernel) : kernel =
  let globals = k.p_kernel_params in
  let locals = VarSet.empty in
  (* Ensures the variable declarations differ from the parameters *)
  let p = normalize_variables k.p_kernel_code (VarSet.union locals globals) in
  let locals, globals = get_variable_decls p (locals, globals)  in
  (**
    1. We rename all variables so that they areall different
    2. We remove if statements
    3. We break down for-loops and variable declarations
    *)
  {
    kernel_locations = k.p_kernel_locations;
    kernel_local_variables = locals;
    kernel_global_variables = globals;
    kernel_code = remove_if p |> reify;
  }

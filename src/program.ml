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

module SubstMake(S:Subst.SUBST) = struct
  module M = Subst.Make(S)

  let program_subst (s:S.t) p : program =
    let on_subst s o =
      match o with
      | Some n -> Some (M.n_subst s n)
      | None -> None
    in

    let rec subst s p =
      match p with
      | Inst ISync -> Inst ISync
      | Inst (IGoal b) -> Inst (IGoal (M.b_subst s b))
      | Inst (IAssert b) -> Inst (IAssert (M.b_subst s b))
      | Inst (IAcc (x, a)) -> Inst (IAcc (x, M.a_subst s a))
      | Block (p::l) ->
        begin match p with
          | Decl (x,v, o) ->
            (* When there is a shadowing we stop replacing the rest of the block *)
            let h = Decl (x,v, on_subst s o) in
            let l = M.add s x (function
              | Some s -> subst s (Block l) |> unblock
              | None -> l
            ) in
            Block (h::l)
          | _ ->
            let h = subst s p in
            let l = subst s (Block l) |> unblock in
            Block (h::l)
        end
      | Block [] -> Block []
      | Decl (x,v,o) -> Decl (x,v, on_subst s o)
      | If (b, p1, p2) -> If (M.b_subst s b, subst s p1, subst s p2)
      | For (x, r, p) ->
        For (x,
          {
            range_expr_start = M.n_subst s r.range_expr_start;
            range_expr_step = M.n_subst s r.range_expr_step;
            range_expr_stop = M.n_subst s r.range_expr_stop;
            range_expr_kind = r.range_expr_kind;
          },
          M.add s x (function
          | Some s -> subst s p
          | None -> p
          )
        )
    in
    subst s p
  end

module ReplacePair = SubstMake(Subst.SubstPair)

let normalize_variables (p:program) xs =
  let rec norm p xs : program * VarSet.t =
    let do_subst x do_cont : program * VarSet.t =
      if VarSet.mem x xs then (
        let new_x : variable = Bindings.generate_fresh_name x xs in
        let new_xs = VarSet.add new_x xs in
        let si = Subst.SubstPair.make (x, Var new_x) in
        do_cont new_x new_xs (fun (p:program) -> norm (ReplacePair.program_subst si p) new_xs)
      ) else (
        let new_xs = VarSet.add x xs in
        do_cont x new_xs (fun p -> norm p new_xs)
      )
    in
    match p with
    | Inst _ -> (p, xs)
    | Block (p :: l) ->
      begin match p with
      | Decl (x,v,n) ->
        do_subst x (fun new_x new_xs do_rec ->
          let p, new_xs = do_rec (Block l) in
          Block (Decl (new_x,v, n) :: unblock p), new_xs
        )
      | _ ->
        let rest, xs = norm (Block l) xs in
        Block (p :: unblock rest), xs
      end
    | Block [] -> Block [], xs
    | Decl (x,v, n) -> do_subst x (fun new_x new_xs kont ->
        Decl (new_x, v, n), new_xs
      )
    | If (b, p1, p2) ->
      let p1, xs = norm p1 xs in
      let p2, xs = norm p2 xs in
      If (b, p1, p2), xs
    | For (x, r, p) ->
      do_subst x (fun new_x new_xs kont ->
        let p, xs = kont p in
        For (x, r, p), xs
      )
  in
  norm p xs |> fst

let rec reify (p:program) : prog =
  (**
    Breaks down syntactic sugar:
    1. Converts declarations into asserts
    2. Convert structured-loops into protocol Foreach
  *)
  match p with
  | Decl (_,_,_) -> [] (* Only handled inside a block *)
  | Inst (IAssert _) -> [] (* Only handled inside a block *)
  | Inst ISync -> [Base Sync]
  | Inst (IGoal b) -> [Base (Unsync (Goal b))]
  | Inst (IAcc (x,y)) -> [Base (Unsync (Acc (x,y)))]
  | Block [] -> []
  | Block (Inst (IAssert b)::l) -> [Cond (b, reify (Block l))]
  | Block (Decl (x,_,Some n)::l) -> [Cond (n_eq (Var x) n, reify (Block l))]
  | Block (i::l) -> reify i @ reify (Block l)
  | If (b,p,q) -> [Cond (b,reify p);Cond(BNot b, reify q)]
  | For (x, r, p) ->
    let index = Var x in
    let pre:bexp = begin match r.range_expr_kind with
      | Default ->
          b_and
            (*  assert (index - INIT) % STRIDE == 0; *)
            (n_eq (n_mod (n_minus index r.range_expr_start) r.range_expr_step) (Num 0))
            (* ensure that the bound is correct *)
            (n_gt r.range_expr_step (Num 0))
      | Pred name ->
          Pred (name, x)
    end in
    [Loop ({
        range_var = x;
        range_lower_bound = r.range_expr_start;
        range_upper_bound = r.range_expr_stop
      },
      (* Ensure the lower bound is smaller than the upper bound *)
      [Cond (
        b_and pre (n_le r.range_expr_start r.range_expr_stop),
        reify p
      )]
    )]

let rec get_variable_decls (p:program) (locals,globals:VarSet.t * VarSet.t) : VarSet.t * VarSet.t =
  match p with
  | Inst _ -> (locals,globals)
  | Block l -> List.fold_right get_variable_decls l (locals,globals)
  | Decl (x, Local, _) -> VarSet.add x locals, globals
  | Decl (x, Global, _) -> locals, VarSet.add x globals
  | If (_, p1, p2) -> get_variable_decls p1 (locals,globals) |> get_variable_decls p2
  | For (_, _, p) -> get_variable_decls p (locals,globals)

let compile (k:p_kernel) : prog kernel =
  let globals = k.p_kernel_params in
  let locals = VarSet.empty in
  (* Ensures the variable declarations differ from the parameters *)
  let p = normalize_variables k.p_kernel_code (VarSet.union locals globals) in
  let locals, globals = get_variable_decls p (locals, globals)  in
  (**
    1. We rename all variables so that they are all different
    2. We break down for-loops and variable declarations
    *)
  {
    kernel_locations = k.p_kernel_locations;
    kernel_local_variables = locals;
    kernel_global_variables = globals;
    kernel_code = reify p;
  }

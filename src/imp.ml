open Exp
open Proto
open Serialize
open Subst

type var_type = Location | Index

type locality = Global | Local

type access_expr = {access_index: nexp list; access_mode: mode}

type instruction =
| ISync
| IAssert of bexp
| IAcc of variable * access

type alias_expr = {alias_source: variable; alias_target: variable; alias_offset: nexp}

type stmt =
| Inst of instruction
| Block of (stmt list)
| LocationAlias of alias_expr
| Decl of (variable * locality * nexp option)
| If of (bexp * stmt * stmt)
| For of (range * stmt)
| Loop of stmt

let s_block l =
  Block (
    List.filter (function
      | Block [] -> false
      | _ -> true
    ) l
  )

let s_for (r:range) (s:stmt) =
  match s with
  | Block [] -> Block []
  | _ -> For (r, s)

let s_loop (s:stmt) =
  match s with
  | Block [] -> Block []
  | _ -> Loop s

let s_if (b:bexp) (p1:stmt) (p2:stmt) : stmt =
  match b, p1, p2 with
  | (Bool false, _, p)
  | (Bool true, p, _)
    -> p
  | (_, Block [], Block []) -> Block []
  | _ -> If (b, p1, p2)

type p_kernel = {
  (* The kernel name *)
  p_kernel_name: string;
  (* A kernel precondition of every phase. *)
  p_kernel_pre: bexp;
  (* The shared locations that can be accessed in the kernel. *)
  p_kernel_locations: VarSet.t;
  (* The internal variables are used in the code of the kernel.  *)
  p_kernel_params: VarSet.t;
  (* The code of a kernel performs the actual memory accesses. *)
  p_kernel_code: stmt;
}

let unblock p =
  match p with
  | Block l -> l
  | _ -> failwith "unblock: expecting a block!"

(** Variable normalization: Makes all variable declarations distinct. *)

let rec loc_subst (alias:alias_expr) (p:stmt) : stmt =
  let rec subst (p:stmt) =
    match p with
    | Inst (IAcc (x, a)) ->
      if var_equal x alias.alias_target then
        (match a.access_index with
        | [n] -> Inst (IAcc (alias.alias_source, { a with access_index = [n_plus alias.alias_offset n] }))
        | _ -> failwith ("Expecting an index with dimension 1, but got " ^ (string_of_int (List.length a.access_index)))
        )
      else
        p
    | Loop p -> Loop (loc_subst alias p)
    | LocationAlias _
    | Decl _
    | Inst _
    | Block [] -> p
    | Block (i::l) ->
      let i = subst i in
      begin match i with
        | LocationAlias a ->
          subst (loc_subst a (Block l))
        | _ ->
          Block (i :: (subst (Block l) |> unblock))
      end
    | If (b, p1, p2) -> If (b, subst p1, subst p2)
    | For (r, l) -> For (r, subst l)
  in
  subst p

module SubstMake(S:Subst.SUBST) = struct
  module M = Subst.Make(S)

  let program_subst (s:S.t) p : stmt =
    let on_subst s o =
      match o with
      | Some n -> Some (M.n_subst s n)
      | None -> None
    in

    let rec subst s p =
      match p with
      | Inst ISync -> Inst ISync
      | LocationAlias a ->
        LocationAlias {a with alias_offset = M.n_subst s a.alias_offset}
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
      | Loop p -> Loop (subst s p)
      | Decl (x,v,o) -> Decl (x,v, on_subst s o)
      | If (b, p1, p2) -> If (M.b_subst s b, subst s p1, subst s p2)
      | For (r, p) ->
        For (M.r_subst s r,
          M.add s r.range_var (function
          | Some s -> subst s p
          | None -> p
          )
        )
    in
    subst s p
  end

module ReplacePair = SubstMake(Subst.SubstPair)

let normalize_variables (p:stmt) xs =
  let rec norm p xs : stmt * VarSet.t =
    let do_subst x do_cont : stmt * VarSet.t =
      if VarSet.mem x xs then (
        let new_x : variable = Bindings.generate_fresh_name x xs in
        let new_xs = VarSet.add new_x xs in
        let si = Subst.SubstPair.make (x, Var new_x) in
        do_cont new_x new_xs (fun (p:stmt) -> norm (ReplacePair.program_subst si p) new_xs)
      ) else (
        let new_xs = VarSet.add x xs in
        do_cont x new_xs (fun p -> norm p new_xs)
      )
    in
    match p with
    | Loop p -> let (p, xs) = norm p xs in (Loop p, xs)
    | LocationAlias _
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
    | For (r, p) ->
      do_subst r.range_var (fun new_x new_xs kont ->
        let p, xs = kont p in
        For (r, p), xs
      )
  in
  norm p xs |> fst

let reify (locations:VarSet.t) (p:stmt) : prog =
  let rec reify =
    function
    | LocationAlias _
    | Decl _ (* Only handled inside a block *)
    | Inst (IAssert _)
    | Block []
      -> [] (* Only handled inside a block *)
    | Inst ISync -> [Sync]
    | Inst (IAcc (x,y)) ->
      if VarSet.mem x locations
      then [Acc (x,y)]
      else []
    | Block (Inst (IAssert b)::l) -> [Cond (b, reify (Block l))]
    (* | Block (Decl (x,_,Some n)::l) ->
      (* When we find a declaration, inline it in the code *)
      Block l
      |> ReplacePair.program_subst (Subst.SubstPair.make (x, n))
      |> reify *)
    | Block (LocationAlias a :: l) ->
      Block l
      |> loc_subst a
      |> reify
    | Block (i::l) -> Common.append_tr (reify i) (reify (Block l))
    | If (b,p, Block []) -> [Cond (b,reify p)]
    | If (b,Block[],q) -> [Cond(BNot b, reify q)]
    | If (b,p,q) -> [Cond (b,reify p);Cond(BNot b, reify q)]
    | Loop p ->
      begin match reify p with
      | [] -> []
      | p -> [Loop (mk_range (var_make "X?") (Num 2), p)]
      end
    | For (r, p) ->
      begin match reify p with
      | [] -> []
      | p -> [Loop (r, p)]
      end
  in
  reify p

let rec get_var_binders (p:stmt) (kvs: (variable * nexp) list) : (variable * nexp) list =
  match p with
  | Decl (x,_,Some n)
    -> (x,n)::kvs
  | Decl (_,_,None)
  | LocationAlias _
  | Inst (IAssert _)
  | Inst ISync
  | Inst (IAcc _)
  | Block []
    -> kvs
  | Block (i::l) -> get_var_binders i kvs |> get_var_binders (Block l)
  | If (_, p, q) -> get_var_binders p kvs |> get_var_binders q
  | Loop p
  | For (_, p)
    -> get_var_binders p kvs

let rec p_subst (kvs: SubstAssoc.t) (p:prog) =
  List.map (i_subst kvs) p
and i_subst (kvs: SubstAssoc.t) (i:inst) =
  match i with
  | Acc e -> Acc (ReplaceAssoc.acc_expr_subst kvs e)
  | Sync -> Sync
  | Cond (b, l) -> Cond (ReplaceAssoc.b_subst kvs b, p_subst kvs l)
  | Loop (r, l) -> Loop (ReplaceAssoc.r_subst kvs r, p_subst kvs l)

(* let get_loc_binders (p:stmt) (kvs: (variable  *)

let stmt_to_s: stmt -> PPrint.t list =
  let open PPrint in
  let rec stmt_to_s : stmt -> PPrint.t list =
    function
    | Inst ISync -> [Line "sync;"]
    | Inst (IAssert b) -> [Line ("assert (" ^ (b_to_s b) ^ ")")]
    | Inst (IAcc (x,a)) -> acc_expr_to_s (x,a)
    | Block l -> [Block (List.map stmt_to_s l |> List.flatten)]
    | LocationAlias l ->
      [Line (
        l.alias_target.var_name ^ " = " ^
        l.alias_source.var_name ^ " + " ^
        n_to_s l.alias_offset ^ ";"
      )]
    | Decl (x, l, n) -> [Line (
        (match l with | Global -> "global" | Local ->  "local") ^ " " ^
        x.var_name ^
        (match n with | Some n -> " = " ^ n_to_s n | None -> "") ^
        ";"
      )]
    | If (b, s1, s2) -> [
        Line ("if (" ^ b_to_s b ^ ") {");
        Block (stmt_to_s s1);
        Line "} else {";
        Block (stmt_to_s s2);
        Line "}"
      ]
    | For (r, s) -> [
        Line ("foreach (" ^ r_to_s r ^ ") {");
        Block (stmt_to_s s);
        Line ("}")
      ]
    | Loop s -> [
        Line ("loop {");
        Block (stmt_to_s s);
        Line ("}")
      ]
  in
  stmt_to_s

let kernel_to_s (k:p_kernel) : PPrint.t list =
  let open PPrint in
  [
    Line ("arrays: " ^ var_set_to_s k.p_kernel_locations ^ ";");
    Line ("scalars: " ^ var_set_to_s k.p_kernel_params ^ ";");
    Line ("pre: " ^ b_to_s k.p_kernel_pre ^";");
    Line "";
    Line "code {";
    Block (stmt_to_s k.p_kernel_code);
    Line "}"
  ]

let print_kernel (k: p_kernel) : unit =
  PPrint.print_doc (kernel_to_s k)

let rec get_variable_decls (p:stmt) (locals,globals:VarSet.t * VarSet.t) : VarSet.t * VarSet.t =
  match p with
  | LocationAlias _
  | Inst _ -> (locals,globals)
  | Block l -> List.fold_right get_variable_decls l (locals,globals)
  | Decl (x, Local, _) -> VarSet.add x locals, globals
  | Decl (x, Global, _) -> locals, VarSet.add x globals
  | If (_, p1, p2) -> get_variable_decls p1 (locals,globals) |> get_variable_decls p2
  | Loop p
  | For (_, p) -> get_variable_decls p (locals,globals)


(** Given a list of key-values, where the values may depend on the keys,
    we want to replace all variables so that the expressions in the keys
    have no references left. *)
let normalize_deps (kvs:(variable * nexp) list) : (variable * nexp) list =
  let defined = List.map fst kvs |> VarSet.of_list in
  (* Compute free names of each substituion expression *)
  let deps : (variable * VarSet.t) list =
    List.map (fun (x,n) ->
      (x, Freenames.free_names_nexp n VarSet.empty |> VarSet.inter defined)
    ) kvs
  in
  (* Compute where each variable is used *)
  let used_by : (variable, variable list) Hashtbl.t =
    let accum : (variable, variable list) Hashtbl.t = Hashtbl.create 100 in
    let rev_deps ((k,ds):(variable * VarSet.t)) : unit =
      List.iter (fun d ->
        Hashtbl.replace accum d (match Hashtbl.find_opt accum d with
          | Some others -> k::others
          | None -> [k]
        )
      ) (VarSet.elements ds)
    in
    List.iter rev_deps deps;
    accum
  in
  (* Convert key-values into a mutable hash table, for performance reasons *)
  let kvs = Common.hashtbl_from_list kvs in
  (* Normalize takes a list of variables with dependencies and *)
  let rec norm (deps: (variable * VarSet.t) list) : unit
  =
    if Common.list_is_empty deps then
      (* no more dependencies to handle return *)
      ()
    else
    let (no_deps, with_deps) =
      List.partition (fun (_,ds) -> VarSet.is_empty ds) deps
    in
    if Common.list_is_empty with_deps then
      ()
    else
    let no_deps_keys = List.map fst no_deps in
    let no_deps : SubstAssoc.t =
      no_deps_keys
      |> List.map (fun x -> (x.var_name, Hashtbl.find kvs x))
      |> SubstAssoc.make
    in
    let used_no_deps : VarSet.t =
      List.fold_left VarSet.union VarSet.empty (List.map snd deps)
      |> VarSet.inter (VarSet.of_list no_deps_keys)
    in
    if VarSet.is_empty used_no_deps then
      ()
    else
      let replaced : (variable, unit) Hashtbl.t = Hashtbl.create 100 in
      (* For every variable no_dep wihtout dependencies *)
      List.iter (fun no_dep ->
        (* For every variable x that uses no_dep *)
        List.iter (fun x ->
          if Hashtbl.mem replaced x then
            (* Any variable that has been replaced once, has already been
               replaced by all no-deps *)
            ()
          else
          (* Get the value associated with variable x *)
          let n = Hashtbl.find kvs x in
          (* Replace all no-deps in n and update the table *)
          Hashtbl.replace kvs x (ReplaceAssoc.n_subst no_deps n);
          (* Mark this variable as being visited *)
          Hashtbl.replace replaced x ()
        ) (Hashtbl.find used_by no_dep)
      ) (VarSet.elements used_no_deps);
      (* Remove all variables without deps from deps, filtering out empty deps *)
      with_deps
      |> List.map (fun (x, ds) -> (x, VarSet.diff ds used_no_deps) )
      |> norm
  in
  norm deps;
  kvs |> Common.hashtbl_elements

let compile (k:p_kernel) : prog kernel =
  let rec pre_from_body (l:prog) : (bexp * prog) =
    match l with
    | [Cond(b,[Cond(b',l)])] -> pre_from_body [Cond(b_and b b', l)]
    | [Cond(b, l)] -> (b, l)
    | l -> (Bool true, l)
  in
  let globals = k.p_kernel_params in
  let locals = VarSet.empty in
  (* Ensures the variable declarations differ from the parameters *)
  let p = normalize_variables k.p_kernel_code (VarSet.union locals globals) in
  let kvs : SubstAssoc.t = get_var_binders p []
    |> normalize_deps
    |> List.map (fun (k,v) -> (k.var_name, v))
    |> SubstAssoc.make
  in
  let locals, globals = get_variable_decls p (locals, globals)  in
  let (more_pre, p) = reify k.p_kernel_locations p |> pre_from_body in
  let p =
    if Hashtbl.length kvs > 0
    then p_subst kvs p
    else p
  in
  (**
    1. We rename all variables so that they are all different
    2. We break down for-loops and variable declarations
    *)
  {
    kernel_pre = b_and k.p_kernel_pre more_pre;
    kernel_locations = k.p_kernel_locations;
    kernel_local_variables = locals;
    kernel_global_variables = globals;
    kernel_code = p;
  }

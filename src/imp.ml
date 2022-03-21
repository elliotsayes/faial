open Exp
open Proto
open Serialize
open Subst

type var_type = Location | Index

type locality = Global | Local

type access_expr = {access_index: nexp list; access_mode: mode}

type alias_expr = {alias_source: variable; alias_target: variable; alias_offset: nexp}

type stmt =
| Sync
| Assert of bexp
| Acc of acc_expr
| Block of (stmt list)
| LocationAlias of alias_expr
| Decl of (variable * locality * nexp option) list
| If of (bexp * stmt * stmt)
| For of (range * stmt)
| Loop of stmt

type prog = stmt list

module Post = struct
  type inst =
  | Sync
  | Acc of acc_expr
  | If of (bexp * inst list * inst list)
  | For of (range * inst list)
  | Loop of inst list
  | Decl of (variable * locality * inst list)

  type prog = inst list

  let rec loc_subst_i (alias:alias_expr) (i:inst) : inst =
    match i with
    | Acc (x, a) ->
      if var_equal x alias.alias_target
      then (
        match a.access_index with
        | [n] ->
          Acc (x, { a with access_index = [n_plus alias.alias_offset n] })
        | _ ->
          let idx = List.length a.access_index |> string_of_int in
          failwith ("Expecting an index with dimension 1, but got " ^ idx)
      )
      else i
    | Decl (x, h, l) -> Decl (x, h, loc_subst_p alias l)
    | Loop s -> Loop (loc_subst_p alias s)
    | If (b, s1, s2) -> If (b, loc_subst_p alias s1, loc_subst_p alias s2)
    | For (r, s) -> For (r, loc_subst_p alias s)
    | Sync -> Sync
  and loc_subst_p (alias:alias_expr) (p:prog) : prog =
    match p with
    | [] -> []
    | i :: p -> loc_subst_i alias i :: loc_subst_p alias p


  module SubstMake(S:Subst.SUBST) = struct
    module M = Subst.Make(S)
    let rec subst_i (st:S.t) (s:inst) : inst =
      match s with
      | Sync -> Sync
      | Acc (x, a) -> Acc (x, M.a_subst st a)
      | Loop p -> Loop (subst_p st p)
      | Decl (x, h, p) ->
        Decl (x, h,
          M.add st x (function
          | Some st' -> subst_p st' p
          | None -> p
          )
        )
      | If (b, p1, p2) -> If (M.b_subst st b, subst_p st p1, subst_p st p2)
      | For (r, p) ->
        For (M.r_subst st r,
          M.add st r.range_var (function
          | Some st -> subst_p st p
          | None -> p
          )
        )

    and subst_p (st:S.t) (p:prog) : prog =
      List.map (subst_i st) p
  
  end

  module ReplacePair = SubstMake(Subst.SubstPair)
  let subst_i = ReplacePair.subst_i
  let subst_p = ReplacePair.subst_p
end

(*
  Translation goals:
  1. lexical scoping is contained in the AST term (simplifies substitution)
  2. inline assignments given by Imp.Decl
  3. inline array alias
  4. inline asserts

  1. In Imp, the lexical scoping of a variable binding is the sequence
  of statements that succeed that statement. In Post, the lexical scoping is
  always  _contained_ in the variable binding operator.
  
  For instance a variable declaration in Imp:
    var x; s1; ...; sn
  Becomes
    var x { s1; ...; sn }
  
  2. In Imp we can have local variable assignments. We inline such assignments
  in Post. However, variable declaration still remains in Post.
  
  In Imp:
    local x = 1; s1; ...; sn
  becomes in Post:
    local x {s1[x=1] ; ... sn[x=1]}
 *)
let imp_to_post (s:stmt) : Post.prog =
  let rec imp_to_post_s (s:stmt) : Post.prog =
    match s with
    | Sync -> [Sync]
    | Acc e -> [Acc e]
    | Block p -> imp_to_post_p p
    | If (b, s1, s2) -> [If (b, imp_to_post_s s1, imp_to_post_s s2)]
    | For (r, s) -> [For (r, imp_to_post_s s)]
    | Loop s -> [Loop (imp_to_post_s s)]
    (* Handled in the context of a prog *)
    | Assert _ -> failwith "unsupported"
    | LocationAlias _ -> failwith "unsupported"
    | Decl _ -> failwith "unsupported"
  and imp_to_post_p (p:prog) : Post.prog =
    match p with
    | [] -> []
    | Assert b :: p -> [If (b, imp_to_post_p p, [])]
    | LocationAlias e :: p -> imp_to_post_p p |> Post.loc_subst_p e
    | Decl [] :: p -> imp_to_post_p p
    | Decl ((x,v,o)::l) :: p ->
      let body = imp_to_post_p (Decl l :: p) in
      let body = match o with
      | Some n -> Post.subst_p (x, n) body
      | None -> body
      in
      [Decl (x, v, body)]
    | s :: p ->
      Common.append_tr (imp_to_post_s s) (imp_to_post_p p)
  in
  imp_to_post_s (Block [s])

let post_to_proto: Post.prog -> Proto.prog =
  let rec post_to_proto_i (i:Post.inst) : Proto.prog =
    match i with
    | Sync -> [Sync]
    | Acc e -> [Acc e]
    | If (_, [], []) -> []
    | If (b, p, []) -> [Cond (b, post_to_proto_p p)]
    | If (b, [], p) -> [Cond (BNot b, post_to_proto_p p)]
    | If (b, p1, p2) -> [Cond (b, post_to_proto_p p1); Cond (BNot b, post_to_proto_p p2)]
    | For (r, p) -> [Loop (r, post_to_proto_p p)]
    | Loop p -> [Loop (mk_range (var_make "X?") (Num 2), post_to_proto_p p)]
    | Decl (x, h, l) -> post_to_proto_p l
  and post_to_proto_p (p:Post.prog) : Proto.prog =
    List.concat_map post_to_proto_i p
  in
  post_to_proto_p
  
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
  p_kernel_arrays: array_t VarMap.t;
  (* The internal variables are used in the code of the kernel.  *)
  p_kernel_params: VarSet.t;
  (* The code of a kernel performs the actual memory accesses. *)
  p_kernel_code: stmt;
}

(** Variable normalization: Makes all variable declarations distinct. *)

let loc_subst (alias:alias_expr) (s:stmt) : stmt =
  let rec s_subst (alias:alias_expr) (s:stmt) : stmt =
    match s with
    | Acc (x, a) ->
      if var_equal x alias.alias_target
      then (
        match a.access_index with
        | [n] ->
          Acc (x, { a with access_index = [n_plus alias.alias_offset n] })
        | _ ->
          let idx = List.length a.access_index |> string_of_int in
          failwith ("Expecting an index with dimension 1, but got " ^ idx)
      )
      else s
    | Loop s -> Loop (s_subst alias s)
    | Block p -> Block (p_subst alias p)
    | If (b, s1, s2) -> If (b, s_subst alias s1, s_subst alias s2)
    | For (r, s) -> For (r, s_subst alias s)
    | LocationAlias _
    | Decl _
    | Sync
    | Assert _ -> s
  and p_subst (alias:alias_expr) (p:prog) : prog =
    match p with
    | [] -> []
    | LocationAlias new_alias :: p -> p_subst alias (p_subst new_alias p)
    | s :: p -> s_subst alias s :: p_subst alias p
  in
  s_subst alias s

module SubstMake(S:Subst.SUBST) = struct
  module M = Subst.Make(S)
  let on_subst (st:S.t) (o:nexp option) : nexp option =
    match o with
    | Some n -> Some (M.n_subst st n)
    | None -> None

  let rec s_subst (st:S.t) (s:stmt) : stmt =
    match s with
    | Sync -> Sync
    | LocationAlias a ->
      LocationAlias {a with alias_offset = M.n_subst st a.alias_offset}
    | Assert b -> Assert (M.b_subst st b)
    | Acc (x, a) -> Acc (x, M.a_subst st a)
    | Block p -> Block (p_subst st p)
    | Loop p -> Loop (s_subst st p)
    | Decl l -> Decl (List.map (fun (x, v, o) -> (x, v, on_subst st o)) l)
    | If (b, p1, p2) -> If (M.b_subst st b, s_subst st p1, s_subst st p2)
    | For (r, p) ->
      For (M.r_subst st r,
        M.add st r.range_var (function
        | Some st -> s_subst st p
        | None -> p
        )
      )

  and p_subst (st:S.t) (p:prog) : prog =
    match p with
    | [] -> []
    | Decl [] :: p ->
      (* Skip decl when there was nothing defined *)
      p_subst st p
    | Decl [(x, v, o)] :: p ->
      (* When there is a single declation, handle shadowing *)
      let p = M.add st x (function
        | Some st' -> p_subst st' p
        | None -> p
      )
      in
      Decl [(x, v, on_subst st o)] :: p
    | Decl (e::l) :: p ->
      (* When there is more than one, flatten it into a sequence of one *)
      p_subst st (Decl [e] :: Decl l :: p)
    | s :: p -> s_subst st s :: p_subst st p
  
  end

module ReplacePair = SubstMake(Subst.SubstPair)

let normalize_variables (s:stmt) (xs:VarSet.t) : stmt =
  let add_var x xs =
    if VarSet.mem x xs then (
      let new_x : variable = Bindings.generate_fresh_name x xs in
      let new_xs = VarSet.add new_x xs in
      let si = Subst.SubstPair.make (x, Var new_x) in
      (Some (new_x, si), new_xs)
    ) else (
      let new_xs = VarSet.add x xs in
      (None, new_xs)
    )
  in
  let rec norm_s (s:stmt) xs : stmt * VarSet.t =
    match s with
    | Loop s ->
      let (s, xs) = norm_s s xs in
      Loop s, xs
    | LocationAlias _
    | Acc _
    | Assert _
    | Sync -> (s, xs)
    | Block p ->
      let (p, xs) = norm_p p xs in
      Block p, xs
    | Decl l ->
      (* Propagate the side effects through each bindings *)
      let rec iter l xs =
        match l with
        | [] -> [], xs
        | (x, v, n)::l ->
          let (x, v, n), xs = match add_var x xs with
          | Some (x, si), xs -> (x, v, n), xs
          | None, xs -> (x, v, n), xs
          in
          let l, xs = iter l xs in
          (x, v, n) :: l, xs
      in
      let l, xs = iter l xs in
      Decl l, xs
    | If (b, s1, s2) ->
      let s1, xs = norm_s s1 xs in
      let s2, xs = norm_s s2 xs in
      If (b, s1, s2), xs
    | For (r, s) ->
      (match add_var r.range_var xs with
      | Some (x, si), xs ->
        (* Update loop variable with new var *)
        let r = { r with range_var = x; } in
        (* Make sure body uses new variable *)
        let s, xs = norm_s (ReplacePair.s_subst si s) xs in
        For (r, s), xs
      | None, xs ->
        (* Otherwise, just normalize the loop body *)
        let s, xs = norm_s s xs in
        For (r, s), xs 
      )
  and norm_p (p:prog) xs =
    match p with
    | [] -> [], xs
    | Decl [] :: p -> p, xs
    | Decl [(x, v, n)] :: p ->
      (match add_var x xs with
      | (Some (x, si), xs) ->
        (* Make sure the code that follows uses the new var, and normalize it *)
        let p, xs = norm_p (ReplacePair.p_subst si p) xs in
        Decl [(x, v, n)] :: p, xs
      | (None, xs) ->
        (* Otherwise, just normalize the code that follows *)
        let p, xs = norm_p p xs in
        Decl [(x, v, n)] :: p, xs 
      )
    | Decl (e::l) :: p ->
      norm_p (Decl [e] :: Decl l :: p) xs
    | s :: p ->
        let s, xs = norm_s s xs in
        let p, xs = norm_p p xs in
        s :: p, xs
  in
  norm_s s xs |> fst

let reify (locations:VarSet.t) (s:stmt) : Proto.prog =
  let rec reify_s : stmt -> Proto.prog =
    function
    | LocationAlias _ (* Only handled inside a block *) 
    | Decl _ (* Handled by normalize_deps *)
    | Assert _ -> []
    | Block p -> reify_p p
    | Sync -> [Sync]
    | Acc (x,y) ->
      if VarSet.mem x locations
      then [Acc (x,y)]
      else []
    | If (b, s, Block []) -> [Cond (b, reify_s s)]
    | If (b, Block[], s) -> [Cond(BNot b, reify_s s)]
    | If (b, s1, s2) -> [Cond (b, reify_s s1); Cond(BNot b, reify_s s2)]
    | Loop s ->
      begin match reify_s s with
      | [] -> []
      | p -> [Loop (mk_range (var_make "X?") (Num 2), p)]
      end
    | For (r, s) ->
      begin match reify_s s with
      | [] -> []
      | p -> [Loop (r, p)]
      end
  and reify_p : prog -> Proto.prog =
    function
    | [] -> []
    | Assert b :: p -> [Cond (b, reify_p p)]
    | LocationAlias a :: p ->
      Block p
      |> loc_subst a
      |> reify_s
    | s :: p -> Common.append_tr (reify_s s) (reify_p  p)
  in
  reify_s s

let rec get_var_binders (p:stmt) (kvs: (variable * nexp) list) : (variable * nexp) list =
  match p with
  | Decl [] -> kvs
  | Decl (e :: l) ->
    let kvs = match e with
    | (x,_,Some n) -> (x,n)::kvs
    | _ -> kvs
    in
    get_var_binders (Decl l) kvs
  | LocationAlias _
  | Assert _
  | Sync
  | Acc _
  | Block []
    -> kvs
  | Block (i::l) -> get_var_binders i kvs |> get_var_binders (Block l)
  | If (_, p, q) -> get_var_binders p kvs |> get_var_binders q
  | Loop p
  | For (_, p)
    -> get_var_binders p kvs

let rec p_subst (kvs: SubstAssoc.t) (p:Proto.prog) : Proto.prog =
  List.map (i_subst kvs) p
and i_subst (kvs: SubstAssoc.t) (i:Proto.inst) : Proto.inst =
  match i with
  | Acc e -> Acc (ReplaceAssoc.acc_expr_subst kvs e)
  | Sync -> Sync
  | Cond (b, l) -> Cond (ReplaceAssoc.b_subst kvs b, p_subst kvs l)
  | Loop (r, l) -> Loop (ReplaceAssoc.r_subst kvs r, p_subst kvs l)

let stmt_to_s: stmt -> PPrint.t list =
  let open PPrint in
  let rec stmt_to_s : stmt -> PPrint.t list =
    function
    | Sync -> [Line "sync;"]
    | Assert b -> [Line ("assert (" ^ (b_to_s b) ^ ")")]
    | Acc e -> acc_expr_to_s e
    | Block l -> [Line "{"; Block (List.map stmt_to_s l |> List.flatten); Line "}"]
    | LocationAlias l ->
      [Line (
        var_name l.alias_target ^ " = " ^
        var_name l.alias_source ^ " + " ^
        n_to_s l.alias_offset ^ ";"
      )]
    | Decl l ->
      let entry (x, l, n) =
        (match l with | Global -> "global" | Local ->  "local") ^ " " ^
        var_name x ^
        (match n with | Some n -> " = " ^ n_to_s n | None -> "")
      in
      let entries = Common.join "," (List.map entry l) in
      [Line (entries ^ ";")]
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
    Line ("arrays: " ^ array_map_to_s k.p_kernel_arrays ^ ";");
    Line ("globals: " ^ var_set_to_s k.p_kernel_params ^ ";");
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
  | Assert _
  | Acc _
  | Sync -> locals, globals
  | Block l -> List.fold_right get_variable_decls l (locals,globals)
  | Decl [] -> locals, globals
  | Decl ((x,h,_) :: l) ->
    let locals, globals = match h with
    | Local -> VarSet.add x locals, globals
    | Global -> locals, VarSet.add x globals
    in
    get_variable_decls (Decl l) (locals, globals)
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
      |> List.map (fun x -> (var_name x, Hashtbl.find kvs x))
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
        ) (match Hashtbl.find_opt used_by no_dep with | Some l -> l | None -> [])
      ) (VarSet.elements used_no_deps);
      (* Remove all variables without deps from deps, filtering out empty deps *)
      with_deps
      |> List.map (fun (x, ds) -> (x, VarSet.diff ds used_no_deps) )
      |> norm
  in
  norm deps;
  kvs |> Common.hashtbl_elements

let compile (k:p_kernel) : Proto.prog kernel =
  let globals = k.p_kernel_params in
  let locals = VarSet.empty in
  (* Ensures the variable declarations differ from the parameters *)
  let p = normalize_variables k.p_kernel_code (VarSet.union locals globals) in
  stmt_to_s k.p_kernel_code |> PPrint.print_doc;
  let kvs : SubstAssoc.t = get_var_binders p []
    |> normalize_deps
    |> List.map (fun (k,v) -> (var_name k, v))
    |> SubstAssoc.make
  in
  let locals, globals = get_variable_decls p (locals, globals)  in
  let rec pre_from_body (l:Proto.prog) : (bexp * Proto.prog) =
    match l with
    | [Cond(b,[Cond(b',l)])] -> pre_from_body [Cond(b_and b b', l)]
    | [Cond(b, l)] -> (b, l)
    | l -> (Bool true, l)
  in
  let (more_pre, p) = reify (var_map_to_set k.p_kernel_arrays) p
    |> pre_from_body in
  let p =
    if Hashtbl.length kvs > 0
    then p_subst kvs p
    else p
  in
  let pre = b_and k.p_kernel_pre more_pre in
  let pre =
    if Hashtbl.length kvs > 0
    then ReplaceAssoc.b_subst kvs pre
    else pre
  in
  (**
    1. We rename all variables so that they are all different
    2. We break down for-loops and variable declarations
    *)
  {
    kernel_name = k.p_kernel_name;
    kernel_pre = pre;
    kernel_arrays = k.p_kernel_arrays;
    kernel_local_variables = locals;
    kernel_global_variables = globals;
    kernel_code = p;
  }

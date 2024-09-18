open Stage0

let (@) = Common.append_tr

open Exp

module Code = struct
  (* The source instruction uses the base defined above *)
  type t =
    | Acc of (Variable.t * Access.t)
    | Sync of Location.t option
    | If of bexp * t * t
    | Loop of Range.t * t
    | Seq of t * t
    | Skip
    | Decl of {var: Variable.t; ty:C_type.t; body: t}

  let rec filter (f:t -> bool) (p:t) : t =
    if not (f p) then Skip else
    match p with
    | Sync _
    | Skip
    | Acc _ -> p
    | Seq (p, q) -> Seq (filter f p, filter f q)
    | If (b, p, q) -> If (b, filter f p, filter f q)
    | Decl d -> Decl { d with body = filter f d.body }
    | Loop (r, p) -> Loop (r, filter f p)

  let rec exists (f:t -> bool) (i: t) : bool =
    f i ||
    match i with
    | Acc _ | Sync _ | Skip -> false
    | Loop (_, p) | Decl {body=p; _} -> exists f p
    | If (_, p, q) | Seq (p, q) -> exists f p || exists f q

  (** Replace variables by constants. *)

  module Make (S:Subst.SUBST) = struct
    module M = Subst.Make(S)
    let rec subst (s:S.t) (i:t) : t =
      match i with
      | Skip -> Skip
      | Seq (p, q) -> Seq (subst s p, subst s q)
      | Acc (x, e) -> Acc (x, M.a_subst s e)
      | Sync l -> Sync l
      | If (b, p, q) -> If (
          M.b_subst s b,
          subst s p,
          subst s q
        )
      | Decl d ->
        M.add s d.var (function
          | Some s -> Decl { d with body = subst s d.body }
          | None -> Decl d
        )
      | Loop (r, p) ->
        let r = M.r_subst s r in
        M.add s r.var (function
          | Some s -> Loop (r, subst s p)
          | None -> Loop (r, p)
        )
  end

  let apply_arch (arrays:Variable.Set.t) : Architecture.t -> t -> t =
    function
    | Grid ->
      filter (
        function
        | Sync _ -> false
        | Acc (x, _) -> Variable.Set.mem x arrays
        | _ -> true
      )
    | Block -> fun s -> s

  module PSubstAssoc = Make(Subst.SubstAssoc)
  module PSubstPair = Make(Subst.SubstPair)

  let seq (p: t) (q: t) : t =
    match p, q with
    | Skip, p | p, Skip -> p
    | _, _ -> Seq (p, q)

  let if_ (b:bexp) (p:t) (q:t) : t =
    match b, p, q with
    | Bool b, _, _ -> if b then p else q
    | _, Skip, Skip -> Skip
    | _, Skip, _ -> If (b_not b, q, Skip)
    | _, _, _ -> If(b, p, q)

  let loop (r:Range.t) (p:t) : t =
    if p = Skip then Skip else
    let is_empty =
      r
      |> Range.is_empty
      |> Exp.b_eval_res
      |> Result.value ~default:false
    in
    if is_empty then Skip else
    Loop (r, p)

  let decl ?(ty=C_type.int) (var:Variable.t) : t -> t =
    function
    | Skip -> Skip
    | body -> Decl {var; ty; body}

  let rec opt : t -> t =
    function
    | Skip -> Skip
    | Decl d -> Decl { d with body = opt d.body }
    | Seq (p, q) -> seq (opt p) (opt q)
    | Acc (x, e) -> Acc (x, Constfold.a_opt e)
    | Sync l -> Sync l
    | If (b, p, q) -> if_ (Constfold.b_opt b) (opt p) (opt q)
    | Loop (r, p) -> loop (Constfold.r_opt r) (opt p)

  let subst_block_dim (block_dim:Dim3.t) (p:t) : t =
    let subst x n p =
      PSubstPair.subst (Variable.from_name x, Num n) p
    in
    p
    |> subst "blockDim.x" block_dim.x
    |> subst "blockDim.y" block_dim.y
    |> subst "blockDim.z" block_dim.z

  let subst_grid_dim (grid_dim:Dim3.t) (p:t) : t =
    let subst x n p =
      PSubstPair.subst (Variable.from_name x, Num n) p
    in
    p
    |> subst "gridDim.x" grid_dim.x
    |> subst "gridDim.y" grid_dim.y
    |> subst "gridDim.z" grid_dim.z

  let vars_distinct : t -> Variable.Set.t -> t =
    let rec uniq (i:t) (xs:Variable.Set.t) : t * Variable.Set.t =
      match i with
      | Skip
      | Acc _
      | Sync _
        -> (i, xs)
      | If (b, p, q) ->
        let (p, xs) = uniq p xs in
        let (q, xs) = uniq q xs in
        (If (b, p, q), xs)
      | Decl d ->
        let x = d.var in
        let p = d.body in
        if Variable.Set.mem x xs then (
          let new_x : Variable.t = Variable.fresh xs x in
          let new_xs = Variable.Set.add new_x xs in
          let s = Subst.SubstPair.make (x, Var new_x) in
          let new_p = PSubstPair.subst s p in
          let (p, new_xs) = uniq new_p new_xs in
          Decl {var = new_x; body=p; ty=d.ty; }, new_xs
        ) else (
          let (p, new_xs) = uniq p (Variable.Set.add x xs) in
          Decl {var=x; body=p; ty=d.ty}, new_xs
        )
      | Loop (r, p) ->
        let x = r.var in
        if Variable.Set.mem x xs then (
          let new_x : Variable.t = Variable.fresh xs x in
          let new_xs = Variable.Set.add new_x xs in
          let s = Subst.SubstPair.make (x, Var new_x) in
          let new_p = PSubstPair.subst s p in
          let (p, new_xs) = uniq new_p new_xs in
          Loop ({ r with var = new_x }, p), new_xs
        ) else (
          let (p, new_xs) = uniq p (Variable.Set.add x xs) in
          Loop (r, p), new_xs
        )
      | Seq (i, p) ->
        let (i, xs) = uniq i xs in
        let (p, xs) = uniq p xs in
        (Seq (i, p), xs)
    in
    fun p known -> uniq p known |> fst

  let rec free_names (i:t) (fns:Variable.Set.t) : Variable.Set.t =
    match i with
    | Skip | Sync _ -> fns
    | Acc (_, a) -> Access.free_names a fns
    | If (b, p, q) ->
      b_free_names b fns
      |> free_names p
      |> free_names q
    | Decl {var=x; body=p; _} ->
      free_names p fns
      |> Variable.Set.remove x
    | Loop (r, p) ->
      free_names p fns
      |> Variable.Set.remove r.var
      |> Range.free_names r
    | Seq (p, q) ->
      free_names p fns
      |> free_names q

  (* Only retain CI-DI accesses *)
  let rec to_ci_di (approx:Variable.Set.t) : t -> t =
    function
    | If (b, p, q) ->
      if Exp.b_intersects approx b then
        Skip
      else
        If (b, to_ci_di approx p, to_ci_di approx q)
    | Loop (r, p) ->
      if Range.intersects approx r then Skip else
      (* the loop variable is CIDI, hence remove any existing CIDI *)
      let approx = Variable.Set.remove (Range.var r) approx in
      Loop (r, to_ci_di approx p)
    | Acc (x, a) ->
      if Access.intersects approx a then
        Skip
      else
        Acc (x, a)
    | Decl {var=x; body=p; _} ->
      (* In this scope x is approximate *)
      to_ci_di (Variable.Set.add x approx) p
    | Seq (p, q) -> Seq (to_ci_di approx p, to_ci_di approx q)
    | Skip -> Skip
    | Sync a -> Sync a

  let rec used_arrays (i:t) (fns:Variable.Set.t) : Variable.Set.t =
    match i with
    | Skip | Sync _ -> fns
    | Acc (x, _) -> Variable.Set.add x fns
    | Decl {body=p; _} | Loop (_, p) ->
      used_arrays p fns
    | If (_, p, q) | Seq (p, q) ->
      used_arrays p fns
      |> used_arrays q

  let rec to_s : t -> Indent.t list =
    function
    | Skip -> [Line "skip;"]
    | Sync _ -> [Line "sync;"]
    | Acc (x, e) -> [Line (Access.to_string ~name:(Variable.name x) e)]
    | If (b, p, Skip) -> [
        Line ("if (" ^ b_to_string b ^ ") {");
        Block (to_s p);
        Line "}"
      ]
    | If (b, p, q) -> [
        Line ("if (" ^ b_to_string b ^ ") {");
        Block (to_s p);
        Line ("} else {");
        Block (to_s q);
        Line "}"
      ]
    | Decl d ->
      let var = Variable.name d.var in
      let ty = C_type.to_string d.ty in
      (Line (ty ^ " " ^ var ^ ";"))
      :: to_s d.body
    | Loop (r, p) ->
      [
        Line ("foreach (" ^ Range.to_string r ^ ") {");
        Block (to_s p);
        Line "}"
      ]
    | Seq (p, q) ->
      to_s p @ to_s q

  let to_string (p:t) : string =
    Indent.to_string (to_s p)

  let print (p: t) : unit =
    Indent.print (to_s p)

end

module Kernel = struct
  (* __global__ kernels are entry points and can be invoked from
    the host. Auxiliary kernels have a __device__ attribute and
    can only be invoked from the GPU code. *)
  type visible = Global | Device
  type 'a t = {
    (* The kernel name *)
    name : string;
    (* The internal variables are used in the code of the kernel.  *)
    global_variables: Params.t;
    (* The internal variables are used in the code of the kernel.  *)
    local_variables: Params.t;
    (* The modifiers of each array *)
    arrays: Memory.t Variable.Map.t;
    (* A thread-local pre-condition that is true on all phases. *)
    pre: bexp;
    (* The code of a kernel performs the actual memory accesses. *)
    code: 'a;
    (* The kernel's visibility *)
    visibility : visible;
    (* Number of blocks *)
    grid_dim: Dim3.t option;
    (* Number of blocks *)
    block_dim: Dim3.t option;
  }

  let apply_arch (a:Architecture.t) (k:Code.t t) : Code.t t =
    let d = Architecture.to_defaults a in
    let arrays =
      Variable.Map.filter (fun _ a -> Memory.is_global a) k.arrays
    in
    { k with
      arrays = (match a with
        | Grid -> arrays
        | Block -> k.arrays);
      code = Code.apply_arch (Variable.MapSetUtil.map_to_set arrays) a k.code;
      global_variables = Params.union_right k.global_variables d.globals;
      local_variables = Params.union_right k.local_variables d.locals;
      pre = b_and (Architecture.Defaults.to_bexp d) k.pre;
    }

  let is_global (k: 'a t) : bool =
    k.visibility = Global

  let is_device (k: 'a t) : bool =
    k.visibility = Device

  let has_shared_arrays (k: 'a t) : bool =
    Variable.Map.exists
      (fun _ m -> Memory.is_shared m)
      k.arrays

  let shared_arrays (k:'a t) : Variable.Set.t =
    k.arrays
    |> Variable.Map.filter (fun _ v -> Memory.is_shared v)
    |> Variable.MapSetUtil.map_to_set

  let global_arrays (k:'a t) : Variable.Set.t =
    k.arrays
    |> Variable.Map.filter (fun _ v -> Memory.is_global v)
    |> Variable.MapSetUtil.map_to_set

  let used_arrays (k: Code.t t) : Variable.Set.t =
    Code.used_arrays k.code Variable.Set.empty

  let constants (k:Code.t t) =
    let rec constants (b: bexp) (kvs:(string*int) list) : (string*int) list =
      match b with
      | CastBool (CastInt b) -> constants b kvs
      | NRel (Eq, Var x, Num n)
      | NRel (Eq, Num n, Var x) ->
        (Variable.name x, n) :: kvs
      | BRel (BAnd, b1, b2) ->
        constants b1 kvs |> constants b2
      | Bool _
      | CastBool _
      | BNot _
      | Pred _
      | NRel _
      | BRel _ -> kvs
    in
    constants k.pre []

  let filter_access (f: Access.t -> bool) (k:Code.t t) : Code.t t =
    { k with
      code = Code.filter (
      function
      | Acc (_, a) -> f a
      | _ -> true
      ) k.code
    }

  let filter_array (to_keep: Variable.t -> bool) (k:Code.t t) : Code.t t =
    { k with
      (* update the set of arrays *)
      arrays = Variable.Map.filter (fun v _ -> to_keep v) k.arrays;
      code = Code.filter (
      function
      | Acc (v, _) -> to_keep v
      | _ -> true
      ) k.code
    }

  (* Create a new kernel with same name, but no code to check *)
  let clear (k:Code.t t) : Code.t t =
    {
      name = k.name;
      arrays = Variable.Map.empty;
      pre = Bool true;
      code = Skip;
      global_variables = Params.empty;
      local_variables = Params.empty;
      visibility = k.visibility;
      block_dim = None;
      grid_dim = None;
    }

  let opt (k:Code.t t) : Code.t t =
    {
      k with
      pre = Constfold.b_opt k.pre;
      code = Code.opt k.code;
    }

  let global_set (k:'a t) : Variable.Set.t =
    Params.to_set k.local_variables

  let local_set (k:'a t) : Variable.Set.t =
    Params.to_set k.local_variables

  let parameter_set (k:'a t) : Variable.Set.t =
    Variable.Set.union (local_set k) (global_set k)

  let assign_globals (kvs:(string*int) list) (k:Code.t t) : Code.t t =
    if Common.list_is_empty kvs then k else
    (* retrieve a set of key-values *)
    let keys =
      kvs
      |> List.split
      |> fst
      |> List.map Variable.from_name
      |> Variable.Set.of_list
    in
    let non_global_keys = Variable.Set.diff keys (global_set k) in
    (if Variable.Set.is_empty non_global_keys then
      raise (invalid_arg ("The following keys are not thread-global parameters: " ^
      Variable.set_to_string non_global_keys))
    else ());
    let kvs =
      kvs
      |> List.map (fun (x,n) -> x, Num n)
      |> Subst.SubstAssoc.make
    in
    {
      k with
      pre = Code.PSubstAssoc.M.b_subst kvs k.pre;
      code = Code.PSubstAssoc.subst kvs k.code;
      global_variables = Params.remove_all keys k.global_variables;
      local_variables = Params.remove_all keys k.local_variables;
    }

  let vars_distinct (k:Code.t t) : Code.t t =
    { k with
      code = Code.vars_distinct k.code (parameter_set k)
    }

  (*
    Makes all variables distinct and hoists declarations as
    thread-locals.
  *)
  let hoist_decls : Code.t t -> Code.t t =
    let rec inline (vars:Params.t) (p:Code.t)
    :
      Params.t * Code.t
    =
      match p with
      | Decl {var=x; body=p; ty;} -> inline (Params.add x ty vars) p
      | Acc _ | Skip | Sync _ -> (vars, p)
      | If (b, p, q) ->
        let (vars, p) = inline vars p in
        let (vars, q) = inline vars q in
        (vars, If (b, p, q))
      | Loop (r, p) ->
        let (vars, p) = inline vars p in
        (vars, Loop (r, p))
      | Seq (p, q) ->
        let (vars, p) = inline vars p in
        let (vars, q) = inline vars q in
        (vars, Seq (p, q))
    in
    fun k ->
    let k = vars_distinct k in
    let (locals, p) = inline Params.empty k.code in
    { k with
      code = p;
      local_variables = Params.union_left locals k.local_variables;
    }

  let inline_dims (dims:(string*Dim3.t) list) (k:Code.t t) : Code.t t =
    let key_vals = List.concat_map (fun (name, d) ->
      Dim3.to_assoc ~prefix:(name ^ ".") d
    ) dims in
    assign_globals key_vals k

  let inline_inferred (k:Code.t t) : Code.t t =
    let key_vals =
      constants k
      |> List.filter (fun (x,_) ->
        (* Make sure we only replace thread-global variables *)
        Params.mem (Variable.from_name x) k.global_variables
      )
    in
    assign_globals key_vals k

  let inline_all ~globals ~block_dim ~grid_dim (k:Code.t t) : Code.t t =
    let or_ o1 o2 =
      if Option.is_some o1 then o1 else o2
    in
    let block_dim = or_ k.block_dim block_dim in
    let grid_dim = or_ k.grid_dim grid_dim in
    let to_dim k d =
      d
      |> Option.map (fun x -> [(k, x)])
      |> Option.value ~default:[]
    in
    k
    |> assign_globals globals
    |> inline_dims (
        to_dim "blockDim" block_dim @
        to_dim "gridDim" grid_dim
      )
    |> inline_inferred

  let used_variables (k:Code.t t) : Variable.Set.t =
    Code.free_names k.code Variable.Set.empty
    |> Exp.b_free_names k.pre

  let trim_binders (k:Code.t t) : Code.t t =
    let fns = used_variables k in
    { k with
      global_variables = Params.retain_all fns k.global_variables;
      local_variables = Params.retain_all fns k.local_variables;
    }

  let free_names (k: Code.t t) : Variable.Set.t =
    let fns = used_variables k in
    let fns = Variable.Set.diff fns (Params.to_set k.local_variables) in
    let fns = Variable.Set.diff fns (Params.to_set k.global_variables) in
    fns

  (*
  Given a protocol with free names, add those as thread-locals.
   *)
  let add_missing_binders (k:Code.t t) : Code.t t =
    let locals = Params.from_set C_type.int (free_names k) in
    { k with
      local_variables = Params.union_left k.local_variables locals;
    }

  let to_ci_di (k:Code.t t) : Code.t t =
    let approx =
      k.local_variables
      |> Params.to_set
    in
    let approx = Variable.Set.diff approx Variable.tid_set in
    { k with
      code = Code.to_ci_di approx k.code }

  let to_s (f:'a -> Indent.t list) (k:'a t) : Indent.t list =
    [
      Line ("name: " ^ k.name ^ ";");
      Line ("arrays: " ^ Memory.map_to_string k.arrays ^ ";");
      Line ("globals: " ^ Params.to_string k.global_variables ^ ";");
      Line ("locals: " ^ Params.to_string k.local_variables ^ ";");
      Line ("invariant:");
      Block (b_to_s k.pre);
      Line ";";
      Line "code:";
      Block (f k.code);
      Line "; end of code"
    ]

  let print (f:'a -> Indent.t list) (k: 'a t) : unit =
    Indent.print (to_s f k)

end


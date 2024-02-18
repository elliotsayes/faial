open Stage0

let (@) = Common.append_tr

open Exp

module Code = struct
  (* The source instruction uses the base defined above *)
  type t =
    | Acc of (Variable.t * Access.t)
    | Sync of Location.t option
    | Cond of bexp * t
    | Loop of Range.t * t
    | Seq of t * t
    | Skip
    | Decl of Variable.t * t

  let rec filter (f:t -> bool) (p:t) : t =
    if not (f p) then Skip else
    match p with
    | Sync _
    | Skip
    | Acc _ -> p
    | Seq (p, q) -> Seq (filter f p, filter f q)
    | Cond (b, p) -> Cond (b, filter f p)
    | Decl (x, p) -> Decl (x, filter f p)
    | Loop (r, p) -> Loop (r, filter f p)

  let rec exists (f:t -> bool) (i: t) : bool =
    f i ||
    match i with
    | Acc _ | Sync _ | Skip -> false
    | Cond (_, p) | Loop (_, p) | Decl (_, p) -> exists f p
    | Seq (p, q) -> exists f p || exists f q


  (** Replace variables by constants. *)

  module Make (S:Subst.SUBST) = struct
    module M = Subst.Make(S)
    let rec subst (s:S.t) (i:t) : t =
      match i with
      | Skip -> Skip
      | Seq (p, q) -> Seq (subst s p, subst s q)
      | Acc (x, e) -> Acc (x, M.a_subst s e)
      | Sync l -> Sync l
      | Cond (b, p) -> Cond (
          M.b_subst s b,
          subst s p
        )
      | Decl (x, p) ->
        M.add s x (function
          | Some s -> Decl (x, subst s p)
          | None -> Decl (x, p)
        )
      | Loop (r, p) ->
        let r = M.r_subst s r in
        M.add s r.var (function
          | Some s -> Loop (r, subst s p)
          | None -> Loop (r, p)
        )
  end

  module PSubstAssoc = Make(Subst.SubstAssoc)
  module PSubstPair = Make(Subst.SubstPair)

  let seq (p: t) (q: t) : t =
    match p, q with
    | Skip, p | p, Skip -> p
    | _, _ -> Seq (p, q)

  let cond (b:bexp) (p:t) : t =
    match b, p with
    | Bool true, _ -> p
    | _, Skip | Bool false, _ -> Skip
    | _, _ -> Cond(b, p)

  let loop (r:Range.t) (p:t) : t =
    match r.lower_bound, r.upper_bound, p with
    | _, _, Skip -> Skip
    | Num lb, Num ub, _ when lb >= ub -> Skip
    | _, _, _ -> Loop (r, p)

  let decl (x:Variable.t) (p:t) : t =
    match p with
    | Skip -> Skip
    | _ -> Decl (x, p)

  let rec opt : t -> t =
    function
    | Skip -> Skip
    | Decl (x, p) -> Decl (x, opt p)
    | Seq (p, q) -> seq (opt p) (opt q)
    | Acc (x, e) -> Acc (x, Constfold.a_opt e)
    | Sync l -> Sync l
    | Cond(b, p) -> cond (Constfold.b_opt b) (opt p)
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
      | Cond (b, p) ->
        let (p, xs) = uniq p xs in
        (Cond (b, p), xs)
      | Decl (x, p) ->
        if Variable.Set.mem x xs then (
          let new_x : Variable.t = Variable.fresh xs x in
          let new_xs = Variable.Set.add new_x xs in
          let s = Subst.SubstPair.make (x, Var new_x) in
          let new_p = PSubstPair.subst s p in
          let (p, new_xs) = uniq new_p new_xs in
          Decl (new_x, p), new_xs
        ) else (
          let (p, new_xs) = uniq p (Variable.Set.add x xs) in
          Decl (x, p), new_xs
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

  let rec to_s : t -> Indent.t list =
    function
    | Skip -> [Line "skip;"]
    | Sync _ -> [Line "sync;"]
    | Acc (x, e) -> [Line (Access.to_string ~name:(Variable.name x) e)]
    | Cond (b, p1) -> [
        Line ("if (" ^ b_to_string b ^ ") {");
        Block (to_s p1);
        Line "}"
      ]
    | Decl (x, p) ->
      (Line ("var " ^ Variable.name x ^ ";"))
      :: to_s p
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
    global_variables: Variable.Set.t;
    (* The internal variables are used in the code of the kernel.  *)
    local_variables: Variable.Set.t;
    (* The modifiers of each array *)
    arrays: Memory.t Variable.Map.t;
    (* A thread-local pre-condition that is true on all phases. *)
    pre: bexp;
    (* The code of a kernel performs the actual memory accesses. *)
    code: 'a;
    (* The kernel's visibility *)
    visibility : visible;
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

  let constants (k:Code.t t) =
    let rec constants (b: bexp) (kvs:(string*int) list) : (string*int) list =
      match b with
      | Bool _ -> kvs
      | NRel (NEq, Var x, Num n)
      | NRel (NEq, Num n, Var x) ->
        (Variable.name x, n) :: kvs
      | BRel (BAnd, b1, b2) ->
        constants b1 kvs |> constants b2
      | BNot _
      | Pred _
      | NRel _
      | BRel _ -> kvs
    in
    constants k.pre []


  (* Create a new kernel with same name, but no code to check *)
  let clear (k:Code.t t) : Code.t t =
    {
      name = k.name;
      arrays = Variable.Map.empty;
      pre = Bool true;
      code = Skip;
      global_variables = Variable.Set.empty;
      local_variables = Variable.Set.empty;
      visibility = k.visibility;
    }

  let opt (k:Code.t t) : Code.t t =
    {
      k with
      pre = Constfold.b_opt k.pre;
      code = Code.opt k.code;
    }

  let replace_constants (kvs:(string*int) list) (k:Code.t t) : Code.t t =
    if Common.list_is_empty kvs then k else
    begin
      let kvs = List.map (fun (x,n) -> x, Num n) kvs in
      let keys = List.split kvs |> fst |> List.map Variable.from_name |> Variable.Set.of_list in
      let kvs = Subst.SubstAssoc.make kvs in
      {
        k with
        pre = Code.PSubstAssoc.M.b_subst kvs k.pre;
        code = Code.PSubstAssoc.subst kvs k.code;
        global_variables = Variable.Set.diff k.global_variables keys;
        local_variables = Variable.Set.diff k.local_variables keys;
      }
    end

  let vars_distinct (k:Code.t t) : Code.t t =
    let vars =
      Variable.Set.union k.global_variables k.local_variables
    in
    { k with
      code = Code.vars_distinct k.code vars
    }

  (*
    Makes all variables distinct and hoists declarations as
    thread-locals.
  *)
  let hoist_decls : Code.t t -> Code.t t =
    let rec inline (vars:Variable.Set.t) (p:Code.t) : Variable.Set.t * Code.t =
      match p with
      | Decl (x, p) -> inline (Variable.Set.add x vars) p
      | Acc _ | Skip | Sync _ -> (vars, p)
      | Cond (b, p) ->
        let (vars, p) = inline vars p in
        (vars, Cond (b, p))
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
    let (locals, p) = inline Variable.Set.empty k.code in
    { k with
      code = p;
      local_variables = Variable.Set.union locals k.local_variables;
    }

  let to_s (f:'a -> Indent.t list) (k:'a t) : Indent.t list =
    [
      Line ("name: " ^ k.name ^ ";");
      Line ("arrays: " ^ Memory.map_to_string k.arrays ^ ";");
      Line ("globals: " ^ Variable.set_to_string k.global_variables ^ ";");
      Line ("locals: " ^ Variable.set_to_string k.local_variables ^ ";");
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


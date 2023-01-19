open Stage0

let (@) = Common.append_tr

open Exp

(* The source instruction uses the base defined above *)
type inst =
  | Acc of (Variable.t * Access.t)
  | Sync
  | Cond of bexp * inst list
  | Loop of Range.t * inst list

(* The source program *)
type prog = inst list

type 'a kernel = {
  (* The kernel name *)
  kernel_name : string;
  (* The internal variables are used in the code of the kernel.  *)
  kernel_global_variables: Variable.Set.t;
  (* The internal variables are used in the code of the kernel.  *)
  kernel_local_variables: Variable.Set.t;
  (* The modifiers of each array *)
  kernel_arrays: Memory.t Variable.Map.t;
  (* A thread-local pre-condition that is true on all phases. *)
  kernel_pre: bexp;
  (* The code of a kernel performs the actual memory accesses. *)
  kernel_code: 'a;
}

let kernel_shared_arrays (k:'a kernel) : Variable.Set.t =
  k.kernel_arrays
  |> Variable.Map.filter (fun _ v -> Memory.is_shared v)
  |> Variable.MapSetUtil.map_to_set

let kernel_global_arrays (k:'a kernel) : Variable.Set.t =
  k.kernel_arrays
  |> Variable.Map.filter (fun _ v -> Memory.is_global v)
  |> Variable.MapSetUtil.map_to_set

let kernel_constants (k:prog kernel) =
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
        M.add s r.var (function
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
        match r.lower_bound, r.upper_bound, p with
        | _, _, [] -> []
        | Num lb, Num ub, _ ->
          if lb >= ub then
            []
          else if lb + 1 = ub then
            PSubstPair.p_subst (r.var, Num lb) p
          else if lb + 2 = ub then
            PSubstPair.p_subst (r.var, Num lb) p
            @
            PSubstPair.p_subst (r.var, Num (lb + 1)) p
          else
            [Loop (r, p)]
        | _, _, _ -> [Loop (r, p)]
      end
  and opt_p (p:prog) =
    List.concat_map opt_i p
  in
  opt_p p

(* Create a new kernel with same name, but no code to check *)
let clear_kernel (k:prog kernel) : prog kernel =
  {
    kernel_name = k.kernel_name;
    kernel_arrays = Variable.Map.empty;
    kernel_pre = Bool true;
    kernel_code = [];
    kernel_global_variables = Variable.Set.empty;
    kernel_local_variables = Variable.Set.empty;
  }

let replace_constants (kvs:(string*int) list) (k:prog kernel) : prog kernel =
  if Common.list_is_empty kvs then k else
  begin
    let kvs = List.map (fun (x,n) -> x, Num n) kvs in
    let keys = List.split kvs |> fst |> List.map Variable.from_name |> Variable.Set.of_list in
    let kvs = Subst.SubstAssoc.make kvs in
    {
      kernel_name = k.kernel_name;
      kernel_arrays = k.kernel_arrays;
      kernel_pre = PSubstAssoc.M.b_subst kvs k.kernel_pre |> Constfold.b_opt;
      kernel_code = PSubstAssoc.p_subst kvs k.kernel_code |> p_opt;
      kernel_global_variables = Variable.Set.diff k.kernel_global_variables keys;
      kernel_local_variables = Variable.Set.diff k.kernel_local_variables keys;
    }
  end


let subst_block_dim (block_dim:Dim3.t) (p:prog) : prog =
  let subst x n p =
    PSubstPair.p_subst (Variable.from_name x, Num n) p
  in
  p
  |> subst "blockDim.x" block_dim.x
  |> subst "blockDim.y" block_dim.y
  |> subst "blockDim.z" block_dim.z

let subst_grid_dim (grid_dim:Dim3.t) (p:prog) : prog =
  let subst x n p =
    PSubstPair.p_subst (Variable.from_name x, Num n) p
  in
  p
  |> subst "gridDim.x" grid_dim.x
  |> subst "gridDim.y" grid_dim.y
  |> subst "gridDim.z" grid_dim.z

let p_cond (b:bexp) (p:prog) : prog =
  match b, p with
  | Bool true, _ -> p
  | _, [] | Bool false, _ -> []
  | _, _ -> [Cond(b, p)] 

let vars_distinct (p:prog)  (known:Variable.Set.t) : prog =
  let rec uniq_i (i:inst) (xs:Variable.Set.t) : inst * Variable.Set.t =
    match i with
    | Acc _
    | Sync
      -> (i, xs)
    | Cond (b, p) ->
      let (p, xs) = uniq_p p xs in
      (Cond (b, p), xs)
    | Loop (r, p) ->
      let x = r.var in
      if Variable.Set.mem x xs then (
        let new_x : Variable.t = Variable.fresh xs x in
        let new_xs = Variable.Set.add new_x xs in
        let s = Subst.SubstPair.make (x, Var new_x) in
        let new_p = PSubstPair.p_subst s p in
        let (p, new_xs) = uniq_p new_p new_xs in
        Loop ({ r with var = new_x }, p), new_xs
      ) else (
        let (p, new_xs) = uniq_p p (Variable.Set.add x xs) in
        Loop (r, p), new_xs
      )
  and uniq_p (p:prog) (xs:Variable.Set.t) : prog * Variable.Set.t =
    match p with
    | [] -> ([], xs)
    | i::p ->
      let (i, xs) = uniq_i i xs in
      let (p, xs) = uniq_p p xs in
      (i::p, xs)
  in
  uniq_p p known |> fst


let rec inst_to_s : inst -> Indent.t list =
  function
  | Sync -> [Line "sync;"]
  | Acc (x, e) -> [Line (Access.to_string ~name:(Variable.name x) e)]
  | Cond (b, p1) -> [
      Line ("if (" ^ b_to_string b ^ ") {");
      Block (List.map inst_to_s p1 |> List.flatten);
      Line "}"
    ]
  | Loop (r, p) ->
    [
      Line ("foreach (" ^ Range.to_string r ^ ") {");
      Block (List.map inst_to_s p |> List.flatten);
      Line "}"
    ]

let prog_to_s (p: prog) : Indent.t list =
  List.map inst_to_s p |> List.flatten

let print_p (p: prog) : unit =
  Indent.print (prog_to_s p)

let kernel_to_s (f:'a -> Indent.t list) (k:'a kernel) : Indent.t list =
  [
    Line ("arrays: " ^ Memory.map_to_string k.kernel_arrays ^ ";");
    Line ("globals: " ^ Variable.set_to_string k.kernel_global_variables ^ ";");
    Line ("locals: " ^ Variable.set_to_string k.kernel_local_variables ^ ";");
    Line ("invariant: " ^ Exp.b_to_string k.kernel_pre ^";");
    Line "";
    Line "code {";
    Block (f k.kernel_code);
    Line "}"
  ]

let print_kernel (f:'a -> Indent.t list) (k: 'a kernel) : unit =
  Indent.print (kernel_to_s f k)

let print_k (k:prog kernel) : unit =
  Indent.print (kernel_to_s prog_to_s k)

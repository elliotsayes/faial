(*
 Given a flat kernel
 *)

open Stage0
open Protocols

let (@) = Common.append_tr

open Common
open Exp
open Flatacc

let prefix (t:task) = "$" ^ task_to_string t ^ "$"
let mk_var (x:string) = Var (Variable.from_name x)
let mk_idx (t:task) (n:int) = mk_var (prefix t ^ "idx$" ^ string_of_int n)

let assign_index (t:task) (idx:int) (n:nexp) : bexp =
  n_eq (mk_idx t idx) n

(*
  For each thread-local variable x generate x$1 and x$2 to represent the
  thread-local assignments of each thread.
 *)
let project_access (locals:Variable.Set.t) (t:task) (ca:CondAccess.t) : CondAccess.t =
  (* Add a suffix to a variable *)
  let var_append (x:Variable.t) (suffix:string) : Variable.t =
    Variable.set_name (Variable.name x ^ suffix) x
  in
  (* Add a suffix to all variables to make them unique. Use $ to ensure
    these variables did not come from C *)
  let task_suffix (t:task) = "$" ^ task_to_string t in
  let proj_var (t:task) (x:Variable.t) : Variable.t =
    var_append x (task_suffix t)
  in
  let rec inline_proj_n (t:task) (n: nexp) : nexp =
    match n with
    | Num _ -> n
    | Var x when Variable.Set.mem x locals -> Var (proj_var t x)
    | Var _ -> n
    | Bin (o, n1, n2) -> Bin (o, inline_proj_n t n1, inline_proj_n t n2)
    | Proj (t', x) -> Var (proj_var t' x)
    | NIf (b, n1, n2) -> NIf (inline_proj_b t b, inline_proj_n t n1, inline_proj_n t n2)
    | NCall (x, n) -> NCall (x, inline_proj_n t n)
  and inline_proj_b (t:task) (b: bexp) : bexp =
    match b with
    | Pred (x, n) -> Pred (x, inline_proj_n t n)
    | Bool _ -> b
    | BNot b -> BNot (inline_proj_b t b)
    | BRel (o, b1, b2) -> BRel (o, inline_proj_b t b1, inline_proj_b t b2)
    | NRel (o, n1, n2) -> NRel (o, inline_proj_n t n1, inline_proj_n t n2)
  in
  let inline_acc (a:Access.t) = Access.map (inline_proj_n t) a in
  {ca with
    access = inline_acc ca.access;
    cond = inline_proj_b t ca.cond;
  }

module SymAccess = struct
  (*

  Each task is represented as: the mode of access, the index of an
  n-dimensional access (eg, for [x][y], x=0 and y=1), and a location
  identifier.

  In SMT terms, we assign each field to a variable.
  For instance, we assign the conditional access id of task A, say 0, to a
  conditional-access-id variable, say $acc$T1, and the code generated becomes
  $acc$T1 = 0 to encode that task A's CondAccess.t id is 0.

  condition /\
  cond-acc id /\
  assign index 0 /\
  ...
  assign index n
  *)
  type t = {
    id: int;
    condition: bexp;
    access: Access.t
  }


  let mode_to_nexp (m:Access.Mode.t) : nexp =
    Num (match m with
    | Rd -> 0
    | Wr _ -> 1)

  let mk_mode (t:task) = mk_var (prefix t ^ "mode")
  let mk_id (t:task) = mk_var (prefix t ^ "id")

  (* Given a task generator serialize a conditional access *)
  let to_bexp (t:task) (a:t) : bexp =
    let this_mode_v : nexp = mk_mode t in
    let other_mode_v : nexp = mk_mode (other_task t) in
    let assign_mode (m:Access.Mode.t): bexp =
      let b = n_eq this_mode_v (mode_to_nexp m) in
      if Access.Mode.is_read m then
        n_eq other_mode_v (mode_to_nexp (Wr None))
        |> b_and b
      else b
    in
    let assign_id : bexp = n_eq (mk_id t) (Num a.id)
    in
    (
      assign_id :: (* assign identifier of the conditional access *)
      a.condition :: (* assign the pre-condition of the access *)
      assign_mode a.access.mode :: (* assign the mode *)
      List.mapi (assign_index t) a.access.index (* assign the values of the index *)
    )
    |> b_and_ex

  (* When we lower the representation, we do not want to have source code
    locations, just an id. *)

  let from_cond_access (locals:Variable.Set.t) (t:task) (idx:int) (ca:CondAccess.t) : t =
    let ca = project_access locals t ca in
    {
      id = idx;
      access = ca.access;
      condition = ca.cond;
    }
end

let cond_access_to_bexp (locals:Variable.Set.t) (t:task) (a:CondAccess.t) : bexp =
  let a = project_access locals t a in
  (
    a.cond ::
    List.mapi (assign_index t) a.access.index
  )
  |> b_and_ex


let assign_indices (dim:int) : bexp =
  (* Make sure all indices match *)
  (* idx0$T1 = idx0$T2  /\ idx1$T1 = idx1$T2 ... /\ idxn$T1 = idxn$T2 /\
     idx0$T1 >= 0 /\ ... /\ idxn$T1 >= 0
  *)
  range (dim - 1)
  |> List.map (fun i ->
    let t1 = mk_idx Task1 i in
    let t2 = mk_idx Task2 i in
    b_and_ex [
      n_eq t1 t2;
      n_ge t1 (Num 0);
    ]
  )
  |> b_and_ex

module Proof = struct
  type t = {
    id: int;
    data_approx: Variable.Set.t list;
    control_approx: Variable.Set.t list;
    kernel_name: string;
    array_name: string;
    preds: Predicates.t list;
    decls: string list;
    labels: (string * string) list;
    goal: bexp;
    code: Flatacc.Code.t;
  }

  let labels (p:t) : (string * string) list =
    p.labels

  let to_json (p:t) : Yojson.Basic.t =
    `Assoc [
      "id", `Int p.id;
      "kernel_name", `String p.kernel_name;
      "array_name", `String p.array_name;
    ]

  let get (idx:int) (p:t) : CondAccess.t =
    List.nth p.code idx

  let make ~kernel_name ~array_name ~id ~goal ~code ~data_approx ~control_approx : t =
    let goal = Constfold.b_opt goal (* Optimize the output expression *) in
    let fns =
      Freenames.free_names_bexp goal Variable.Set.empty
      |> Variable.Set.elements
    in
    let decls = List.map Variable.name fns in
    let labels = List.filter_map (fun x ->
      Variable.label_opt x
      |> Option.map (fun l ->
        (Variable.name x, l)
      )
    ) fns in
    let preds = Predicates.get_predicates goal in
    {
      id; preds; decls; goal; array_name; kernel_name; labels; code;
      data_approx; control_approx;
    }

  let to_string (p:t) : Indent.t list =
    let open Common in
    let open Indent in
    let preds =
      let open Predicates in
      List.map (fun x -> x.pred_name) p.preds
      |> join ", "
    in
    [
        Line ("array: " ^ p.array_name);
        Line ("data approx:");
        Block (List.map (fun x -> Line ("[" ^ Variable.set_to_string x ^ "]")) p.data_approx);
        Line ("control approx:");
        Block (List.map (fun x -> Line ("[" ^ Variable.set_to_string x ^ "]")) p.control_approx);
        Line ("kernel: " ^ p.kernel_name);
        Line ("predicates: " ^ preds ^ ";");
        Line ("decls: " ^ (p.decls |> join ", ") ^ ";");
        Line ("goal:");
        Block (b_to_s p.goal);
        Line (";")
    ]

  let from_code
    (locals:Variable.Set.t)
    (code:Flatacc.Code.t)
  :
    bexp
  =
    (*
      tid = access_1 \/
      tid = access_2 \/
      ...
      tid = access_n
    *)
    let assign_accesses (t:task) : bexp =
      code
      |> Flatacc.Code.to_list (* get conditional accesses *)
      |> List.mapi (SymAccess.from_cond_access locals t) (* get symbolic access *)
      |> List.map (SymAccess.to_bexp t) (* generate code *)
      |> b_or_ex
    in
    b_and_ex [
      (* Assign the accesses of task 1 *)
      assign_accesses Task1;
      (* Assign the accesses of task 2 *)
      assign_accesses Task2;
      (* There is no need to try out all combinations of ids,
         so this contrain ensures that Task1 is never a larger access than
         Task2. *)
      n_le (SymAccess.mk_id Task1) (SymAccess.mk_id Task2);
      (*
        All indices of task1 are equal to the indices of task2
      *)
      Code.dim code |> Option.get |> assign_indices
    ]

  let from_flat (proof_id:int) (k:Flatacc.Kernel.t) : t =
    let (data_approx, control_approx) =
      let locals =
        k.local_variables
        |> Variable.Set.remove (Variable.from_name "threadIdx.x")
        |> Variable.Set.remove (Variable.from_name "threadIdx.y")
        |> Variable.Set.remove (Variable.from_name "threadIdx.z")
      in
      (
        List.map (CondAccess.data_approx locals) k.code,
        List.map (CondAccess.control_approx locals k.pre) k.code
      )
    in
    let goal =
      from_code k.local_variables k.code
      |> b_and k.pre
    in
    make
      ~id:proof_id
      ~kernel_name:k.name
      ~array_name:k.array_name
      ~goal
      ~code:k.code
      ~data_approx
      ~control_approx
end

let translate (stream:Flatacc.Kernel.t Streamutil.stream) : Proof.t Streamutil.stream =
  Streamutil.mapi Proof.from_flat stream

(* ------------------- SERIALIZE ---------------------- *)


let print_kernels (ks : Proof.t Streamutil.stream) : unit =
  print_endline "; symbexp";
  let count = ref 0 in
  Streamutil.iter (fun (p:Proof.t) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; bool " ^ (string_of_int curr));
    Proof.to_string p |> Indent.print
  ) ks;
  print_endline "; end of symbexp"

(*
 Given a flat kernel
 *)

open Stage0
open Protocols

let (@) = Common.append_tr

open Common
open Exp
open Flatacc


module Ids = struct
  let prefix (t:task) : string = "$" ^ task_to_string t ^ "$"
  (* Variable representing index accessing the array *)
  let index (t:task) (n:int) : string = prefix t ^ "idx$" ^ string_of_int n
  (* The access identifier *)
  let access_id (t:task) : string = prefix t ^ "id"
end

module Gen = struct
  let var (x:string) : nexp = Var (Variable.from_name x)
  let index (t:task) (n:int) : nexp = Ids.index t n |> var
  (* Access mode *)
  let mode (t:task) : nexp = var (Ids.prefix t ^ "mode")
  let assign_mode (t:task) (m:Access.Mode.t): bexp =
    let mode_to_nexp (m:Access.Mode.t) : nexp =
      Num (match m with
      | Rd -> 0
      | Wr _ -> 1)
    in
    let this_mode_v : nexp = mode t in
    let other_mode_v : nexp = mode (other_task t) in
    let b = n_eq this_mode_v (mode_to_nexp m) in
    if Access.Mode.is_read m then
      n_eq other_mode_v (mode_to_nexp (Wr None))
      |> b_and b
    else b

  let access_id (t:task) : nexp = Ids.access_id t |> var
  (* assign identifier of the conditional access *)
  let assign_access_id (t:task) (aid:int) : bexp =
    n_eq (access_id t) (Num aid)

  let assign_index (op:Exp.nrel) (t:task) (idx:int) (n:nexp) : bexp =
    n_rel op (index t idx) n

  (* Constrains the indices to be all non-negative and match for both threads. *)
  let assign_dim (dim:int) : bexp =
    (* Make sure all indices match *)
    (* idx0$T1 = idx0$T2  /\ idx1$T1 = idx1$T2 ... /\ idxn$T1 = idxn$T2 /\
      idx0$T1 >= 0 /\ ... /\ idxn$T1 >= 0
    *)
    range (dim - 1)
    |> List.map (fun i ->
      let t1 = index Task1 i in
      let t2 = index Task2 i in
      b_and_ex [
        n_eq t1 t2;
        n_ge t1 (Num 0);
      ]
    )
    |> b_and_ex


  let project (t:task) (x:Variable.t) : Variable.t =
    (* Add a suffix to all variables to make them unique. Use $ to ensure
      these variables did not come from C *)
    let task_suffix (t:task) = "$" ^ task_to_string t in
    Variable.update_name (fun n -> n ^ task_suffix t) x

end



(*
  For each thread-local variable x generate x$1 and x$2 to represent the
  thread-local assignments of each thread.
 *)
let project_access (locals:Variable.Set.t) (t:task) (ca:CondAccess.t) : CondAccess.t =
  let rec inline_proj_n (t:task) (n: nexp) : nexp =
    match n with
    | Num _ -> n
    | Var x when Variable.Set.mem x locals -> Var (Gen.project t x)
    | Var _ -> n
    | Bin (o, n1, n2) -> Bin (o, inline_proj_n t n1, inline_proj_n t n2)
    | NIf (b, n1, n2) -> NIf (inline_proj_b t b, inline_proj_n t n1, inline_proj_n t n2)
    | NCall (x, n) -> NCall (x, inline_proj_n t n)
  and inline_proj_b (t:task) (b: bexp) : bexp =
    match b with
    | ThreadEqual n ->
      NRel (NEq, inline_proj_n Task1 n, inline_proj_n Task2 n)
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

  let to_string (a:t) : string =
    "{ access_id = " ^ string_of_int a.id ^
    " condition = " ^ Exp.b_to_string a.condition ^
    " access = " ^ Access.to_string a.access ^
    " }"

  (* Given a task generator serialize a conditional access *)
  let to_bexp (t:task) (a:t) : bexp =

    (
      Gen.assign_access_id t a.id ::
      a.condition :: (* assign the pre-condition of the access *)
      Gen.assign_mode t a.access.mode :: (* assign the mode *)
      List.mapi (Gen.assign_index Exp.NEq t) a.access.index (* assign the values of the index *)
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
    List.mapi (Gen.assign_index Exp.NEq t) a.access.index
  )
  |> b_and_ex

module AccessSummary = struct
  type t = {
    location: Location.t;
    access: Access.t;
    globals: Variable.Set.t;
    data_approx: Variable.Set.t;
    control_approx: Variable.Set.t;
  }

  let variables (a:t) : Variable.Set.t =
    a.globals
    |> Variable.Set.union a.data_approx
    |> Variable.Set.union a.control_approx

  let to_string (a:t) : string =
    "{access=" ^ Access.to_string a.access ^
    ", data=[" ^ Variable.set_to_string a.data_approx ^
    "], ctrl=[" ^ Variable.set_to_string a.control_approx ^
    "], globals=[" ^ Variable.set_to_string a.globals ^
    "]}"
end

module Proof = struct
  type t = {
    id: int;
    kernel_name: string;
    array_name: string;
    preds: Predicates.t list;
    decls: string list;
    labels: (string * string) list;
    goal: bexp;
    accesses: AccessSummary.t list;
  }

  let add_rel_index (o:Exp.nrel) (idx:int list) (p:t) : t =
    let idx_eq =
      idx
      |> List.mapi (fun i v ->
        Gen.assign_index o Task1 i (Num v)
      )
      |> b_and_ex
    in
    { p with goal = b_and p.goal idx_eq }

  let add_tid (tid:task) (idx:Dim3.t) (p:t) : t =
    let goal =
      b_and_ex [
        n_eq (Var (Gen.project tid Variable.tidx)) (Num idx.x);
        n_eq (Var (Gen.project tid Variable.tidy)) (Num idx.y);
        n_eq (Var (Gen.project tid Variable.tidz)) (Num idx.z);
        p.goal
      ]
    in
    { p with goal }

  let labels (p:t) : (string * string) list =
    p.labels

  let to_json (p:t) : Yojson.Basic.t =
    `Assoc [
      "id", `Int p.id;
      "kernel_name", `String p.kernel_name;
      "array_name", `String p.array_name;
    ]

  let get ~access_id (p:t) : AccessSummary.t =
    List.nth p.accesses access_id

  let make:
    kernel_name:string ->
    array_name:string ->
    id:int ->
    accesses:AccessSummary.t list ->
    goal:bexp ->
    t
  =
    fun ~kernel_name ~array_name ~id ~accesses ~goal ->
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
      id; preds; decls; goal; array_name; kernel_name; labels; accesses;
    }

  let to_s (p:t) : Indent.t list =
    let open Common in
    let open Indent in
    let preds =
      let open Predicates in
      List.map (fun x -> x.pred_name) p.preds
      |> join ", "
    in
    [
        Line ("id: " ^ string_of_int p.id);
        Line ("array: " ^ p.array_name);
        Line ("kernel: " ^ p.kernel_name);
        Line ("predicates: " ^ preds ^ ";");
        Line ("decls: " ^ (p.decls |> join ", ") ^ ";");
        Line ("accesses: " ^ (List.map AccessSummary.to_string p.accesses |> String.concat ", "));
        Line ("goal:");
        Block (b_to_s p.goal);
        Line (";")
    ]

  let to_string (p:t) : string =
    to_s p |> Indent.to_string

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
      n_le (Gen.access_id Task1) (Gen.access_id Task2);
      (*
        All indices of task1 are equal to the indices of task2
      *)
      Code.dim code |> Option.get |> Gen.assign_dim
    ]

  let from_flat (proof_id:int) (k:Flatacc.Kernel.t) : t =
    let locals =
      Variable.Set.union
        k.exact_local_variables
        k.approx_local_variables
    in
    let goal =
      from_code locals k.code
      |> b_and k.pre
    in
    let pre_fns = Freenames.free_names_bexp k.pre Variable.Set.empty in
    let accesses = List.map (fun (a:CondAccess.t) ->
      let open AccessSummary in
      let cond_fns = Freenames.free_names_bexp a.cond Variable.Set.empty in
      let data_fns = Freenames.free_names_access a.access Variable.Set.empty in
      let ctrl_fns = Variable.Set.union pre_fns cond_fns in
      let all_fns = Variable.Set.union data_fns ctrl_fns in
      {
        location = a.location;
        access = a.access;
        globals = Variable.Set.diff all_fns locals;
        data_approx = Variable.Set.inter k.approx_local_variables data_fns;
        control_approx = Variable.Set.inter k.approx_local_variables ctrl_fns;
      }
    ) k.code
    in
    make
      ~id:proof_id
      ~kernel_name:k.name
      ~array_name:k.array_name
      ~goal
      ~accesses
end

let add_rel_index
  (o:Exp.nrel)
  (idx:int list)
  (s:Proof.t Streamutil.stream)
:
  Proof.t Streamutil.stream
=
  if idx = [] then
    s
  else
    Streamutil.map (Proof.add_rel_index o idx) s

let add_tid
  (tid:task)
  (o:Dim3.t option)
  (s:Proof.t Streamutil.stream)
:
  Proof.t Streamutil.stream
=
  match o with
  | Some idx ->
    Streamutil.map (Proof.add_tid tid idx) s
  | None ->
    s

let translate
  (stream:Flatacc.Kernel.t Streamutil.stream)
:
  Proof.t Streamutil.stream
=
  Streamutil.mapi Proof.from_flat stream

(* ------------------- SERIALIZE ---------------------- *)


let print_kernels (ks : Proof.t Streamutil.stream) : unit =
  print_endline "; symbexp";
  Streamutil.iter (fun (p:Proof.t) ->
    print_endline ("; proof");
    Proof.to_string p |> print_endline
  ) ks;
  print_endline "; end of symbexp"

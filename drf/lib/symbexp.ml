(*
 Given a flat kernel
 *)

open Stage0
open Protocols

let (@) = Common.append_tr

open Common
open Exp
open Flatacc

module LocationIndex = struct
  type t = {index: int; location:Location.t}
  let mk (index:int) (location:Location.t) : t = {index; location}
  let index (x:t) = x.index
  let location (x:t) = x.location
end

(* The assign_task datatype creates two constructors.
   - assign_mode: given a mode, returns a boolean expression that represents
     assigning the given mode to the current mode-variable.
   - assign_index: given an index and an index value (nexp) returns a boolean
     expression that assigns the expression to the current index.
*)
let prefix (t:task) = "$" ^ task_to_string t ^ "$"
let mk_var (x:string) = Var (Variable.from_name x)
let mk_idx (t:task) (n:int) = mk_var (prefix t ^ "idx$" ^ string_of_int n)

let assign_index (t:task) (idx:int) (n:nexp) : bexp =
  n_eq (mk_idx t idx) n

module AssignTask = struct
  (*

  Each task is represented as: the mode of access, the index of an
  n-dimensional access (eg, for [x][y], x=0 and y=1), and a location
  identifier.

  In SMT terms, we assign each field to a variable.
  For instance, we assign the mode of task A, say Rd, to a mode variable, say $mode$T1,
  so the code generated becomes $mode$T1 = 0 to encode that task A's mode is Rd.

  This particular data-structure stores generators that given a mode, which
  is then instantiated by a value of type `task`.
  *)
  type t = {
    mode : Access.Mode.t -> bexp;
    index: int -> nexp -> bexp;
    loc: LocationIndex.t -> bexp;
  }

  let mode_to_nexp (m:Access.Mode.t) : nexp =
    Num (match m with
    | Rd -> 0
    | Wr -> 1)

  let mk_mode (t:task) = mk_var (prefix t ^ "mode")
  let mk_loc (t:task) = mk_var (prefix t ^ "loc")

  (* Returns the generators for the given task *)
  let from_task (t:task) : t =
    let this_mode_v : nexp = mk_mode t in
    let other_mode_v : nexp = mk_mode (other_task t) in
    let eq_mode (m:Access.Mode.t): bexp =
      let b = n_eq this_mode_v (mode_to_nexp m) in
      if Access.Mode.is_read m then
        n_eq other_mode_v (mode_to_nexp Wr)
        |> b_and b
      else b
    in
    let assign_loc (l:LocationIndex.t) : bexp =
          n_eq (mk_loc (other_task t))
              (Num (LocationIndex.index l))
    in
    {
      mode = eq_mode;
      index = assign_index t;
      loc = assign_loc;
    }

end

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

    A symbolic access represents an access of a particular task.

    Given an AssignTask code generator, we can then generate the code for
    a particular task.

   *)
  type t = {
    location: LocationIndex.t;
    condition: bexp;
    access: Access.t
  }

  let mk ~location ~condition ~access = {location; condition; access}

  (* Given a task generator serialize a conditional access *)
  let to_bexp (gen:AssignTask.t) (a:t) : bexp =
    let head = [
        a.condition;
        gen.mode a.access.mode;
    ] in
    let head = gen.loc a.location :: head in
    head @ List.mapi gen.index a.access.index
    |> b_and_ex

  (* When we lower the representation, we do not want to have source code
    locations, just an id. *)

  let from_cond_access (locals:Variable.Set.t) (t:task) (idx:int) (ca:CondAccess.t) : t =
    let ca = project_access locals t ca in
    {
      location = LocationIndex.mk idx ca.location;
      access = ca.access;
      condition = ca.cond;
    }
end



module Proof = struct
  type t = {
    id: int;
    kernel_name: string;
    array_name: string;
    locations: Location.t list;
    preds: Predicates.t list;
    decls: string list;
    labels: (string * string) list;
    goal: bexp;
  }

  let labels (p:t) : (string * string) list =
    p.labels

  let to_json (p:t) : Yojson.Basic.t =
    `Assoc [
      "id", `Int p.id;
      "kernel_name", `String p.kernel_name;
      "array_name", `String p.array_name;
      "locations", `List (List.map Location.to_json p.locations);
    ]

  let mk ~kernel_name ~array_name ~id ~goal ~locations : t =
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
    {id; preds; decls; goal; array_name; kernel_name; locations; labels;}

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
          Line ("kernel: " ^ p.kernel_name);
          Line ("predicates: " ^ preds ^ ";");
          Line ("decls: " ^ (p.decls |> join ", ") ^ ";");
          Line ("goal: " ^ b_to_string p.goal ^ ";");
      ]


  let dim_gen (dim:int) : bexp =
    (* Make sure all indices match *)
    (* $T1$index$0 = $T2$index$0 ... *)
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

  let from_flat_code
    (locals:Variable.Set.t)
    (accs:Flatacc.Code.t)
  :
    bexp
  =
    (* Pick one access *)
    let task_to_bexp (t:task) : bexp =
      accs
      |> Flatacc.Code.to_list (* get conditional accesses *)
      |> List.mapi (SymAccess.from_cond_access locals t) (* get symbolic access *)
      |> List.map (AssignTask.from_task t |> SymAccess.to_bexp) (* generate code *)
      |> b_or_ex
    in
    b_and_ex [
      task_to_bexp Task1;
      task_to_bexp Task2;
      Code.dim accs |> Option.get |> dim_gen
    ]

  let from_flat (proof_id:int) (k:Flatacc.Kernel.t) : t =
    let goal =
      from_flat_code k.local_variables k.code
      |> b_and k.pre
      |> Constfold.b_opt (* Optimize the output expression *)
    in
    let locations = Code.to_list k.code |> List.map CondAccess.location in
    mk
      ~id:proof_id
      ~kernel_name:k.name
      ~array_name:k.array_name
      ~locations
      ~goal

end

let translate (stream:Flatacc.Kernel.t Streamutil.stream) : (Proof.t Streamutil.stream) =
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

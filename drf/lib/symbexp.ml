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
type assign_task = {
  assign_mode : Access.Mode.t -> bexp;
  assign_index: int -> nexp -> bexp;
  assign_loc: LocationIndex.t -> bexp;
}

module SymAccess = struct
  type t = {location: LocationIndex.t; condition: bexp; access: Access.t}
  let mk ~location ~condition ~access = {location; condition; access}
  (* Given a task generator serialize a conditional access *)
  let to_bexp (t:assign_task) (a:t) : bexp =
    let head = [
        a.condition;
        t.assign_mode a.access.mode;
    ] in
    let head = t.assign_loc a.location :: head in
    head @ List.mapi t.assign_index a.access.index
    |> b_and_ex

end

module Proof = struct
  type t = {
    id: int;
    kernel_name: string;
    array_name: string;
    locations: Location.t list;
    preds: Predicates.t list;
    decls: string list;
    goal: bexp;
  }
  let mk ~kernel_name ~array_name ~id ~goal ~locations : t =
    let decls =
      Freenames.free_names_bexp goal Variable.Set.empty
      |> Variable.Set.elements
      |> List.map Variable.name
    in
    let preds = Predicates.get_predicates goal in
    {id; preds; decls; goal; array_name; kernel_name; locations;}

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

end

type h_prog = {
  prog_locals: Variable.Set.t;
  prog_accesses: cond_access list;
}

(* When we lower the representation, we do not want to have source code
   locations. Instead, we each *)

let proj_access (locals:Variable.Set.t) (t:task) (idx:int) (ca:cond_access) : SymAccess.t =
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
  let location = LocationIndex.mk idx ca.ca_location in
  SymAccess.mk ~location ~access:(inline_acc ca.ca_access) ~condition:(inline_proj_b t ca.ca_cond)

let proj_accesses locals t : cond_access list -> SymAccess.t list = List.mapi (proj_access locals t)

let mode_to_nexp (m:Access.Mode.t) : nexp =
  Num (match m with
  | Rd -> 0
  | Wr -> 1)

let mk_var (x:string) = Var (Variable.from_name x)
let prefix (t:task) = "$" ^ task_to_string t ^ "$"
let mk_mode (t:task) = mk_var (prefix t ^ "mode")
let mk_loc (t:task) = mk_var (prefix t ^ "loc")
let mk_idx (t:task) (n:int) = mk_var (prefix t ^ "idx$" ^ string_of_int n)


(* Returns the generators for the given task *)
let mk_task_gen (t:task) : assign_task =
  let this_mode_v : nexp = mk_mode t in
  let other_mode_v : nexp = mk_mode (other_task t) in
  let idx_v : int -> nexp = mk_idx t in
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
  let assign_index (idx:int) (n:nexp) : bexp = n_eq (idx_v idx) n
  in
  {
    assign_mode = eq_mode;
    assign_index = assign_index;
    assign_loc = assign_loc;
  }


let cond_acc_list_to_bexp (t:assign_task) (l:SymAccess.t list) : bexp =
  List.map (SymAccess.to_bexp t) l
  |> b_or_ex

let h_prog_to_bexp
  (locals:Variable.Set.t)
  (accs:cond_access list)
:
  bexp
=
  (* Pick one access *)
  let task_to_bexp (t:task) : bexp =
    let gen = mk_task_gen t in
    let accs = proj_accesses locals t accs in
    cond_acc_list_to_bexp gen accs
  in
  (* Make sure all indices match *)
  (* $T1$index$0 = $T2$index$0 ... *)
  let gen_eq_index (n:int) : bexp =
    range (n - 1)
    |> List.map (fun i ->
      let t1 = mk_idx Task1 i in
      let t2 = mk_idx Task2 i in
      b_and_ex [
        n_eq t1 t2;
        n_ge t1 (Num 0);
      ]
    )
    |> b_and_ex
  in
  (* The dimention is the index count *)
  let get_dim (l:cond_access list) : int =
    match l with
    | [] -> failwith "Phase split should not generate empty phases!"
    | c :: _ -> List.length c.ca_access.index
  in
  b_and_ex [
    task_to_bexp Task1;
    task_to_bexp Task2;
    get_dim accs |> gen_eq_index
  ]

let f_kernel_to_proof (proof_id:int) (k:f_kernel) : Proof.t =
  let goal =
    h_prog_to_bexp k.f_kernel_local_variables k.f_kernel_accesses
    |> b_and k.f_kernel_pre
    |> Constfold.b_opt (* Optimize the output expression *)
  in
  let locations =
    k.f_kernel_accesses
    |> List.map (fun x -> x.ca_location)
  in
  Proof.mk
    ~id:proof_id
    ~kernel_name:k.f_kernel_name
    ~array_name:k.f_kernel_array
    ~locations
    ~goal


let translate (stream:f_kernel Streamutil.stream) : (Proof.t Streamutil.stream) =
  Streamutil.mapi f_kernel_to_proof stream

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

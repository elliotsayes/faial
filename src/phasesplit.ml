open Exp
open Proto
open Common
open Serialize
open Subst
open Streamutil
open Wellformed
open Phasealign
open Hash_rt
open Ppx_compare_lib.Builtin (* compare_list *)

type u_kernel = {
  (* The shared locations that can be accessed in the kernel. *)
  u_kernel_locations: VarSet.t;
  (* The internal variables are used in the code of the kernel.  *)
  u_kernel_global_variables: VarSet.t;
  (* The internal variables are used in the code of the kernel.  *)
  u_kernel_local_variables: VarSet.t;
  (* Global ranges *)
  u_kernel_ranges: range list;
  (* The code of a kernel performs the actual memory accesses. *)
  u_kernel_code: u_inst;
  (* A thread-local pre-condition that is true on all phases. *)
}

(* ---------------- SECOND STAGE OF TRANSLATION ---------------------- *)

type barrier_interval = {
    bi_code: u_inst;
    bi_ranges: range list;
  }
  [@@deriving hash, compare]

let bi_add (bi:barrier_interval) (r:range) : barrier_interval =
  { bi with bi_ranges = r :: bi.bi_ranges }

(* Implements |> *)
let a_prog_to_bi (pre:bexp) : a_prog -> barrier_interval stream =
  let rec i_phase: a_inst -> barrier_interval stream =
    function
      (* ^P; sync |> { P } *)
    | ASync u ->
      {
        bi_code = UCond (pre, u);
        bi_ranges = []
      }
      |> Streamutil.one
    | ALoop (p, r, q) ->
      (* Rule:
        P |> p    q = { for x in [n,m) Q | Q \in p }
        ------------------------------
        for x in [n,m) {Q} |> q
      *)
      (* Break down the body into phases, and prefix each phase with the
          binding *)
      p_phase p
      |> Streamutil.sequence (
        p_phase q
        |> Streamutil.map (fun bi ->
          (* For every phase in q, prefix it with variable in r *)
          bi_add bi r
        )
      )
  and p_phase : a_prog -> barrier_interval stream =
    function
    | [] -> Streamutil.empty
    | i :: p ->
      (* Rule:
        P |> p      Q |> q
        ------------------
        P;Q |> p U q
        *)
      i_phase i
      |> Streamutil.sequence (p_phase p)
  in
  p_phase

let u_free_names (p:u_prog) : VarSet.t -> VarSet.t =
  let rec fn_i (i:u_inst) (fns:VarSet.t) : VarSet.t =
    match i with
    | UAssert b -> Freenames.free_names_bexp b fns
    | UAcc (_,e) ->
      Freenames.free_names_access e fns
    | ULoop (r, l) ->
      Freenames.free_names_range r fns |> fn_p l
    | UCond (b, l) ->
      Freenames.free_names_bexp b fns |> fn_p l
  and fn_p (p:u_prog) (fns:VarSet.t) : VarSet.t =
    List.fold_right fn_i p fns
  in
  fn_p p

exception PhasesplitException of (string * Sourceloc.location) list

module BarrierIntervalHash =
  struct
    type t = barrier_interval
    let equal i j : bool = compare_barrier_interval i j = 0
   let hash i = hash_barrier_interval i
  end

module BITable = Hashtbl.Make(BarrierIntervalHash)

let translate (ks: a_prog kernel stream) (expect_typing_fail:bool) : u_kernel stream =
  let translate_k (k: a_prog kernel) : u_kernel stream =
    let p_to_k ((bi,locations):(barrier_interval * VarSet.t)) : u_kernel =
      (* Check for undefs *)
      (* 1. compute all globals *)
      let globals =
        List.map (fun r -> r.range_var) bi.bi_ranges
        |> VarSet.of_list
        |> VarSet.union k.kernel_global_variables
      in
      (* 2. compute all free names in the ranges *)
      let fns = List.fold_right Freenames.free_names_range bi.bi_ranges VarSet.empty in
      (* 3. check if there are any locals *)
      let errs = VarSet.diff fns globals
        |> VarSet.elements
        |> List.map (fun (x:variable) ->
          "Barrier divergence error: cannot use thread-local variable '" ^
          x.var_name ^ "' in synchronized control flow",
          x.var_loc
          )
      in
      if List.length errs > 0 then
        raise (PhasesplitException errs)
      else
        {
          u_kernel_local_variables = k.kernel_local_variables;
          u_kernel_global_variables = k.kernel_global_variables;
          u_kernel_locations = locations;
          u_kernel_ranges = bi.bi_ranges;
          u_kernel_code = bi.bi_code;
        }
    in
    let known:unit BITable.t = BITable.create 100 in
    a_prog_to_bi k.kernel_pre k.kernel_code
    |> map_opt (fun b ->
      (* if false then *)
      if BITable.mem known b then
        None
      else begin
        BITable.add known b ();
        (* Get locations of u_prog *)
        let locations:VarSet.t = Wellformed.get_locs [b.bi_code] VarSet.empty in
        if VarSet.is_empty locations then None
        else Some (b, locations)
      end
    )
    |> Streamutil.map p_to_k
  in
  map translate_k ks |> concat


(* ---------------------- SERIALIZATION ------------------------ *)

let u_kernel_to_s (k:u_kernel) : PPrint.t list =
  let open PPrint in
  let ranges =
    List.map r_to_s k.u_kernel_ranges
    |> join "; "
  in
  [
      Line ("locations: " ^ var_set_to_s k.u_kernel_locations ^ ";");
      Line ("globals: " ^ var_set_to_s k.u_kernel_global_variables ^ ";");
      Line ("locals: " ^ var_set_to_s k.u_kernel_local_variables ^ ";");
      Line ("ranges: " ^ ranges ^ ";");
      Line "{";
      Block (u_inst_to_s k.u_kernel_code);
      Line "}"
  ]


let print_kernels2 (ks : u_kernel Streamutil.stream) : unit =
  print_endline "; conc";
  let count = ref 0 in
  Streamutil.iter (fun (k:u_kernel) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; phase " ^ (string_of_int curr));
    PPrint.print_doc (u_kernel_to_s k)
  ) ks;
  print_endline "; end of conc"

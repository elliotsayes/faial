(*
 Given a location-split kernel, generate a flat kernel.

 A flat-kernel has the following characteristics:
 - free from control-flow structures
 - all binders are hoisted
 - all concurrent accesses are available
 *)
open Stage0
open Protocols
open Exp

module CondAccess = struct
  type t = {
    location: Location.t;
    access: Access.t;
    cond: bexp;
  }

  let dim (a:t) : int = List.length a.access.index

  let location (x:t) : Location.t = x.location

  let access (x:t) : Access.t = x.access

  let control_approx (thread_locals:Variable.Set.t) (pre:bexp) (x:t) : Variable.Set.t =
    Variable.Set.inter
      thread_locals
      (Freenames.free_names_bexp (b_and x.cond pre) Variable.Set.empty)

  let data_approx (thread_locals:Variable.Set.t) (x:t) : Variable.Set.t =
    Variable.Set.inter
      thread_locals
      (Freenames.free_names_access x.access Variable.Set.empty)

  let to_s (a:t) : Indent.t list =
    let lineno = (Location.line a.location |> Index.to_base1 |> string_of_int) ^ ": " in
    [
      Line (lineno ^ Access.to_string a.access ^ " if");
      Block (b_to_s a.cond);
      Line ";";
    ]
end

module Code = struct
  type t = CondAccess.t list

  let to_list : t -> CondAccess.t list =
    fun x -> x

  let to_s (l:t) : Indent.t list =
    List.concat_map CondAccess.to_s l

  (* The dimention is the index count *)
  let dim (l:t) : int option =
    List.nth_opt l 0 |> Option.map CondAccess.dim

  let from_unsync : Unsync.t -> t =
    let rec flatten (accum:t) (b:bexp) : Unsync.t -> t =
      function
      | Skip -> accum
      | Assert _ -> failwith "Internall error: call Unsync.inline_asserts first!"
      | Acc (x, e) -> {location = Variable.location x; access = e; cond = b} :: accum
      | Cond (b', p) -> flatten accum (b_and b' b) p
      | Loop (r, p) -> flatten accum (b_and (Range.to_cond r) b) p
      | Seq (p, q) ->
        let accum = flatten accum b p in
        flatten accum b q
    in
    fun u ->
      flatten [] (Bool true) (Unsync.inline_asserts u)
end

module Kernel = struct
  type t = {
    name: string;
    array_name: string;
    approx_local_variables: Variable.Set.t;
    exact_local_variables: Variable.Set.t;
    code: Code.t;
    pre: bexp;
  }

  let to_s (k:t) : Indent.t list =
    [
        Line ("array: " ^ k.array_name ^ ";");
        Line ("exact locals: " ^ Variable.set_to_string k.exact_local_variables ^ ";");
        Line ("approx locals: " ^ Variable.set_to_string k.approx_local_variables ^ ";");
        Line ("pre: " ^ b_to_string k.pre ^ ";");
        Line "{";
        Block (Code.to_s k.code);
        Line "}"
    ]

  let from_loc_split (k:Locsplit.Kernel.t) : t =
    let approx_local_variables =
      Variable.Set.diff k.local_variables Variable.tid_var_set
      |> Unsync.unsafe_binders k.code
    in
    let exact_local_variables =
      approx_local_variables
      |> Variable.Set.diff (Unsync.binders k.code Variable.Set.empty)
      |> Variable.Set.union Variable.tid_var_set
    in
    {
      name = k.name;
      array_name = k.array_name;
      code = Code.from_unsync k.code;
      exact_local_variables;
      approx_local_variables;
      pre = b_and_ex (List.map Range.to_cond k.ranges);
    }
end


let translate (stream:Locsplit.Kernel.t Streamutil.stream) : Kernel.t Streamutil.stream =
  let open Streamutil in
  map Kernel.from_loc_split stream

(* ------------------- SERIALIZE ---------------------- *)

let print_kernels (ks : Kernel.t Streamutil.stream) : unit =
  print_endline "; flatacc";
  let count = ref 0 in
  Streamutil.iter (fun (k:Kernel.t) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; acc " ^ (string_of_int curr));
    Indent.print (Kernel.to_s k)
  ) ks;
  print_endline "; end of flatacc"

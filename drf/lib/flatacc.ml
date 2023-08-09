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
  type t = {location: Location.t; access: Access.t; cond: bexp; }

  let dim (a:t) : int = List.length a.access.index

  let location (x:t) : Location.t = x.location

  let to_s (a:t) : Indent.t =
    let lineno = (Location.line a.location |> Index.to_base1 |> string_of_int) ^ ": " in
    Line (lineno ^ Access.to_string a.access ^ " if " ^
      b_to_string a.cond ^";")
end

module Code = struct
  type t =
    | Cond of CondAccess.t
    | Seq of t * t
    | Skip

  let to_list : t -> CondAccess.t list =
    let rec to_list (accum:CondAccess.t list) : t -> CondAccess.t list =
      function
      | Skip -> accum
      | Cond a -> a :: accum
      | Seq (p, q) ->
        to_list (to_list accum p) q
    in
    to_list []

  let rec to_s : t -> Indent.t list =
    function
    | Skip -> [Line "skip;"]
    | Seq (p, q) -> to_s p @ to_s q
    | Cond a -> [CondAccess.to_s a]

  (* The dimention is the index count *)
  let rec dim : t -> int option =
    function
    | Skip -> None
    | Cond a -> Some (CondAccess.dim a)
    | Seq (p, q) ->
      (match dim p with
      | Some n -> Some n
      | None -> dim q)

  let from_unsync : Unsync.t -> t =
    let rec flatten (b:bexp) : Unsync.t -> t =
      function
      | Skip -> Skip
      | Assert _ -> failwith "Internall error: call Unsync.inline_asserts first!"
      | Acc (x, e) -> Cond {location = Variable.location x; access = e; cond = b}
      | Cond (b', p) -> flatten (b_and b' b) p
      | Loop (r, p) -> flatten (b_and (Range.to_cond r) b) p
      | Seq (p, q) -> Seq (flatten b p, flatten b q)
    in
    fun u ->
      flatten (Bool true) (Unsync.inline_asserts u)
end

module Kernel = struct
  type t = {
    name: string;
    array_name: string;
    local_variables: Variable.Set.t;
    code: Code.t;
    pre: bexp;
  }

  let to_s (k:t) : Indent.t list =
    [
        Line ("array: " ^ k.array_name ^ ";");
        Line ("locals: " ^ Variable.set_to_string k.local_variables ^ ";");
        Line ("pre: " ^ b_to_string k.pre ^ ";");
        Line "{";
        Block (Code.to_s k.code);
        Line "}"
    ]

  let from_loc_split (k:Locsplit.Kernel.t) : t =
    {
      name = k.name;
      array_name = k.array_name;
      code = Code.from_unsync k.code;
      local_variables = Unsync.binders k.code k.local_variables;
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

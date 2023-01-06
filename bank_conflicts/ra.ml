open Stage0
open Protocols

type inst =
  | Tick of int
  | Loop of Range.t * t
and t = inst list

let indent (p: t) : Indent.t list =
  let rec inst_to_s : inst -> Indent.t list =
    let open Indent in
    function
    | Tick k -> [Line ("tick(" ^ string_of_int k ^ ");")]
    | Loop (r, p) ->
      [
        Line ("foreach (" ^ Range.to_string r ^ ") {");
        Block (List.map inst_to_s p |> List.flatten);
        Line "}"
      ]
  in
  List.map inst_to_s p |> List.flatten

let to_string (x:t) : string =
  indent x |> Indent.to_string

let print (x:t) : unit =
  indent x |> Indent.print

let from_kernel (num_banks:int) (thread_count:Vec3.t) (k: Proto.prog Proto.kernel) : t =
  let shared = Shared_access.shared_memory k.kernel_arrays in
  let idx_analysis : Exp.nexp -> int =
    Index_analysis.analyze num_banks thread_count k.kernel_local_variables
  in
  let rec from_i : Proto.inst -> inst list =
    function
    | Acc (x, {index=l; _}) ->
      (* Flatten n-dimensional array and apply word size *)
      (match Variable.Map.find_opt x shared with
        | Some v ->
          let e =
            l
            |> Shared_access.byte_count_multiplier v.byte_count
            |> Shared_access.flatten_multi_dim v.dim
          in
          [Tick (idx_analysis e)]
        | None -> [])
    | Sync -> []
    | Cond (_, p) -> from_p p
    | Loop (r, p) ->
      let r = Shared_access.uniform thread_count r |> Ojson.unwrap_or r in
      [Loop (r, from_p p)]

  and from_p (l: Proto.prog) : t =
    List.concat_map from_i l
  in
  from_p k.kernel_code

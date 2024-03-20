open Stage0
open Protocols

module Constant = struct
  type t = {var: Variable.t; init: int option}

  let to_string (e:t) : string =
    let init =
      match e.init with
      | None -> ""
      | Some x -> " = " ^ string_of_int x
    in
    Variable.name e.var ^ init

  (* Converts a constant to a variable assignment *)
  let to_assign (default:int) (e:t) : Variable.t * int =
    e.var, e.init |> Option.value ~default

end

type t = {var: Variable.t; constants: Constant.t list}

type assign_t = { current: int; assigns: (Variable.t * Exp.nexp) list }

(* Converts an enumerate to assignments *)
let to_assigns (e:t) : (Variable.t * Exp.nexp) list =
  let a =
    List.fold_left (fun (a:assign_t) (e:Constant.t) ->
      let (x, i) = Constant.to_assign a.current e in
      {current = i + 1; assigns = (x, Exp.Num i) :: a.assigns}
    )
    {current=0; assigns=[]} e.constants
  in
  a.assigns

let to_string (x:t) : string =
  let c =
    x.constants
    |> List.map Constant.to_string
    |> String.concat ", "
  in
  "enum " ^ Variable.name x.var ^ " {" ^ c ^ "}"

let to_s (x:t) : Indent.t list =
  [Line (to_string x ^ ";")]

let ignore_set : Variable.Set.t =
  [
    "cudaResourceViewFormat";
    "cudaTextureReadMode";
    "cudaTextureAddressMode";
    "cudaResourceType";
    "cudaGraphNodeType";
    "cudaGraphExecUpdateResult";
    "cudaGraphMemAttributeType";
    "cudaExternalSemaphoreHandleType";
    "cudaExternalMemoryHandleType";
    "cudaMemAccessFlags";
    "cudaMemPoolAttr";
    "cudaMemLocationType";
    "cudaMemAllocationHandleType";
    "cudaMemAllocationType";
    "cudaMemRangeAttribute";
    "cudaMemoryAdvise";
    "cudaMemcpyKind";
    "cudaFuncAttribute";
    "cudaFlushGPUDirectRDMAWritesScope";
    "cudaFlushGPUDirectRDMAWritesTarget";
    "cudaStreamCaptureStatus";
    "cudaLaunchAttributeID";
    "cudaStreamCaptureMode";
    "cudaChannelFormatKind";
    "cudaDeviceP2PAttr";
    "cudaLimit";
    "cudaSharedMemConfig";
    "cudaDeviceAttr";
    "cudaFuncCache";
    "cudaError";
    "cudaTextureFilterMode";
  ]
  |> List.map Variable.from_name
  |> Variable.Set.of_list

let ignore (e:t) : bool =
  Variable.Set.mem e.var ignore_set


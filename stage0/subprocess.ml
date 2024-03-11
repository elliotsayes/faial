
module type CloseProcess = sig
  type t
  val close_process : t -> Unix.process_status
end

module WithProcess (C:CloseProcess) = struct
  let with_process
    (handle:C.t -> 'a)
    (ch:C.t)
  :
    (Unix.process_status * 'a)
  =
    let res = try handle ch with
      | exc ->
        let _ = C.close_process ch in
        raise exc
    in
      (C.close_process ch, res)
end

module WithProcessIn = WithProcess(struct
  type t = in_channel
  let close_process = Unix.close_process_in
end)

let with_process_in :
  (in_channel -> 'a) ->
  in_channel ->
  (Unix.process_status * 'a)
=
  WithProcessIn.with_process

module WithProcessOut = WithProcess(struct
  type t = out_channel
  let close_process = Unix.close_process_out
end)

let with_process_out :
  (out_channel -> 'a) ->
  out_channel ->
  (Unix.process_status * 'a)
=
  WithProcessOut.with_process

module WithProcessInOut = WithProcess(struct
  type t = in_channel * out_channel
  let close_process = Unix.close_process
end)

let with_process_in_out :
  (in_channel * out_channel -> 'a) ->
  in_channel * out_channel ->
  (Unix.process_status * 'a)
=
  WithProcessInOut.with_process

let ic_to_string ?(chunk_size=1024) (ic:in_channel) : string =
  let buffer = Buffer.create chunk_size in
  let rec loop () =
    try
      Buffer.add_channel buffer ic chunk_size; loop ()
    with End_of_file ->
      Buffer.contents buffer
  in
  loop ()

let process_status_to_string : Unix.process_status -> string =
  function
  | WEXITED n -> "Process exited with return code: " ^ string_of_int n
  | WSIGNALED n -> "Process was killed by a signal: " ^ string_of_int n
  | WSTOPPED n -> "Process was stopped by a signal: " ^ string_of_int n

(*
  Runs a program with a string given as an input
  *)
let run ?(stdin="") ~exe args : (Unix.process_status * string) =
  let cmd = Filename.quote_command exe args in
  Unix.open_process cmd
  |> with_process_in_out (fun (ic, oc) ->
    (* Send the expression to be processed *)
    output_string oc stdin;
    (* Close output to ensure it is processed *)
    close_out oc;
    (* Receive the output *)
    ic_to_string ic
  )

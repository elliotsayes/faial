let run_process (cmd:string) (f:in_channel -> 'a) : (Unix.process_status * 'a) =
  let in_c = Unix.open_process_in cmd in
  let j = try f in_c with exc ->
    let _ = Unix.close_process_in in_c in
    raise exc
  in
    (Unix.close_process_in in_c, j)

let cu_to_json_opt ?(exe="cu-to-json") ?(ignore_fail=false) (fname : string) : (Yojson.Basic.t, int * string) Result.t =
  let cmd = exe ^ " " ^ fname in
  let (r, j) = run_process cmd
    (fun ic -> try Ok (Yojson.Basic.from_channel ic) with
      Yojson.Json_error e -> Error e
    )
  in
  match r, j with
  | Unix.WEXITED 0, Ok j -> Ok j
  | Unix.WEXITED n, Ok j ->
    if ignore_fail then Ok j
    else Error (n, "Expecting exit status 0, but got " ^ string_of_int n)
  | Unix.WEXITED n, Error e -> Error (n, "Error parsing output: " ^ e) 
  | _, Error e -> Error (1, e)
  | _, _ -> Error (1, "Unknown error")

let cu_to_json ?(exe="cu-to-json") ?(ignore_fail=true) (fname : string) : Yojson.Basic.t =
  match cu_to_json_opt ~exe:exe ~ignore_fail:ignore_fail fname with
  | Ok x -> x
  | Error (r, m) ->
    prerr_endline ("cu-to-json: " ^ m);
    exit r

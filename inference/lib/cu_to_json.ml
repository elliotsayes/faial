open Stage0

let cu_to_json_opt
  ?(exe="cu-to-json")
  ?(ignore_fail=false)
  ?(includes=[])
  (fname : string)
:
  (Yojson.Basic.t, int * string) Result.t
=
  let includes = List.map (fun x -> "-I" ^ x) includes in
  let args = [fname] @ includes in
  let cmd = Filename.quote_command exe args in
  let (r, j) =
    Unix.open_process_in cmd
    |> Common.with_process_in (fun ic -> try Ok (Yojson.Basic.from_channel ic) with
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

let cu_to_json
  ?(exe="cu-to-json")
  ?(ignore_fail=false)
  ?(includes=[])
  (fname : string)
:
  Yojson.Basic.t
=
  match cu_to_json_opt ~exe ~includes ~ignore_fail fname with
  | Ok x -> x
  | Error (r, m) ->
    prerr_endline ("cu-to-json: " ^ m);
    exit r

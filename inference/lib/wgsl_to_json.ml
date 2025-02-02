let wgsl_to_json_res
  (wgsl_json : string)
:
  (Yojson.Basic.t, int * string) Result.t
=
  try Ok (Yojson.Basic.from_string wgsl_json) with
  Yojson.Json_error e -> Error (1, "Failed to parse JSON: " ^ e)

let wgsl_to_json
  ?(exe="wgsl-to-json")
  (* If some integer is given, then we return that on exit, otherwise we return
     whatever wgsl-to-json returns *)
  ?(on_error=exit)
  (wgsl_json : string)
:
  Yojson.Basic.t
=
  match wgsl_to_json_res wgsl_json with
  | Ok x -> x
  | Error (r, m) ->
    prerr_endline ("wgsl-to-json: " ^ m);
    on_error r

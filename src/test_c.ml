open Cast
open C_to_imp
(* ------------------------------------------------------------------------ *)

let () =
  match Yojson.Basic.from_channel stdin |> parse_kernels with
  | Ok ks ->
    List.iter (fun k ->
      match C_to_imp.parse_stmt k.code with
      | Ok _ -> ()
      | Error es -> (
          C_to_imp.print_error es;
          exit(-1)
        )
    ) ks
  | Error e ->
    Rjson.print_j_error e;
    exit(-1)

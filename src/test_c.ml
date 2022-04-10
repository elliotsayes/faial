open Cast
open C_to_imp
(* ------------------------------------------------------------------------ *)

let () =
  match Yojson.Basic.from_channel stdin |> parse_kernels with
  | Ok ks ->
    List.iteri (fun i k ->
      "Kernel: " ^ string_of_int i |> print_endline;
      print_kernel k
    ) ks
  | Error e -> Rjson.print_j_error e

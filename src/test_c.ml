module StringMap = Common.StringMap
module VarSet = Exp.VarSet
module VarMap = Exp.VarMap

(* ------------------------------------------------------------------------ *)

let warn_kernels (arrays:VarSet.t StringMap.t) (p:Dlang.d_program) : unit =
  ()


let make_array_map (ks:Imp.p_kernel list) : VarSet.t StringMap.t =
  ks
  |> List.map (fun k ->
      let open Imp in
      let arrays = 
        (* Convert map from name to attribute, to set of name *)
        VarMap.bindings k.p_kernel_arrays
        |> List.split
        |> fst
        |> VarSet.of_list
      in
      (k.p_kernel_name, arrays)
    )
  |> Common.list_to_string_map

let analyze j : Cast.c_program  * Dlang.d_program * (Imp.p_kernel list) =
  let open Indexflow in
  let open Cast in
  let open D_to_imp in
  match Cast.parse_program j with
  | Ok k1 ->
    let k2 = Dlang.rewrite_program k1 in
      (match D_to_imp.parse_program k2 with
      | Ok k3 -> (k1, k2, k3)
      | Error e ->
        Cast.print_program k1;
        print_endline "------";
        Dlang.print_program k2;
        print_endline "-------";
        D_to_imp.print_error e;
        exit(-1)
      )

  | Error e ->
    Rjson.print_error e;
    exit(-1)


let () =
  let (k1, k2, k3) =
    Yojson.Basic.from_channel stdin
    |> analyze
  in
  warn_kernels (make_array_map k3) k2;
  (*
  Cast.print_program k1;
  print_endline "------";
  Dlang.print_program k2;
  print_endline "-------";
  *)
  List.iter Imp.print_kernel k3

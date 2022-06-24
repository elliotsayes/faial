module StringMap = Common.StringMap
module VarSet = Exp.VarSet
module VarMap = Exp.VarMap


let analyze (j:Yojson.Basic.t) : Cast.c_program  * Dlang.d_program * (Imp.p_kernel list) =
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


let () = (*k1 c k2 d k3 imp *)
  let (k1, k2, k3) =
    Yojson.Basic.from_channel stdin
    |> analyze
  in
  Cast.print_program k1;
  print_endline "------";
  Dlang.print_program k2;
  print_endline "------";
  List.iter Imp.print_kernel k3

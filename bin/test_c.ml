module StringMap = Common.StringMap
module VarSet = Exp.VarSet
module VarMap = Exp.VarMap

(* ------------------------------------------------------------------------ *)
let (get_variable: Dlang.d_exp -> Exp.variable option) = function
    Dlang.ParmVarDecl d_var -> Some d_var.name
  | Dlang.VarDecl d_var -> Some d_var.name
  | _ -> None

let warn_call_use_array (arrays:VarSet.t StringMap.t) (p:Dlang.d_program) : unit =
  let print_matched = fun arrays -> fun (func, args) -> args
    |> List.filter_map get_variable
    |> VarSet.of_list
    |> VarSet.inter arrays
    |> VarSet.elements
    |> List.map Exp.var_name
    |> List.iter (fun name -> "WARN: Function call with array: " ^ (Dlang.exp_to_s func) ^ " " ^ name |> Stdlib.prerr_endline) in
  p |> List.filter_map (function
        | Dlang.Kernel kernel -> StringMap.find_opt kernel.name arrays
            |> Option.map (fun arrays -> (kernel.code, arrays))
        | _ -> None)
    |> List.iter (
        fun (code, arrays) -> code |> (Dlang.for_dexp_in_dstmt (function
               | CXXOperatorCallExpr expr -> (expr.func, expr.args) |> print_matched arrays
               | CallExpr expr -> (expr.func, expr.args) |> print_matched arrays
               | _ -> ()
             )
           )
       )

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
  warn_call_use_array (make_array_map k3) k2;

  Cast.print_program k1;
  print_endline "------";
  Dlang.print_program k2;
  print_endline "";
  k2 |> List.length |> print_int;
  print_endline "-------";
  List.iter Imp.print_kernel k3

open Stage0
open Protocols
open Cgen

module VarSet = Variable.Set
module VarMap = Variable.Map

(* Kernel to TOML conversion *)
let arrays_to_l (vm : Memory.t VarMap.t) : (string * Otoml.t) list =
  VarMap.bindings vm
  |> List.map (fun (k, v) -> (var_name k, Otoml.TomlString (arr_type v)))

let scalars_to_l (vs : VarSet.t) : (string * Otoml.t) list =
  VarSet.elements (VarSet.diff vs thread_globals)
  |> List.map (fun v -> (var_name v, Otoml.TomlString "int"))

let kernel_to_table
    (racuda : bool)
    (gv : Gv_parser.t)
    (k : Proto.prog Proto.kernel)
  : Otoml.t =
  let header = Indent.to_string [header_to_s racuda gv k] in
  let body = Indent.to_string [body_to_s (prog_to_s racuda) k] in
  let global_arr = VarMap.filter (fun _ -> Memory.is_global) k.kernel_arrays in
  TomlTable
    [
      ("pass", TomlBoolean true);
      ("includes", TomlArray []);
      ("header", TomlString header);
      ("body", TomlString body);
      ("scalars", TomlTable (scalars_to_l k.kernel_global_variables));
      ("arrays", TomlTable (arrays_to_l global_arr));
    ]

let gen_toml (racuda : bool) (gv : Gv_parser.t) (k : Proto.prog Proto.kernel)
  : string =
  kernel_to_table racuda gv k |> Otoml.Printer.to_string

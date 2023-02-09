open Stage0
open Protocols
open Cgen

module VarSet = Variable.Set
module VarMap = Variable.Map

(* Kernel to TOML conversion *)
let gv_args_to_l (racuda : bool) (gv : Gv_parser.t) : (string * Otoml.t) list =
  let dim_to_l (d : Dim3.t) : Otoml.t list =
    [TomlInteger d.x; TomlInteger d.y; TomlInteger d.z]
  in
  if racuda then
    [
      ("grid_dim", TomlArray (dim_to_l gv.grid_dim));
      ("block_dim", TomlArray (dim_to_l gv.block_dim));
      ("pass", TomlBoolean gv.pass);
    ]
  else [("pass", TomlBoolean true)]

let arrays_to_l (vm : Memory.t VarMap.t) : (string * Otoml.t) list =
  VarMap.bindings vm
  |> List.map (fun (k, v) -> (Variable.name k, Otoml.TomlString (arr_type v)))

let scalars_to_l (vs : VarSet.t) : (string * Otoml.t) list =
  VarSet.elements (VarSet.diff vs thread_globals)
  |> List.map (fun v -> (Variable.name v, Otoml.TomlString "int"))

let kernel_to_table
    (racuda : bool)
    (gv : Gv_parser.t)
    (k : Proto.prog Proto.kernel)
  : Otoml.t =
  let preamble = gv_args_to_l racuda gv in
  let header = Indent.to_string [header_to_s racuda gv k ~toml:true] in
  let body = Indent.to_string [body_to_s (prog_to_s racuda) racuda k] in
  let base_arr = if racuda then ["__dummy", Otoml.TomlString "int"] else [] in
  let global_arr = VarMap.filter (fun _ -> Memory.is_global) k.kernel_arrays in
  TomlTable
    (
      preamble @
      [
        ("includes", TomlArray []);
        ("header", TomlString header);
        ("body", TomlString body);
        ("scalars", TomlTable (scalars_to_l k.kernel_global_variables));
        ("arrays", TomlTable (base_arr @ arrays_to_l global_arr));
      ]
    )

let gen_toml (racuda : bool) (gv : Gv_parser.t) (k : Proto.prog Proto.kernel)
  : string =
  kernel_to_table racuda gv k |> Otoml.Printer.to_string

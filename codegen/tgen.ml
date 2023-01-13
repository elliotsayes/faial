open Stage0
open Protocols
open Proto
open Cgen

module VarSet = Variable.Set
module VarMap = Variable.Map

(* Kernel to TOML conversion *)
let arrays_to_l (vm : Memory.t VarMap.t) : (string * Otoml.t) list =
  VarMap.bindings vm
  |> List.map (fun (k, v) -> (var_name k, Otoml.TomlString (arr_type v false)))

let scalars_to_l (vs : VarSet.t) : (string * Otoml.t) list =
  VarSet.elements (VarSet.diff vs thread_globals)
  |> List.map (fun v -> (var_name v, Otoml.TomlString "int"))

let kernel_to_table (racuda : bool) (k : prog kernel) : Otoml.t =
  let type_decls = if racuda then [] else decl_unknown_types k.kernel_arrays in
  let funct_protos = base_protos racuda @ arr_to_proto k.kernel_arrays racuda in
  let header = (type_decls @ funct_protos |> Common.join "\n") ^ "\n" in
  let body = Indent.to_string [body_to_s (prog_to_s racuda) k] in
  let global_arr = VarMap.filter (fun _ -> Memory.is_global) k.kernel_arrays in
  let open Otoml in
  TomlTable
    [
      ("pass", TomlBoolean true);
      ("includes", TomlArray []);
      ("header", TomlString header);
      ("body", TomlString body);
      ("scalars", TomlTable (scalars_to_l k.kernel_global_variables));
      ("arrays", TomlTable (arrays_to_l global_arr));
    ]

let gen_toml (racuda : bool) (k : prog kernel) : string =
  kernel_to_table racuda k |> Otoml.Printer.to_string

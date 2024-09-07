open Protocols

let globals_to_arrays
  (_args: W_lang.Decl.t list)
:
  Memory.t Variable.Map.t
=
  Variable.Map.empty

let entry_to_kernel
  (globals:W_lang.Decl.t list)
  (e: W_lang.EntryPoint.t)
:
  Imp.Kernel.t
=
  {
    name = e.name;
    ty = "?";
    arrays = globals_to_arrays globals;
    params = Params.empty;
    code = Block [];
    visibility = Global;
  }

let translate (p: W_lang.Program.t) : Imp.Kernel.t list =
  let globals : W_lang.Decl.t list =
    List.filter_map (
      let open W_lang.Def in
      function
      | EntryPoint _ -> None
      | Declaration d -> Some d
    ) p
  in
  p
  |> List.filter_map (
    let open W_lang.Def in
    function
    | EntryPoint e -> Some e
    | Declaration _ -> None
  )
  |> List.map (entry_to_kernel globals)

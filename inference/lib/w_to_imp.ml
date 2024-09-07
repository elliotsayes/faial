open Protocols

let tr_address_space : W_lang.AddressSpace.t-> Mem_hierarchy.t option =
  function
  | WorkGroup -> Some Mem_hierarchy.SharedMemory
  | Storage ReadWrite
  | Storage WriteOnly -> Some Mem_hierarchy.GlobalMemory
  | Storage ReadOnly
  | Uniform
  | Handle
  | PushConstant
  | Function
  | Private -> None

let tr_type (ty:W_lang.Type.t) : (int list * string list) option =
  match ty.inner with
  | Array {base; size} ->
    Some (Option.to_list size , [W_lang.Type.to_string base])
  | _ -> None

let tr_decl (d: W_lang.Decl.t) : (Variable.t * Memory.t) option =
  let ( let* ) = Option.bind in
  let* h : Mem_hierarchy.t = tr_address_space d.space in
  let* (size, data_type) = tr_type d.ty in
  let name = d.name in
  Some (Variable.from_name name, Memory.{hierarchy=h; size; data_type})

let globals_to_arrays
  (globals: W_lang.Decl.t list)
:
  Memory.t Variable.Map.t
=
  globals
  |> List.filter_map tr_decl
  |> Variable.Map.of_list

let rec tr_stmt : W_lang.Statement.t -> Imp.Stmt.t option =
  function
  | Block l -> Some (tr_block l)
  | Return None -> None
  | _ -> None
and tr_block (l: W_lang.Statement.t list) : Imp.Stmt.t =
  Block (List.filter_map tr_stmt l)

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
    code = tr_block e.function_.body;
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

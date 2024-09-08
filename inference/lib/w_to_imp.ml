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

module BaseType = struct
  type t =
    | Int
    | Bool
    | Container of t

  let deref (ty:t option) : t option =
    match ty with
    | Some (Container b) -> Some b
    | _ -> None

  let container (ty: t option) : t option =
    Option.map (fun s -> Container s) ty

  let join (ty1:t option) (ty2:t option) : t option =
    if ty1 = ty2 then ty1
    else
    match ty1, ty2 with
    | Some Int, Some Bool
    | Some Bool, Some Int
    | Some Int, Some Int -> Some Int
    | Some Bool, Some Bool -> Some Bool
    | _, _ -> None

  let literal : W_lang.Literal.t -> t option =
    function
    | F32 _
    | F64 _
    | AbstractFloat _ -> None
    | U32 _
    | U64 _
    | I32 _
    | I64 _
    | AbstractInt _ -> Some Int
    | Bool _ -> Some Bool

  let scalar_kind : W_lang.ScalarKind.t -> t option =
    let open W_lang.ScalarKind in
    function
    | Sint
    | Uint
    | AbstractInt -> Some Int
    | Bool -> Some Bool
    | _ -> None

  let scalar (s:W_lang.Scalar.t) : t option =
    scalar_kind s.kind

  let rec type_ (ty:W_lang.Type.t) : t option =
    let open W_lang.Type in
    match ty.inner with
    | Scalar s
    | Atomic s ->
      scalar s
    | Pointer {base=ty; _}
    | Array {base=ty; _} ->
      type_ ty |> container
    | Vector {scalar=s; _} ->
      scalar s |> container
    | _ -> None

  let rec expression : W_lang.Expression.t -> t option =
    function
    | ArrayLength _ ->
      Some Int
    | As {kind=s; _} ->
      scalar_kind s
    | Literal l ->
      literal l
    | LocalVariable {ty; _}
    | GlobalVariable {ty; _}
    | FunctionArgument {ty; _}
    | WorkGroupUniformLoadResult ty
    | SubgroupOperationResult ty
    | AtomicResult {ty; _}
    | ZeroValue ty
    | Compose {ty; _} ->
      type_ ty
    | Unary {expr=e}
    | Load e ->
      expression e
    | Select {accept=left; reject=right; _ }
    | Binary {left; right; _} ->
      join (expression left) (expression right)
    | Access {base; _}
    | AccessIndex {base; _} ->
      expression base |> deref
    | CallResult _
    | Derivative _
    | Relational _
    | Math _
    | RayQueryProceedResult
    | RayQueryGetIntersection _
    | SubgroupBallotResult
    | ImageQuery _
    | Override
    | Splat _
    | Swizzle _
    | ImageSample _
    | Constant (* TODO *)
    | ImageLoad _ ->
      None
end

module Literal = struct
  let n_tr (l:W_lang.Literal.t) : Exp.nexp option =
    W_lang.Literal.to_int l |> Option.map (fun x -> Exp.Num x)

  let b_tr (l:W_lang.Literal.t) : Exp.bexp option =
    W_lang.Literal.to_bool l |> Option.map (fun x -> Exp.Bool x)
end

module Variables = struct
  let tr : W_lang.Expression.t -> (C_type.t * Variable.t) option =
    let open W_lang.Expression in
    function
    | GlobalVariable {name; ty; _}
    | FunctionArgument {name; ty; _}
    | LocalVariable {name; ty;_} ->
      Some (C_type.make (W_lang.Type.to_string ty), Variable.from_name name)
    | _ -> None
end

module Expressions = struct
  module Context = struct
    open Imp
    type t = Stmt.t list

    let counter = ref 1

    let empty : t = []

    let add_var ?(label="") (f:Variable.t -> Stmt.t list) (st:t) : (t * Variable.t) =
      let count = !counter in
      counter := count + 1;
      let name : string = "@AccessState" ^ string_of_int count in
      let label = if label = "" then None else Some label in
      let x = {Variable.name=name; Variable.label=label;Variable.location=None} in
      (f x @ st, x)

    let add_read (ty:C_type.t) (array:Variable.t) (index:Exp.nexp list) (st:t) : (t * Variable.t) =
      add_var (fun target ->
        let open Read in
        [Stmt.Read {target;ty; array; index}]
      ) st

    let pure (st:t) (x:'a) : t * 'a =
      (st, x)

    let opt_pure (st:t) (x:'a option) : (t * 'a) option =
      x |> Option.map (pure st)

  end

  let n_todo : Exp.nexp = Var (Variable.from_name "TODO")

  let b_todo : Exp.bexp = CastBool n_todo

  let ( let* ) = Option.bind

  let n_tr_ex
    ((ctx,e) : Context.t * W_lang.Expression.t )
  :
    (Context.t * Exp.nexp) option
  =
    match e with
    | AccessIndex {base=FunctionArgument f; index}
      when W_lang.FunctionArgument.is_tid f ->
      let tid = List.nth Variable.tid_list index in
      Some (ctx, Exp.Var tid)
    | Literal l -> Literal.n_tr l |> Context.opt_pure ctx
    | GlobalVariable _
    | LocalVariable _
    | FunctionArgument _ ->
      let* (_, v) = Variables.tr e in
      Some (ctx, Exp.Var v)
    | _ -> Some (ctx, n_todo)
  and b_tr_ex
    ((ctx,e) : Context.t * W_lang.Expression.t )
  :
    (Context.t * Exp.bexp) option
  =
    match e with
    | Literal l -> Literal.b_tr l |> Context.opt_pure ctx
    | _ -> Some (ctx, b_todo)

  let rec any_tr
    (e : W_lang.Expression.t )
    (ctx : Context.t)
  :
    Context.t
  =
    match e with
    | FunctionArgument _
    | GlobalVariable _
    | LocalVariable _
    | Literal _
    | Constant
    | Override
    | ZeroValue _
    | ImageSample _
    | ImageLoad _
    | CallResult _
    | AtomicResult _
    | WorkGroupUniformLoadResult _
    | RayQueryProceedResult
    | SubgroupBallotResult
    | SubgroupOperationResult _
    | ImageQuery _ ->
      ctx
    | Math _
    | Compose _ -> (* TODO *)
      ctx
    | Access {base; index} ->
      (* try to add a read, otherwise no changes to ctx*)
      (
        let* (ty, array) = Variables.tr base in
        let* (ctx, index) = n_tr_ex (ctx, index) in
        let (ctx, _) = Context.add_read ty array [index] ctx in
        Some ctx
      )
      |> Option.value ~default:ctx
    | AccessIndex {base=e; _}
    | RayQueryGetIntersection {query=e; _}
    | Splat {value=e; _}
    | Swizzle {vector=e; _}
    | Load e
    | Derivative {expr=e; _}
    | ArrayLength e
    | As {expr=e; _}
    | Relational {argument=e; _}
    | Unary {expr=e; _} ->
      any_tr e ctx
    | Binary {left=e1; right=e2; _} ->
      ctx |> any_tr e1 |> any_tr e2
    | Select {condition=e1; accept=e2; reject=e3;} ->
      ctx |> any_tr e1 |> any_tr e2 |> any_tr e3


  type inference =
    | FoundInt of Exp.nexp
    | FoundBool of Exp.bexp
    | Unsupported

  let tr (e:W_lang.Expression.t) : (Imp.Stmt.t list * inference) option =
    match BaseType.expression e with
    | Some Bool ->
      let* (ctx, b) = b_tr_ex ([], e) in
      Some (ctx, FoundBool b)
    | Some Int ->
      let* (ctx, n) = n_tr_ex ([], e) in
      Some (ctx, FoundInt n)
    | _ ->
      Some (any_tr e [], Unsupported)

  let n_tr (e:W_lang.Expression.t) : (Imp.Stmt.t list * Exp.nexp) option =
    let* (ctx, e) = tr e in
    match e with
    | FoundInt e -> Some (ctx, e)
    | FoundBool e -> Some (ctx, Exp.CastInt e)
    | _ -> None

(*
  let b_tr (e:W_lang.Expression.t) : (Imp.Stmt.t list * Exp.bexp) option =
    let* (ctx, e) = tr e in
    match e with
    | FoundInt e -> Some (ctx, Exp.CastBool e)
    | FoundBool e -> Some (ctx, e)
    | Unsupported -> None
*)
end

module Statements = struct
  let rec tr : W_lang.Statement.t -> Imp.Stmt.t list =
    let open Imp.Stmt in
    function
    | Store {pointer=Access {base; index}; value} ->
      let ( let* ) = Option.bind in
      (
        let* (_, array) = Variables.tr base in
        let* (stmts1, index) = Expressions.n_tr index in
        let stmts2 = Expressions.any_tr value [] in
        Some (stmts1 @ stmts2 @ [Write {array; index=[index]; payload=None}])
      ) |> Option.value ~default:[]
    | _ ->
      []
  and tr_block (l:W_lang.Statement.t list) : Imp.Stmt.t =
    Imp.Stmt.Block (List.concat_map tr l)
end

module EntryPoints = struct
  let tr
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
      code = Statements.tr_block e.function_.body;
      visibility = Global;
    }
end

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
  |> List.map (EntryPoints.tr globals)

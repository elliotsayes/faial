open Protocols

let ( let* ) = Option.bind

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

module Literals = struct
  open Imp

  let n_tr (l:W_lang.Literal.t) : Exp.nexp option =
    W_lang.Literal.to_int l |> Option.map (fun x -> Exp.Num x)

  let b_tr (l:W_lang.Literal.t) : Exp.bexp option =
    W_lang.Literal.to_bool l |> Option.map (fun x -> Exp.Bool x)

  let tr (l:W_lang.Literal.t) : Infer_exp.t =
    match W_lang.Literal.to_int l with
    | Some n -> Infer_exp.num n
    | None ->
      (match W_lang.Literal.to_bool l with
        | Some b -> Infer_exp.bool b
        | None -> Infer_exp.unknown (W_lang.Literal.to_string l))
end

module Variables = struct
  let tr : W_lang.Expression.t -> (C_type.t * Variable.t) option =
    let open W_lang.Expression in
    function
    | Ident {name; ty; _} ->
      Some (C_type.make (W_lang.Type.to_string ty), Variable.from_name name)
    | _ -> None
end

module Expressions = struct
  open W_lang
  type t =
    | Unsupported
    | Literal of Literal.t
    | Constant
    | ZeroValue of Type.t
    | Compose of {
        ty: Type.t;
        components: t list;
      }
    | AccessIndex of {
        base: t;
        index: int;
      }
    | Splat of {
        size: VectorSize.t;
        value: t;
      }
    | Ident of Ident.t
    | Unary of {
  (*         op: UnaryOperator, *)
        expr: t;
      }
    | Binary of {
        op: BinaryOperator.t;
        left: t;
        right: t;
      }
    | Select of {
        condition: t;
        accept: t;
        reject: t;
      }
    | Relational of {
  (*         fun: RelationalFunction, *)
        argument: t;
      }
    | Math of {
        fun_: MathFunction.t;
        args: t list;
      }
    | As of {
        expr: t;
        kind: ScalarKind.t;
        convert: int option;
      }
    | CallResult of string
    | AtomicResult of {
        ty: Type.t;
        comparison: bool;
      }
    | WorkGroupUniformLoadResult of Type.t
    | ArrayLength of t
    | SubgroupBallotResult
    | SubgroupOperationResult of Type.t

  module Context = struct

    type expr = t

    module Read = struct
      type t = {
        target: string;
        array: string;
        ty: Type.t;
        index: expr;
      }
    end

    type t = Read.t list

    let counter = ref 1

    let empty : t = []

    let add_var (f:string -> Read.t list) (st:t) : (t * string) =
      let count = !counter in
      counter := count + 1;
      let name : string = "@AccessState" ^ string_of_int count in
      (f name @ st, name)

    let add_read (ty:Type.t) (array:string) (index:expr) (st:t) : (t * expr) =
      let (ctx, name) = add_var (fun target -> [{target; array; ty; index}]) st
      in
      (ctx, Ident {name; ty; kind=LocalVariable})

    let pure (st:t) (x:'a) : t * 'a =
      (st, x)

    let opt_pure (st:t) (x:'a option) : (t * 'a) option =
      x |> Option.map (pure st)

    let rec rewrite (ctx:t) (e: W_lang.Expression.t) : t * expr =
      let pure (e:expr) : t * expr = (ctx, e) in
      let unsupported = (ctx, Unsupported) in
      match e with
      | Literal l -> pure (Literal l)
      | Constant -> pure Constant
      | Override -> unsupported
      | ZeroValue ty -> pure (ZeroValue ty)
      | Compose {ty; components} ->
        let (ctx, components) = l_rewrite ctx components in
        (ctx, Compose {ty; components})
      | Access {base=Ident {ty; name; _}; index;} ->
        let (ctx, index) = rewrite ctx index in
        add_read ty name index ctx
      | Access _ -> unsupported
      | AccessIndex {base; index} ->
        let (ctx, base) = rewrite ctx base in
        (ctx, AccessIndex {base; index})
      | Splat {size; value} ->
        let (ctx, value) = rewrite ctx value in
        (ctx, Splat {size; value})
      | Swizzle {vector; _} ->
        (add ctx vector, Unsupported)
      | Ident i ->
        pure (Ident i)
      | Load e ->
        let (ctx, e) = rewrite ctx e in
        (ctx, e)
      | ImageSample {
          image;
          sampler;
          coordinate;
          array_index;
          offset;
          depth_ref
        } ->
        let ctx = add ctx image in
        let ctx = add ctx sampler in
        let ctx = add ctx coordinate in
        let ctx = o_add ctx array_index in
        let ctx = o_add ctx offset in
        let ctx = o_add ctx depth_ref in
        (ctx, Unsupported)
      | ImageLoad {image; coordinate; array_index; sample; level} ->
        let ctx = add ctx image in
        let ctx = add ctx coordinate in
        let ctx = o_add ctx array_index in
        let ctx = o_add ctx sample in
        let ctx = o_add ctx level in
        (ctx, Unsupported)
      | ImageQuery {image; query} ->
        let ctx = add ctx image in
        let ctx =
          match query with
          | Size (Some e) -> add ctx e
          | _ -> ctx
        in
        (ctx, Unsupported)
      | Unary {expr} ->
        let (ctx, expr) = rewrite ctx expr in
        (ctx, Unary {expr})
      | Binary {op; left; right} ->
        let (ctx, left) = rewrite ctx left in
        let (ctx, right) = rewrite ctx right in
        (ctx, Binary {op; left; right})
      | Select {condition; accept; reject;} ->
        let (ctx, condition) = rewrite ctx condition in
        let (ctx, accept) = rewrite ctx accept in
        let (ctx, reject) = rewrite ctx reject in
        (ctx, Select {condition; accept; reject;})
      | Derivative {expr;} ->
        let ctx = add ctx expr in
        (ctx, Unsupported)
      | Relational {argument;} ->
        let (ctx, argument) = rewrite ctx argument in
        (ctx, Relational {argument;})
      | Math {fun_; args;} ->
        let (ctx, args) = l_rewrite ctx args in
        (ctx, Math {fun_; args;})
      | As {expr; kind; convert;} ->
        let (ctx, expr) = rewrite ctx expr in
        (ctx, As {expr; kind; convert;})
      | CallResult r ->
        pure (CallResult r)
      | AtomicResult {ty; comparison} ->
        pure (AtomicResult {ty; comparison})
      | WorkGroupUniformLoadResult ty ->
        pure (WorkGroupUniformLoadResult ty)
      | ArrayLength expr ->
        let ctx = add ctx expr in
        (ctx, Unsupported)
      | RayQueryProceedResult -> unsupported
      | RayQueryGetIntersection {query; _} ->
        let ctx = add ctx query in
        (ctx, Unsupported)
      | SubgroupBallotResult -> pure SubgroupBallotResult
      | SubgroupOperationResult ty -> pure (SubgroupOperationResult ty)
    and add (ctx:t) (e: W_lang.Expression.t) : t =
      rewrite ctx e |> fst
    and o_add (ctx:t) (e: W_lang.Expression.t option) : t =
      match e with
      | Some e -> add ctx e
      | None -> ctx
    and l_rewrite (ctx:t) (l: W_lang.Expression.t list) : t * expr list =
      List.fold_left_map rewrite ctx l

    let rec to_i_exp : expr -> Imp.Infer_exp.t =
      function
      | Literal l -> Literals.tr l
      | AccessIndex {base=Ident f; index}
        when W_lang.Ident.is_tid f ->
        let tid = List.nth Variable.tid_list index in
        NExp (Var tid)
      | Ident {name; _} ->
        NExp (Var (Variable.from_name name))
      | Binary {op; left; right} ->
        let left = to_i_exp left in
        let right = to_i_exp right in
        let open N_binary in
        let open N_rel in
        let open B_rel in
        (match op with
        | Add -> NExp (Binary (Plus, left, right))
        | Subtract -> NExp (Binary (Minus, left, right))
        | Multiply -> NExp (Binary (Mult, left, right))
        | Divide -> NExp (Binary (Div, left, right))
        | Modulo -> NExp (Binary (Mod, left, right))
        | Equal -> BExp (NRel (Eq, left, right))
        | NotEqual -> BExp (NRel (Neq, left, right))
        | Less -> BExp (NRel (Lt, left, right))
        | LessEqual -> BExp (NRel (Le, left, right))
        | Greater -> BExp (NRel (Gt, left, right))
        | GreaterEqual -> BExp (NRel (Ge, left, right))
        | And -> NExp (Binary (BitAnd, left, right))
        | ExclusiveOr -> NExp (Binary (BitXOr, left, right))
        | InclusiveOr -> NExp (Binary (BitOr, left, right))
        | LogicalAnd -> BExp (BRel (BAnd, left, right))
        | LogicalOr -> BExp (BRel (BOr, left, right))
        | ShiftLeft -> NExp (Binary (LeftShift, left, right))
        | ShiftRight -> NExp (Binary (RightShift, left, right))
        )
      | Select {condition; accept; reject;} ->
        NExp (NIf (to_i_exp condition, to_i_exp accept, to_i_exp reject))
      | As {expr; _} ->
        to_i_exp expr
      | Unary _ -> Unknown "Unary"
      | Splat _ -> Unknown "Splat"
      | ZeroValue _ -> Unknown "ZeroValue"
      | Compose _ -> Unknown "Compose"
      | _ -> Unknown "?"
  end

  let n_todo : Exp.nexp = Var (Variable.from_name "TODO")

  let b_todo : Exp.bexp = CastBool n_todo


  let rec n_tr (e: W_lang.Expression.t) : (Imp.Stmt.t list * Exp.nexp) =
    let (reads, e) = Context.rewrite [] e in
    let reads = List.concat_map r_tr reads in
    let (decls, e) =
      e
      |> Context.to_i_exp
      |> Imp.Infer_exp.infer_nexp
    in
    (reads @ decls, e)

  and r_tr (r: Context.Read.t) : Imp.Stmt.t list =
    (* get base type *)
    let ty =
      Type.deref r.ty
      |> Option.get
      |> Type.to_string
      |> C_type.make
    in
    let target = Variable.from_name r.target in
    let array = Variable.from_name r.array in
    let (decls, index) =
      r.index
      |> Context.to_i_exp
      |> Imp.Infer_exp.infer_nexp
    in
    decls
    @
    [
      Imp.Stmt.Read {
        index=[index]; array; target; ty;
      }
    ]

  let b_tr (e: W_lang.Expression.t) : (Imp.Stmt.t list * Exp.bexp) =
    let (reads, e) = Context.rewrite [] e in
    let reads = List.concat_map r_tr reads in
    let (decls, e) =
      e
      |> Context.to_i_exp
      |> Imp.Infer_exp.infer_bexp
    in
    (reads @ decls, e)

  let ( let* ) = Option.bind

end

module Statements = struct
  let rec tr : W_lang.Statement.t -> Imp.Stmt.t list =
    let open Imp.Stmt in
    let ret = Option.value ~default:[] in
    function
    | Store {pointer=Access {base; index}; value} ->
      ret (
        let* (_, array) = Variables.tr base in
        let (stmts1, index) = Expressions.n_tr index in
        let (stmts2, value) = Expressions.n_tr value in
        let payload =
          match value with
          | Num n -> Some n
          | _ -> None
        in
        Some (stmts1 @ stmts2 @ [Write {array; index=[index]; payload}])
      )
    | Store {pointer=Ident ({ty; _}) as i; value} when W_lang.Type.is_int ty ->
      ret (
        let* (ty, var) = Variables.tr i in
        let (stmts, data) = Expressions.n_tr value in
        Some (stmts @ [ Assign {var; data; ty} ])
      )
    | Block l ->
      [tr_block l]
    | Loop {body=(If {condition; accept=[];reject=[Break]})::body; continuing=[Store _] as c} ->
      ret (
        let (stmts, cond) = Expressions.b_tr condition in
        let inc = tr_block c in
        let body = tr_block body in
        let f : Imp.For.t = {
          init = None;
          cond = Some cond;
          inc = Some inc;
        } in
        let* r =
          match Imp.For.to_range f with
          | Some r -> Some r
          | None -> print_endline ("Missed range: " ^ Imp.For.to_string f);
            None
        in
        Some (
          stmts @ [For (r, body)]
        )
      )
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

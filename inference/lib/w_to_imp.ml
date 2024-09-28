open Protocols

let ( let* ) = Option.bind

module Arrays = struct
  let tr_address_space : W_lang.AddressSpace.t-> Mem_hierarchy.t option =
    function
    | WorkGroup ->
      Some Mem_hierarchy.SharedMemory
    | Storage ReadWrite
    | Storage WriteOnly
    | Storage ReadOnly ->
      Some Mem_hierarchy.GlobalMemory
    | Uniform
    | Handle
    | PushConstant
    | Function
    | Private ->
      None

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

  let tr
    (globals: W_lang.Decl.t list)
  :
    Memory.t Variable.Map.t
  =
    globals
    |> List.filter_map tr_decl
    |> Variable.Map.of_list
end

module Literals = struct
  open Imp

  type t =
    | Bool of bool
    | Int of int

  let parse : W_lang.Literal.t -> t option =
    function
    | U32 v -> Some (Int v)
    | I32 v -> Some (Int v)
    | U64 v -> Some (Int v)
    | I64 v -> Some (Int v)
    | AbstractInt v -> Some (Int v)
    | Bool v -> Some (Bool v)
    | F64 _
    | F32 _
    | AbstractFloat _ ->
      None

  let n_tr (l:W_lang.Literal.t) : Exp.nexp option =
    W_lang.Literal.to_int l |> Option.map (fun x -> Exp.Num x)

  let b_tr (l:W_lang.Literal.t) : Exp.bexp option =
    W_lang.Literal.to_bool l |> Option.map (fun x -> Exp.Bool x)

  let tr : t -> Infer_exp.t =
    function
    | Int n -> Infer_exp.num n
    | Bool b -> Infer_exp.bool b
end


module Types = struct
  open W_lang
  let tr (ty:Type.t) : C_type.t =
    match ty.inner with
    | Scalar s when Scalar.is_int s ->
      let unsigned = if Scalar.is_unsigned s then "u" else "" in
      let width = string_of_int (s.width * 8) in
      C_type.make (unsigned ^ "int" ^ width ^ "_t")
    | _ ->
      ty
      |> Type.to_string
      |> C_type.make
end

module Globals = struct
  let tr (d:W_lang.Decl.t) : (Variable.t * C_type.t) list =
    match d.ty.inner with
    | Scalar s when W_lang.Scalar.is_int s -> [(Variable.from_name d.name, Types.tr d.ty)]
    | Array _ -> [(Variable.from_name (d.name ^ ".len"), C_type.int)]
    | Struct {members=l; _} ->
      List.filter_map (fun m ->
          let open W_lang.Type in
          if W_lang.Type.is_int m.ty then
            Some ((Variable.from_name (d.name ^ "." ^ m.name), Types.tr m.ty))
          else
            None
        ) l
    | _ -> []
end

module Variables = struct
  let tr : W_lang.Expression.t -> (C_type.t * Variable.t) option =
    let open W_lang.Expression in
    function
    | Ident {var; ty; _} ->
      Some (Types.tr ty, var)
    | _ -> None
end

module NDAccess = struct
  open W_lang
  type t = {
    array: Ident.t;
    index: Expression.t list;
  }

  (* We are only able to flatten an NDAccess when the identifier is
     a function-arg/global _and_ the type is a series of structs/vecs
     _and_ every lookup is a literal number.
    *)
  let flatten (e:t) : Ident.t option =
    let ( let* ) = Option.bind in
    if not (Ident.is_global e.array || Ident.is_function_argument e.array)
    then None else
    let rec loop (ident:Ident.t) (index:Expression.t list) : Ident.t option =
      if index = [] then Some ident else
      let* (field, index) =
        match index with
        | Literal l :: index ->
          l
          |> Literal.to_int
          |> Option.map (fun i -> (i, index))

        | _ -> None
      in
      let* ident = Ident.inline_field field ident in
      loop ident index
    in
    loop e.array e.index

  let from_expression (location:Stage0.Location.t) (e:Expression.t) : t option =
    let ( let* ) = Option.bind in
    let* e, l =
      match e with
      | Access {base=e; index; _} -> Some (e, [index])
      | AccessIndex {base=e; index; _} -> Some (e, [Expression.int index])
      | _ -> None
    in
    let rec from_exp (e:Expression.t) (l:Expression.t list) : t option =
      match e with
      | Access {base; index; _} ->
        from_exp base (index :: l)
      | AccessIndex {base; index; _} ->
        from_exp base (Expression.int index :: l)
      | Ident i ->
        Some {array=Ident.set_location location i; index=l}
      | _ -> None
    in
    from_exp e l
end

module Expressions = struct
  open W_lang
  type t =
    | Unsupported
    | Literal of Literals.t
    | ZeroValue of Type.t
    | Compose of {
        ty: Type.t;
        components: t list;
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
    | AtomicResult of {
        ty: Type.t;
        comparison: bool;
      }
    | WorkGroupUniformLoadResult of Type.t
    | SubgroupBallotResult
    | SubgroupOperationResult of Type.t

  let int (i:int) : t =
    Literal (Literals.Int i)


  module Context = struct

    type expr = t

    module Read = struct
      type t = {
        target: (Type.t * Variable.t) option;
        array: Variable.t;
        index: expr list;
      }
    end

    type t = Read.t list

    let counter = ref 1

    let empty : t = []

    let add_var (f:Variable.t -> Read.t) (st:t) : (t * Variable.t) =
      let count = !counter in
      counter := count + 1;
      let var = "@AccessState" ^ string_of_int count |> Variable.from_name in
      (f var :: st, var)

    let add_read
      ~array:(array:Ident.t)
      ~index:(index:expr list)
      (st:t)
    :
      (t * expr)
    =
      let ty = Type.deref_dim (List.length index) array.ty |> Option.get in
      let (ctx, var) =
        add_var (fun target ->
          {target=Some (ty, target); array=array.var; index}
        ) st
      in
      (ctx, Ident {var; ty; kind=LocalVariable})

    let add_read_to
      ~target:(target:Ident.t)
      ~array:(array:Variable.t)
      ~index:(index:expr list)
      (ctx:t)
    :
      (t * expr)
    =
      ({target=None; array; index} :: ctx, Ident target)

    let pure (st:t) (x:'a) : t * 'a =
      (st, x)

    let opt_pure (st:t) (x:'a option) : (t * 'a) option =
      x |> Option.map (pure st)

    let rec rewrite (ctx:t) (e: W_lang.Expression.t) : t * expr =
      let ( let* ) = Option.bind in
      let pure (e:expr) : t * expr = (ctx, e) in
      let unsupported = (ctx, Unsupported) in
      let ret_or (l:W_lang.Expression.t list) (o:(t * expr) option) : t * expr =
        match o with
        | Some v -> v
        | None -> (l_rewrite ctx l |> fst, Unsupported)
      in
      match e with
      | Literal l ->
        pure (
          match Literals.parse l with
          | Some l -> Literal l
          | None -> Unsupported
        )
      | Constant -> unsupported
      | Override -> unsupported
      | ZeroValue ty -> pure (ZeroValue ty)
      | Compose {ty; components} ->
        let (ctx, components) = l_rewrite ctx components in
        (ctx, Compose {ty; components})
      | Access {base; location; index} ->
        ret_or [base; index] (
          let* a = NDAccess.from_expression location e in
          let (ctx, index) = l_rewrite ctx a.index in
          Some (add_read ~array:a.array ~index ctx)
        )
      | AccessIndex {base; location; _} ->
        ret_or [base] (
          let* a = NDAccess.from_expression location e in
          let (ctx, index) = l_rewrite ctx a.index in
          match NDAccess.flatten a with
          | Some target -> Some (add_read_to ~target ~array:a.array.var ~index ctx)
          | None -> Some (add_read ~array:a.array ~index ctx)
        )
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
      | AtomicResult {ty; comparison} ->
        pure (AtomicResult {ty; comparison})
      | WorkGroupUniformLoadResult ty ->
        pure (WorkGroupUniformLoadResult ty)
      | ArrayLength (Ident i) ->
        pure (Ident (W_lang.Ident.add_suffix ".len" i))
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
      (*
      | AccessIndex {base=Ident f; index; location} ->
        let var =
          f
          |> Ident.inline_field index
          |> Ident.var
          |> Variable.set_location location
        in
        NExp (Var var)
      *)
      | Ident {var; _} ->
        NExp (Var var)
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
      | Math {fun_=Min; args=[arg1; arg2]} ->
        NExp (Imp.Infer_exp.min (to_i_exp arg1) (to_i_exp arg2))
      | Math {fun_=Max; args=[arg1; arg2]} ->
        NExp (Imp.Infer_exp.max (to_i_exp arg1) (to_i_exp arg2))
      | _ -> Unknown "?"
  end (* end of Context *)

  let n_todo : Exp.nexp = Var (Variable.from_name "TODO")

  let b_todo : Exp.bexp = CastBool n_todo

  let rec n_tr (e: W_lang.Expression.t) : (Imp.Stmt.t list * Exp.nexp) =
    let (reads, e) = extract_reads e in
    let (decls, e) =
      e
      |> Context.to_i_exp
      |> Imp.Infer_exp.infer_nexp
    in
    (reads @ decls, e)

  and extract_reads (e:W_lang.Expression.t) : (Imp.Stmt.t list * t) =
    let (reads, e) = Context.rewrite [] e in
    (List.concat_map r_tr (List.rev reads), e)

  and l_extract_reads (l:W_lang.Expression.t list) : (Imp.Stmt.t list * t list) =
    let (reads, l) = Context.l_rewrite [] l in
    (List.concat_map r_tr (List.rev reads), l)

  and r_tr (r: Context.Read.t) : Imp.Stmt.t list =
    (* get base type *)
    let target = Option.map (fun (ty, x) -> (Types.tr ty, x)) r.target in
    let array = r.array in
    let (decls, index) =
      r.index
      |> List.map Context.to_i_exp
      |> Imp.Infer_exp.infer_nexp_list
    in
    decls
    @
    [ Imp.Stmt.Read {index; array; target;} ]

  let b_tr (e: W_lang.Expression.t) : (Imp.Stmt.t list * Exp.bexp) =
    let (reads, e) = extract_reads e in
    let (decls, e) =
      e
      |> Context.to_i_exp
      |> Imp.Infer_exp.infer_bexp
    in
    (reads @ decls, e)


  let n_list_tr (l: W_lang.Expression.t list) : (Imp.Stmt.t list * Exp.nexp list) =
    let (reads, l) = l_extract_reads l in
    let (decls, l) =
      l
      |> List.map Context.to_i_exp
      |> Imp.Infer_exp.infer_nexp_list
    in
    (reads @ decls, l)

  let reads (l:W_lang.Expression.t list) : Imp.Stmt.t list =
    n_list_tr l |> fst

end

module Signature = struct
  open W_lang
  type t = (string * Type.t) list
  let from_function (f:Function.t) : t =
    f.arguments
    |> List.map (fun a ->
        let open FunctionArgument in
        (a.name, a.ty)
      )
end

module Typing = struct
  open W_lang
  open Stage0.Common

  type t = Signature.t StringMap.t

  let add_function (f:Function.t) (self:t) : t =
    let f_ty = Signature.from_function f in
    StringMap.add f.name f_ty self

  let add (d:Def.t) (self:t) : t =
    match d with
    | EntryPoint e -> add_function e.function_ self
    | Function f -> add_function f self
    | Declaration _ -> self

  let empty : t = StringMap.empty

  let from_program : Program.t -> t =
    List.fold_left (fun s d -> add d s) empty

  let lookup (name:string) (ctx:t) : Signature.t =
    StringMap.find name ctx

end

module Context = struct
  open W_lang

  type t = {
    arrays: Memory.t Variable.Map.t;
    params: Params.t;
    typing: Typing.t;
  }

  let from_program (p:Program.t) : t =
    let globals : W_lang.Decl.t list =
      List.filter_map (
        let open W_lang.Def in
        function
        | EntryPoint _ | Function _ -> None
        | Declaration d -> Some d
      ) p
    in
    let arrays = Arrays.tr globals in
    let params =
      globals
      |> List.concat_map Globals.tr
      |> Params.from_list
    in
    let typing = Typing.from_program p in
    { arrays; params; typing }

  let lookup (name:string) (ctx:t) : Signature.t =
    Typing.lookup name ctx.typing

end

module Statements = struct
  let tr (ctx:Context.t) : W_lang.Statement.t list -> Imp.Stmt.t list =
    let rec tr : W_lang.Statement.t -> Imp.Stmt.t list =
      let open Imp.Stmt in
      let ret = Option.value ~default:[] in
      let ret_or (l:W_lang.Expression.t list) (r:Imp.Stmt.t list option) =
        match r with
        | Some r -> r
        | None -> Expressions.reads l
      in
      function
      | Store {pointer=Access {location; _} as e; value; }
      | Store {pointer=AccessIndex {location; _} as e; value}
        ->
        ret_or [e; value] (
          let* a = NDAccess.from_expression location e in
          let (stmts1, index) = Expressions.n_list_tr a.index in
          let (stmts2, value) = Expressions.n_tr value in
          let payload =
            match value with
            | Num n -> Some n
            | _ -> None
          in
          Some (stmts1 @ stmts2 @ [Write {array=a.array.var; index; payload}])
        )
      | Store {pointer=Ident ({ty; _}) as i; value} when W_lang.Type.is_int ty ->
        ret (
          let* (ty, var) = Variables.tr i in
          let (stmts, data) = Expressions.n_tr value in
          Some (stmts @ [ Assign {var; data; ty} ])
        )
      | Block l ->
        [tr_block l]
      | If {condition; accept=[Return None]; reject=[]}
      | If {condition; accept=[Break]; reject=[]}
      | If {condition; accept=[Continue]; reject=[]} ->
        let (stmts, c) = Expressions.b_tr condition in
        stmts @ [Assert (Imp.Assert.make (Exp.b_not c) Local)]
      | If {condition; accept; reject} ->
        let (stmts, c) = Expressions.b_tr condition in
        stmts @ [
          If (c, tr_block accept, tr_block reject)
        ]
      | Loop {body; continuing=[Store _] as c; break_if=Some condition} ->
        let (stmts, cond) = Expressions.b_tr condition in
        let inc = tr_block c in
        let body = tr_block body in
        let f : Imp.For.t = {
          init = None;
          cond = Some (Exp.b_not cond);
          inc = Some inc;
        } in
        stmts @ [Imp.For.to_stmt f body]
      | Loop {body=(If {condition; accept=[];reject=[Break]})::body; continuing=[Store _] as c; break_if=None} ->
        let (stmts, cond) = Expressions.b_tr condition in
        let inc = tr_block c in
        let body = tr_block body in
        let f : Imp.For.t = {
          init = None;
          cond = Some cond;
          inc = Some inc;
        } in
        stmts @ [Imp.For.to_stmt f body]
      | Loop {body=(If {condition; accept=[Break];reject=[]})::body; break_if=None; continuing=[Store _] as c} ->
        let (stmts, cond) = Expressions.b_tr condition in
        let inc = tr_block c in
        let body = tr_block body in
        let f : Imp.For.t = {
          init = None;
          cond = Some (Exp.b_not cond);
          inc = Some inc;
        } in
        stmts @ [Imp.For.to_stmt f body]
      | Barrier _ -> [Imp.Stmt.Sync None]

      | Call {result; function_=kernel; arguments=args} ->
        let stmts, args = Expressions.n_list_tr args in
        let args : (Variable.t * Imp.Arg.t) list =
          List.map2 (fun arg (name, ty) ->
            let arg =
              let open Imp in
              if W_lang.Type.is_array ty then
                (match arg with
                | Exp.Var x -> Arg.Array (Imp.Array_use.make x)
                | _ -> Unsupported)
              else if W_lang.Type.is_int ty then
                (* handle scalar *)
                Scalar arg
              else
                (* unsupported *)
                Unsupported
            in
            (Variable.from_name name, arg)
          ) args (Context.lookup kernel ctx)
        in
        stmts
        @
        (match result with
          | Some {var; _} ->
            [Decl [Imp.Decl.unset var]]
          | None -> []
        )
        @
        [ Imp.Stmt.Call Imp.Call.{ args; kernel; ty=kernel; } ]
      | Return (Some e) ->
        let (stmts, _) = Expressions.n_tr e in
        stmts
      | _ ->
        []

    and tr_list (l:W_lang.Statement.t list) : Imp.Stmt.t list =
      List.concat_map tr l

    and tr_block (l:W_lang.Statement.t list) : Imp.Stmt.t =
      Imp.Stmt.Block (tr_list l)

    in
    tr_list

end

module LocalDeclarations = struct
  let tr_local
    (l:W_lang.LocalDeclaration.t)
  :
    Imp.Stmt.t list
  =
    let stmts, init =
      match l.init with
      | Some init ->
        let (stmts, init) = Expressions.n_tr init in
        (stmts, Some init)
      | None -> ([], None)
    in
    stmts
    @
    if W_lang.Type.is_int l.ty then
    [
      Imp.Stmt.Decl [{
        var=l.var;
        ty=Types.tr l.ty;
        init;
      }]
    ]
    else []
  let tr : W_lang.LocalDeclaration.t list -> Imp.Stmt.t list =
    List.concat_map tr_local
end

module Functions = struct
  let tr
    (ctx:Context.t)
    (e: W_lang.Function.t)
  :
    Imp.Kernel.t
  =
    {
      name = e.name;
      ty = e.name;
      arrays = ctx.arrays;
      params = ctx.params;
      code =
        Block (
        LocalDeclarations.tr e.locals
        @ Statements.tr ctx e.body);
      visibility = Device;
      grid_dim = None;
      block_dim = None;
    }
end


module EntryPoints = struct
  let tr
    (ctx:Context.t)
    (e: W_lang.EntryPoint.t)
  :
    Imp.Kernel.t
  =
    { (Functions.tr ctx e.function_) with
      block_dim = Some e.workgroup_size;
      visibility = Global;
    }
end

let translate (p: W_lang.Program.t) : Imp.Kernel.t list =
  let ctx = Context.from_program p in
  p
  |> List.filter_map (
      let open W_lang.Def in
      function
      | EntryPoint e -> Some (EntryPoints.tr ctx e)
      | Function f -> Some (Functions.tr ctx f)
      | Declaration _ -> None
    )

open Stage0
open Protocols

module Code = struct
  type t =
    | Index of Exp.nexp
    | Loop of Range.t * t
    | Cond of Exp.bexp * t
    | Decl of Variable.t * t

  module SubstMake (S:Subst.SUBST) = struct
    module M = Subst.Make(S)

    let rec subst (s:S.t) : t -> t =
      function
      | Loop (r, acc) -> Loop (M.r_subst s r, subst s acc)
      | Cond (b, acc) -> Cond (M.b_subst s b, subst s acc)
      | Index a -> Index (M.n_subst s a)
      | Decl (x, acc) ->
        M.add s x (
          function
          | Some s -> Decl (x, subst s acc)
          | None -> Decl (x, acc)
        )

  end

  module S1 = SubstMake(Subst.SubstPair)

  let subst = S1.subst

  let rec to_string : t -> string =
    function
    | Loop (r, acc) ->
        "for (" ^ Range.to_string r ^ ")\n" ^ to_string acc
    | Cond (b, acc) ->
        "if (" ^ Exp.b_to_string b ^ ")\n" ^ to_string acc
    | Index a ->
        "[" ^ Exp.n_to_string a ^ "]"
    | Decl (x, p) ->
        "var " ^ Variable.name x ^ " " ^ to_string p ^ "\n"

  let rec map_index (f:Exp.nexp -> Exp.nexp) : t -> t =
    function
    | Index a -> Index (f a)
    | Loop (r, p) -> Loop (r, map_index f p)
    | Cond (e, p) -> Cond (e, map_index f p)
    | Decl (x, p) -> Decl (x, map_index f p)

  let rec index : t -> Exp.nexp =
    function
    | Index a -> a
    | Loop (_, p)
    | Cond (_, p)
    | Decl (_, p) -> index p

  let flatten (a:t) : t =
    Index (index a)

  let trim_decls : t -> t =
    let rec opt : t -> Variable.Set.t * t =
      function
      | Index e ->
        Freenames.free_names_nexp e Variable.Set.empty,
        Index e
      | Loop (r, a) ->
        let fns, a = opt a in
        Freenames.free_names_range r fns, Loop (r, a)
      | Cond (e, a) ->
        let fns, a = opt a in
        Freenames.free_names_bexp e fns, Cond (e, a)
      | Decl (x, a) ->
        let fns, a = opt a in
        let a =
          if Variable.Set.mem x fns then
            Decl(x, a)
          else
            a
        in
        fns, a
    in
    fun a ->
      opt a |> snd

  let minimize : t -> t =
    let rec min : t -> Exp.bexp list * Variable.Set.t * t =
      function
      | Index e ->
        [],
        Freenames.free_names_nexp e Variable.Set.empty,
        Index e
      | Loop (r, a) ->
        let l, fns, a = min a in
        if Variable.Set.mem r.var fns then
          let r_fns = Freenames.free_names_range r Variable.Set.empty in
          let loop_l, l = List.partition (Exp.b_mem r.var) l in
          if Variable.Set.inter r_fns fns |> Variable.Set.is_empty then
            l, fns, a
          else
            let a =
              if loop_l = [] then
                a
              else
                Cond (Exp.b_and_ex loop_l, a)
            in
            l, Variable.Set.union r_fns (Variable.Set.add r.var fns), Loop (r, a)
        else
          l, fns, a
      | Cond (e, a) ->
        let l, fns, a = min a in
        let e_l =
          Exp.b_and_split e
          (* only keep variables that mention variables from the body *)
          |> List.filter (Exp.b_exists (fun x -> Variable.Set.mem x fns))
        in
        Common.append_rev1 e_l l, Freenames.free_names_bexp e fns, a
      | Decl (x, a) ->
        let l, fns, a = min a in
        let a =
          if Variable.Set.mem x fns then
            Decl (x, a)
          else
            a
        in
        l, fns, a
    in
    fun a ->
      let l, _, a = min a in
      if l = [] then
        a
      else
        Cond (Exp.b_and_ex l, a)

  let transaction_count (params:Config.t) : Variable.Set.t -> t -> (int, string) Result.t =
    let rec transaction_count (locals:Variable.Set.t) : t -> (int, string) Result.t =
      function
      | Index a ->
        a
        |> Index_analysis.transaction_count params locals
      | Loop (r, a) ->
        let locals =
          if Range.exists (fun x -> Variable.Set.mem x locals) r then
            Variable.Set.add r.var locals
          else
            locals
        in
        transaction_count locals a
      | Cond (_, a) ->
        transaction_count locals a
      | Decl (x, a) ->
        transaction_count (Variable.Set.add x locals) a
    in
    transaction_count

  let to_approx (x:Variable.t) : t -> Approx.Code.t =
    let rec to_approx : t -> Approx.Code.t =
      function
      | Index a -> Acc (x, Access.read [a])
      | Loop (r, a) -> Loop (r, to_approx a)
      | Cond (b, a) -> Cond (b, to_approx a)
      | Decl (x, a) -> Approx.Code.decl x (to_approx a)
    in
    to_approx

  module Make (L:Logger.Logger) = struct
    module O = Offset_analysis.Make(L)
    module R = Uniform_range.Make(L)
    module L = Linearize_index.Make(L)

    let from_proto
      (arrays:Memory.t Variable.Map.t)
      (cfg:Config.t)
    :
      Proto.Code.t -> (Variable.t * t) Seq.t
    =
      let open Exp in
      let lin = L.linearize cfg arrays in
      let rec on_p : Proto.Code.t -> (Variable.t * t) Seq.t =
        function
        | Acc (x, {index=l; _}) ->
          l
          |> lin x
          |> Option.map (fun e ->
              let e = O.remove_offset e in
              Seq.return (x, Index e)
            )
          |> Option.value ~default:Seq.empty
        | Sync _ ->
          Seq.empty
        | Decl {body=p; var; _} ->
          on_p p |> Seq.map (fun (x, i) -> x, Decl (var, i))
        | If (b, p, q) ->
          Seq.append
            (on_p p |> Seq.map (fun (x,p) -> x, Cond (b, p)))
            (on_p q |> Seq.map (fun (x,q) -> x, Cond (Exp.b_not b, q)))
        | Loop (r, p) ->
          on_p p
          |> Seq.map (fun (x,i) ->
            match R.uniform cfg.block_dim r with
            | Some r' ->
              let cnd =
                b_and
                  (n_ge (Var r.var) r.lower_bound)
                  (n_lt (Var r.var) r.upper_bound)
              in
              x, Loop (r', Cond(cnd, i))
            | None ->
              x, Loop (r, i)
          )
        | Skip -> Seq.empty
        | Seq (p, q) ->
          Seq.append (on_p p) (on_p q)
      in
      on_p
  end
  module Silent = Make(Logger.Silent)
  module Default = Make(Logger.Colors)
  let from_proto :
      Memory.t Variable.Map.t ->
      Config.t ->
      Proto.Code.t ->
      (Variable.t * t) Seq.t
    = Default.from_proto
end

module Kernel = struct
  type t = {
    (* The kernel name *)
    name : string;
    (* The array name *)
    array: Variable.t;
    (* The internal variables are used in the code of the kernel.  *)
    global_variables: Params.t;
    (* The internal variables are used in the code of the kernel.  *)
    local_variables: Params.t;
    (* The code of a kernel performs the actual memory accesses. *)
    code: Code.t;
  }

  let transaction_count (params:Config.t) (k:t) : (int, string) Result.t =
    Code.transaction_count params (Params.to_set k.local_variables) k.code

  let location (k:t) : Location.t =
    Variable.location k.array

  let to_string (k:t) : string =
    Code.to_string k.code

  let minimize (k:t) : t =
    { k with code = Code.minimize k.code }

  let map_index (f:Exp.nexp -> Exp.nexp) (k:t) : t =
    {k with code = Code.map_index f k.code }

  let to_check (k:t) : Approx.Check.t =
    let code = Code.to_approx k.array k.code in
    let vars =
      Variable.Set.union
        (Params.to_set k.global_variables) Variable.tid_var_set
    in
    Approx.Check.from_code vars code

  let trim_decls (k:t) : t =
    { k with code = Code.trim_decls k.code; }

  module Make (L:Logger.Logger) = struct
    module R = Uniform_range.Make(L)
    module L = Linearize_index.Make(L)

    (*
    Given a kernel return a sequence of slices.
    *)
    let from_proto
      (cfg:Config.t)
      (k: Proto.Code.t Proto.Kernel.t)
    :
      t Seq.t
    =
      k.code
      |> Proto.Code.subst_block_dim cfg.block_dim
      |> Proto.Code.subst_grid_dim cfg.grid_dim
      |> Code.from_proto k.arrays cfg
      |> Seq.map (fun (array, p) ->
        let code = if k.pre = Bool true then p else Code.Cond (k.pre, p) in
        {
          name = k.name;
          global_variables = k.global_variables;
          local_variables = k.local_variables;
          code;
          array;
        }
      )

  end

  module Silent = Make(Logger.Silent)
  module Default = Make(Logger.Colors)
  let from_proto :
    Config.t ->
    Proto.Code.t Proto.Kernel.t ->
    t Seq.t
  = Default.from_proto

  let flatten (k:t) : t =
    { k with code = Code.flatten k.code }
end

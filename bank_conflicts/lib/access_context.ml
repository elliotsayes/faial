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
        "for (" ^ Range.to_string r ^ ") " ^ to_string acc
    | Cond (b, acc) ->
        "if (" ^ Exp.b_to_string b ^ ") " ^ to_string acc
    | Index a ->
        "[" ^ Exp.n_to_string a ^ "]"
    | Decl (x, p) ->
        "var " ^ Variable.name x ^ " " ^ to_string p

  let rec index : t -> Exp.nexp =
    function
    | Index a -> a
    | Loop (_, p)
    | Cond (_, p)
    | Decl (_, p) -> index p

  let flatten (a:t) : t =
    Index (index a)

  let optimize : t -> t =
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
(*
  let rec to_approx : t -> Approx.Code.t =
    function
    | Index a -> Acc (a.array, Access.read [a.index])
    | Loop (r, a) -> Loop (r, to_approx a)
    | Cond (b, a) -> Cond (b, to_approx a)
    | Decl (x, a) -> Approx.Code.decl x (to_approx a)
*)

  module Make (L:Logger.Logger) = struct
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

  let location (k:t) : Location.t =
    Variable.location k.array

  let to_string (k:t) : string =
    Code.to_string k.code

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
        let code = Code.optimize (Cond (k.pre, p)) in
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

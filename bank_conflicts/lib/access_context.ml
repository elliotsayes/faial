open Stage0
open Protocols

type shared_access = {shared_array: Variable.t; index: Exp.nexp}

type t =
  | Loop of Range.t * t
  | Cond of Exp.bexp * t
  | Index of shared_access
  | Decl of Variable.t * t

module SubstMake (S:Subst.SUBST) = struct
  module M = Subst.Make(S)

  let rec subst (s:S.t) : t -> t =
    function
    | Loop (r, acc) -> Loop (M.r_subst s r, subst s acc)
    | Cond (b, acc) -> Cond (M.b_subst s b, subst s acc)
    | Index a -> Index { a with index = M.n_subst s a.index }
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
      "acc(" ^ Exp.n_to_string a.index ^ ")"
  | Decl (x, p) ->
      "var " ^ Variable.name x ^ " " ^ to_string p

let rec shared_array : t -> Variable.t =
  function
  | Index a -> a.shared_array
  | Loop (_, p)
  | Cond (_, p)
  | Decl (_, p) -> shared_array p

let location (x: t) : Location.t =
  x
  |> shared_array
  |> Variable.location


module Make (L:Logger.Logger) = struct
  module R = Uniform_range.Make(L)
  module L = Linearize_index.Make(L)
  (*
  Given a kernel return a sequence of slices.
   *)
  let from_kernel (cfg:Config.t) (k: Proto.Code.t Proto.Kernel.t) : t Seq.t =
    let open Exp in
    let lin = L.linearize cfg k.arrays in
    let rec on_p : Proto.Code.t -> t Seq.t =
      function
      | Acc (x, {index=l; _}) ->
        l
        |> lin x
        |> Option.map (fun e ->
            Seq.return (Index {shared_array=x; index=e})
          )
        |> Option.value ~default:Seq.empty
      | Sync _ ->
        Seq.empty
      | Decl {body=p; var; _} ->
        on_p p |> Seq.map (fun (i:t) -> Decl (var, i))
      | Cond (b, p) ->
        on_p p
        |> Seq.map (fun (i:t) : t -> Cond (b, i))
      | Loop (r, p) ->
        on_p p
        |> Seq.map (fun i ->
          match R.uniform cfg.block_dim r with
          | Some r' ->
            let cnd =
              b_and
                (n_ge (Var r.var) r.lower_bound)
                (n_lt (Var r.var) r.upper_bound)
            in
            Loop (r', Cond(cnd, i))
          | None ->
            Loop (r, i)
        )
      | Skip -> Seq.empty
      | Seq (p, q) ->
        Seq.append (on_p p) (on_p q)
    in
    k.code
    |> Proto.Code.subst_block_dim cfg.block_dim
    |> Proto.Code.subst_grid_dim cfg.grid_dim
    |> on_p
end

module Silent = Make(Logger.Silent)
module Default = Make(Logger.Colors)

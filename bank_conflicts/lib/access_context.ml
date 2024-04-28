open Stage0
open Protocols

type access = {array: Variable.t; index: Exp.nexp}

type t =
  | Index of access
  | Loop of Range.t * t
  | Cond of Exp.bexp * t
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

let rec access : t -> access =
  function
  | Index a -> a
  | Loop (_, p)
  | Cond (_, p)
  | Decl (_, p) -> access p

let array (a:t) : Variable.t =
  (access a).array

let index (a:t) : Exp.nexp =
  (access a).index

let location (x: t) : Location.t =
  x
  |> array
  |> Variable.location

let flatten (a:t) : t =
  Index (access a)

let optimize : t -> t =
  let rec opt : t -> Variable.Set.t * t =
    function
    | Index a ->
      let e = a.index in
      Freenames.free_names_nexp e Variable.Set.empty,
      Index { a with index = e }
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
            Seq.return (Index {array=x; index=e})
          )
        |> Option.value ~default:Seq.empty
      | Sync _ ->
        Seq.empty
      | Decl {body=p; var; _} ->
        on_p p |> Seq.map (fun (i:t) -> Decl (var, i))
      | If (b, p, q) ->
        Seq.append
          (on_p p |> Seq.map (fun (p:t) : t -> Cond (b, p)))
          (on_p q |> Seq.map (fun (q:t) : t -> Cond (Exp.b_not b, q)))
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
    |> Seq.map optimize
end

module Silent = Make(Logger.Silent)
module Default = Make(Logger.Colors)

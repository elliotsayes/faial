(*

 This module is not used internally; it's only used by the code
 generation algorithm.

 *)

open Stage0
open Protocols

(*
  Given a protocol, generates a sequence of accesses with their
  surrounding context (loops and conditionals).

  Additionally, we:
    - convert from multiple-dimension accesses to a single dimension
    - take into account the byte size of the array type
    - convert non-uniform loops into uniform loops
*)

type array_size = { byte_count: int; dim: int list}

module Make (L:Logger.Logger) = struct
  module R = Uniform_range.Make(L)
  module L = Linearize_index.Make(L)

  (*
    Given a protocol, apply all the transformations above: type-mult,
    nd-array, and uniform ranges.
   *)
  let simplify_kernel
    (cfg: Config.t)
    (k: Proto.Code.t Proto.Kernel.t)
  :
    Proto.Code.t Proto.Kernel.t
  =
    let lin = L.linearize cfg k.arrays in
    let rec simpl : Proto.Code.t -> Proto.Code.t =
      function
      | Acc (x, ({index=l; _} as a)) ->
        (* Flatten n-dimensional array and apply word size *)
        let a =
          l
          |> lin x
          |> Option.map (fun e -> { a with index=[e] })
          |> Option.value ~default:a
        in
        Acc (x, a)
      | Skip -> Skip
      | If (b, p, q) -> If (b, simpl p, simpl q)
      | Decl d -> Decl {d with body= simpl d.body}
      | Loop (r, p) ->
        let p = simpl p in
        (match R.uniform cfg.block_dim r with
        | Some r' ->
          let cnd =
            let open Exp in
            b_and
              (n_ge (Var r.var) r.lower_bound)
              (n_lt (Var r.var) r.upper_bound)
          in
          Loop (r', If(cnd, p, Skip))
        | None ->
          Loop (r, p)
        )
      | Sync l -> Sync l
      | Seq (p, q) -> Seq (simpl p, simpl q)
    in
    let arrays =
      k.arrays
      |> Variable.Map.map (fun m ->
        let open Memory in
        let m = { m with data_type = ["int"] } in
        if Memory.is_shared m && List.length m.size > 0 then (
          { m with size = [ List.fold_left ( * ) 1 m.size ] }
        ) else
          m
      )
    in
    { k with
      code =
        k.code
        |> Proto.Code.subst_block_dim cfg.block_dim
        |> Proto.Code.subst_grid_dim cfg.grid_dim
        |> simpl;
      arrays = arrays;
    }
end

module Silent = Make(Logger.Silent)
module Default = Make(Logger.Colors)

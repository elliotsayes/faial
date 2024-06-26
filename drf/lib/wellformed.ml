open Stage0
open Protocols
open Exp
open Proto

module Opt = Unsync.Opt

module Code = struct
  type t =
    | SInst of Sync.t
    | UInst of Unsync.t
    | Both of Sync.t * Unsync.t

  let add_u (u:Unsync.t) : t -> t =
    function
    | SInst s -> SInst (Sync.add u s)
    | UInst u2 -> UInst (Opt.seq u u2)
    | Both (p, u2) -> Both (Sync.add u p, u2)

  let add_s (s:Sync.t) : t -> t =
    function
    | SInst s2 -> SInst (Seq (s, s2))
    | UInst u -> Both (s, u)
    | Both (s2, u) -> Both (Seq (s, s2), u)

  let seq (p:t) (q:t) : t =
    match p, q with
    | UInst u, s -> add_u u s
    | SInst p, s -> add_s p s
    | Both (p, u), s -> add_s p (add_u u s)

  let free_names (p:t) (fns:Variable.Set.t) : Variable.Set.t =
    match p with
    | SInst s -> Sync.free_names s fns
    | UInst s -> Unsync.free_names s fns
    | Both (p, q) ->
      Sync.free_names p fns
      |> Unsync.free_names q

  (* Given a regular program, return a well-formed one *)
  let from_proto : Proto.Code.t -> Sync.t Streamutil.stream =
    let open Streamutil in
    let rec infer : Proto.Code.t -> t Streamutil.stream =
      function
      | Skip -> UInst Skip |> one
      | Acc e -> UInst (Acc e) |> one
      | Sync _ -> SInst Sync.skip |> one
      | Decl _ -> failwith "Invoke Proto.hoist_decls first."
      | If (b, p, q) ->
        let branch b p =
          infer p
          |> flat_map (
            function
            (* TODO: why should we reject synchronized conditionals inside loops? *)
            | SInst p ->
              [
                UInst (Assert (b_not b));
                SInst (Sync.inline_cond b p);
              ] |> from_list
            | Both (p, c) ->
              [
                UInst (Assert (b_not b));
                Both (Sync.inline_cond b p, Opt.seq (Assert b) c);
              ] |> from_list
            | UInst c ->
              UInst (Opt.cond b c) |> one
          )
        in
        branch b p
        |> flat_map (fun p ->
          branch (Exp.b_not b) q |> map (seq p)
        )
      | Loop (r, p) ->
        infer p
        |> map (
          function
          | Both (p, c) -> SInst (Loop (Skip, r, p, c))
          | SInst p -> SInst (Loop (Skip, r, p, Skip))
          | UInst c -> UInst (Opt.loop r c)
        )
      | Seq (p, q) ->
        infer p
        |> flat_map (fun p ->
          infer q |> map (seq p)
        )
    in
    fun p ->
      infer p
      |> map (function
        | SInst p -> p
        | UInst c -> Sync c
        | Both (p, c) -> Sync.Seq (p, Sync.Sync c)
      )
end

let binders (k: Sync.t Kernel.t) : Variable.Set.t =
  Sync.free_names k.code Variable.Set.empty
  |> Exp.b_free_names k.pre

let trim_binders (k: Sync.t Kernel.t) : Sync.t Kernel.t =
  let fns = binders k in
  { k with
    global_variables = Params.retain_all fns k.global_variables;
    local_variables = Params.retain_all fns k.local_variables;
  }

let translate (k: Proto.Code.t Kernel.t) : Sync.t Kernel.t Streamutil.stream =
  let k = Proto.Kernel.hoist_decls k in
  Code.from_proto k.code
  |> Streamutil.map (fun p -> { k with code = p})

(* ---------------- Pretty printing -------------------- *)

let print_kernel (k : Sync.t Kernel.t) : unit =
  Proto.Kernel.print Sync.to_s k

let print_kernels (ks : Sync.t Kernel.t Streamutil.stream) : unit =
  print_endline "; w-lang";
  let count = ref 0 in
  Streamutil.iter (fun k ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; w-prog " ^ (string_of_int curr));
    print_kernel k
  ) ks;
  print_endline "; end of w-lang"

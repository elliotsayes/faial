open Stage0
open Protocols
open Exp
open Proto

type t =
  | SInst of Sync.t
  | UInst of Unsync.t
  | Both of Sync.t * Unsync.t

let add_u (u:Unsync.t) : t -> t =
  function
  | SInst s -> SInst (Sync.add u s)
  | UInst u2 -> UInst (Seq (u, u2))
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

(* Given a regular program, return a well-formed one *)
let make_well_formed : Proto.Code.t -> Sync.t Streamutil.stream =
  let open Streamutil in
  let rec infer (in_loop:bool) : Proto.Code.t -> t Streamutil.stream =
    function
    | Skip -> UInst Skip |> one
    | Acc e -> UInst (Acc e) |> one
    | Sync -> SInst Sync.skip |> one
    | Decl _ -> failwith "Invoke Proto.hoist_decls first."
    | Cond (b, p) ->
      infer in_loop p
      |> flat_map (
        function
        | Both _ when in_loop ->
          failwith "We do not support synchronized conditionals inside loops"
        | SInst _ when in_loop ->
          failwith "We do not support synchronized conditionals inside loops"
        | SInst p ->
          [
            UInst (Assert (b_not b));
            SInst (Sync.inline_cond b p);
          ] |> from_list
        | Both (p, c) ->
          [
            UInst (Assert (b_not b));
            Both (Sync.inline_cond b p, Seq (Assert b, c));
          ] |> from_list
        | UInst c ->
          UInst (Cond (b, c)) |> one
      )
    | Loop (r, p) ->
      infer true p
      |> map (
        function
        | Both (p, c) -> SInst (Loop (Skip, r, p, c))
        | SInst p -> SInst (Loop (Skip, r, p, Skip))
        | UInst c -> UInst (Loop (r, c))
      )
    | Seq (p, q) ->
      infer in_loop p
      |> flat_map (fun p ->
        infer in_loop q |> map (seq p)
      )
  in
  fun p ->
    infer false p
    |> map (function
      | SInst p -> p
      | UInst c -> Sync c
      | Both (p, c) -> Sync.Seq (p, Sync.Sync c)
    )

let translate (k: Proto.Code.t Kernel.t) : Sync.t Kernel.t Streamutil.stream =
  let k = Proto.Kernel.hoist_decls k in
  make_well_formed k.code
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

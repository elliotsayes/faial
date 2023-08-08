open Stage0
open Protocols
open Exp
open Proto
open Streamutil

type w_or_u_inst =
  | WInst of Sync.t
  | UInst of Unsync.t
  | Both of Sync.t * Unsync.t


(* Given a regular program, return a well-formed one *)
let make_well_formed (p:Proto.prog) : Sync.t Streamutil.stream =
  let rec i_infer (in_loop:bool) (i:Proto.inst): w_or_u_inst Streamutil.stream =
    let open Streamutil in
    match i with
    | Acc e -> UInst (Acc e) |> one
    | Sync -> WInst Sync.skip |> one
    | Cond (b, p) ->
      p_infer in_loop p |>
      map (function
      | (Some p, c) ->
        if in_loop then
          failwith "We do not support synchronized conditionals inside loops"
        else
          [
            UInst (Assert (b_not b));
            Both (Sync.inline_cond b p, Seq (Assert b, c));
          ] |> from_list
      | (None, c) -> UInst (Cond (b, c)) |> one
      )
      |> concat
    | Loop (r, p) ->
      p_infer true p |>
      map (function
      | Some p, c -> WInst (Loop (Skip, r, p, c))
      | None, c -> UInst (Loop (r, c))
      )
  and p_infer (in_loop:bool) (p:Proto.prog) : (Sync.t option * Unsync.t) Streamutil.stream =
    match p with
    | i :: p ->
      i_infer in_loop i
      |> map (fun j ->
        p_infer in_loop p
        |> map (function
        | (None, c2) ->
          begin match j with
          | WInst w -> (Some w, c2)
          | UInst p -> (None, Unsync.Seq (p, c2))
          | Both (p, c1) -> (Some p, Seq (c1, c2))
          end
        | (Some p, c2) ->
          begin match j with
          | WInst i -> Some (Seq (i, p)), c2
          | UInst c -> Some (Sync.map_first (Unsync.seq c) p), c2
          | Both (i, c) -> Some (Seq (i, Sync.map_first (Unsync.seq c) p)), c2
          end
        )
      ) |> concat
    | [] -> (None, Unsync.Skip) |> one
  in
  let open Streamutil in
  p_infer false p
  |> map (function
    | Some p, c -> Sync.Seq (p, Sync.Sync c)
    | None, c -> Sync.Sync c
  )

let translate (k: Proto.prog kernel) : Sync.t kernel Streamutil.stream =
  let vars = Variable.Set.union k.kernel_local_variables k.kernel_global_variables in
  let p = Proto.vars_distinct k.kernel_code vars in
  make_well_formed p
  |> Streamutil.map (fun p -> { k with kernel_code = p})

(* ---------------- Pretty printing -------------------- *)

let print_kernel (k : Sync.t kernel) : unit =
  Proto.print_kernel Sync.to_s k

let print_kernels (ks : Sync.t kernel Streamutil.stream) : unit =
  print_endline "; w-lang";
  let count = ref 0 in
  Streamutil.iter (fun k ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; w-prog " ^ (string_of_int curr));
    print_kernel k
  ) ks;
  print_endline "; end of w-lang"

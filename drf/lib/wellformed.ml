open Stage0
open Protocols

let (@) = Common.append_tr

open Exp
open Proto
open Common
open Subst
open Streamutil

type u_inst =
  | UAssert of bexp
  | UAcc of (Variable.t * Access.t)
  | UCond of bexp * u_inst list
  | ULoop of Range.t * u_inst list


type u_prog = u_inst list

type w_inst =
  | SSync of u_prog
  | SLoop of u_prog * Range.t * w_inst list * u_prog

type w_prog = w_inst list

type w_or_u_inst =
  | WInst of w_inst
  | UInst of u_inst
  | Both of w_prog * u_prog


module Make (S:SUBST) = struct
  module M = Subst.Make(S)

  let u_subst: S.t -> u_prog -> u_prog =
    let rec i_subst (s:S.t) (i:u_inst) : u_inst =
      match i with
      | UAssert b -> UAssert (M.b_subst s b)
      | UAcc (x, e) -> UAcc (x, M.a_subst s e)
      | UCond (b, p) -> UCond (
          M.b_subst s b,
          p_subst s p
        )
      | ULoop (r, p) ->
        let p = M.add s r.var (function
          | Some s -> p_subst s p
          | None -> p
        ) in
        ULoop (M.r_subst s r, p)
    and p_subst (s:S.t) : u_prog -> u_prog =
      List.map (i_subst s)
    in
    p_subst

  let w_subst: S.t -> w_prog -> w_prog =
    let rec i_subst (s:S.t) (i:w_inst) : w_inst =
      match i with
      | SSync c -> SSync (u_subst s c)
      | SLoop (c1, r, p, c2) ->
        let (p, c2) = M.add s r.var (function
          | Some s -> p_subst s p, u_subst s c2
          | None -> p, c2
        ) in
        SLoop (u_subst s c1, M.r_subst s r, p, c2)
    and p_subst (s:S.t) : w_prog -> w_prog =
      List.map (i_subst s)
    in
    p_subst

end

module S1 = Make(SubstPair)

let w_subst = S1.w_subst

let u_subst = S1.u_subst

let u_seq (u1:u_prog) (u2:u_prog) =
  (* The order of appending doesn't matter for unsync insts *)
  append_rev1 u1 u2

(* Given a regular program, return a well-formed one *)
let make_well_formed (p:Proto.prog) : w_prog Streamutil.stream =
  let inline_cond (b:bexp) (w:w_prog) : w_prog =
    let b = Constfold.b_opt b in
    let rec i_inline (w:w_inst) : w_prog =
      match w with
      | SSync c -> [SSync (UAssert b :: c)]
      | SLoop (c1, r, w, c2) -> [SLoop ((UAssert b::c1), r, p_inline w, UAssert b :: c2)]
    and p_inline (p:w_prog) =
      List.concat_map i_inline p
    in
    match b with
    | Bool true -> w
    | Bool false -> []
    | _ -> p_inline w
  in
  let w_seq (c:u_prog) (w:w_prog) =
    match w with
    | SSync c2 :: w -> SSync (u_seq c c2) :: w
    | SLoop (c2, r, w1, c3) :: w2 -> SLoop (u_seq c c2, r, w1, c3) :: w2
    | [] -> []
  in
  let w_add (c:u_inst) (w:w_prog) =
    match w with
    | SSync c2 :: w -> SSync (c :: c2) :: w
    | SLoop (c2, r, w1, c3) :: w2 -> SLoop (c::c2, r, w1, c3) :: w2
    | [] -> []
  in
  let rec i_infer (in_loop:bool) (i:Proto.inst): w_or_u_inst Streamutil.stream =
    let open Streamutil in
    match i with
    | Acc e -> UInst (UAcc e) |> one
    | Sync -> WInst (SSync []) |> one
    | Cond (b, p) ->
      p_infer in_loop p |>
      map (function
      | (Some p, c) ->
        if in_loop then
          failwith "We do not support synchronized conditionals inside loops"
        else
          [
            UInst (UAssert (b_not b));
            Both (inline_cond b p, UAssert b :: c);
          ] |> from_list
      | (None, c) -> UInst (UCond (b, c)) |> one
      )
      |> concat
    | Loop (r, p) ->
      p_infer true p |>
      map (function
      | Some p, c -> WInst (SLoop ([], r, p, c))
      | None, c -> UInst (ULoop (r, c))
      )
  and p_infer (in_loop:bool) (p:Proto.prog) : (w_prog option * u_prog) Streamutil.stream =
    match p with
    | i :: p ->
      i_infer in_loop i
      |> map (fun j ->
        p_infer in_loop p
        |> map (function
        | (None, c2) ->
          begin match j with
          | WInst w -> (Some [w], c2)
          | UInst p -> (None, p::c2)
          | Both (p, c1) -> (Some p, u_seq c1 c2) 
          end
        | (Some p, c2) ->
          begin match j with
          | WInst i -> Some (i::p), c2
          | UInst c -> Some (w_add c p), c2
          | Both (i, c) -> Some (i @ (w_seq c p)), c2
          end
        )
      ) |> concat
    | [] -> (None, []) |> one
  in
  let open Streamutil in
  p_infer false p
  |> map (function
    | Some p, c -> p @ [SSync c]
    | None, c -> [SSync c]
  )

let translate (k: Proto.prog kernel) : w_prog kernel Streamutil.stream =
  let vars = Variable.Set.union k.kernel_local_variables k.kernel_global_variables in
  let p = Proto.vars_distinct k.kernel_code vars in
  make_well_formed p
  |> Streamutil.map (fun p -> { k with kernel_code = p})

(* ---------------- Pretty printing -------------------- *)

let rec get_locs (p:u_prog) (known:Variable.Set.t) =
  match p with
  | UAssert _ :: l -> get_locs l known
  | UAcc (x,a) :: l -> get_locs l (if Access.is_write a then Variable.Set.add x known else known)
  | ULoop (_, l1)::l2
  | UCond (_, l1)::l2
    -> get_locs l1 known |> get_locs l2
  | [] -> known

let rec u_inst_to_s (i: u_inst): Indent.t list =
  let open Indent in
  match i with
  | UAssert b -> [Line ("assert " ^ Exp.b_to_string b ^ ";")]
  | UAcc (x, e) -> [Line (Access.to_string ~name:(Variable.name x) e)]
  | UCond (b, p1) -> [
      Line ("if (" ^ Exp.b_to_string b ^ ") {");
      Block (u_prog_to_s p1);
      Line "}"
    ]
  | ULoop (r, p) ->
    [
      Line ("foreach (" ^ Range.to_string r ^ ") {");
      Block (u_prog_to_s p);
      Line "}"
    ]
and u_prog_to_s (p: u_prog) : Indent.t list =
  List.map u_inst_to_s p |> List.flatten

let u_prog_to_string (p:u_prog) : string =
  u_prog_to_s p |> Indent.to_string

let w_prog_to_s: w_prog -> Indent.t list =
  let open Indent in
  let rec inst_to_s : w_inst -> Indent.t list =
    function
    | SSync e -> u_prog_to_s e @ [Line "sync;"]
    | SLoop (c1, r, p, c2) ->
      u_prog_to_s c1
      @
      [
        Line ("foreach* (" ^ Range.to_string r ^ ") {");
        Block (
          (List.map inst_to_s p |> List.flatten)
          @
          u_prog_to_s c2
        );
        Line "}"
      ]
  in
  let prog_to_s (p: w_prog) : t list =
    List.map inst_to_s p |> List.flatten
  in
  prog_to_s

let print_kernel (k : w_prog kernel) : unit =
  Proto.print_kernel w_prog_to_s k

let print_kernels (ks : w_prog kernel Streamutil.stream) : unit =
  print_endline "; w-lang";
  let count = ref 0 in
  Streamutil.iter (fun k ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; w-prog " ^ (string_of_int curr));
    print_kernel k
  ) ks;
  print_endline "; end of w-lang"

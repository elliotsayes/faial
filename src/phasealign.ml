open Exp
open Proto
open Common
open Serialize
open Subst
open Streamutil
open Hash_rt
open Ppx_compare_lib.Builtin

type u_inst =
  | UAssert of bexp
  | UAcc of acc_expr
  | UCond of bexp * u_inst list
  | ULoop of range * u_inst list
   [@@deriving hash, compare]

type u_prog = u_inst list [@@deriving hash, compare]

type w_inst =
  | SSync of u_prog
  | SLoop of u_prog * range * w_inst list * u_prog

type w_prog = w_inst list

type w_or_u_inst =
  | WInst of w_inst
  | UInst of u_inst
  | Both of w_prog * u_prog

type a_inst =
  | ASync of u_prog
  | ALoop of a_inst list * range * a_inst list

type a_prog = a_inst list


module Make (S:SUBST) = struct
  module M = Subst.Make(S)

  let u_subst: S.t -> u_prog -> u_prog =
    let rec i_subst (s:S.t) (i:u_inst) : u_inst =
      match i with
      | UAssert b -> UAssert (M.b_subst s b)
      | UAcc e -> UAcc (M.acc_expr_subst s e)
      | UCond (b, p) -> UCond (
          M.b_subst s b,
          p_subst s p
        )
      | ULoop (r, p) ->
        let p = M.add s r.range_var (function
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
        let (p, c2) = M.add s r.range_var (function
          | Some s -> p_subst s p, u_subst s c2
          | None -> p, c2
        ) in
        SLoop (u_subst s c1, M.r_subst s r, p, c2)
    and p_subst (s:S.t) : w_prog -> w_prog =
      List.map (i_subst s)
    in
    p_subst


  let a_subst: S.t -> a_prog -> a_prog =
    let rec i_subst (s:S.t) (i:a_inst) : a_inst =
      match i with
      | ASync c -> ASync (u_subst s c)
      | ALoop (p, r, q) ->
        let q = M.add s r.range_var (function
          | Some s -> p_subst s q
          | None -> q
        ) in
        ALoop (p_subst s p, M.r_subst s r, q)
    and p_subst (s:S.t) : a_prog -> a_prog =
      List.map (i_subst s)
    in
    p_subst

end

module S1 = Make(SubstPair)
let w_subst = S1.w_subst
let u_subst = S1.u_subst
let a_subst = S1.a_subst

let u_seq (u1:u_prog) (u2:u_prog) =
  (* The order of appending doesn't matter for unsync insts *)
  append_rev u1 u2



(* Given a regular program, return a well-formed one *)
let make_well_formed (p:Proto.prog) : w_prog Streamutil.stream =
  let rec inline_cond (b:bexp) (w:w_prog) : w_prog =
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
  let rec w_add (c:u_inst) (w:w_prog) =
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
          | Both (i, c) -> Some (append_tr i (w_seq c p)), c2
          end
        )
      ) |> concat
    | [] -> (None, []) |> one
  in
  let open Streamutil in
  p_infer false p
  |> map (function
    | Some p, c -> append_tr p [SSync c]
    | None, c -> [SSync c]
  )


let align (w:w_prog) : a_prog =
  let rec seq (c:u_prog) (w:a_prog) : a_prog =
    match w with
    | ASync c' :: w -> ASync (u_seq c c') :: w
    | ALoop (p, r, q)::w -> ALoop (seq c p, r, q)::w
    | [] -> failwith "UNEXPECTED!"
  in
  let rec i_align (i:w_inst) : a_inst * u_prog =
    match i with
    | SSync c -> (ASync c, [])
    | SLoop (c1, r, p, c2) ->
      let (q, c3) = p_align p in
      let q1 = seq c1 (a_subst (r.range_var, r.range_lower_bound) q) in
      let c = u_seq c3 c2 in
      let r' = Predicates.range_inc r in
      let x = r.range_var in
      let x_dec = Predicates.step_dec r.range_step (Var r.range_var) in
      (ALoop (q1, r', seq (u_subst (x, x_dec) c) q),
        u_subst (x, Predicates.range_last r) c)
  and p_align (p:w_prog) : a_prog * u_prog =
    match p with
    | [i] ->
      let (i, c) = i_align i in
      [i], c
    | i :: p ->
      let (i, c1) = i_align i in
      let (q, c2) = p_align p in
      i :: seq c1 q, c2
    | [] -> failwith "Unexpected empty synchronized code!"
  in
  let open Streamutil in
  match p_align w with
    (p, c) -> ASync c :: p

let translate2 (k: Proto.prog kernel) : a_prog kernel =
  let vars = VarSet.union k.kernel_local_variables k.kernel_global_variables in
  let p = Proto.vars_distinct k.kernel_code vars in
  let p =
    let open Streamutil in
    make_well_formed p |>
    map align |>
    to_list |>
    List.concat
  in
  { k with kernel_code = p}

let rec get_locs2 (p:u_prog) (known:VarSet.t) =
  match p with
  | UAssert _ :: l -> get_locs2 l known
  | UAcc (x,a) :: l -> get_locs2 l (if a.access_mode = Exp.W then VarSet.add x known else known)
  | ULoop (_, l1)::l2
  | UCond (_, l1)::l2
    -> get_locs2 l1 known |> get_locs2 l2
  | [] -> known

let rec u_inst_to_s (i: u_inst): PPrint.t list =
  let open PPrint in
  match i with
  | UAssert b -> [Line ("assert " ^ b_to_s b ^ ";")]
  | UAcc e -> acc_expr_to_s e
  | UCond (b, p1) -> [
      Line ("if (" ^ b_to_s b ^ ") {");
      Block (u_prog_to_s p1);
      Line "}"
    ]
  | ULoop (r, p) ->
    [
      Line ("foreach (" ^ r_to_s r ^ ") {");
      Block (u_prog_to_s p);
      Line "}"
    ]
and u_prog_to_s (p: u_prog) : PPrint.t list =
  List.map u_inst_to_s p |> List.flatten

let u_prog_to_string (p:u_prog) : string =
  u_prog_to_s p |> PPrint.doc_to_string

let w_prog_to_s: w_prog -> PPrint.t list =
  let open PPrint in
  let rec inst_to_s : w_inst -> PPrint.t list =
    function
    | SSync e -> u_prog_to_s e @ [Line "sync;"]
    | SLoop (c1, r, p, c2) ->
      u_prog_to_s c1
      @
      [
        Line ("foreach* (" ^ r_to_s r ^ ") {");
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

let a_prog_to_s: a_prog -> PPrint.t list =
  let open PPrint in
  let rec inst_to_s : a_inst -> PPrint.t list =
    function
    | ASync e -> u_prog_to_s e @ [Line "sync;"]
    | ALoop (p, r, q) ->
      (List.map inst_to_s p |> List.flatten)
      @
      [
        Line ("foreach* (" ^ r_to_s r ^ ") {");
        Block (
          List.map inst_to_s q |> List.flatten
        );
        Line "}"
      ]
  in
  let prog_to_s (p: a_prog) : t list =
    List.map inst_to_s p |> List.flatten
  in
  prog_to_s


(* ---------------------- SERIALIZATION ------------------------ *)
let print_kernel (k : a_prog kernel) : unit =
  Proto.print_kernel a_prog_to_s k

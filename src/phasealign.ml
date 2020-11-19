open Exp
open Proto
open Common
open Serialize
open Subst
open Streamutil
open Hash_rt
open Ppx_compare_lib.Builtin


type 'a inst =
  | Base of 'a
  | Assert of bexp
  | Cond of bexp * 'a inst list
  | Local of variable * 'a inst list
  [@@deriving hash, compare]

type 'a prog = 'a inst list [@@deriving hash, compare]

type p_inst = Proto.acc_expr inst [@@deriving hash, compare]

type p_prog = Proto.acc_expr prog [@@deriving hash, compare]

type u_inst =
  | UAcc of acc_expr
  | UCond of bexp * u_inst list
  | ULoop of range * u_inst list

type u_prog = u_inst list

type w_inst =
  | SSync of u_prog
  | SCond of bexp * w_inst list
  | SLoop of u_prog * range * w_inst list * u_prog

type w_prog = w_inst list

type w_or_u_inst =
  | WInst of w_inst
  | UInst of u_inst
  | Both of w_inst * u_inst

type a_inst =
  | ASync of u_prog
  | ALoop of a_inst list * range * a_inst list

type a_prog = a_inst list


module Make (S:SUBST) = struct
  module M = Subst.Make(S)

  let u_subst: S.t -> u_prog -> u_prog =
    let rec i_subst (s:S.t) (i:u_inst) : u_inst =
      match i with
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
      | SCond (b, p) -> SCond (
          M.b_subst s b,
          p_subst s p
        )
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
let make_well_formed (p:Proto.prog) : w_prog =
  let rec i_infer (i:Proto.inst): w_or_u_inst =
    match i with
    | Acc e -> UInst (UAcc e)
    | Sync -> WInst (SSync [])
    | Cond (b, p) ->
      begin match p_infer p with
      | (Some p, c) -> Both (SCond (b, p), UCond (b, c))
      | (None, c) -> UInst (UCond (b, c))
      end
    | Loop (r, p) ->
      begin match p_infer p with
      | Some p, c -> WInst (SLoop ([], r, p, c))
      | None, c -> UInst (ULoop (r, c))
      end
  and p_infer (p:Proto.prog) : w_prog option * u_prog =
    match p with
    | i :: p ->
      let j = i_infer i in
      begin match p_infer p with
      | (None, c2) ->
        begin match j with
        | WInst w -> (Some [w], c2)
        | UInst p -> (None, p::c2)
        | Both (p, c1) -> (Some [p], c1::c2)
        end
      | (Some p, c2) ->
        begin match j with
        | WInst i -> Some (i::p), c2
        | UInst c -> Some (w_add c p), c2
        | Both (i, c) -> Some (i:: w_add c p), c2
        end
      end
    | [] -> (None, [])
  and w_add (c:u_inst) (w:w_prog) =
    match w with
    | SSync c2 :: w -> SSync (c :: c2) :: w
    | SCond (b, w1) :: w2 -> SCond (b, w_add c w1) :: w_add c w2
    | SLoop (c2, r, w1, c3) :: w2 -> SLoop (c::c2, r, w1, c3) :: w2
    | [] -> []
  in
  match p_infer p with
  | Some p, c -> p @ [SSync c]
  | None, c -> [SSync c]

let rec inline_ifs (w:w_prog) : w_prog =
  let rec i_inline (b:bexp) (w:w_inst) =
    match w with
    | SSync c -> SSync [UCond (b, c)]
    | SCond (b', w) -> SCond (b_and b b', w)
    | SLoop (c1, r, w, c2) -> SLoop ([UCond (b, c1)], r, p_inline b w, [UCond (b, c2)])
  and p_inline (b:bexp) (p:w_prog) =
    List.map (i_inline b) p
  in
  match w with
  | i :: w ->
    begin match i with
    | SCond (b, w1) -> p_inline b w1 @ inline_ifs w
    | _ -> i :: inline_ifs w
    end
  | [] -> []

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
    | SCond _ -> failwith "Unexpected conditional inside fors"
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
  match p_align w with
  | (p, c) -> ASync c :: p


(* This is an internal datatype. The output of normalize is one of 4 cases: *)

let make_local (r:range) (ls:'a prog) : 'a inst =
  Local (r.range_var, [Cond (range_to_cond r, ls)])

type s_prog =
    (* A single phase (unsynch code ended by sync) *)
  | NPhase of p_prog
    (* An unrolled for-loop *)
  | NFor of range * s_prog
  | NSeq of s_prog * s_prog

type n_prog =
  | Unaligned of s_prog * p_prog
  | Open of p_prog

let rec get_locs (known:VarSet.t) (p:p_prog) =
  match p with
  | Base (x,a) :: l -> get_locs (if a.access_mode = Exp.W then VarSet.add x known else known) l
  | Assert _ :: l -> get_locs known l
  | Cond (_, l1)::l2 | Local (_, l1)::l2 -> get_locs known (l1 @ l2)
  | [] -> known

let opt_p_prog (p:p_prog) =

  let rec opt_p_prog (p:p_prog) =
    match p with
    | Assert (Bool true) :: l -> opt_p_prog l
    | Assert b :: Assert b' :: l -> opt_p_prog (Assert (b_and b b') :: l)
    | Assert b :: l -> opt_p_prog [Cond (b, l)]
    | Cond (b, []) :: l -> opt_p_prog l
    | Cond (b1, Cond (b2, l1) :: l2) :: l3 ->
      opt_p_prog (Cond (b_and b1 b2, l1) :: Cond (b1, l2) :: l3)
    | x :: l -> x :: opt_p_prog l
    | [] -> []
  in

  let rec keep_locs (known:VarSet.t) (p:p_prog) =
    match p with
    | Base (x, a) :: l ->
      let l = keep_locs known l in
      if VarSet.mem x known then (Base (x, a) ):: l else l
    | Assert b :: l -> Assert b :: (keep_locs known l)
    | Cond (b, l1) :: l2 -> Cond (b, keep_locs known l1) :: keep_locs known l2
    | Local (x, l1) :: l2 -> Local( x, keep_locs known l1) :: keep_locs known l2
    | [] -> []
  in

  let p = opt_p_prog p in
  keep_locs (get_locs VarSet.empty p) p


let rec opt_s_prog (s:s_prog) =
  match s with
  | NPhase p -> NPhase (opt_p_prog p)
  | NFor (r, s) -> NFor (r, opt_s_prog s)
  | NSeq (p, q) -> NSeq (opt_s_prog p, opt_s_prog q)

let rec opt_n_prog (n:n_prog) =
  match n with
  | Unaligned (s, p) -> Unaligned (opt_s_prog s, opt_p_prog p)
  | Open p -> Open (opt_p_prog p)

(* ---------------- SUBSTITUTION ----------------------------------- *)

let prog_subst (f:SubstPair.t -> 'a -> 'a) (s:SubstPair.t) (p:'a prog) : 'a prog =
  let rec i_subst (s:SubstPair.t) (i:'a inst) : 'a inst =
    match i with
    | Base b -> Base (f s b)
    | Assert b -> Assert (ReplacePair.b_subst s b)
    | Cond (b, p) -> Cond (
        ReplacePair.b_subst s b,
        p_subst s p
      )
    | Local (x, p) ->
      ReplacePair.add s x (function
        | Some s -> Local (x, p_subst s p)
        | None -> Local (x, p)
      )
  and p_subst (s:SubstPair.t) : 'a prog  -> 'a_prog =
    List.map (i_subst s)
  in
  p_subst s p

let p_subst : SubstPair.t -> p_prog -> p_prog =
  prog_subst ReplacePair.acc_expr_subst

let rec s_subst (s:SubstPair.t): s_prog -> s_prog =
  function
  | NPhase b -> NPhase (p_subst s b)
  | NSeq (p, q) -> NSeq (s_subst s p, s_subst s q)
  | NFor (r, q) ->
    let q = ReplacePair.add s r.range_var (function
      | Some s -> s_subst s q
      | None -> q)
    in
    NFor (ReplacePair.r_subst s r, q)

(* ------------------------------------------------------------------------- *)

let prog_to_s (f : 'a -> PPrint.t list) : 'a prog -> PPrint.t list =
  let open PPrint in
  let rec i_to_s: 'a inst -> t list =
    function
    | Base a -> f a
    | Assert b -> [Line ("assert " ^ b_to_s b ^ ";")]
    | Cond (b, p) ->
      [
        Line ("if (" ^ b_to_s b ^ ") {");
        Block (p_to_s p);
        Line "}"
      ]
    | Local (x, ls) ->
      Line ("local " ^ ident x ^ ";")
      ::
      p_to_s ls

  and p_to_s (ls: 'a prog) : t list =
    List.map i_to_s ls |> List.flatten
  in
  p_to_s

let p_prog_to_s (p:p_prog) : PPrint.t list =
  prog_to_s PPrint.acc_expr_to_s p

let rec s_prog_to_s: s_prog -> PPrint.t list =
  let open PPrint in
  function
  | NPhase p -> p_prog_to_s p @ [Line "sync;"]
  | NSeq (p, q) -> s_prog_to_s p @ s_prog_to_s q
  | NFor (r, p) ->
    [
      Line ("foreach* (" ^ r_to_s r ^ ") {");
      Block (s_prog_to_s p);
      Line "}"
    ]

let n_prog_to_s : n_prog -> PPrint.t list =
  let open PPrint in
  function
  | Open p ->
    [
      Line "unsync {";
      Block (p_prog_to_s p);
      Line "}"
    ]
  | Unaligned (p, q) ->
    [
      Line "norm {";
      Block (s_prog_to_s p);
      Line "} unsync {";
      Block (p_prog_to_s q);
      Line "}"
    ]


(* ---------------- MAKE VARIABLES DISTINCT -------------------------------- *)


(* Make variables distinct. *)

let var_uniq_prog (f:SubstPair.t -> 'a -> 'a) (known:VarSet.t) (p:'a prog) : 'a prog =
  let open Bindings in
  let rec norm_inst (i:'a inst) (xs:VarSet.t) : 'a inst * VarSet.t =
    match i with
    | Local (x, p) ->
      if VarSet.mem x xs then (
        let new_x : variable = generate_fresh_name x xs in
        let new_xs = VarSet.add new_x xs in
        let s = Subst.SubstPair.make (x, Var new_x) in
        let new_p = prog_subst f s p in
        let (p, new_xs) = norm_prog new_p new_xs in
        Local (new_x, p), new_xs
      ) else (
        let (p, new_xs) = norm_prog p (VarSet.add x xs) in
        Local (x, p), new_xs
      )
    | Cond (b, p) ->
      let (p, xs) = norm_prog p xs in
      (Cond (b, p), xs)
    | Assert _
    | Base _ -> i, xs
  and norm_prog (p:'a prog) (xs:VarSet.t) : 'a prog * VarSet.t =
    match p with
    | [] -> ([], xs)
    | i::p ->
      let (i, xs) = norm_inst i xs in
      let (p, xs) = norm_prog p xs in
      (i::p, xs)
  in
  norm_prog p known |> fst

(* ---------------- FIST STAGE OF TRANSLATION ---------------------- *)

(* Prepend an instruction to the closest phase *)

(* In the paper this appears as P;Q and represents prepending a piece of
   code that does NOT have barriers (p_prog) with the normalized code (s_prog)
   that contains the first sync. One main distinction of the paper is that
   our implementation is typesafe (see type s_prog). *)

(* Represents |> *)

(* Represents the 4 |> rules for sequence*)



let normalize (p: Proto.prog) : n_prog stream =
  let rec inline_if (b:bexp) (p: s_prog) : s_prog =
    match p with
    | NPhase u -> NPhase ([Cond (b, u)])
    | NFor (r, p) -> NFor (r, inline_if b p)
    | NSeq (p, q) -> NSeq (inline_if b p, inline_if b q)
  in
  let rec n_cond (b:bexp) (p: s_prog) : s_prog =
    match p with
    | NPhase u -> NPhase (Assert b :: u)
    | NFor (r, p) -> NFor (r, n_cond b p)
    | NSeq (p, q) -> NSeq (n_cond b p, n_cond b q)
  in
  let rec prepend (u: p_prog) (p : s_prog) : s_prog =
    match p with
    | NPhase u' -> NPhase (u @ u')
    | NFor (r, p) -> failwith "CANNOT HAPPEN!"
    | NSeq (p, q) -> NSeq (prepend u p, q)
  in
  let seq (p:n_prog) (q:n_prog) : n_prog  =
    let seq1 (p:p_prog) (q:n_prog) : n_prog =
      match q with
      | Open q -> Open (p @ q)
      | Unaligned (q, u') -> Unaligned (prepend p q, u')
    in
    let seq2 (p:s_prog) (q:n_prog) : n_prog =
      match q with
      | Open q -> Unaligned (p, q)
      | Unaligned (q1, q2) -> Unaligned (NSeq (p, q1), q2)
    in
    match p, q with
    | Open [], n
    | n, Open [] -> n
    | Open p, _ -> seq1 p q
    | Unaligned (p1, p2), _ -> seq2 p1 (seq1 p2 q)
  in

  let rec norm_i (i:Proto.inst) : n_prog stream =
    let open Streamutil in
    match i with
    | Sync ->
      Unaligned (NPhase [], []) |> one
    | Acc e ->
      Open [Base e] |> one
    | Cond (b, p) ->
      norm_p p |>
      flat_map (function
      | Open p -> Open [Cond (b, p)] |> one
      | Unaligned (p,q) ->
        [
          Unaligned (inline_if b p, [Cond(b, q)]);
          Open [Assert (b_not b)]
        ] |> from_list
      )

    | Loop ({range_var=x;range_lower_bound=lb;range_upper_bound=ub; range_step=s} as r, p) ->
      begin match norm_p p |> Streamutil.to_list with
      | [Open p] -> Open [make_local r p]
      | [Unaligned (p1, p2)] ->
        let new_ub = Predicates.range_last r in
        let p1' = n_cond (n_lt lb ub) (s_subst (x, lb) p1) in
        let p2' = (*Assert (n_lt lb ub) ::*)
                  p_subst (x, new_ub) p2 in
        let new_p1 = s_subst (x, Predicates.step_inc r.range_step (Var r.range_var)) p1 in
        let new_r = { r with range_upper_bound = new_ub } in
        (* Rule:
          aligned(p) = (q,c3)      c = c3;c2
          -------------------------------------------------------
          align(c1;for x [n,m) {P,c2}) =
            (c1 . q[x:=n]);
            for x in [n+1, m) {
                   c[x:=x - 1] . q
                 }
            ,
            c[x:= m - 1]
          *)
        Unaligned (NSeq (p1', NFor (new_r, prepend p2 new_p1)), p2')
      | l -> failwith "Conditionals cannot appear inside for-loops"
      end
      |> one
  and norm_p (p:Proto.prog) : n_prog stream =
    match p with
    | [] -> Open [] |> one
    | i :: p ->
      product (norm_i i) (norm_p p)
      |> map (fun (i, p) -> seq i p)
      |> map opt_n_prog
  in
  norm_p p

let translate (k: Proto.prog kernel) : n_prog kernel stream =
  normalize k.kernel_code
  |> Streamutil.map (fun p ->
    { k with kernel_code = p }
  )

(* ---------------------- SERIALIZATION ------------------------ *)

let print_kernels (ks : n_prog kernel Streamutil.stream) : unit =
  print_endline "# begin align";
  let count = ref 0 in
  Streamutil.iter (fun (k:n_prog kernel) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("\n## version " ^ (string_of_int curr));
    Serialize.PPrint.print_kernel n_prog_to_s k
    (*Serialize.kernel_ser n_prog_ser k |> s_print*)
  ) ks;
  print_endline "\n# end align"

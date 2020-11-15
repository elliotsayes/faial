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


(* This is an internal datatype. The output of normalize is one of 4 cases: *)
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

(* -------------------- UTILITY CONSTRUCTORS ---------------------- *)

let range_to_cond (r:range) : bexp =
  (match r.range_step with
  | Default (Num 1) -> []
  | Default n -> [
      (* x % step == 0 *)
      n_eq (n_mod (Var r.range_var) n) (Num 0);
      (* Ensure that the step is positive *)
      n_gt n (Num 0)
    ]
  | StepName name -> [Pred(name, Var r.range_var)]
  )
  @
  [
    n_le r.range_lower_bound (Var r.range_var);
    n_lt (Var r.range_var) r.range_upper_bound;
  ]
  |> b_and_ex

let range_has_next (r:range) : bexp =
  n_lt r.range_lower_bound r.range_upper_bound

let range_next_var (r:range) : nexp =
  let x = Var r.range_var in
  match r.range_step with
  | Default n -> n_plus x n
  | StepName "pow2" -> n_mult x (Num 2)
  | StepName "pow3" -> n_mult x (Num 3)
  | StepName x -> failwith ("I don't know how to unroll a loop with a step " ^ x)

let range_prev_upper_bound (r:range) : nexp =
  let x = r.range_upper_bound in
  match r.range_step with
  | Default n -> n_minus x n
  | StepName "pow2" -> n_div x (Num 2)
  | StepName "pow3" -> n_div x (Num 3)
  | StepName x -> failwith ("I don't know how to unroll a loop with a step " ^ x)


let range_is_empty (r:range) : bexp =
  n_ge r.range_lower_bound r.range_upper_bound

let make_local (r:range) (ls:'a prog) : 'a inst =
  Local (r.range_var, [Cond (range_to_cond r, ls)])

let range_first (r:range) : bexp =
  n_eq (Var r.range_var) r.range_lower_bound

let range_advance (r:range) : range =
  {r with range_lower_bound = n_plus r.range_lower_bound (Num 1) }

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
        let new_ub = range_prev_upper_bound r in
        let p1' = n_cond (n_lt lb ub) (s_subst (x, lb) p1) in
        let p2' = Assert (n_lt lb ub) ::
                  p_subst (x, new_ub) p2 in
        let new_p1 = s_subst (x, range_next_var r) p1 in
        let new_r = { r with range_upper_bound = new_ub } in
        (* Rule:
                                P1' = P1 {n/x}
          P |> P1, P2           P2' = P2{m-1/x}
          -------------------------------------------------------
          for x [n,m) {P} |>
            assert (n < m) {P1'};
            for [n,m-1) {P2;P1 {x+1/x}},
            assert (n<m); P2'
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

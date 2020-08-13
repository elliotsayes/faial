open Exp
open Proto
open Common
open Serialize
open Subst
open Streamutil

type 'a inst =
  | Base of 'a
  | Assert of bexp
  | Cond of bexp * 'a inst list
  | Local of variable * 'a inst list

type 'a prog = 'a inst list

type p_inst = Proto.acc_inst inst
type p_prog = Proto.acc_inst prog

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
  | Aligned of s_prog

(* -------------------- UTILITY CONSTRUCTORS ---------------------- *)

let range_to_cond (r:range) =
  b_and
    (n_le r.range_lower_bound (Var r.range_var))
    (n_lt (Var r.range_var) r.range_upper_bound)

let range_has_next (r:range) : bexp =
  n_lt r.range_lower_bound r.range_upper_bound

let range_is_empty (r:range) : bexp =
  n_ge r.range_lower_bound r.range_upper_bound

let make_local (r:range) (ls:'a prog) : 'a inst =
  Local (r.range_var, [Cond (range_to_cond r, ls)])

let range_first (r:range) : SubstPair.t =
  (r.range_var, r.range_lower_bound)

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
  prog_subst ReplacePair.acc_inst_subst

let rec s_subst (s:SubstPair.t): s_prog -> s_prog =
  function
  | NPhase b -> NPhase (p_subst s b)
  | NSeq (p, q) -> NSeq (s_subst s p, s_subst s q)
  (*
  | NCond (b, p) -> NCond (ReplacePair.b_subst s b, s_subst s p)
  *)
  | NFor (r, q) ->
    let q = ReplacePair.add s r.range_var (function
      | Some s -> s_subst s q
      | None -> q)
    in
    NFor (ReplacePair.r_subst s r, q)

(* ------------------------------------------------------------------------- *)

let rec inst_ser (f : 'a -> Sexplib.Sexp.t) : 'a inst -> Sexplib.Sexp.t =
  function
  | Base a -> f a
  | Assert b -> unop "assert" (b_ser b)
  | Cond (b, ls) ->
    prog_ser f ls
    |> s_list
    |> binop "if" (b_ser b)
  | Local (x, ls) ->
    let open Sexplib in
    prog_ser f ls
    |> s_list
    |> binop "local" (Sexp.Atom x.var_name)

and prog_ser (f : 'a -> Sexplib.Sexp.t) (ls: 'a prog) : Sexplib.Sexp.t list =
  List.map (inst_ser f) ls

let p_prog_ser (p:p_prog) : Sexplib.Sexp.t =
  prog_ser acc_inst_ser p |> s_list

let rec s_prog_ser: s_prog -> Sexplib.Sexp.t list =
  function
  | NPhase p -> [unop "block" (p_prog_ser p)]
  | NSeq (p, q) -> s_prog_ser p @ s_prog_ser q
  (*
  | NCond (b, p) ->
    let open Serialize in
    [binop "ncond" (b_ser b) (s_prog_ser p |> s_list)]
  *)
  | NFor (r, p) ->
    let open Serialize in
    [binop "nfor" (r_ser r) (s_prog_ser p |> s_list)]

let n_prog_ser : n_prog -> Sexplib.Sexp.t =
  function
  | Open p -> unop "unsync" (p_prog_ser p)
  | Aligned p -> unop "aligned" (s_prog_ser p |> s_list)
  | Unaligned (p, q) -> binop "unaligned" (s_prog_ser p |> s_list) (p_prog_ser q)

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



let normalize (p: Proto.prog) : n_prog Stream.t =
  let rec n_cond (b:bexp) (p: s_prog) : s_prog =
    match p with
    | NPhase u -> NPhase (Assert b :: u)
    | NFor (r, p) -> NFor (r, n_cond b p)
    | NSeq (p, q) -> NSeq (n_cond b p, n_cond b q)
  in
  let rec prepend (u: p_prog) (p : s_prog) : n_prog Stream.t =
    match p with
      | NPhase u' -> Aligned (NPhase (u @ u')) |> one
        (* foreach (x in 0 .. n) {
             foo
             sync
           }
           *)
      | NFor (r, p) ->
        (* Unroll the loop *)
        let p' = n_cond (range_has_next r) (s_subst (range_first r) p) in
        prepend u p'
        |> flat_map (fun n -> seq n (Aligned (NFor (range_advance r, p))))
        |> sequence (Open (Assert (range_is_empty r) :: u) |> one)
      | NSeq (p, q) ->
        prepend u p
        |> flat_map (fun (n:n_prog) -> seq n (Aligned q))
  and seq (p:n_prog) (q:n_prog) : n_prog Stream.t  =
    match p, q with
    | Open [], _ -> q |> one
    | _, Open [] -> p |> one
    (* Rule: ^P |> _|_, P *)
    | Open p, Open q -> Open (p@q) |> one
    (* Rule:
    ^P    Q |> Q1, Q2
    -----------------
    P;Q |> (P;Q1), Q2
    *)
    | Open p, Unaligned (q1, q2) ->
      prepend p q1
      |> flat_map (fun n -> seq n (Open q2))
    | Open p, Aligned q ->
    (*
      print_endline "open+aligned";
      *)
      prepend p q
    (* Rule:
      P |> P1, P2      ^Q
      -------------------
      P;Q |> P1, (P2,Q)
      *)
    | Unaligned (p1,p2), Open q2 -> Unaligned (p1, p2@q2) |> one
    | Unaligned (p1,p2), Unaligned (q1, q2) ->
      (* Rule:
        P |> P1, P2     Q |> Q1, Q2
        ---------------------------
        P;Q |> P1;(P2;Q1), Q2
        *)
      prepend p2 q1
      |> flat_map (fun (p2_q1:n_prog) ->
        seq (Aligned p1) p2_q1 (* P1 ; (P2;Q1) *)
      )
      |> flat_map (fun (p1_p2_q1:n_prog) ->
        seq p1_p2_q1 (Open q2) (* P1;(P2;Q1), Q2 *)
      )

    | Unaligned (p1, p2), Aligned q ->
      prepend p2 q
      |> flat_map (fun p2_q -> seq (Aligned p1) p2_q)
    | Aligned p, Open q -> Unaligned (p, q) |> one
    | Aligned p, Unaligned (q1, q2) -> Unaligned (NSeq (p, q1), q2) |> one
    | Aligned p, Aligned q -> Aligned (NSeq (p, q)) |> one
  in

  let rec norm_i (i:Proto.inst) : n_prog Stream.t =
    let open Streamutil in
    match i with
    | Base Sync ->
(*      print_endline "sync";*)
      Aligned (NPhase []) |> one
    | Base (Unsync u) ->
(*      print_endline "rw"; *)
      Open [Base u] |> one
    | Cond (b, p) ->
      norm_p p |>
      flat_map (function
      | Open p -> Open [Cond (b, p)] |> one
      | Aligned p ->
        [
          Aligned (n_cond b p);
          Open [Assert (b_not b)]
        ] |> Stream.of_list
      | Unaligned (p,q) ->
        [
          Unaligned (n_cond b p, Assert b :: q);
          Open [Assert (b_not b)]
        ] |> Stream.of_list
      )

    | Loop ({range_var=x;range_lower_bound=lb;range_upper_bound=ub} as r, p) ->
      norm_p p
      |> flat_map (function
      | Open p -> Open [make_local r p] |> one
      | Aligned p -> Aligned (NFor (r, p)) |> one
      | Unaligned (p1, p2) ->
        let new_ub = n_minus ub (Num 1) in
        let p1' = n_cond (n_lt lb ub) (s_subst (x, lb) p1) in
        let p2' = Assert (n_lt lb ub) :: p_subst (x, new_ub) p2 in
        let new_p1 = s_subst (x, n_plus (Var x) (Num 1)) p1 in
        let new_r = { r with range_upper_bound = new_ub } in
        prepend p2 new_p1 |>
        map (function
          | Open p2_new_p1 -> Unaligned (p1', make_local new_r p2_new_p1 :: p2')
          | Aligned p2_new_p1 -> Unaligned (NSeq (p1', NFor (new_r, p2_new_p1)), p2')
          | Unaligned (p2_new_p1, new_p2') -> failwith "This should never happen! A prepend with aligned on the right-hand side never returns unaligned"
            (* Rule:
                                    P1' = P1 {n/x}
              P |> P1, P2           P2' = P2{m-1/x}
              -------------------------------------------------------
              for x [n,m) {P} |>
                if (n < m) {P1'};sync;
                for [n,m-1) {P2;P1 {x+1/x}},
                assert (n<m); P2'
             *)
        )
        |> sequence (
            (* Rule:
              P |> P1, P2
              ---------------------------------------
              for x [n,m) {P} |> _|_, assert (n >= m)
             *)
          Open [Assert (n_ge lb ub)] |> one
        )
      )

  and norm_p (p:Proto.prog) : n_prog Stream.t =
    match p with
    | [] -> Open [] |> one
    | i :: p ->
      product (norm_i i) (norm_p p)
      |> flat_map (fun (i, p) -> seq i p)
  in
  norm_p p

let translate (k: Proto.prog kernel) : n_prog kernel Stream.t =
  normalize k.kernel_code
  |> Streamutil.map (fun p ->
    { k with kernel_code = p }
  )

(* ---------------------- SERIALIZATION ------------------------ *)

let print_kernels (ks : n_prog kernel Stream.t) : unit =
  print_endline "; begin align";
  let count = ref 0 in
  Stream.iter (fun (k:n_prog kernel) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; version " ^ (string_of_int curr));
    Serialize.kernel_ser n_prog_ser k |> s_print
  ) ks;
  print_endline "; end align"

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
    (* A conditional phase *)
  | NCond of bexp * s_prog
    (* A sequence of two normalized programs *)
  | NSeq of s_prog * s_prog
    (* An unrolled for-loop *)
  | NFor of range * s_prog


(* -------------------- UTILITY CONSTRUCTORS ---------------------- *)

let range_to_cond (r:range) =
  b_and
    (n_le r.range_lower_bound (Var r.range_var))
    (n_lt (Var r.range_var) r.range_upper_bound)

let make_local (r:range) (ls:'a prog) : 'a inst =
  Local (r.range_var, [Cond (range_to_cond r, ls)])

(* ---------------- SUBSTITUTION ----------------------------------- *)

let prog_subst (f:SubstPair.t -> 'a -> 'a) (s:SubstPair.t) (p:'a prog) : 'a prog =
  let rec i_subst (s:SubstPair.t) (i:'a inst) : 'a inst =
    match i with
    | Base b -> Base (f s b)
    | Assert b -> Assert (ReplacePair.b_subst s b)
    | Cond (b, p1) -> Cond (
        ReplacePair.b_subst s b,
        p_subst s p1
      )
    | Local (x, p) ->
      ReplacePair.add s x (function
        | Some s -> Local (x, p_subst s p)
        | None -> i
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
  | NCond (b, p) -> NCond (ReplacePair.b_subst s b, s_subst s p)
  | NFor (r, q) ->
    let q = ReplacePair.add s r.range_var (function
      | Some s -> s_subst s q
      | None -> q)
    in
    NFor (ReplacePair.r_subst s r, q)

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

let rec prepend (u:p_prog) : s_prog -> s_prog =
  function
    | NPhase u' -> NPhase (u @ u')
    | NCond (b, p) -> NCond (b, prepend u p)
    | NSeq (p, q) -> NSeq (prepend u p, q)
    | NFor (r, p) -> NFor (r, prepend u p)

(* Represents |> *)

(* Represents the 4 |> rules for sequence*)

let seq (p:s_prog option * p_prog) (q:s_prog option * p_prog) : (s_prog option * p_prog)  =
  match p, q with
  (* Rule: ^P |> _|_, P *)
  | (None,p2),(None,q2) -> (None, (p2@q2))
  (* Rule:
   ^P    Q |> Q1, Q2
   -----------------
   P;Q |> (P;Q1), Q2
   *)
  | (None,p2),(Some q1, q2) -> (Some (prepend p2 q1),q2)
  (* Rule:
    P |> P1, P2      ^Q
    -------------------
    P;Q |> P1, (P2,Q)
    *)
  | (Some p1,p2),(None, q2) -> (Some p1,(p2@q2))
  | (Some p1,p2),(Some q1, q2) ->
    (* Rule:
      P |> P1, P2     Q |> Q1, Q2
      ---------------------------
      P;Q |> P1;(P2;Q1), Q2
      *)
    Some (NSeq (p1, prepend p2 q1)), q2

let normalize_unsync (i:Proto.inst) : p_inst option =
  let rec norm_i: Proto.inst -> p_inst = function
    | Base Sync -> raise Exit
    | Base (Unsync u) -> Base u
    | Cond (b, p) -> Cond (b, norm_p p)
    | Loop (r, p) -> make_local r (norm_p p)
  and norm_p (p: Proto.prog): p_prog =
    List.map norm_i p
  in
  try Some (norm_i i) with
  | Exit -> None

(* Typesafe normalization *)
let rec normalize1 (i: Proto.inst) : (s_prog option * p_prog) Stream.t =
  match normalize_unsync i with
  (* Rule: ^ P |> _|_, P *)
  | Some i -> (None, [i]) |> Streamutil.one
  | None ->
  (* Otherwise *)
  match i with
    (* Rule: sync |> skip, skip *)
  | Base Sync -> (Some (NPhase []),[]) |> Streamutil.one
    (* Rule: ^P |> _|_, P *)
  | Base (Unsync u) -> (None, [Base u]) |> Streamutil.one
  | Cond (b, p) ->
    normalize p |>
    (* For each possible element *)
    Streamutil.map (fun (o, p2) ->
      match o with
      (* Rule:
        P |> _|_, P
        -----------
        if (b) P |> _|_, assert (b) P
        *)
      | None -> Streamutil.one (None, Assert b::p2)
      | Some p1 ->
        [
          (* Rule:
            P |> P1,P2
            ----------
            if (b) P |> (if (b) {P1}, assert b;P2)

          *)
          Some (NCond (b, p1)) , Assert b::p2;
          (* Rule:
            P |> P1,P2
            ----------
            if (b) P |> _|_, assert !b

          *)
          None, [Assert (b_not b)]
        ]
        |> Stream.of_list
    ) |> Streamutil.concat
  | Loop ({range_var=x;range_lower_bound=lb;range_upper_bound=ub} as r, body) ->
      normalize body |>
      Streamutil.map (function
        | (Some p1, p2) ->
          let dec_ub = Bin (Minus, ub, Num 1) in
          let p1' = NCond (n_lt lb ub, s_subst (x, lb) p1) in
          let p2' = Assert (n_lt lb ub):: p_subst (x, dec_ub) p2 in
          let inc_var = Bin (Plus,Var x,Num 1) in
          let subbed_p1 = s_subst (x, inc_var) p1 in
          let r' = { r with range_upper_bound = dec_ub } in
          [
            (* Rule:
                                    P1' = P1 {n/x}
              P |> P1, P2           P2' = P2{m-1/x}
              -------------------------------------------------------
              for x [n,m) {P} |>
                if (n < m) {P1'};sync;
                for [n,m-1) {P2;P1 {x+1/x}},
                assert (n<m); P2'
             *)
            Some (NSeq (p1', NFor (r', prepend p2 subbed_p1))), p2';
            (* Rule:
              P |> P1, P2
              ---------------------------------------
              for x [n,m) {P} |> _|_, assert (n >= m)
             *)
            None, [Assert (n_ge lb ub)]
          ] |> Stream.of_list
          (* Rule: ^P |> _|_, P *)
        | (None, u) -> (None, [make_local r u]) |> Streamutil.one
      ) |> Streamutil.concat
(* Typesafe normalization of programs *)
and normalize: Proto.prog -> (s_prog option * p_prog) Stream.t =
  function
  | [] -> (None, []) |> Streamutil.one
  | x::xs ->
    Streamutil.product (normalize1 x) (normalize xs)
    |> Streamutil.map (fun (x,y) -> seq x y)

(* Implements norms(P):

   norms(P) = {Q1;Q2 | P |> Q1,Q2} U { Q | P |> _|_, Q}
 *)
let make_phases ((o,q):s_prog option * p_prog) : s_prog =
  match o with
  | None -> NPhase q
  | Some p -> NSeq (p, NPhase q)

(* Takes a program with Syncs and generates a program with phased blocks *)
let prog_to_s_prog (s:Proto.prog) : s_prog Stream.t =
  (* P |> Q1, Q2 *)
  normalize s
  (* norms(P) *)
  |> Streamutil.map (fun p -> make_phases p)

let translate (k: Proto.prog kernel) : s_prog kernel Stream.t =
  prog_to_s_prog k.kernel_code
  |> Streamutil.map (fun p ->
    { k with kernel_code = p }
  )

(* ---------------------- SERIALIZATION ------------------------ *)

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
  | NCond (b, p) ->
    let open Serialize in
    [binop "ncond" (b_ser b) (s_prog_ser p |> s_list)]
  | NFor (r, p) ->
    let open Serialize in
    [binop "nfor" (r_ser r) (s_prog_ser p |> s_list)]

let print_kernels (ks : s_prog kernel Stream.t) : unit =
  print_endline "; begin align";
  let count = ref 0 in
  Stream.iter (fun (k:s_prog kernel) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; version " ^ (string_of_int curr));
    Serialize.kernel_ser (fun x -> s_prog_ser x |> s_list) k |> s_print
  ) ks;
  print_endline "; end align"

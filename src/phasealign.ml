open Exp
open Proto
open Common
open Phaseord
open Serialize
open Subst
open Streamutil

(* ---------------- FIST STAGE OF TRANSLATION ---------------------- *)

(* Represents norms(P) *)

let rec prepend (u:u_prog) : s_prog -> Proto.s_prog =
  function
    | Base u' :: l -> Base (u @ u') :: l
    | Cond (b, p) :: l ->
      Cond (b, prepend u p) ::
      prepend u l
    | Loop (_, _) :: _ as l -> l
    | [] -> []

let seq (p:s_prog option * u_prog) (q:s_prog option * u_prog) : s_prog option * u_prog =
  match p, q with
  | (None,p2),(None,q2) -> (None, (p2@q2))
  | (None,p2),(Some q1, q2) -> (Some (prepend p2 q1),q2)
  | (Some p1,p2),(None, q2) -> (Some p1,(p2@q2))
  | (Some p1,p2),(Some q1, q2) -> (Some (p1@(prepend p2 q1)), q2)

(* Typesafe normalization *)
let rec normalize1 : inst -> s_prog option * u_prog =
  function
  | Base Sync -> (Some [Base []],[])
  | Base (Unsync u) -> (None, [Base u])
  | Cond (b, p) ->
    begin
      match normalize p with
      (* There is no synchronized part, so we only return 1 conditional *)
      | (None, p) -> (None, [Cond (b, p)])
      (* Duplicate the condition, one synchronized and the other unsync *)
      | (Some p, p') -> (Some [Cond (b, p)], [Cond (b, p')])
    end
  | Loop ({range_var=x;range_lower_bound=lb;range_upper_bound=ub} as r, body) ->
      begin match normalize body with
        | (Some p1, p2) ->
          let dec_ub = Bin (Minus, ub, Num 1) in
          let p1' = Cond (
            NRel (NLt, lb, ub),
            ReplacePair.s_subst (x, lb) p1
          ) in
          let p2' = Cond (
            NRel (NLt, lb, ub),
            ReplacePair.u_subst (x, dec_ub) p2
          ) in
          let inc_var = Bin (Plus,Var x,Num 1) in
          let subbed_p1 = ReplacePair.s_subst (x, inc_var) p1 in
          let r' = { r with range_upper_bound = dec_ub } in
          (Some [p1';Loop (r',prepend p2 subbed_p1)] , [p2'])
        | (None, u) -> (None, [Loop (r, u)])
      end
(* Typesafe normalization of programs *)
and normalize: Proto.prog -> s_prog option * u_prog = function
  | [] -> (None, [])
  | x::xs -> seq (normalize1 x) (normalize xs)

(* Takes a program with Syncs and generates a program with phased blocks *)
let prog_to_s_prog (s:Proto.prog) : s_prog =
  match normalize s with
  | None, x -> [Base x]
  | (Some b, after) -> b @ [Base after]


let translate (k: Proto.prog kernel) : s_prog kernel =
  { k with kernel_code = prog_to_s_prog k.kernel_code }

let print_kernel (k:s_prog kernel) =
  let open Serialize in
  print_endline "; phased";
  kernel_ser s_prog_ser k |> s_print


(*
  let rec run (s:t) =
    let rec run_aux (s:t) (accum,phase) =
      match s with
      | (Loop (var,r,t1))::l ->
          let i = eval_expr E.to_int r.r_lowerbound in
          let j = eval_expr E.to_int r.r_upperbound in
          let subbed_t1 = a_subst var (E.to_expr i) t1 in
          let t1' = a_subst var (Incr (E.to_expr i)) t1 in
          if i < j then
            (
              let r' = createRange (Incr (E.to_expr i)) r.r_upperbound in
              run_aux (subbed_t1@(Loop (var,r',t1'))::l) (accum,phase)
            )
          else run_aux l (accum,phase)
      | (Codeline (n,c))::l ->
          let n' = eval_expr E.to_int n in
          run_aux l (accum,(E.to_expr n')::phase)
      | Sync::l -> run_aux l (phase::accum,[])
      | [] -> (accum,phase)
    in
    let (accum,phase) = run_aux s ([],[]) in
    let ret = phase::accum in
    let ret = List.rev ret in
    List.map List.rev ret
*)
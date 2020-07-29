open Proto
open Common
open Phaseord
open Serialize
open Subst

(* ---------------- FIST STAGE OF TRANSLATION ---------------------- *)

(* Represents norms(P) *)

let rec prepend (u:u_prog) : s_prog -> Proto.s_prog =
  function
    | Base u' :: l -> Base (u @ u') :: l
    | Cond (b, p) :: l -> Cond (b, prepend u p) :: l
    | Loop (_, _) :: _ -> failwith "Unexpected input: expecting a normalized program"
    | [] -> failwith "Unexpected input: expecting a normalized program"

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
  | Loop ({range_var=x;range_lower_bound=lb;range_upper_bound=ub} as r,body) ->
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
        | (None, u) -> (None, u)
      end
(* Typesafe normalization of programs *)
and normalize: Proto.prog -> s_prog option * u_prog = function
  | [] -> (None,[])
  | x::xs -> seq (normalize1 x) (normalize xs)

(* Takes a program with Syncs and generates a program with phased blocks *)
let prog_to_s_prog (s:Proto.prog) : s_prog =
  match normalize s with
  | None, x -> [Base x]
  | (Some b, after) -> b@ [Base after]


(* ---------------- SECOND STAGE OF TRANSLATION ---------------------- *)

(* A synchronized-program has multiple goals to prove, so want to flatten
   each phased block into a list. Phased blocks may be nested inside a
   conditional or a variable declaration (loop), and in which case,
   we must ensure we preserve that structure. *)

let rec s_inst_to_phase_list : 'a base_inst -> ('a phase) list =
  function
  | Base p -> [Phase p]
  | Loop (r, l) ->
    List.map s_inst_to_phase_list l
    |> List.flatten
    |> List.map (fun p ->
      Global (r, p)
    )
  | Cond (b, l) ->
    List.map s_inst_to_phase_list l
    |> List.flatten
    |> List.map (fun p ->
      Pre (b, p)
    )

let s_prog_to_phase_list (l: ('a base_inst) list) : ('a phase) list  =
  List.map s_inst_to_phase_list l |> List.flatten

(* ---------------- THIRD STAGE OF TRANSLATION ---------------------- *)

let project_prog (t:task) : u_prog -> y_prog =
  base_prog_map
    (function
    | Goal b -> Goal b
    | Assert b -> Assert b
    | Acc (x, e) -> Acc (x, e, t)
    )

let project_phase (t:task) : u_prog phase -> y_prog phase =
  phase_map (project_prog t)

let project_phase_list : (u_prog phase) list -> (y_prog phase * y_prog phase) list =
  List.map (fun p -> (project_phase Task1 p, project_phase Task2 p))

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
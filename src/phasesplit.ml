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


let kernel_to_s_kernel (k: Proto.prog kernel) : s_prog kernel =
  { k with kernel_code = prog_to_s_prog k.kernel_code }

(* ---------------- SECOND STAGE OF TRANSLATION ---------------------- *)

(* A synchronized-program has multiple goals to prove, so want to flatten
   each phased block into a list. Phased blocks may be nested inside a
   conditional or a variable declaration (loop), and in which case,
   we must ensure we preserve that structure. *)

let rec inst_to_phase_stream : 'a base_inst -> ('a phase) Stream.t =
  function
  | Base p -> Stream.of_list [Phase p]
  | Loop (r, l) ->
    prog_to_phase_stream l
    |> stream_map (fun p ->
      Global (r, p)
    )
  | Cond (b, l) ->
    prog_to_phase_stream l
    |> stream_map (fun p ->
      Pre (b, p)
    )

and prog_to_phase_stream (l: ('a base_inst) list) : ('a phase) Stream.t =
  List.fold_left
    (fun s i -> inst_to_phase_stream i |> stream_seq s)
    (stream_make None)
    l

let s_kernel_to_p_kernel (k : s_prog kernel) : (u_prog phase kernel) Stream.t  =
  Streamutil.stream_map (fun p ->
      { k with kernel_code = p }
    )
    (prog_to_phase_stream k.kernel_code)

(* ------------------------ THIRD STAGE OF TRANSLATION ---------------------- *)

let rec filter_loc_inst (x:variable) (i:u_inst) : l_inst option =
  match i with
  | Base (Goal b) -> None
  | Base (Acc (y, e)) ->
    begin
      if var_equal x y then
        Some (Base e)
      else
        None
    end
  | Cond (b, l) ->
    begin
      let l = filter_loc_prog x l in
      match l with
      | Some l -> Some (Cond (b, l))
      | None -> None
    end
  | Loop (r, l) ->
    begin
      let l = filter_loc_prog x l in
      match l with
      | Some l -> Some (Loop (r, l))
      | None -> None
    end
and filter_loc_prog (x:variable) (l:u_prog) : l_prog option =
  let l = List.map (filter_loc_inst x) l |> flatten_opt in
  match l with
  | [] -> None
  | _ -> Some l

let rec filter_loc_phase (x:variable) (p:u_prog phase) : l_prog phase option =
  match p with
  | Phase l ->
    begin match filter_loc_prog x l with
    | Some l -> Some (Phase l)
    | None -> None
    end
  | Pre (b, p) ->
    begin match filter_loc_phase x p with
    | Some p -> Some (Pre (b, p))
    | None -> None
    end
  | Global (r, p) ->
    begin match filter_loc_phase x p with
    | Some p -> Some (Global (r, p))
    | None -> None
    end

let p_kernel_to_l_kernel_list (k:u_prog phase kernel) : l_kernel list =
  VarSet.elements k.kernel_locations
  |> Common.map_opt (fun x ->
    match filter_loc_phase x k.kernel_code with
    | Some p -> Some {
        l_kernel_location = x;
        l_kernel_global_variables = k.kernel_global_variables;
        l_kernel_local_variables = k.kernel_local_variables;
        l_kernel_code = p;
      }
    | None -> None
  )


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
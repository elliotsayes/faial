open Proto
open Common
open Phaseord
open Serialize
open Subst

(* ------------------
  LANGUAGE P
  A->P->C->L->H
------------------  *)
module PLang = struct

  type unsync_instruction =
  | Loop of range * (unsync_instruction list)
  | Access of variable * access

  type instruction =
  | Loop of range * (instruction list)
  | Phased of (unsync_instruction list)

  type t = instruction list

  let rec ser_u_1 p =
    let open Sexplib in
    match p with
    | Access (x,a) -> call "loc" [Sexp.Atom x.var_name; a_ser a]
    | Loop (r, l) -> binop "loop" (r_ser r) (ser_u l)

  and ser_u l =
    let open Sexplib in
    Sexp.List (List.map ser_u_1 l)

  let rec ser_i_1 p =
    let open Sexplib in
    match p with
    | Phased l -> unop "phased" (ser_u l)
    | Loop (r, l) -> binop "loop" (r_ser r) (ser_i l)
  and ser_i l =
    let open Sexplib in
    Sexp.List (List.map ser_i_1 l)

  let print_lang k =
    ser_i k
      |> Sexplib.Sexp.to_string_hum
      |> print_endline

  let rec p_subst (kv:variable*nexp) (s:instruction list) : instruction list =
    let rec p_subst_unsync (kv:variable*nexp) (s:unsync_instruction list) : unsync_instruction list =
      match s with
      | (Loop (r, b))::ss ->
        (Loop (ReplacePair.r_subst kv r, p_subst_unsync kv b))::
        (p_subst_unsync kv ss)
      | (Access (x, a))::ss ->
        (Access (x, ReplacePair.a_subst kv a))::(p_subst_unsync kv ss)
      | [] -> []
    in
    match s with
    | (Loop (r, b))::ss ->
      (Loop (ReplacePair.r_subst kv r, p_subst kv b))::(p_subst kv ss)
    | (Phased u)::ss ->
      (Phased (p_subst_unsync kv u))::(p_subst kv ss)
    | [] -> []
(*
  let rec run (s:t) =
    let rec run_aux (s:t) accum =
      match s with
      | (Phased n)::l -> run_aux l ((extract_unsync n [])::accum)
      | (Loop (r,t1))::l ->
        let i = eval_expr E.to_int r.r_lowerbound in
        let j = eval_expr E.to_int r.r_upperbound in
        let r' = createRange (Incr (E.to_expr i)) r.r_upperbound in
        let subbed_t1  = p_subst r.r_var (E.to_expr i) t1 in
        if i < j then run_aux (subbed_t1@(Loop (var,r',t1))::l) accum
        else run_aux l accum
      | [] -> accum

    and extract_unsync (x:(unsync_instruction) list) accum =
      match x with
      | (Access acc)::xs ->
        let n' = eval_expr E.to_int acc.access_index in
        extract_unsync xs ((E.to_expr n')::accum)
      | (Loop (var,r,t))::xs ->
        let i = eval_expr E.to_int r.r_lowerbound in
        let j = eval_expr E.to_int r.r_upperbound in
        let r' = createRange (Incr ((E.to_expr i))) r.r_upperbound in
        if i < j then extract_unsync (t@(Loop (var,r',t))::xs) accum
        else extract_unsync xs accum
      | [] -> List.rev accum

    in
    let accum = run_aux s [] in
    let ret = List.rev accum in
    ret
*)
end


(* ------------------
  LANGUAGE C
  A->P->C->L->H
------------------  *)
module CLang = struct

  module P = PLang

  type slice =
  | Unsync of (P.unsync_instruction list)
  | Decl of range * (slice list)

  type t = slice list

  let rec ser_1 p =
    let open Sexplib in
    match p with
    | Unsync l -> call "unsync" [P.ser_u l]
    | Decl (r, l) -> binop "decl" (r_ser r) (ser l)
  and ser l =
    let open Sexplib in
    Sexp.List (List.map ser_1 l)

  let print_lang k =
    ser k
      |> Sexplib.Sexp.to_string_hum
      |> print_endline

  (* Represents phases(P) and white triangle (separation part) *)
  let rec translate (s:P.t) : t =
    match s with
    | (P.Phased x)::xs -> (Unsync x)::(translate xs)
    | (P.Loop (r,b))::xs ->
      let decl = List.map (fun x -> (Decl (r,[x]))) (translate b) in
      decl@(translate xs)
    | [] -> []

end

(* ------------------
  LANGUAGE L
------------------  *)
module LLang = struct

  module P = PLang
  module C = CLang

  type unsync_instruction =
  | Decl of range * (unsync_instruction list)
  | Access of variable * access * task


  let rec ser_1 p =
    let open Sexplib in
    match p with
    | Access (x,a, t) -> call "loc" [Sexp.Atom x.var_name; a_ser a; t_ser t]
    | Decl (r, l) -> binop "decl" (r_ser r) (ser l)

  and ser l =
    let open Sexplib in
    Sexp.List (List.map ser_1 l)

  let print_lang k =
    ser k
      |> Sexplib.Sexp.to_string_hum
      |> print_endline

  let rec l_subst (kv: variable * nexp) : unsync_instruction list -> unsync_instruction list =
    List.map (l_subst_inst kv)
  and l_subst_inst (kv: variable * nexp) : unsync_instruction -> unsync_instruction =
    function
    | Decl (r, b) ->
      let r = ReplacePair.r_subst kv r in
      let shadows = var_equal r.range_var (fst kv) in
      Decl (r, if shadows then b else l_subst kv b)
    | Access (x, a, t) -> Access (x, ReplacePair.a_subst kv a, t)

  let rec project (t:task) : P.unsync_instruction list -> unsync_instruction list =
    List.map (project_inst t)
  and project_inst (t:task) : P.unsync_instruction -> unsync_instruction =
    function
    | P.Access (x, a) -> Access (x, a, t)
    | P.Loop (r, b) -> Decl (r, (project t b))
(*
  let rec run (s:(unsync_instruction) list) tid =
    match s with
    | (Decl (v,r,b))::ss ->
        let i = eval_expr E.to_int r.r_lowerbound in
        let j = eval_expr E.to_int r.r_upperbound in
        let r' = createRange (Incr (E.to_expr i)) r.r_upperbound in
        let subbed_b  = l_subst v (E.to_expr i) b in
        if i < j then (run subbed_b tid )@(run [(Decl (v,r',b))] tid)
        else run ss tid
    | (Access (a,i))::ss ->
        (* matches index to tid *)
        if i=tid then
        (
          let a' = (eval_expr E.to_int a.access_index) in
          (E.to_expr a')::(run ss tid)
        )
        else run ss tid
    | [] -> []
*)
  let rec translate (d:(P.unsync_instruction) list) (tid_count: nexp) =
    let r1 = {
      range_var = var_make "$T1";
      range_lower_bound=Num 1;
      range_upper_bound=tid_count
    } in
    let r2 = {
      range_var = var_make "$T2";
      range_lower_bound = Num 0;
      range_upper_bound = Var (var_make "$T1")
    } in
    Decl (r1,
      (l_subst (var_make "$TID", Var (var_make "$T1"))
        (project Task1 d)
      )@
      [
        Decl (r2,
          l_subst (var_make "$TID", Var (var_make "$T2"))
          (project Task2 d)
        )
      ]
    )

end
(* ------------------
  LANGUAGE H
------------------  *)
module HLang = struct
  module L = LLang
  module C = CLang

  type slice =
  (* should it be L.unsync_instruction list ?? *)
  | Unsync of (L.unsync_instruction)
  | Global of range * (slice list)

  type t = slice list

  let rec ser_1 p =
    let open Sexplib in
    match p with
    | Unsync u -> unop "unsync" (L.ser_1 u)
    | Global (r, l) -> binop "global" (r_ser r) (ser l)
  and ser l =
    let open Sexplib in
    Sexp.List (List.map ser_1 l)

  let print_lang k =
    ser k
      |> Sexplib.Sexp.to_string_hum
      |> print_endline

  let rec h_subst (kv:variable*nexp) (s:(slice) list) :(slice) list =
    match s with
    | (Unsync ui)::ss -> (Unsync (List.hd (L.l_subst kv [ui])))::(h_subst kv ss)
    | (Global (r, b))::ss ->
        let r = ReplacePair.r_subst kv r in
        let shadows = var_equal r.range_var (fst kv) in
        (Global (r, if shadows then b else h_subst kv b))::
        (h_subst kv ss)
    | [] -> []
(*
  let rec run (s:t) =
    let rec run_aux (s:t) accum =
      let rec run_L (s:L.unsync_instruction) =
        L.run [s] (E.to_expr 1)
      in
      match s with
      | (Unsync u)::ss -> run_aux ss ((run_L u)::accum)
      | (Global (v,r,b))::ss ->
          let i = eval_expr E.to_int r.r_lowerbound in
          let j = eval_expr E.to_int r.r_upperbound in
          let r' = createRange (Incr ((E.to_expr i))) r.r_upperbound in
          let subbed_b  = h_subst v (E.to_expr i) b in
          if i < j then run_aux (subbed_b@(Global (v,r',b))::ss) accum
          else run_aux ss accum
      | [] -> accum
    in
    let accum = run_aux s [] in
    let ret = List.rev accum in
    ret
*)
  let rec translate (s:C.t) : t =
    match s with
    | (C.Unsync ui)::uis ->
        (Unsync (L.translate ui (Num 2)))::(translate uis)
    | (C.Decl (r,b))::uis -> (Global (r,(translate b)))::(translate uis)
    | [] -> []

(*
  type flat_kernel = {
    flat_kernel_pre: bexp list;
    flat_kernel_proofs: (bexp list) list;
    flat_kernel_steps: (variable * access_t) list;
    flat_kernel_single_vars: VarSet.t;
    flat_kernel_multi_vars: VarSet.t;
  }

  let flatten_kernel (s:slice list) =
    let p = Loops.normalize_variables ... in
    match s with
    | (Unsync u)::ss ->
    | (Global (v,r,b))::ss ->
    | [] -> []

  let extract_steps e =
    let rec iter e accum  =
      let upper_bound e = iter e (Num 0, []) |> fst in
      match e with
      | (Unsync u)::rest -> iter rest (u::accum)
      | (Global (r,b))::rest ->
        (* TODO *)
      | [] -> accum
    in
    iter e (Num 0, []) |> snd |> List.rev
*)

end

(* ------------------
  LANGUAGE A
  source language.
------------------  *)

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
  | Base (Unsync (Acc (x,e))) -> (None, [Base (Acc (x,e))])
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

let rec s_prog_to_phase_list (l: ('a base_inst) list) : ('a phase) list  =
  List.map s_inst_to_phase_list l |> List.flatten
and s_inst_to_phase_list : 'a base_inst -> ('a phase) list =
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

let project : (u_prog phase) list -> (y_prog phase * y_prog phase) list =
  List.map (fun p -> (project_phase Task1 p, project_phase Task2 p))



module ALang = struct

  module P = PLang
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
  let print_lang (p:Proto.prog) =
    Serialize.proto_ser p
      |> Sexplib.Sexp.to_string_hum
      |> print_endline


  let rec translate (s:Proto.prog) : P.t =
    let open Proto in
    (* Rule of black triangle, called normalization *)
    let merge o1 o2 =
      match o1, o2 with
      | (None,p2),(None,q2) -> (None, (p2@q2))
      | (None,p2),(Some q1, q2) -> (Some (p2@q1),q2)
      | (Some p1,p2),(None, q2) -> (Some p1,(p2@q2))
      | (Some p1,p2),(Some q1, q2) -> (Some (p1@p2@q1),q2)
    in
    let rec normalize1 : inst -> Proto.prog option * Proto.prog =
      function
      | Base Sync -> (Some [Base Sync],[])
      | Base (Unsync (Acc (x,e))) -> (None, [Base (Unsync (Acc (x,e)))])
      | Cond (b, p) ->
        begin
          match normalize p with
          (* There is no synchronized part, so we only return 1 conditional *)
          | (None, p) -> (None, [Cond (b, p)])
          (* Duplicate the condition, one synchronized and the other unsync *)
          | (Some p, p') -> (Some [Cond (b, p)], [Cond (b, p')])
        end
      | Loop ({range_var=x;range_lower_bound=lb;range_upper_bound=ub} as r,body) as s ->
          begin match normalize body with
            | (Some p1, p2) ->
              let dec_ub = Bin (Minus, ub, Num 1) in
              let p1' = Cond (
                NRel (NLt, lb, ub),
                ReplacePair.p_subst (x, lb) p1
              ) in
              let p2' = Cond (
                NRel (NLt, lb, ub),
                ReplacePair.p_subst (x, dec_ub) p2
              ) in
              let inc_var = Bin (Plus,Var x,Num 1) in
              let subbed_p1 = ReplacePair.p_subst (x, inc_var) p1 in
              let r' = { r with range_upper_bound = dec_ub } in
              (Some [p1';Loop (r',p2@subbed_p1)] , [p2'])
            | (None, _) -> (None, [s])
          end
    (* Rule of black triangle, called normalization *)
    and normalize: Proto.prog -> Proto.prog option * Proto.prog = function
      | [] -> (None,[])
      | x::xs -> merge (normalize1 x) (normalize xs)
    in

    let rec translate_aux (s:prog) (accum:P.t) (phase:P.unsync_instruction list)  =
      match s with
      | (Base Sync)::l -> translate_aux l (accum@[(P.Phased phase)]) []
      | (Base (Unsync (Acc (x,a))))::l -> translate_aux l accum (phase@[(P.Access (x,a))])
      | (Loop (r,t))::l ->
        translate_aux l (accum@[P.Loop (r,translate_aux t [] [])]) []
      | [] -> accum@[P.Phased phase]
    in

    let p:Proto.prog = match normalize s with
    | None, x -> x
    | (Some b, after) -> b@after
    in
    translate_aux p [] []

end

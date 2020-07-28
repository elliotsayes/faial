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

module ALang = struct

  module P = PLang

  type instruction =
  | Access of variable * access
  | Sync
  | Cond of bexp * instruction list * instruction list
  | Loop of range * instruction list

  type t = instruction list

  let rec ser_1 p =
    let open Sexplib in
    match p with
    | Access (x, a) -> call "loc" [Sexp.Atom x.var_name; a_ser a]
    | Cond (b, p, q) -> call "if" [ser p; ser q]
    | Loop (r, l) -> binop "loop" (r_ser r) (ser l)
    | Sync -> Sexp.Atom "sync"
  and ser l =
    let open Sexplib in
    Sexp.List (List.map ser_1 l)

  let print_lang k =
    ser k
      |> Sexplib.Sexp.to_string_hum
      |> print_endline

  let rec a_subst (kv: variable * nexp): t -> t =
    List.map (a_subst_inst kv)
  and a_subst_inst (kv: variable * nexp) (i:instruction) : instruction =
    match i with
    | Loop (r,b) ->
      let r = ReplacePair.r_subst kv r in
      let shadows = var_equal r.range_var (fst kv) in
      Loop (r, if shadows then b else a_subst kv b)
    | Access (x,a) -> Access (x,ReplacePair.a_subst kv a)
    | Cond (b, p, q) ->
      Cond (
        ReplacePair.b_subst kv b,
        a_subst kv p,
        a_subst kv q
      )
    | Sync -> Sync
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

(* Represents norms(P) *)
  let rec translate (s:t) =
    (* Rule of black triangle, called normalization *)
    let rec normalize1 s =
      match s with
      | Sync -> (Some [Sync],[])
      | Access (x,a) -> (None, [Access (x,a)])
      | Loop ({range_var=x;range_lower_bound=lb;range_upper_bound=ub} as r,body) ->
          (match normalize body with
            | (Some p1, p2) ->
              let dec_ub = Bin (Minus, ub, Num 1) in
              let p1' = Cond (
                NRel (NLt, lb, ub),
                a_subst (x, lb) p1,
                []
              ) in
              let p2' = Cond (
                NRel (NLt, lb, ub),
                a_subst (x, dec_ub) p2,
                []
              ) in
              let inc_var = Bin (Plus,Var x,Num 1) in
              let subbed_p1 = a_subst (x, inc_var) p1 in
              let r' = { r with range_upper_bound = dec_ub } in
              ( Some ([p1';Loop (r',p2@subbed_p1)]) , [p2'])
            | (None, _) -> (None, [s])
          )
    (* Rule of black triangle, called normalization *)
    and normalize (s:t) =
      match s with
      | [] -> (None,[])
      | x::xs ->
        (match normalize1 x,normalize xs with
          | (None,p2),(None,q2) -> (None, (p2@q2))
          | (None,p2),(Some q1, q2) -> (Some (p2@q1),q2)
          | (Some p1,p2),(None, q2) -> (Some p1,(p2@q2))
          | (Some p1,p2),(Some q1, q2) -> (Some (p1@p2@q1),q2)
        )

    in

    let rec translate_aux (s:t) accum phase  =
      match s with
      | (Sync)::l -> translate_aux l (accum@[(P.Phased phase)]) []
      | (Access (x,a))::l -> translate_aux l accum (phase@[(P.Access (x,a))])
      | (Loop (r,t))::l -> translate_aux l (accum@[P.Loop (r,(translate_aux t [] []))]) []
      | [] -> accum
    in

    let (before,after) = normalize s in
    match before with
    | None -> (translate_aux (after@[Sync]) [] [])
    | (Some b) -> (translate_aux ((b@after)@[Sync]) [] [])


end

module ProtoLang = struct

  module A = ALang

  let rec translate (s:Proto.prog) : A.t =
    List.map translate_inst s |> List.flatten

  and translate_inst : inst -> A.t =
    function
    | Proto.Goal _
    | Proto.Assert _ -> []
    | Proto.Sync -> [A.Sync]
    | Proto.Cond (b, p, q) -> [Cond (b, translate p, translate q)]
    | Proto.Acc (x,a) -> [A.Access (x,a)]
    | Proto.Loop (r, e) -> [A.Loop (r, translate e)]

end

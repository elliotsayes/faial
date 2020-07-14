open Proto
open Common
open Phaseord
open Serialize

type range = {
  r_var: variable;
  r_lowerbound: nexp;
  r_upperbound: nexp;
}

let r_ser r =
  call "range" [
    Sexplib.Sexp.Atom r.r_var.var_name;
    n_ser r.r_lowerbound;
    n_ser r.r_upperbound;
  ]

let createRange var low upp =
  {r_var=var;r_lowerbound=low;r_upperbound=upp}

let proto_to_range (r:Proto.range) : range =
  createRange (r.range_var) (Num 0) (r.range_upper_bound)

let createAccess (codl:nexp list) (cndl:bexp) (m:mode)=
  {access_index=codl;access_cond=cndl;access_mode=m}

let rec nsubst (name: string) (value: nexp) (e:nexp) =
  match e with
  | Var x ->
    if (x.var_name)=name then value
    else e
  | Bin (x,e1,e2) -> Bin (x,nsubst name value e1,nsubst name value e2)
  | Proj (t,e1) -> Proj (t,nsubst name value e1)
  | Num x -> e

let rec bsubst (name: string) (value: nexp) (e:bexp) =
  match e with
  | Bool b -> e
  | NRel (x,e1,e2) -> NRel (x,nsubst name value e1, nsubst name value e2)
  | BRel (x,e1,e2) -> BRel (x,bsubst name value e1, bsubst name value e2)
  | BNot e -> BNot (bsubst name value e)
  | Pred (s,v) -> e


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

  let rec p_subst (name:string) (value: nexp) (s:(instruction) list) :(instruction) list =
    let rec p_subst_unsync (name:string) (value: nexp) (s:(unsync_instruction) list) :(unsync_instruction) list =
      match s with
      | (Loop (r,b))::ss ->
        let r' = createRange (r.r_var) (nsubst name value r.r_lowerbound) (nsubst name value r.r_upperbound) in
        let b' = p_subst_unsync name value b in
        (Loop (r',b'))::(p_subst_unsync name value ss)
      | (Access (x,a))::ss ->
        let a1 = List.map (nsubst name value) a.access_index in
        let a2 = bsubst name value a.access_cond in
        (Access (x,(createAccess a1 a2 a.access_mode)))::(p_subst_unsync name value ss)
      | [] -> []
    in
    match s with
    | (Loop (r,b))::ss ->
        let r' = createRange (r.r_var) (nsubst name value r.r_lowerbound) (nsubst name value r.r_upperbound) in
        let b' = p_subst name value b in
        (Loop (r',b'))::(p_subst name value ss)
    | (Phased u)::ss ->
        (Phased (p_subst_unsync name value u))::(p_subst name value ss)
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
  | Access of variable * access * nexp


  let rec ser_1 p =
    let open Sexplib in
    match p with
    | Access (x,a,n) -> call "loc" [Sexp.Atom x.var_name; a_ser a; n_ser n]
    | Decl (r, l) -> binop "decl" (r_ser r) (ser l)
  and ser l =
    let open Sexplib in
    Sexp.List (List.map ser_1 l)

  let print_lang k =
    ser k
      |> Sexplib.Sexp.to_string_hum
      |> print_endline

  let rec l_subst (name: string) (value: nexp) (s:(unsync_instruction) list) :(unsync_instruction) list =
    match s with
    | (Decl (r,b))::ss ->
      let r' = createRange r.r_var (nsubst name value r.r_lowerbound) (nsubst name value r.r_upperbound) in
      let b' = l_subst name value b in
      (Decl (r',b'))::(l_subst name value ss)
    | (Access (x,e1,e2))::ss ->
      let n = List.map (nsubst name value) e1.access_index in
      let b = bsubst name value e1.access_cond in
      (Access (x,(createAccess n b e1.access_mode),(nsubst name value e2)))::(l_subst name value ss)
    | [] -> []

  let rec project (s:(P.unsync_instruction) list) :(unsync_instruction) list =
    match s with
    | (P.Access (x,a))::ss ->
      (Access (x,a, Var (var_make "$TID")))::(project ss)
    | (P.Loop (r,b))::ss -> (Decl (r,(project b)))::(project ss)
    | [] -> []
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
    Decl (createRange (var_make "$T1") (Num 1) tid_count,
      (l_subst "$TID" (Var (var_make "$T1")) (project d))
        @[Decl (createRange (var_make "$T2") (Num 0) (Var (var_make "$T1")),
          l_subst "$TID" (Var (var_make "$T2")) (project d)
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

  let rec h_subst (name:string) (value: nexp) (s:(slice) list) :(slice) list =
    match s with
    | (Unsync ui)::ss -> (Unsync (List.hd (L.l_subst name value [ui])))::(h_subst name value ss)
    | (Global (r,b))::ss ->
        let r' = createRange r.r_var (nsubst name value r.r_lowerbound) (nsubst name value r.r_upperbound) in
        let b' = h_subst name value b in
        (Global (r',b'))::(h_subst name value ss)
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
  | Loop of range * ((instruction) list)

  type t = (instruction) list

  let rec ser_1 p =
    let open Sexplib in
    match p with
    | Access (x,a) -> call "loc" [Sexp.Atom x.var_name; a_ser a]
    | Loop (r, l) -> binop "loop" (r_ser r) (ser l)
    | Sync -> Sexp.Atom "sync"
  and ser l =
    let open Sexplib in
    Sexp.List (List.map ser_1 l)

  let print_lang k =
    ser k
      |> Sexplib.Sexp.to_string_hum
      |> print_endline

  let rec a_subst (name: string) (value: nexp) (s:(instruction) list) :(instruction) list =
    match s with
    | (Loop (r,b))::ss ->
        let r' = createRange r.r_var (nsubst name value r.r_lowerbound) (nsubst name value r.r_upperbound) in
        let b' = a_subst name value b in
        (Loop (r',b'))::(a_subst name value ss)
    | (Access (x,a))::ss ->
        let n' = List.map (nsubst name value) a.access_index in
        let b' = bsubst name value a.access_cond in
        (Access (x,createAccess n' b' a.access_mode))::(a_subst name value ss)
    | Sync::ss -> (Sync)::(a_subst name value ss)
    | [] -> []
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
    (* This is U+ operator *)
    let rec inlineCondition s n f =
      match s with
      | [] -> []
      | (Access (x,a))::xs ->
          let n' = List.map f a.access_index in
          let a' = createAccess n' (BRel (BAnd,a.access_cond,n)) a.access_mode in
          (Access (x,a'))::(inlineCondition xs n f)
      | (Sync)::xs -> (Sync)::(inlineCondition xs n f)
      | (Loop (r,b))::xs ->
          let n' = (BRel (BAnd,NRel (NLt,r.r_lowerbound,r.r_upperbound),n)) in
          (Loop (r,inlineCondition b n' f))::(inlineCondition xs n f)
    in
    (* Rule of black triangle, called normalization *)
    let rec normalize1 s =
      match s with
      | Sync -> (Some [Sync],[])
      | Access (x,a) -> (None, [Access (x,a)])
      | Loop (r,body) ->
          (match normalize body with
            | (Some p1, p2) ->
              let dec_ub = Bin (Minus,r.r_upperbound,Num 1) in
              let p1' = inlineCondition p1 (NRel (NLt,r.r_lowerbound,r.r_upperbound)) (nsubst (r.r_var.var_name) r.r_lowerbound) in
              let p2' = inlineCondition p2 (NRel (NLt,r.r_lowerbound,r.r_upperbound)) (nsubst (r.r_var.var_name) dec_ub) in
              let inc_var = Bin (Plus,Var r.r_var,Num 1) in
              let subbed_p1 = a_subst (r.r_var.var_name) inc_var p1 in
              let r' = createRange r.r_var r.r_lowerbound dec_ub in
              ( Some (p1'@[Loop (r',p2@subbed_p1)]) , p2')
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

  let rec translate s : A.t =
    match s with
    | Proto.Skip
    | Proto.Goal _
    | Proto.Assert _ ->
      []
    | Proto.Seq (e1, e2) ->
        (translate e1) @ (translate e2)
    | Proto.Sync -> [A.Sync]
    | Proto.Acc (x,a) -> [A.Access (x,a)]
    | Proto.Loop ({range_var = var; range_upper_bound = ub}, e) ->
        let r' = createRange var (Num 0) ub in
        [A.Loop (r', translate e)]

end

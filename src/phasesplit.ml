type 'a nexpr =
| Value of 'a
| Incr of 'a nexpr
| Decr of 'a nexpr
| Variable of string
| Number of int

type 'a bexpr =
| True
| And of 'a bexpr * 'a bexpr
| LessThan of 'a nexpr * 'a nexpr

type 'a range = {
  r_lowerbound: 'a nexpr;
  r_upperbound: 'a nexpr;
}

let createRange low upp =
  {r_lowerbound=low;r_upperbound=upp}

type 'a aexpr = {
  ac_codelines: 'a nexpr;
  ac_conditions: 'a bexpr;
}

let createAccess (codl:'a nexpr) (cndl:'a bexpr) =
  {ac_codelines=codl;ac_conditions=cndl}

let getCodelines (ac:'a aexpr) =
  ac.ac_codelines

let getConditions (ac:'a aexpr) =
  ac.ac_conditions



module type EXPR =
  sig
    type t
    (* Stringify *)
    val to_string: t -> string
    (* Convert to int *)
    val to_int: t -> int
    (* Evaluate the expression *)
    val to_expr: int -> t nexpr
  end

(* Expressions *)
let rec expr_to_string f (e: 'a nexpr) =
  match e with
  | Value n -> f n
  | Incr e -> expr_to_string f e ^ " + 1"
  | Decr e -> expr_to_string f e ^ " - 1"
  | Variable x -> x
  | Number x -> string_of_int x

(* Boolean Expressions *)
let rec bexpr_to_string f (e: 'a bexpr) =
  match e with
  | True -> "True"
  | And (e1,e2) -> bexpr_to_string f e1 ^ " & " ^ bexpr_to_string f e2
  | LessThan (e1,e2) -> expr_to_string f e1 ^ " < " ^ expr_to_string f e2

let rec subst (name: string) (value: 'a nexpr) (e:'a nexpr) =
  match e with
  | Variable x ->
    if x=name then value
    else e
  | Incr x -> Incr (subst name value x)
  | Decr x -> Decr (subst name value x)
  | Value x -> e
  | Number x -> e

let rec boolean_subst (name: string) (value: 'a nexpr) (e:'a bexpr) =
  match e with
  | True -> e
  | And (e1,e2) -> (And (boolean_subst name value e1,boolean_subst name value e2))
  | LessThan (e1,e2) -> (LessThan (subst name value e1,subst name value e2))

let rec eval_expr f (e:'a nexpr) :int =
  match e with
  | Value n -> f n
  | Incr e -> (eval_expr f e) + 1
  | Decr e -> (eval_expr f e) - 1
  | Variable x -> failwith "Eval Expr: free variable."
  | Number x -> x

let rec expr_to_bexpr (e1:'a nexpr) (e2:'a nexpr) = LessThan (e1,e2)



(* ------------------
  LANGUAGE P
  A->P->C->L->H
------------------  *)
module PLang (E:EXPR) = struct



  type unsync_instruction =
  | Loop of string * (E.t range) * (unsync_instruction list)
  | Access of (E.t aexpr)

  type instruction =
  | Loop of string * (E.t range) * (instruction list)
  | Phased of (unsync_instruction list)

  type t = instruction list

  let rec to_string (s:t) =
    let rec to_string_indent (s:t) indent =
      let rec ui_to_string l indent =
        let rec uts_aux l accum indent =
          match l with
          | [] -> accum
          | (Access ac)::xs -> uts_aux xs (accum^String.make (indent*2) ' '^"Access "^(expr_to_string E.to_string ac.ac_codelines)^" if ["^(bexpr_to_string E.to_string ac.ac_conditions)^"];\n") indent
          | (Loop (var,r,body))::xs -> uts_aux xs (accum^String.make (indent*2) ' '^"For " ^ var ^ " in [" ^ (expr_to_string E.to_string r.r_lowerbound) ^ ", " ^ (expr_to_string E.to_string r.r_upperbound) ^ ") {\n" ^ (uts_aux body "" (indent+1)) ^ String.make (indent*2) ' '^"}\n") indent
        in
        uts_aux l "" indent
      in
      match s with
      | (Loop (var,r,t1))::l -> String.make (indent*2) ' '^"For " ^ var ^ " in [" ^ (expr_to_string E.to_string r.r_lowerbound) ^ ", " ^ (expr_to_string E.to_string r.r_upperbound) ^ ") {\n" ^ (to_string_indent t1 (indent+1)) ^ String.make (indent*2) ' '^"}\n" ^ (to_string_indent l indent)
      | (Phased ui_list)::l -> String.make (indent*2) ' '^"Phased " ^ "{\n" ^ (ui_to_string ui_list (indent+1)) ^ String.make (indent*2) ' '^"}\n" ^ (to_string_indent l indent)
      | [] -> ""
    in
    to_string_indent s 0

  let rec p_subst (name:string) (value: 'a nexpr) (s:(instruction) list) :(instruction) list =
    let rec p_subst_unsync (name:string) (value: 'a nexpr) (s:(unsync_instruction) list) :(unsync_instruction) list =
      match s with
      | (Loop (v,r,b))::ss ->
        let r' = createRange (subst name value r.r_lowerbound) (subst name value r.r_upperbound) in
        let b' = p_subst_unsync name value b in
        (Loop (v,r',b'))::(p_subst_unsync name value ss)
      | (Access a)::ss ->
        let a1 = subst name value a.ac_codelines in
        let a2 = boolean_subst name value a.ac_conditions in
        (Access (createAccess a1 a2))::(p_subst_unsync name value ss)
      | [] -> []
    in
    match s with
    | (Loop (v,r,b))::ss ->
        let r' = createRange (subst name value r.r_lowerbound) (subst name value r.r_upperbound) in
        let b' = p_subst name value b in
        (Loop (v,r',b'))::(p_subst name value ss)
    | (Phased u)::ss ->
        (Phased (p_subst_unsync name value u))::(p_subst name value ss)
    | [] -> []

  let rec run (s:t) =
    let rec run_aux (s:t) accum =
      match s with
      | (Phased n)::l -> run_aux l ((extract_unsync n [])::accum)
      | (Loop (var,r,t1))::l ->
        let i = eval_expr E.to_int r.r_lowerbound in
        let j = eval_expr E.to_int r.r_upperbound in
        let r' = createRange (Incr (E.to_expr i)) r.r_upperbound in
        let subbed_t1  = p_subst var (E.to_expr i) t1 in
        if i < j then run_aux (subbed_t1@(Loop (var,r',t1))::l) accum
        else run_aux l accum
      | [] -> accum

    and extract_unsync (x:(unsync_instruction) list) accum =
      match x with
      | (Access acc)::xs ->
        let n' = eval_expr E.to_int acc.ac_codelines in
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

end


(* ------------------
  LANGUAGE C
  A->P->C->L->H
------------------  *)
module CLang (E:EXPR) = struct

  module P = PLang(E)

  type slice =
  | Unsync of (P.unsync_instruction list)
  | Decl of (string) * (E.t range) * (slice list)

  type t = slice list

  let rec to_string (s:t) =
    let rec to_string_indent (s:t) indent =
      let rec ui_to_string l indent =
        let rec uts_aux l accum indent =
          match l with
          | [] -> accum
          | (P.Access ac)::xs -> uts_aux xs (accum^String.make (indent*2) ' '^"Access "^(expr_to_string E.to_string ac.ac_codelines)^" if ["^(bexpr_to_string E.to_string ac.ac_conditions)^"];\n") indent
          | (P.Loop (var,r,body))::xs -> uts_aux xs (accum^String.make (indent*2) ' '^"For " ^ var ^ " in [" ^ (expr_to_string E.to_string r.r_lowerbound) ^ ", " ^ (expr_to_string E.to_string r.r_upperbound) ^ ") {\n" ^ (uts_aux body "" (indent+1)) ^ String.make (indent*2) ' '^"}\n") indent
        in
        uts_aux l "" indent
      in
      match s with
      | (Decl (var,r,t1))::l -> String.make (indent*2) ' '^"Decl " ^ var ^ " in [" ^ (expr_to_string E.to_string r.r_lowerbound) ^ ", " ^ (expr_to_string E.to_string r.r_upperbound) ^ ") {\n" ^ (to_string_indent t1 (indent+1)) ^ String.make (indent*2) ' '^"}\n" ^ (to_string_indent l indent)
      | (Unsync ui_list)::l -> String.make (indent*2) ' '^"Unsync " ^ "{\n" ^ (ui_to_string ui_list (indent+1)) ^ String.make (indent*2) ' '^"}\n" ^ (to_string_indent l indent)
      | [] -> ""
    in
    to_string_indent s 0
  (* Represents phases(P) and white triangle (separation part) *)
  let rec translate s =
    match s with
    | (P.Phased x)::xs -> (Unsync x)::(translate xs)
    | (P.Loop (v,r,b))::xs ->
      let decl = List.map (fun x -> (Decl (v,r,[x]))) (translate b) in
      decl@(translate xs)
    | [] -> []

end

(* ------------------
  LANGUAGE L
------------------  *)
module LLang (E:EXPR) = struct

  module P = PLang(E)
  module C = CLang(E)

  type unsync_instruction =
  | Decl of (string) * (E.t range) * (unsync_instruction list)
  | Access of (E.t aexpr) * (E.t nexpr)

  let rec to_string (s:(unsync_instruction) list) indent =
    let rec to_string_indent_1 (s:unsync_instruction) indent =
      match s with
      | (Decl (var,r,t1)) ->
          let indentation = String.make (indent*2) ' ' in
          let lb = (expr_to_string E.to_string r.r_lowerbound) in
          let ub = (expr_to_string E.to_string r.r_upperbound) in
          indentation ^ "Decl "
          ^ var ^ " in [" ^ lb ^ ", " ^ ub ^ ") {\n"
          ^ (to_string_indent t1 (indent+1)) ^ indentation ^"}\n"
      | (Access (ac,index)) ->
          String.make (indent*2) ' '
          ^"Access "^(expr_to_string E.to_string ac.ac_codelines)
          ^" if ["^(bexpr_to_string E.to_string ac.ac_conditions)
          ^"] , tid: " ^ (expr_to_string E.to_string index) ^ ";\n"

    and to_string_indent (s:(unsync_instruction) list) indent =
      match s with
      | h::l ->
          (to_string_indent_1 h indent) ^ (to_string_indent l indent)
      | [] -> ""
    in
    to_string_indent s indent

  let rec l_subst (name: string) (value: 'a nexpr) (s:(unsync_instruction) list) :(unsync_instruction) list =
    match s with
    | (Decl (v,r,b))::ss ->
      let r' = createRange (subst name value r.r_lowerbound) (subst name value r.r_upperbound) in
      let b' = l_subst name value b in
      (Decl (v,r',b'))::(l_subst name value ss)
    | (Access (e1,e2))::ss ->
      let n = subst name value e1.ac_codelines in
      let b = boolean_subst name value e1.ac_conditions in
      (Access ((createAccess n b),(subst name value e2)))::(l_subst name value ss)
    | [] -> []

  let rec project (s:(P.unsync_instruction) list) :(unsync_instruction) list =
    match s with
    | (P.Access a)::ss ->
      (Access (a, Variable "$TID"))::(project ss)
    | (P.Loop (var,r,b))::ss -> (Decl (var,r,(project b)))::(project ss)
    | [] -> []

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
          let a' = (eval_expr E.to_int a.ac_codelines) in
          (E.to_expr a')::(run ss tid)
        )
        else run ss tid
    | [] -> []

  let rec translate (d:(P.unsync_instruction) list) (tid_count: E.t nexpr) =
    Decl ("$T1",createRange (Number 1) tid_count,
      (l_subst "$TID" (Variable "$T1") (project d))
        @[Decl ("$T2",createRange (Number 0) (Variable "$T1"),
          l_subst "$TID" (Variable "$T2") (project d)
            )
          ]
        )

end
(* ------------------
  LANGUAGE H
------------------  *)
module HLang (E:EXPR) = struct
  module L = LLang(E)
  module C = CLang(E)

  type slice =
  (* should it be L.unsync_instruction list ?? *)
  | Unsync of (L.unsync_instruction)
  | Global of (string) * (E.t range) * (slice list)

  type t = slice list


  let rec to_string (s:t) =
    let rec to_string_indent (s:t) indent =
      match s with
      | (Global (var,r,t1))::l -> String.make (indent*2) ' '^"Global "
          ^ var ^ " in [" ^ (expr_to_string E.to_string r.r_lowerbound)
          ^ ", " ^ (expr_to_string E.to_string r.r_upperbound) ^ ") {\n"
          ^ (to_string_indent t1 (indent+1)) ^ String.make (indent*2) ' '^"}\n"
          ^ (to_string_indent l indent)

      | (Unsync ui)::l -> String.make (indent*2) ' '^"Unsync {\n"
          ^(L.to_string [ui] (indent+1)) ^ String.make (indent*2) ' '^"}\n"
          ^ (to_string_indent l indent)

      | [] -> ""
    in
    to_string_indent s 0

  let rec h_subst (name:string) (value: 'a nexpr) (s:(slice) list) :(slice) list =
    match s with
    | (Unsync ui)::ss -> (Unsync (List.hd (L.l_subst name value [ui])))::(h_subst name value ss)
    | (Global (v,r,b))::ss ->
        let r' = createRange (subst name value r.r_lowerbound) (subst name value r.r_upperbound) in
        let b' = h_subst name value b in
        (Global (v,r',b'))::(h_subst name value ss)
    | [] -> []

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

  let rec translate (s:(C.t)) :t =
    match s with
    | (C.Unsync ui)::uis ->
        (Unsync (L.translate ui (Number 2)))::(translate uis)
    | (C.Decl (v,r,b))::uis -> (Global (v,r,(translate b)))::(translate uis)
    | [] -> []

end

(* ------------------
  LANGUAGE A
  source language.
------------------  *)

module ALang (E:EXPR) = struct

  module P = PLang(E)

  type instruction =
  | Codeline of (E.t nexpr) * (E.t bexpr)
  | Sync
  | Loop of string * (E.t range) * ((instruction) list)

  type t = (instruction) list

  let rec to_string (s:t) =
    let rec to_string_indent (s:t) indent =
      let rec list_to_string (n:(E.t nexpr)) c =
        (expr_to_string E.to_string n) ^ ", if [" ^ (bexpr_to_string E.to_string c) ^ "]"
      in
      match s with
      | Sync::l ->  String.make (indent*2) ' '^"Sync;\n" ^ (to_string_indent l indent)
      | (Loop (var,r,t1))::l ->  String.make (indent*2) ' '^"For " ^ var ^ " in [" ^ (expr_to_string E.to_string r.r_lowerbound) ^ ", " ^ (expr_to_string E.to_string r.r_upperbound) ^ ") {\n" ^ (to_string_indent t1 (indent+1)) ^ String.make (indent*2) ' '^ "}\n" ^ (to_string_indent l indent)
      | (Codeline (n,c))::l ->  String.make (indent*2) ' '^"Codeline " ^ (list_to_string n c) ^ ";\n" ^ (to_string_indent l indent)
      | [] -> ""
    in
    to_string_indent s 0

  let rec a_subst (name: string) (value: 'a nexpr) (s:(instruction) list) :(instruction) list =
    match s with
    | (Loop (v,r,b))::ss ->
        let r' = createRange (subst name value r.r_lowerbound) (subst name value r.r_upperbound) in
        let b' = a_subst name value b in
        (Loop (v,r',b'))::(a_subst name value ss)
    | (Codeline (n,b))::ss ->
        let n' = subst name value n in
        let b' = boolean_subst name value b in
        (Codeline (n',b'))::(a_subst name value ss)
    | Sync::ss -> (Sync)::(a_subst name value ss)
    | [] -> []

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

(* Represents norms(P) *)
  let rec translate (s:t) =
    (* This is U+ operator *)
    let rec injectCondition s n f =
      match s with
      | [] -> []
      | (Codeline (n',c'))::xs ->
          let n'' = f n' in
          (Codeline (n'',And (c',n)))::(injectCondition xs n f)
      | (Sync)::xs -> (Sync)::(injectCondition xs n f)
      | (Loop (var,r,b))::xs ->
          (Loop (var,r,injectCondition b (And (LessThan (r.r_lowerbound,r.r_upperbound),n)) f))::(injectCondition xs n f)
    in
    (* Rule of black triangle, called normalization *)
    let rec normalize1 s =
      match s with
      | Sync -> (Some [Sync],[])
      | Codeline (n,c) -> (None, [Codeline (n,c)])
      | Loop (var,r,body) ->
          (match normalize body with
            | (Some p1, p2) ->
              let p1' = injectCondition p1 (LessThan (r.r_lowerbound,r.r_upperbound)) (subst var r.r_lowerbound) in
              let p2' = injectCondition p2 (LessThan (r.r_lowerbound,r.r_upperbound)) (subst var (Decr r.r_upperbound)) in
              let subbed_p1 = a_subst var (Incr (Variable var)) p1 in
              let r' = createRange r.r_lowerbound (Decr r.r_upperbound) in
              ( Some (p1'@[Loop (var,r',p2@subbed_p1)]) , p2')
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
      | (Codeline (n,c))::l -> translate_aux l accum (phase@[(P.Access (createAccess n c))])
      | (Loop (var,r,t))::l -> translate_aux l (accum@[P.Loop (var,r,(translate_aux t [] []))]) []
      | [] -> accum
    in

    let (before,after) = normalize s in
    match before with
    | None -> (translate_aux (after@[Sync]) [] [])
    | (Some b) -> (translate_aux ((b@after)@[Sync]) [] [])
end

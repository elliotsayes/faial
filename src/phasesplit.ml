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

let rangeCreate low upp =
  {r_lowerbound=low;r_upperbound=upp}

type 'a aexpr = {
  ac_codelines: 'a nexpr;
  ac_conditions: 'a bexpr;
}

let accessCreate (codl:'a nexpr) (cndl:'a bexpr) =
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

let rec eval_expr f (e:'a nexpr) :int =
  match e with
  | Value n -> f n
  | Incr e -> (eval_expr f e) + 1
  | Decr e -> (eval_expr f e) - 1
  | Variable x -> failwith "Eval Expr"

let rec expr_to_bexpr (e1:'a nexpr) (e2:'a nexpr) = LessThan (e1,e2)



(* ------------------
  LANGUAGE T
  target language.
  comes after S.
------------------  *)
module PLang (E:EXPR) = struct



  type unsync_instruction =
  | Loop of string * (E.t range) * (unsync_instruction list)
  | Access of (E.t aexpr)

  type instruction =
  | Loop of (E.t nexpr) * (E.t nexpr) * (E.t nexpr) * (instruction list)
  | Phased of (unsync_instruction list)

  type t = instruction list

  let rec to_string (s:t) =
    let rec to_string_indent (s:t) indent =
      let rec ui_to_string l indent =
        let rec uts_aux l accum indent =
          match l with
          | [] -> accum
          | (Access ac)::xs -> uts_aux xs (accum^String.make (indent*2) ' '^"Access "^(expr_to_string E.to_string ac.ac_codelines)^" if ["^(bexpr_to_string E.to_string ac.ac_conditions)^"];\n") indent
          | (Loop (var,lb,ub,body))::xs -> uts_aux xs (accum^String.make (indent*2) ' '^"For " ^ (expr_to_string E.to_string var) ^ " in [" ^ (expr_to_string E.to_string lb) ^ ", " ^ (expr_to_string E.to_string ub) ^ ") {\n" ^ (uts_aux body "" (indent+1)) ^ String.make (indent*2) ' '^"}\n") indent
        in
        uts_aux l "" indent
      in
      match s with
      | (Loop (var,lb,ub,t1))::l -> String.make (indent*2) ' '^"For " ^ (expr_to_string E.to_string var) ^ " in [" ^ (expr_to_string E.to_string lb) ^ ", " ^ (expr_to_string E.to_string ub) ^ ") {\n" ^ (to_string_indent t1 (indent+1)) ^ String.make (indent*2) ' '^"}\n" ^ (to_string_indent l indent)
      | (Phased ui_list)::l -> String.make (indent*2) ' '^"Phased " ^ "{\n" ^ (ui_to_string ui_list (indent+1)) ^ String.make (indent*2) ' '^"}\n" ^ (to_string_indent l indent)
      | [] -> ""
    in
    to_string_indent s 0




  let rec run (s:t) =
    let rec run_aux (s:t) accum =
      match s with
      | (Phased n)::l -> run_aux l ((extract_unsync n [])::accum)
      | (Loop (var,lb,ub,t1))::l ->
        let i = eval_expr E.to_int lb in
        let j = eval_expr E.to_int ub in
        if i < j then run_aux (t1@(Loop (var,Incr (E.to_expr i),ub,t1))::l) accum
        else run_aux l accum
      | [] -> accum

    and extract_unsync (x:(unsync_instruction) list) accum =
      match x with
      | (Access acc)::xs ->
        extract_unsync xs ((acc.ac_codelines)::accum)
      | (Loop (var,lb,ub,t))::xs ->
        let i = eval_expr E.to_int lb in
        let j = eval_expr E.to_int ub in
        if i < j then extract_unsync (t@(Loop (var,Incr (E.to_expr i),ub,t))::xs) accum
        else extract_unsync xs accum
      | [] -> List.rev accum

    in
    let accum = run_aux s [] in
    let ret = List.rev accum in
    ret

end


(* ------------------
  LANGUAGE U
  comes after T
------------------  *)
module CLang (E:EXPR) = struct

  module T = PLang(E)

  type slice =
  | Unsync of (P.unsync_instruction list)
  | Decl of (string) * (E.t nexpr) * (E.t nexpr) * (slice list)

  type t = slice list

  let rec to_string (s:t) =
    let rec to_string_indent (s:t) indent =
      let rec ui_to_string l indent =
        let rec uts_aux l accum indent =
          match l with
          | [] -> accum
          | (P.Access ac)::xs -> uts_aux xs (accum^String.make (indent*2) ' '^"Access "^(expr_to_string E.to_string ac.ac_codelines)^" if ["^(bexpr_to_string E.to_string ac.ac_conditions)^"];\n") indent
          | (P.Loop (var,lb,ub,body))::xs -> uts_aux xs (accum^String.make (indent*2) ' '^"For " ^ (expr_to_string E.to_string var) ^ " in [" ^ (expr_to_string E.to_string lb) ^ ", " ^ (expr_to_string E.to_string ub) ^ ") {\n" ^ (uts_aux body "" (indent+1)) ^ String.make (indent*2) ' '^"}\n") indent
        in
        uts_aux l "" indent
      in
      match s with
      | (Decl (var,lb,ub,t1))::l -> String.make (indent*2) ' '^"Decl " ^ (expr_to_string E.to_string var) ^ " in [" ^ (expr_to_string E.to_string lb) ^ ", " ^ (expr_to_string E.to_string ub) ^ ") {\n" ^ (to_string_indent t1 (indent+1)) ^ String.make (indent*2) ' '^"}\n" ^ (to_string_indent l indent)
      | (Unsync ui_list)::l -> String.make (indent*2) ' '^"Unsync " ^ "{\n" ^ (ui_to_string ui_list (indent+1)) ^ String.make (indent*2) ' '^"}\n" ^ (to_string_indent l indent)
      | [] -> ""
    in
    to_string_indent s 0
  (* Represents phases(P) and white triangle (separation part) *)
  let rec translate s =
    match s with
    | (P.Phased x)::xs -> (Unsync x)::(translate xs)
    | (P.Loop (v,l,u,b))::xs ->
      let decl = List.map (fun x -> (Decl (v,l,u,[x]))) (translate b) in
      decl@(translate xs)
    | [] -> []

end

(* ------------------
  LANGUAGE H
  source language.
------------------  *)
module LLang (E:EXPR) = struct

  module P = PLang(E)
  module C = CLang(E)

  type unsync_instruction =
  | Decl of (string) * (E.t range) * (unsync_instruction list)
  | Access of (E.t aexpr) * (E.t nexpr)

  type t = unsync_instruction list

  let rec project (s:C.unsync_instruction list) :(unsync_instruction) list =
    match s with
    | (C.Access a)::uls ->
      (Access (a, Variable "$TID"))::(project uls)
    | (C.Decl (var,r,b))::uls -> (Var (var,r,(project b)))::(project uls)
    | [] -> []

  let rec translate (s:C.t) (tid_count: E.t nexpr) =
    (Decl ("$T1",createRange (Number 1,tid_count),
      (subst "$TID" (Variable "$T1") (project s))
      ::[(Decl ("$T2",createRange (Number 0, Variable "$T1"),
        [(subst "$TID" (Variable "$T2") (project s))]
        ))]))

end

(* ------------------
  LANGUAGE S
  source language.
------------------  *)

module ALang (E:EXPR) = struct

  module P = PLang(E)

  type instruction =
  | Codeline of (E.t nexpr) * (E.t bexpr)
  | Sync
  | Loop of (E.t nexpr) * (E.t nexpr) * (E.t nexpr) * ((instruction) list)

  type t = (instruction) list

  let rec to_string (s:t) =
    let rec to_string_indent (s:t) indent =
      let rec list_to_string (n:(E.t nexpr)) c =
        (expr_to_string E.to_string n) ^ ", if [" ^ (bexpr_to_string E.to_string c) ^ "]"
      in
      match s with
      | Sync::l ->  String.make (indent*2) ' '^"Sync;\n" ^ (to_string_indent l indent)
      | (Loop (var,lb,ub,t1))::l ->  String.make (indent*2) ' '^"For " ^ (expr_to_string E.to_string var) ^ " in [" ^ (expr_to_string E.to_string lb) ^ ", " ^ (expr_to_string E.to_string ub) ^ ") {\n" ^ (to_string_indent t1 (indent+1)) ^ String.make (indent*2) ' '^ "}\n" ^ (to_string_indent l indent)
      | (Codeline (n,c))::l ->  String.make (indent*2) ' '^"Codeline " ^ (list_to_string n c) ^ ";\n" ^ (to_string_indent l indent)
      | [] -> ""
    in
    to_string_indent s 0

  let rec run (s:t) =
    let rec run_aux (s:t) (accum,phase) =
      match s with
      | (Loop (var,lb,ub,t1))::l ->
          let i = eval_expr E.to_int lb in
          let j = eval_expr E.to_int ub in
          if i < j then run_aux (t1@(Loop (var,Incr (E.to_expr i),ub,t1))::l) (accum,phase)
          else run_aux l (accum,phase)
      | (Codeline (n,c))::l -> run_aux l (accum,n::phase)
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
      | (Loop (var,lb,ub,b))::xs ->
          (Loop (var,lb,ub,injectCondition b (And (LessThan (lb,ub),n)) f))::(injectCondition xs n f)
    in
    (* Rule of black triangle, called normalization *)
    let rec normalize1 s =
      match s with
      | Sync -> (Some [Sync],[])
      | Codeline (n,c) -> (None, [Codeline (n,c)])
      | Loop (var,lb,ub,body) ->
          (match normalize body with
            | (Some p1, p2) ->
              let p1' = injectCondition p1 (LessThan (lb,ub)) (subst var lb) in
              let p2' = injectCondition p2 (LessThan (lb,ub)) (subst var (Decr ub)) in
              (* !!! subst p1 x x+1 TODO *)
              ( Some (p1'@[Loop (var,Incr lb,ub,p2@p1)]) , p2')
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
      | (Codeline (n,c))::l -> translate_aux l accum (phase@[(P.Access (accessCreate n c))])
      | (Loop (var,lb,ub,t))::l -> translate_aux l (accum@[P.Loop (var,lb,ub,(translate_aux t [] []))]) []
      | [] -> accum
    in

    let (before,after) = normalize s in
    match before with
    | None -> (translate_aux (after@[Sync]) [] [])
    | (Some b) -> (translate_aux ((b@after)@[Sync]) [] [])
end

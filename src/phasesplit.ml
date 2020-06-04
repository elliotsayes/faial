type 'a expr =
| Value of 'a
| Decr of 'a expr
| Variable of string

type 'a bexpr =
| True
| And of 'a bexpr * 'a bexpr
| GtZero of 'a expr

module type EXPR =
  sig
    type t
    (* Stringify *)
    val to_string: t -> string
    (* Identity *)
    val to_int: t -> int
    (* Evaluate the expression *)
    val to_expr: int -> t expr
  end

(* Expressions *)
let rec expr_to_string f (e: 'a expr) =
  match e with
  | Value n -> f n
  | Decr e -> expr_to_string f e ^ " - 1"
  | Variable x -> x
(* Boolean Expressions *)
let rec bexpr_to_string f (e: 'a bexpr) =
  match e with
  | True -> "True"
  | And (e1,e2) -> bexpr_to_string f e1 ^ " & " ^ bexpr_to_string f e2
  | GtZero e -> expr_to_string f e ^ " > 0"

let rec subst (name: string) (value: 'a expr) (e:'a expr) =
  match e with
  | Variable x ->
    if x=name then value
    else e
  | Decr x -> Decr (subst name value x)
  | Value x -> e

let rec eval_expr f (e:'a expr) :int =
  match e with
  | Value n -> f n
  | Decr e -> (eval_expr f e) - 1
  | Variable x -> failwith "Eval Expr"

let rec expr_to_bexpr (e:'a expr) = GtZero e



module TLang (E:EXPR) = struct

  type access = {
    ac_codelines: E.t expr;
    ac_conditions: E.t bexpr;
  }

  type unsync_instruction =
  | Loop of (E.t expr) * (unsync_instruction list)
  | Access of access

  type instruction =
  | Loop of (E.t expr) * (instruction list)
  | Phased of (unsync_instruction list)

  type t = instruction list

  let rec to_string (s:t) =
    let rec to_string_indent (s:t) indent =
      let rec ui_to_string l indent =
        let rec uts_aux l accum indent =
          let rec list_to_string li =
            match li with
            | [] -> ""
            | li::[] -> (expr_to_string E.to_string li)
            | li::lis -> (expr_to_string E.to_string li)^","^(list_to_string lis)
          in
          match l with
          | [] -> accum
          | (Access ac)::xs -> uts_aux xs (accum^String.make (indent*2) ' '^"Access "^(expr_to_string E.to_string ac.ac_codelines)^" if ["^(bexpr_to_string E.to_string ac.ac_conditions)^"];\n") indent
          | (Loop (n,body))::xs -> uts_aux xs (accum^String.make (indent*2) ' '^"Loop " ^ (expr_to_string E.to_string n) ^ " {\n" ^ (uts_aux body "" (indent+1)) ^ String.make (indent*2) ' '^"}\n") indent
        in
        uts_aux l "" indent
      in
      match s with
      | (Loop (n,t1))::l -> String.make (indent*2) ' '^"Loop " ^ (expr_to_string E.to_string n) ^ " {\n" ^ (to_string_indent t1 (indent+1)) ^ String.make (indent*2) ' '^"}\n" ^ (to_string_indent l indent)
      | (Phased ui_list)::l -> String.make (indent*2) ' '^"Phased " ^ "{\n" ^ (ui_to_string ui_list (indent+1)) ^ String.make (indent*2) ' '^"}\n" ^ (to_string_indent l indent)
      | [] -> ""
    in
    to_string_indent s 0


  let accessCreate (codl:E.t expr) (cndl:E.t bexpr) =
    {ac_codelines=codl;ac_conditions=cndl}

  let getCodelines (ac:access) =
    ac.ac_codelines

  let getConditions (ac:access) =
    ac.ac_conditions

  let rec run (s:t) =
    let rec run_aux (s:t) accum =
      match s with
      | (Phased n)::l -> run_aux l ((extract_unsync n [])::accum)
      | (Loop (e,t1))::l ->
        let n = eval_expr E.to_int e in
        if n > 0 then run_aux (t1@(Loop (Decr (E.to_expr n),t1))::l) accum
        else run_aux l accum
      | [] -> accum

    and extract_unsync (x:(unsync_instruction) list) accum =
      match x with
      | (Access acc)::xs -> extract_unsync xs ((acc.ac_codelines)::accum)
      | (Loop (e,t))::xs ->
        let n = eval_expr E.to_int e in
        if n > 0 then extract_unsync (t@(Loop (Decr (E.to_expr n),t))::xs) accum
        else extract_unsync xs accum
      | [] -> List.rev accum

    in
    let accum = run_aux s [] in
    let ret = List.rev accum in
    ret

end


module SLang (E:EXPR) = struct

  module T = TLang(E)

  type instruction =
  | Codeline of (E.t expr) * (E.t bexpr)
  | Sync
  | Loop of (E.t expr) * ((instruction) list)

  type t = (instruction) list

  let rec to_string (s:t) =
    let rec to_string_indent (s:t) indent =
      let rec list_to_string (n:(E.t expr)) c =
        (expr_to_string E.to_string n) ^ ", if [" ^ (bexpr_to_string E.to_string c) ^ "]"
      in
      match s with
      | Sync::l ->  String.make (indent*2) ' '^"Sync;\n" ^ (to_string_indent l indent)
      | (Loop (n,t1))::l ->  String.make (indent*2) ' '^"Loop " ^ (expr_to_string E.to_string n) ^ " (\n" ^ (to_string_indent t1 (indent+1)) ^ String.make (indent*2) ' '^ ")\n" ^ (to_string_indent l indent)
      | (Codeline (n,c))::l ->  String.make (indent*2) ' '^"Codeline " ^ (list_to_string n c) ^ ";\n" ^ (to_string_indent l indent)
      | [] -> ""
    in
    to_string_indent s 0

  let rec run (s:t) =
    let rec run_aux (s:t) (accum,phase) =
      match s with
      | (Loop (e,t1))::l ->
          let n = eval_expr E.to_int e in
          if n > 0 then run_aux (t1@(Loop (Decr (E.to_expr n),t1))::l) (accum,phase)
          else run_aux l (accum,phase)
      | (Codeline (n,c))::l -> run_aux l (accum,n::phase)
      | Sync::l -> run_aux l (phase::accum,[])
      | [] -> (accum,phase)
    in
    let (accum,phase) = run_aux s ([],[]) in
    let ret = phase::accum in
    let ret = List.rev ret in
    List.map List.rev ret


  let rec translate (s:t) =
    let rec injectCondition s n =
      match s with
      | [] -> []
      | (Codeline (n',c'))::xs ->
          (Codeline (n',And (c',n)))::(injectCondition xs n)
      | (Sync)::xs -> (Sync)::(injectCondition xs n)
      | (Loop (n',b))::xs ->
          (Loop (n',injectCondition b (And (expr_to_bexpr n',n))))::(injectCondition xs n)
    in

    let rec normalize1 s =
      match s with
      | Sync -> (Some [Sync],[])
      | Codeline (n,c) -> (None, [Codeline (n,c)])
      | Loop (n,body) ->
          (match normalize body with
            | (Some p1, p2) ->
              let p1' = injectCondition p1 (expr_to_bexpr n) in
              let p2' = injectCondition p2 (expr_to_bexpr n) in
              ( Some (p1'@[Loop (Decr n,p2@p1)]) , p2')
            | (None, _) -> (None, [s])
            (*| _ -> (None,[])  (* To remove non-exhaustive match warning *)*)
          )
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
      | (Sync)::l -> translate_aux l (accum@[(T.Phased phase)]) []
      | (Codeline (n,c))::l -> translate_aux l accum (phase@[(T.Access (T.accessCreate n c))])
      | (Loop (n,t))::l -> translate_aux l (accum@[T.Loop (n,(translate_aux t [] []))]) []
      | [] -> accum
    in

    let (before,after) = normalize s in
    match before with
    | None -> (translate_aux (after@[Sync]) [] [])
    | (Some b) -> (translate_aux ((b@after)@[Sync]) [] [])
end

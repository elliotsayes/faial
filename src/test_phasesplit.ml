open Phasesplit

module IntExpr = struct
  type t = int

  let to_string =  string_of_int
  let to_int (x:t) : int  = x
  let to_expr (x:int) : t expr = Value x
end

module S = SLang(IntExpr)
module T = TLang(IntExpr)

let sexample1 : S.t =
  let open S in
  [Loop (Variable "x",Value 0, Value 3,
          [Codeline (Variable "x",True);Sync;Codeline (Variable "y",True);Sync;Codeline (Value 3,True)]
        )]

let sexample2 : S.t =
  let open S in
  [Codeline (Value 1,True);(Loop (Variable "x",Value 1, Value 3,
          [Codeline (Value 2,True);Codeline (Value 3,True);Sync;Codeline (Value 4,True)]
        ))]



(*
let sexample3 : SLang.t =
  (* from SLang import * *)
  let open SLang in
  [Codeline 0;Loop (3, [Codeline 1;Sync;Codeline 2;Sync;Codeline 3]);Codeline 4]

let sexample4 : SLang.t =
  let open SLang in
  [Codeline 0;Loop (4,[Sync;Codeline 1;Codeline 2]);Codeline 3]

(* Loop with no SYNC *)
let sexample5 : SLang.t =
  let open SLang in
  [Codeline 1;Codeline 11;(Loop (4,
          [Codeline 2;Codeline 3;Codeline 4]
        ))]
*)
let sexample6 : S.t =
  let open S in
  [Codeline (Value (-1),True);(Loop (Variable "y",Value 0, Value 2,
          sexample1
        ));Codeline (Value 5,True)]
(*
let sexample6' : SLang.t =
  let open SLang in
  [Codeline (-1);Codeline 0; Codeline 1; Sync;
   Loop (2, [Codeline 2;Sync;Codeline 3;Codeline 1;Sync]);
   Codeline 2; Sync;
   Loop (3,[
     Codeline 3;Codeline 4;Codeline 0;Codeline 1;Sync;
     Loop (2,[Codeline 2;Sync;Codeline 3;Codeline 1;Sync]);
     Codeline 2;Sync]);
   Codeline 3;Codeline 4;Codeline 5;Sync]

let sexample7 : SLang.t =
  let open SLang in
  [Codeline 1; Sync;
    Loop (2,Test
      [Loop (3,[
        Loop (4,[Codeline 2;Sync;Codeline 3])
        ]); Codeline 4
      ]); Sync; Codeline 5
    ;Codeline 6]

let sexample8 : SLang.t =
  let open SLang in
  [Codeline 1; Sync; Loop (0,[Codeline 5;Loop (0,[Codeline 6;]);Sync]); Codeline 2]

let sexample9 : SLang.t =
  let open SLang in
  [Codeline 1; Sync; Loop (1,[Codeline 5;Loop (4,[Codeline 6;Sync])]); Codeline 2]

let texample1 : TLang.t =
  let open TLang in
  [Phased {ph_codelines=[1;2;3];ph_conditions=[]};Phased {ph_codelines=[4;5];ph_conditions=[]}]
*)

(*-------------- Helper Functions ---------------------------------------*)

(* Print Lists *)
let print_list l =
  let l = List.map (expr_to_string IntExpr.to_string) l in
  List.fold_left (fun x a -> x^", "^a) "" l |> print_endline

let print_list_list =
  List.iter (fun x -> print_list x)

(* Tried an easy check for list equality - doesn't work for traces.
Probably due to the order of how the traces are built up. *)
let rec list_equal a b =
  match a,b with
  | [],[] -> true
  | [],y -> (match y with
      | [[]] -> true
      | _ -> false
      )
  | x,[] -> false
  | x::xs,y::ys ->
    if (x=y) then
      true && (list_equal xs ys)
    else
      false

let print_compare_output (ex: S.t) =
  print_endline ("----------------\nSLang:\n----------------\n"^(S.to_string ex));
  print_endline ("----------------\nTLang:\n----------------\n"^(T.to_string (S.translate ex)))

let print_compare_trace (ex:S.t) =

  print_endline ("----------------\nSLang:\n----------------");
  print_list_list (S.run ex);
  print_endline ("\n----------------\nTLang:\n----------------");
  print_list_list (T.run (S.translate ex));

  if (list_equal (S.run ex) (T.run (S.translate ex)))=true
  then print_endline "Traces are equal." else print_endline "Traces are NOT equal!"

(*-------------- Tests ----------------------------------------------------*)

let aa = print_compare_output sexample1
(*let aa = (TLang.run (SLang.translate sexample6))*)

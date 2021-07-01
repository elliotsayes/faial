
(*
  _____________________    read-index     _____________________
  x[threadIdx.x] = threadIdx.x;
  int z = x[threadIdx.x];
  x[z] = 0;
  ______________________________________________________________
 *)

(*
x[99] = 99;           ( 99,wr[99],99 )
int z = x[99];        ( 99,rd[99],99 )  //laswrite_H,99(P,99) = 99
x[99] = 0;            ( 99,wr[99],99 )
*)


(*
(
*)
open Babycuda
open Exp

(*
  wr[0] = 1;
  x = rd[0];
  skip
*)
let test1 =
  (Seq (
    (Write (Num 0,Num 1))
    ,
    (Read (var_make "x",Num 0,Skip))
    )
  )

(*
  wr[0] = 11;
  wr[tid] = 22;
  x = rd[0];
  skip
*)
let test2 =
  (Seq (
    (Write (Num 0,Num 11))
    ,
    (Seq (
      (Write (Var (var_make "tid"),Num 22))
      ,
      (Read (var_make "x",Num 0 ,Skip))
      )
    )
    )
  )


let acc_to_idx_string (acc: access_t) =
  string_of_int acc.access_index
let acc_to_data_string (acc: access_t) =
  string_of_int acc.access_data


(* user selects index *)
let select_index x : int =
  (* let int_x = List.map (fun a -> a.access_data) x in *)
  let length_data = LWMap.cardinal x in
  let (xkey,xacc) = LWMap.choose x in
  let () = print_string ((string_of_int length_data)
    ^ " unique values found written to index "
    ^ (acc_to_idx_string xacc)
    ^ ". Values are:\n[ "
    )
  in
  let () = LWMap.iter
    (fun xkey xacc -> print_string (
        "tid="
        ^ (string_of_int xkey) ^ ",WR["
        ^ (acc_to_idx_string xacc) ^ "]="
        ^ (acc_to_data_string xacc) ^ " ; " ))
    x
  in
  let () = print_string "]\nTo choose one, type its index from the list above.\n\n"
  in
  let chosen_index = read_int () in
  (* let () = print_string "\nRead-Accesses:\n" in *)
  (* List.nth (List.map (fun a -> a.access_data) x) chosen_index *)
  let rec iter_map ml count =
    match ml with
    | [] -> failwith "No value at index!"
    | ml::mls -> if count=chosen_index then ml.access_data
      else iter_map mls (count+1)
  in
  iter_map (LWMap.bindings x |> List.map (fun (_,z) -> z)) 0

(* let _ = my_eval select_index [] 0 [] test2 |> my_print *)
let number_of_threads = 3
let _ = u_par select_index test2 number_of_threads |> my_print

(* concat 0..k threads *)
(*
- see are data races still indentifiable by looking at the program state.
- can we run a data race program, and observe it there?
- each phase
*)

(*
x[0] = tid_1;                         x[0] = tid_2;

sync;                                 sync;
x[0] = tid_1;                         x[0] = tid_2;
x[0] = tid_1+1;                       x[0] = tid_2+1;
read_value = x[0];                    read_value = x[0];
read value =                           read_value =
*)

(*
x[0]
x[0] = tid;
x[0] = tid+1;
read_value = x[0];

*)

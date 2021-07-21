open Babycuda
open Exp
open Serialize

(*
  wr[0] = 1;
  x = rd[0];
  skip
*)
let test1 =
S_unsynced_inst
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
S_unsynced_inst
  (Seq (
    (Write (Num 0,Num 11))
    ,
    (Seq (
      (Write (Var (var_make "tid"),Num 22))
      ,
      (Read (var_make "x",Num 0 ,Skip))
      )
    ))
  )

(*
  wr[tid] = tid;
  x = rd[tid];
  wr[x+1] = 0;
*)
let test3 =
  let tid = (Var (var_make "tid")) in
  let x = (Var (var_make "x")) in
  S_unsynced_inst
  (Seq (
    (Write (tid,tid)
    ,
      (Read (var_make "x",tid,
        (Write (Bin (Plus,x,Num 1),Num 0)
        )
      ))
    )
  ))
(*
  wr[tid] = tid;
  x = rd[tid];
  wr[x] = 0;
*)
let test4 =
  let tid = (Var (var_make "tid")) in
  let x = (Var (var_make "x")) in
  S_unsynced_inst
  (Seq (
    (Write (tid,tid)
    ,
      (Read (var_make "x",tid,
        (Write (x,Num 0)
        )
      ))
    )
  ))


let test5 =
  let tid = (Var (var_make "tid")) in
  let x = (Var (var_make "x")) in
  (S_unsynced_inst (
  (Seq (
    (Write (tid,tid)
    ,
      (Read (var_make "x",tid,
        (Write (x,Num 33)
        )
      ))
    )
  ))))

let test6 =
  let tid = var_make "tid" in
  let r1 = {
    range_var=var_make "r";
    range_lower_bound=Num 0;
    (* range_upper_bound=Num N; *)
    range_upper_bound=Var (var_make "N");
    range_step=Default (Num 1);}
  in
  let r2 = {
    range_var=var_make "i";
    range_lower_bound=Num 0;
    (* range_upper_bound=Num 3; *)
    range_upper_bound=Var (var_make "M");
    range_step=Default (Num 1);}
  in
  let r3 = {
    range_var=var_make "j";
    range_lower_bound=Num 0;
    (* range_upper_bound=Num 3; *)
    range_upper_bound=Var (var_make "M");
    range_step=Default (Num 1);}
  in
  (S_Loop (r1,
    S_Seq (
        S_unsynced_inst (
          (Loop (r2,Write (Var tid,Num 0)))
          )
      ,
      S_Seq (S_Sync,
        S_unsynced_inst (Loop (r3, Read (var_make "a",
            Bin (Plus,Var tid,Var (var_make "j")),
         Skip
        )))
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
  let rec iter_map ml count =
    match ml with
    | [] -> failwith "No value at index!"
    | ml::mls -> if count=chosen_index then ml.access_data
      else iter_map mls (count+1)
  in
  iter_map (LWMap.bindings x |> List.map (fun (_,z) -> z)) 0

let get_first_binding x : int =
  let rec iter_map ml =
    match ml with
    | [] -> failwith "No value in History!"
    | ml::mls -> ml.access_data
  in
  iter_map (LWMap.bindings x |> List.map (fun (_,z) -> z))

let run_tests f test num_threads =
  let x = s_eval f test num_threads in
  print_string ("All Accesses ("^(string_of_int num_threads)^" Threads):\n");
  List.iter print_phase x;
  let a = List.map find_data_race x in
  List.iter dr_print_phase_list a

(* let () = run_tests get_first_binding test5 3 *)
(* let () = PPrint.print_doc (Proto.prog_to_s (translate test6)) *)
let () = print_endline (string_of_bool (Babycuda.check test2))

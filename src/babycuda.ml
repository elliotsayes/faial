open Exp
open Subst

type unsync_inst =
  | Read of variable * nexp * unsync_inst
  | Write of nexp * nexp
  | Skip
  | Seq of unsync_inst * unsync_inst
  | Loop of range * unsync_inst

type access_t = {
  access_owner: int;
  access_mode: Exp.mode;
  access_index: int;
  access_data: int;
}

module LWMap = Map.Make(
  struct type t = int
  let compare = Stdlib.compare
  end)

type phase_t = access_t list
type history_t = phase_t list

(* -----------------------------------*)

let rec print_phase (p:phase_t) =
  let mode_to_string m =
    match m with
    | Exp.W -> "WR"
    | Exp.R -> "RD"
  in
  let rec my_print_aux p =
    match p with
    | [] -> ""
    | x::xs -> (
        "\ttid=" ^ (string_of_int x.access_owner) ^ "," ^
          (mode_to_string x.access_mode) ^
          "[" ^ (string_of_int x.access_index) ^ "]="^
          (string_of_int x.access_data) ^ "\n"
        ^ (my_print_aux xs)
        )
    in
    my_print_aux p |> print_string

let rec dr_print_phase_list pl =
  match pl with
  | [] -> print_phase []
  | pl::pls ->
    begin
      (match pl with
      | pl1::[pl2] ->
        (if pl1.access_data=pl2.access_data then
          print_string "-(benign data-race)-----\n"
        else
          print_string "------------------------\n");
      | _ -> print_string "------------------------\n"
      );
      print_phase pl;
      dr_print_phase_list pls
    end

(* ==========================
            UN-SYNC
========================== *)

(*  substitution *)
let rec unsync_subst (var: variable) (value: nexp) (instruction: unsync_inst) : unsync_inst =
  match instruction with
  | Skip -> instruction
  | Seq (u1, u2) ->
      let p1 = unsync_subst var value u1 in
      let p2 = unsync_subst var value u2 in
      Seq (p1, p2)
  | Read (x, e, u) ->
    let u = if x=var then u
      else unsync_subst var value u
    in
    Read (x, ReplacePair.n_subst (var,value) e, u)
  | Write (e1, e2) -> Write (ReplacePair.n_subst (var,value) e1, ReplacePair.n_subst (var,value) e2)
  | Loop (r, u) ->
    let r = ReplacePair.r_subst (var,value) r in
    let u = if r.range_var=var then u
      else unsync_subst var value u
    in
    Loop (r, u)

(* removes duplicate access, oldest first (used in lastwrite)*)
let most_recent_write (in_arr: access_t list) =
  let rec most_recent_write_aux (in_arr: access_t list) (accum: 'a LWMap.t) =
    match in_arr with
    | [] -> accum
    | x::xs -> if (not (LWMap.mem x.access_owner accum)) then
        most_recent_write_aux xs (LWMap.add x.access_owner x accum)
      else
        most_recent_write_aux xs accum
  in
  most_recent_write_aux in_arr LWMap.empty

let rec lastwrite (f: 'a LWMap.t -> int) (h: history_t) (owner:int) (p:phase_t) (index:int) =
  let cur_phase_data =
    List.filter (fun a -> a.access_mode=Exp.W && a.access_index=index) p
      |> most_recent_write
  in
  let length_data = cur_phase_data |>  LWMap.cardinal
  in
  if length_data = 0 then
    match h with
    | h1::hs -> lastwrite f hs owner h1 index
    | [] -> failwith "Lastwrite Error! History has no writes to specified index."
  else
    f cur_phase_data


let rec u_eval (f: 'a LWMap.t -> int) (h: history_t) (i:int) (p:phase_t) (instruction: unsync_inst) : phase_t =
  match instruction with
  | Skip -> p
  | Seq (u1, u2) ->
      let p1 = u_eval f h i p u1 in
      u_eval f h i p1 u2
  | Read (x, e, u) ->
      let j = n_eval e in
      let v = lastwrite f h i p j in
      let u' = unsync_subst x (Num v) u in
      let p1 = u_eval f h i p u' in
      let new_read = {
        access_owner=i;
        access_mode=Exp.R;
        access_index=j;
        access_data=v} in
      new_read :: p1
  | Write (e1, e2) ->
      let j = n_eval e1 in
      let v = n_eval e2 in
      let new_write = {
        access_owner=i;
        access_mode=Exp.W;
        access_index=j;
        access_data=v} in
      new_write :: p
  | Loop (r, u) ->
      let b = b_eval (Exp.n_ge r.range_lower_bound r.range_upper_bound) in
      if b then
        p
      else
        let u' = unsync_subst r.range_var r.range_lower_bound u in
        let p1 = u_eval f h i p u' in
        let new_range =
          {
            range_var = r.range_var;
            range_lower_bound = Num ((n_eval r.range_lower_bound) + 1);
            range_upper_bound = r.range_upper_bound;
            range_step = Default (Num 1)
          } in
        let u2 = Loop (new_range, u) in
        u_eval f h i p1 u2

(* return adverse data-races and benign data-races *)
let rec find_data_race (p:phase_t) : (phase_t list) =
  let is_data_race (a:phase_t) : bool =
    let a1 = List.hd a in
    let a2 = List.hd (List.tl a) in
    a1.access_owner<>a2.access_owner
      && a1.access_index=a2.access_index
        && (a1.access_mode=Exp.W || a2.access_mode=Exp.W)
  in
  let rec find_data_race_aux p =
    let rec find_data_race_aux_inner (p':access_t) (ps:phase_t) =
      List.map (fun x -> p'::[x]) ps |> List.filter is_data_race
    in
    match p with
    | [] -> []
    | p'::ps ->
        (find_data_race_aux_inner p' ps) @ (find_data_race_aux ps)
  in
  let result = find_data_race_aux p in
  print_string ("\n" ^(string_of_int (List.length result))^ " Data Races Found:\n");
  result

let rec u_par (f: 'a LWMap.t -> int) (instruction: unsync_inst) (threads: int) : phase_t =
  let rec u_par_aux tid_order p =
    match tid_order with
    | [] -> []
    | x::xs ->
      let p' = u_eval f [] x p (unsync_subst (var_make "tid") (Num x) instruction) in
      p'@(u_par_aux xs [])
    (* if count<0 then []
    else
      (u_eval f [] count [] (unsync_subst (var_make "tid") (Num count) instruction))
        @ (u_par_aux (count-1)) *)
  in
  (* let s_threads = (Common.shuffle (Common.range 0 (threads-1))) in *)
  let s_threads = (Common.range 0 (threads-1)) in
  (* let string_s_threads = List.map (string_of_int) s_threads
    |> List.fold_left (fun x y -> x ^ ", " ^ y)
    ""
  in *)
  (* let () = print_string ("Evaluating threads in this order: "^string_s_threads^"\n") in *)
  u_par_aux s_threads []

(* ==========================
            SYNC
========================== *)

type sync_inst =
  | S_unsynced_inst of unsync_inst
  | S_Sync
  | S_Seq of sync_inst * sync_inst
  | S_Loop of range * sync_inst

(*  substitution *)
let rec sync_subst (var: variable) (value: nexp) (instruction: sync_inst) : sync_inst =
  match instruction with
  | S_unsynced_inst u -> S_unsynced_inst (unsync_subst var value u)
  | S_Sync -> instruction
  | S_Seq (u1, u2) ->
      let p1 = sync_subst var value u1 in
      let p2 = sync_subst var value u2 in
      S_Seq (p1, p2)
  | S_Loop (r, u) ->
    let r = ReplacePair.r_subst (var,value) r in
    let u = if r.range_var=var then u
      else sync_subst var value u
    in
    S_Loop (r, u)

let rec s_eval (f: 'a LWMap.t -> int) (instruction: sync_inst) (threads: int) =
  match instruction with
  | S_unsynced_inst u -> [ u_par f u threads ]
  | S_Sync -> [[];[]]
  | S_Seq (u1, u2) ->
      let h1 = s_eval f u1 threads in
      let h2 = s_eval f u2 threads in
      h1 @ h2
  | S_Loop (r, u) ->
      let b = b_eval (Exp.n_ge r.range_lower_bound r.range_upper_bound) in
      if b then
        [[]]
      else
        let u' = sync_subst r.range_var r.range_lower_bound u in
        let h = s_eval f u' threads in
        let new_range =
          {
            range_var = r.range_var;
            range_lower_bound = Num ((n_eval r.range_lower_bound) + 1);
            range_upper_bound = r.range_upper_bound;
            range_step = Default (Num 1)
          } in
        let u2 = S_Loop (new_range, u) in
        let h' = s_eval f u2 threads in
        h @ h'


let rec translate (inst: sync_inst) : Proto.prog =
  let rec unsync_translate (un_inst: unsync_inst) =
    match un_inst with
    | Skip -> []
    | Seq (u1, u2) -> (unsync_translate u1) @ (unsync_translate u2)
    | Read (x, e, u) ->
      [ Proto.Acc (var_make "arr", {access_index=[e]; access_mode=Exp.R}) ] @ unsync_translate u
    | Write (e1, e2) ->
      [ Proto.Acc (var_make "arr", {access_index=[e1]; access_mode=Exp.W}) ]
    | Loop (r, u) -> [Proto.Loop (r, unsync_translate u)]
  in
  match inst with
  | S_unsynced_inst u -> unsync_translate u
  | S_Sync -> [Proto.Sync]
  | S_Seq (u1, u2) -> (translate u1) @ (translate u2)
  | S_Loop (r, u) -> [Proto.Loop (r, translate u)]

(* ==========================
  Check (false if handled by Faial)
========================== *)

module CheckMap = Map.Make(
  struct type t = Exp.nexp
  let compare = Stdlib.compare
  end)

let rec check (inst: sync_inst) : bool =
  let rec unsync_check (u_inst: unsync_inst) (s: VarSet.t) =
    match u_inst with
    | Skip -> true
    | Seq (u1, u2) ->
      unsync_check u1 s && unsync_check u2 s
    | Read (x, e, u) ->
      let new_s = VarSet.add x s in
      unsync_check u new_s
    | Write (e1, e2) ->
      let fs : VarSet.t = Freenames.free_names_nexp e1 VarSet.empty in
      let vs = VarSet.inter fs s in
      VarSet.is_empty vs
    | Loop (r, u) ->
      let b = b_eval (Exp.n_ge r.range_lower_bound r.range_upper_bound) in
      if b then
        true
      else
        let u' = unsync_subst r.range_var r.range_lower_bound u in
        let new_range =
          {
            range_var = r.range_var;
            range_lower_bound = Num ((n_eval r.range_lower_bound) + 1);
            range_upper_bound = r.range_upper_bound;
            range_step = Default (Num 1)
          } in
        let u2 = Loop (new_range, u) in
        unsync_check u' s && unsync_check u2 s
  and sync_check (s_inst: sync_inst) : bool =
    match inst with
    | S_unsynced_inst u -> unsync_check u VarSet.empty
    | S_Sync -> true
    | S_Seq (u1, u2) -> sync_check u1 && sync_check u2
    | S_Loop (r, u) ->
        let b = b_eval (Exp.n_ge r.range_lower_bound r.range_upper_bound) in
        if b then
          true
        else
          let u' = sync_subst r.range_var r.range_lower_bound u in
          let new_range =
            {
              range_var = r.range_var;
              range_lower_bound = Num ((n_eval r.range_lower_bound) + 1);
              range_upper_bound = r.range_upper_bound;
              range_step = Default (Num 1)
            } in
          let u2 = S_Loop (new_range, u) in
          sync_check u' && sync_check u2
  in
  sync_check inst

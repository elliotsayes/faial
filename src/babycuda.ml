open Exp
open Subst

type inst =
  | Read of variable * nexp * inst
  | Write of nexp * nexp
  | Skip
  | Seq of inst * inst
  | Loop of range * inst

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

let rec my_print (p:phase_t) =
  let mode_to_string m =
    match m with
    | Exp.W -> "WR"
    | Exp.R -> "RD"
  in
  let rec my_print_aux p =
    match p with
    | [] -> ""
    | x::xs -> (
        "tid=" ^ (string_of_int x.access_owner) ^ "," ^
          (mode_to_string x.access_mode) ^
          "[" ^ (string_of_int x.access_index) ^ "]="^
          (string_of_int x.access_data) ^ "\n"
        ^ (my_print_aux xs)
        )
    in
    my_print_aux p |> print_string

(*  substitution *)
let rec i_subst (var: variable) (value: nexp) (instruction: inst) : inst =
  match instruction with
  | Skip -> instruction
  | Seq (u1, u2) ->
      let p1 = i_subst var value u1 in
      let p2 = i_subst var value u2 in
      Seq (p1, p2)
  | Read (x, e, u) ->
    let u = if x=var then u
      else i_subst var value u
    in
    Read (x, ReplacePair.n_subst (var,value) e, u)
  | Write (e1, e2) -> Write (ReplacePair.n_subst (var,value) e1, ReplacePair.n_subst (var,value) e2)
  | Loop (r, u) ->
    let r = ReplacePair.r_subst (var,value) r in
    let u = if r.range_var=var then u
      else i_subst var value u
    in
    Loop (r, u)

(* removes duplicate access, oldest first (used in lastwrite)*)
let unique_latest (in_arr: access_t list) =
  let rec unique_latest_aux (in_arr: access_t list) (accum: 'a LWMap.t) =
    match in_arr with
    | [] -> accum
    | x::xs -> if (not (LWMap.mem x.access_owner accum)) then
        unique_latest_aux xs (LWMap.add x.access_owner x accum)
      else
        unique_latest_aux xs accum
  in
  unique_latest_aux in_arr LWMap.empty

let rec lastwrite (f: 'a LWMap.t -> int) (h: history_t) (owner:int) (p:phase_t) (index:int) =
  let cur_phase_data =
    List.filter (fun a -> a.access_mode=Exp.W && a.access_index=index) p
      |> unique_latest
  in
  let length_data = cur_phase_data |>  LWMap.cardinal
  in
  if length_data = 0 then
    match h with
    | h1::hs -> lastwrite f hs owner h1 index
    | [] -> failwith "Lastwrite Error! History has no writes to specified index."
  else
    f cur_phase_data


let rec my_eval (f: 'a LWMap.t -> int) (h: history_t) (i:int) (p:phase_t) (instruction: inst) : phase_t =
  match instruction with
  | Skip -> p
  | Seq (u1, u2) ->
      let p1 = my_eval f h i p u1 in
      my_eval f h i p1 u2
  | Read (x, e, u) ->
      let j = n_eval e in
      let v = lastwrite f h i p j in
      let u' = i_subst x (Num v) u in
      let p1 = my_eval f h i p u' in
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
        let u' = i_subst r.range_var r.range_lower_bound u in
        let p1 = my_eval f h i p u' in
        let new_range =
          {
            range_var = r.range_var;
            range_lower_bound = Num ((n_eval r.range_lower_bound) + 1);
            range_upper_bound = r.range_upper_bound;
            range_step = Default (Num 1)
          } in
        let u2 = Loop (new_range, u) in
        my_eval f h i p1 u2


let rec find_data_race (p:phase_t) : bool =
  match p with
  | [] -> false
  | p::ps -> true

let rec u_par (f: 'a LWMap.t -> int) (instruction: inst) (threads: int) : phase_t =
  let rec u_par_aux count =
    if count<0 then []
    else
      (my_eval f [] count [] (i_subst (var_make "tid") (Num count) instruction))
        @ (u_par_aux (count-1))
  in
  u_par_aux (threads-1)

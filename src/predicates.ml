open Common
open Exp

type 'a codegen = {
  codegen_arg: string;
  codegen_body: 'a;
}

type step_handler = {
  step_handler_name: string;
  step_handler_body: nexp -> nexp;
  step_handler_inc: nexp -> nexp;
  step_handler_dec: nexp -> nexp;
  step_handler_trunc: nexp -> nexp;
}

type t = {
  pred_name: string;
  pred_body: nexp -> bexp;
  pred_step: step_handler option;
}

let pred_to_codegen (pred:t) : bexp codegen =
  {
    codegen_arg = "x";
    codegen_body = pred.pred_body (Var (var_make "x"));
  }

let step_to_codegen (s:step_handler) : nexp codegen =
  {
    codegen_arg = "x";
    codegen_body = s.step_handler_body (Var (var_make "x"));
  }


let is_even n =
  n mod 2 = 0

let pow base exponent =
  if exponent < 0 then invalid_arg "exponent can not be negative" else
  let rec aux accumulator base = function
    | 0 -> accumulator
    | 1 -> base * accumulator
    | e when is_even e -> aux accumulator (base * base) (e / 2)
    | e -> aux (base * accumulator) (base * base) ((e - 1) / 2) in
  aux 1 base exponent

let eq_nums x l : bexp =
  List.map (fun i -> n_eq x (Num i)) l
  |> b_or_ex

let gen_pow (base:int) (n:nexp) : bexp =
  let ub = 0xFFFFFFFF in
  (* Generate a list of powers *)
  let rec pows (n:int) : int list =
    let x = pow base n in
    if x > ub then []
    else if x == ub then [x]
    else x :: pows (n + 1)
  in
  pows 0 |> eq_nums n

let highest_power (base:int) (n:int) : int =
  let exponent : float = Float.log(Float.of_int n)
    /. Float.log(Float.of_int base) in
  pow base (Float.to_int(exponent))

let all_predicates : t list =
  let mk_uint size : t =
    let n : string = "uint" ^(string_of_int size) in
    let b (x:nexp) : bexp = n_le x (Num ((pow 2 size) - 1)) in
    {
        pred_name = n;
        pred_body = b;
        pred_step = None;
    }
  in
  List.map (fun (base:int) ->
    let gen_name = "trunc_" ^ string_of_int base in
    let trunc = function
      | Num n -> Num (highest_power base n)
      | e -> NCall (gen_name, e)
    in
    let rec gen (n:int) (x:nexp) : nexp =
      if n <= 0 then (Num 1)
      else
        let p = Num (pow base n) in
        NIf (n_gt x p, p, gen (n - 1) x)
    in
    let inc (x:nexp) : nexp = n_mult x (Num base) in
    let dec (x:nexp) : nexp = n_div x (Num base) in
    let handler =
      {
        step_handler_body = gen base;
        step_handler_name = gen_name;
        step_handler_inc = inc;
        step_handler_dec = dec;
        step_handler_trunc = trunc;
      }
    in
    { pred_name = "pow" ^ string_of_int base;
      pred_body = gen_pow base;
      pred_step = Some handler;
    }
  ) (range 2 4)
  @
  [
      mk_uint 32;
      mk_uint 16;
      mk_uint 8;
  ]

let make_pred_db (l:t list) : (string, t) Hashtbl.t =
  List.map (fun p-> (p.pred_name, p)) l
  |> Common.hashtbl_from_list

let all_predicates_db : (string, t) Hashtbl.t =
  make_pred_db all_predicates

let all_step_handlers_db : (string, step_handler) Hashtbl.t =
  all_predicates
  |> Common.map_opt (fun x ->
  match x.pred_step with
  | Some s -> Some (s.step_handler_name, s)
  | None -> None)
  |> hashtbl_from_list

let pred_call_opt (name:string) (n:nexp) : bexp option =
  match Hashtbl.find_opt all_predicates_db name with
  | Some p -> Some (p.pred_body n)
  | None -> None

let func_call_opt (name:string) (n:nexp) : nexp option =
  match Hashtbl.find_opt all_step_handlers_db name with
  | Some s -> Some (s.step_handler_body n)
  | None -> None

let get_predicates (b:bexp) : t list =
  let rec get_names (b:bexp) (preds:StringSet.t) : StringSet.t =
    match b with
    | Pred (x, _) -> StringSet.add x preds
    | BRel (_, b1, b2) -> get_names b1 preds |> get_names b2
    | BNot b -> get_names b preds
    | NRel (_, _, _)
    | Bool _ -> preds
  in
  get_names b StringSet.empty
  |> StringSet.elements
  |> List.map (Hashtbl.find all_predicates_db)

let get_functions (b:bexp) : step_handler list =
  let rec get_names_b (b:bexp) (ns:StringSet.t) : StringSet.t =
    match b with
    | BRel (_, b1, b2) -> get_names_b b1 ns |> get_names_b b2
    | BNot b -> get_names_b b ns
    | NRel (_, n1, n2) -> get_names_n n1 ns |> get_names_n n2
    | Bool _ | Pred _ -> ns
  and get_names_n (n:nexp) (ns:StringSet.t) : StringSet.t =
    match n with
    | Var _ | Num _ | Proj _ -> ns
    | Bin (_, n1, n2) -> get_names_n n1 ns |> get_names_n n2
    | NIf (b, n1, n2) -> get_names_b b ns |> get_names_n n1 |> get_names_n n2
    | NCall (x, _) -> StringSet.add x ns
  in
  get_names_b b StringSet.empty
  |> StringSet.elements
  |> List.map (Hashtbl.find all_step_handlers_db)

let get_step_handler (pred_name:string) : step_handler =
    let (p:t) = Hashtbl.find all_predicates_db pred_name in
    match p.pred_step with
    | Some h -> h
    | None -> failwith ("Cannot use predicate '" ^ pred_name ^ "' as a step expression.")

let step_inc (s:step_expr) : nexp -> nexp =
  match s with
  | Default n -> n_plus n
  | StepName pred_name -> (get_step_handler pred_name).step_handler_inc

let step_dec (s:step_expr) : nexp -> nexp =
  match s with
  | Default n -> n_minus n
  | StepName pred_name -> (get_step_handler pred_name).step_handler_dec

let step_trunc (s:step_expr) : nexp -> nexp =
  match s with
  | Default divisor -> fun n -> n_minus n (n_mod n divisor)
  | StepName pred_name -> (get_step_handler pred_name).step_handler_trunc

let range_last (r:range) : nexp =
  step_trunc r.range_step (n_minus r.range_upper_bound (Num 1))

let range_inc (r:range) : range =
  { r with range_lower_bound = n_plus r.range_lower_bound (Num 1) }

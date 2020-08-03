open Common
open Proto

type t = {
  pred_name: string;
  pred_body: nexp -> bexp;
}

type codegen = {
  codegen_arg: string;
  codegen_body: bexp;
}

let pred_to_codegen (pred:t) : codegen =
  {
    codegen_arg = "x";
    codegen_body = pred.pred_body (Var (var_make "x"));
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

let gen_pow base x : bexp =
  let ub = 0xFFFFFFFF in
  let rec pows n : int list =
    let x = pow base n in
    if x > ub then []
    else if x == ub then [x]
    else x :: pows (n + 1)
  in
  pows 0 |> eq_nums x

let all_predicates : t list =
  List.map (fun base ->
    { pred_name = "pow" ^ string_of_int base;
      pred_body = gen_pow base
    }
  ) (range 2 4)
  @
  [
    { pred_name = "uint32"; pred_body = fun x -> n_le x (Num 0xFFFFFFFF) };
    { pred_name = "uint16"; pred_body = fun x -> n_le x (Num 0xFFFF) };
    { pred_name = "uint8"; pred_body = fun x -> n_le x (Num 0xFF) };
  ]

let make_pred_db (l:t list) : (string, t) Hashtbl.t =
  List.map (fun p-> (p.pred_name, p)) l
  |> Common.hashtbl_from_list

let find_opt (name:string) : t option =
  let db = make_pred_db all_predicates in
  Hashtbl.find_opt db name

let call_opt (name:string) (n:nexp) : bexp option =
  match find_opt name with
  | Some p -> Some (p.pred_body n)
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
  let db = make_pred_db all_predicates in
  get_names b StringSet.empty
  |> StringSet.elements
  |> List.map (Hashtbl.find db)

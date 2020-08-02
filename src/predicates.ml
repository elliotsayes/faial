open Common
open Proto

type t = {
  pred_name: string;
  pred_arg: string;
  pred_body: bexp;
}

let mk_pred (name:string) (body:nexp -> bexp) : t =
  { pred_name = name; pred_arg = "x"; pred_body = body (Var (var_make "x")) }

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
  List.map (fun x ->
    mk_pred ("pow" ^ string_of_int x) (gen_pow x)
  ) (range 2 4)
  @
  [
    mk_pred "uint32" (fun x -> n_le x (Num 0xFFFFFFFF));
    mk_pred "uint16" (fun x -> n_le x (Num 0xFFFF));
    mk_pred "uint8" (fun x -> n_le x (Num 0xFF));
  ]

let make_pred_db (l:t list) : (string, t) Hashtbl.t =
  List.map (fun p-> (p.pred_name, p)) l
  |> Common.hashtbl_from_list


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

open Stage0

let (@) = Common.append_tr

type nexp =
  | Var of Variable.t
  | Num of int
  | Binary of N_binary.t * nexp * nexp
  | Unary of N_unary.t * nexp
  | NCall of string * nexp
  | NIf of bexp * nexp * nexp
  | Other of nexp
  | CastInt of bexp

and bexp =
  | Bool of bool
  | NRel of N_rel.t * nexp * nexp
  | BRel of B_rel.t * bexp * bexp
  | BNot of bexp
  | Pred of string * nexp
  | CastBool of nexp

let rec n_eval_res (n: nexp) : (int, string) Result.t =
  let ( let* ) = Result.bind in
  match n with
  | Var x -> Error ("n_eval: variable " ^ Variable.name x)
  | Num n -> Ok n
  | CastInt b ->
    let* b = b_eval_res b in
    Ok (if b then 1 else 0)
  | Unary (o, n) ->
    let* n = n_eval_res n in
    Ok (N_unary.eval o n)
  | Binary (o, n1, n2) ->
    let* n1 = n_eval_res n1 in
    let* n2 = n_eval_res n2 in
    Ok (N_binary.eval o n1 n2)
  | NCall (x,_) -> Error ("n_eval: call " ^ x)
  | NIf (b, n1, n2) ->
    let* b = b_eval_res b in
    if b then n_eval_res n1 else n_eval_res n2
  | Other _ -> Error "n_eval: other"

and b_eval_res (b: bexp) : (bool, string) Result.t =
  let (let*) = Result.bind in
  match b with
  | Bool b -> Ok b
  | CastBool n ->
    let* n = n_eval_res n in
    Ok (n <> 0)
  | NRel (o, n1, n2) ->
    let* n1 = n_eval_res n1 in
    let* n2 = n_eval_res n2 in
    Ok (N_rel.eval o n1 n2)
  | BRel (o, b1, b2) ->
    let* b1 = b_eval_res b1 in
    let* b2 = b_eval_res b2 in
    Ok (B_rel.eval o b1 b2)
  | BNot b ->
    let* b = b_eval_res b in
    Ok (not b)
  | Pred (x, _) ->
    Error ("b_eval: pred " ^ x)

let n_eval_opt (n: nexp) : int option =
  n_eval_res n |> Result.to_option

let b_eval_opt (b: bexp) : bool option =
  b_eval_res b |> Result.to_option

let n_eval (n: nexp) : int =
  match n_eval_res n with
  | Ok n -> n
  | Error e -> failwith e

let b_eval (b: bexp) : bool =
  match b_eval_res b with
  | Ok b -> b
  | Error e -> failwith e

let num (n:int) : nexp = Num n

let n_zero = Num 0

let n_lt (e1:nexp) (e2:nexp) : bexp =
  match e1, e2 with
  | Num e1, Num e2 -> Bool (e1 < e2)
  | _, _ -> NRel (Lt, e1, e2)

let n_gt (e1:nexp) (e2:nexp) : bexp =
  match e1, e2 with
  | Num e1, Num e2 -> Bool (e1 > e2)
  | _, _ -> NRel (Gt, e1, e2)

let n_le (e1:nexp) (e2:nexp) : bexp =
  match e1, e2 with
  | Num e1, Num e2 -> Bool (e1 <= e2)
  | _, _ -> NRel (Le, e1, e2)

let n_ge (e1:nexp) (e2:nexp) : bexp =
  match e1, e2 with
  | Num e1, Num e2 -> Bool (e1 >= e2)
  | _, _ -> NRel (Ge, e1, e2)

let n_eq (e1:nexp) (e2:nexp) : bexp =
  match e1, e2 with
  | CastInt e, Num 0 | Num 0, CastInt e-> BNot e
  | CastInt e, Num 1 | Num 1, CastInt e -> e
  | Num e1, Num e2 -> Bool (e1 = e2)
  | _, _ -> NRel (Eq, e1, e2)

let n_neq (e1:nexp) (e2:nexp) : bexp =
  match e1, e2 with
  | CastInt e, Num 0 -> e
  | Num e1, Num e2 -> Bool (e1 <> e2)
  | _, _ -> NRel (Neq, e1, e2)

let n_rel : N_rel.t -> nexp -> nexp -> bexp =
  function
  | N_rel.Lt -> n_lt
  | Gt -> n_gt
  | Eq -> n_eq
  | Neq -> n_neq
  | Le -> n_le
  | Ge -> n_ge


let n_if b n1 n2 =
  match b with
  | Bool b -> if b then n1 else n2
  | _ -> NIf (b, n1, n2)

let n_plus n1 n2 =
  match n1, n2 with
  | Num 0, n | n, Num 0 -> n
  | Num n1, Num n2 -> Num (n1 + n2)
  | Num n1, Binary (Plus, Num n2, e)
  | Binary (Plus, Num n1, e), Num n2 ->
    Binary (Plus, Num (n1 + n2), e)
  | _, Num _ -> Binary (Plus, n2, n1)
  | _, _ -> Binary (Plus, n1, n2)

let n_inc (n:nexp) : nexp =
  n_plus n (Num 1)

let n_minus n1 n2 =
  match n1, n2 with
  | n, Num 0 -> n
  | Num n1, Num n2 -> Num (n1 - n2)
  | _, _ -> Binary (Minus, n1, n2)

let n_dec (n:nexp) : nexp =
  n_minus n (Num 1)

let n_mult n1 n2 =
  match n1, n2 with
  | Num 1, n | n, Num 1 -> n
  | Num 0, _ | _, Num 0 -> Num 0
  | Num n1, Num n2 -> Num (n1 * n2)
  | Num n1, Binary (Mult, Num n2, e)
  | Num n1, Binary (Mult, e, Num n2)
  | Binary (Mult, Num n1, e), Num n2
  | Binary (Mult, e, Num n1), Num n2 ->
    Binary (Mult, Num (n1 * n2), e)
  | _, _ -> Binary (Mult, n1, n2)

let n_uminus (n:nexp) : nexp = Unary (N_unary.Negate, n)

let n_div n1 n2 =
  match n1, n2 with
  | _, Num 1 -> n1
  | Num 0, _ -> Num 0
  | _, Num 0 -> failwith ("Division by 0")
  | Num n1, Num n2 -> Num (n1 / n2)
  | _, _ -> Binary (Div, n1, n2)

let n_mod n1 n2 =
  match n1, n2 with
  | Num n1, Num n2 -> Num (Common.modulo n1 n2)
  | _, _ -> Binary (Mod, n1, n2)

let n_left_shift (l: nexp) (r:nexp) : nexp =
  match l, r with
  | a, Num n -> Binary (Mult, a, Num (Common.pow ~base:2 n))
  | _, _ -> Binary (LeftShift, l, r)

let n_right_shift (l: nexp) (r:nexp) : nexp =
  match l, r with
  | a, Num n -> Binary (Div, a, Num (Common.pow ~base:2 n))
  | _, _ -> Binary (RightShift, l, r)

let n_bin o n1 n2 =
  try
    match o, n1, n2 with
    | _, Num n1, Num n2 -> Num (N_binary.eval o n1 n2)
    | N_binary.Plus, _, _ -> n_plus n1 n2
    | Minus, _, _ -> n_minus n1 n2
    | Mult, _, _ -> n_mult n1 n2
    | Div, _, _ -> n_div n1 n2
    | Mod, _, _ -> n_mod n1 n2
    | LeftShift, _, _ -> n_left_shift n1 n2
    | RightShift, _, _ -> n_right_shift n1 n2
    | _, _, _ -> Binary (o, n1, n2)
  with
    Division_by_zero -> Binary (o, n1, n2)

let b_or b1 b2 =
  match b1, b2 with
  | Bool true, _ | _, Bool true -> Bool true
  | Bool false, b | b, Bool false -> b
  | _, _ -> BRel (BOr, b1, b2)

let b_and b1 b2 =
  match b1, b2 with
  | Bool true, b | b, Bool true -> b
  | Bool false, _ | _, Bool false -> Bool false
  | _, _ -> BRel (BAnd, b1, b2)

let b_rel o b1 b2 =
  match o, b1, b2 with
  | _, Bool b1, Bool b2 -> Bool (B_rel.eval o b1 b2)
  | B_rel.BAnd, b1, b2 -> b_and b1 b2
  | BOr, b1, b2 -> b_or b1 b2

let rec b_not : bexp -> bexp =
  function
  | BNot b -> b
  | BRel (BAnd, b1, b2) -> b_or (b_not b1) (b_not b2)
  | BRel (BOr, b1, b2) -> b_and (b_not b1) (b_not b2)
  | NRel (Eq, n1, n2) -> n_neq n1 n2
  | NRel (Neq, n1, n2) -> n_eq n1 n2
  | NRel (Lt, n1, n2) -> n_ge n1 n2
  | NRel (Gt, n1, n2) -> n_le n1 n2
  | NRel (Le, n1, n2) -> n_gt n1 n2
  | NRel (Ge, n1, n2) -> n_lt n1 n2
  | Bool b -> Bool (not b)
  | b -> BNot b

let b_impl b1 b2 =
  match b1 with
  | Bool true -> b2
  | Bool false -> Bool true
  | _ -> b_or (b_not b1) b2

let b_true = Bool true
let b_false = Bool false

let n_bit_not : nexp -> nexp =
  function
  | Num n -> Num (Int32.(of_int n |> lognot |> to_int))
  | e -> Unary (BitNot, e)

let cast_int : bexp -> nexp =
  function
  | Bool true -> Num 1
  | Bool false -> Num 0
  | CastBool n -> n
  | b -> CastInt b

let cast_bool : nexp -> bexp =
  function
  | Num n -> Bool (n <> 0)
  | CastInt b -> b
  | n -> CastBool n

let rec b_and_ex l =
  match l with
  | [] -> Bool true
  | [x] -> x
  | x::l -> b_and x (b_and_ex l)

let rec b_or_ex l =
  match l with
  | [] -> Bool true
  | [x] -> x
  | x::l -> b_or x (b_or_ex l)

let thread_eq (e:nexp) : bexp =
  n_eq e (Other e)

let distinct (idx:Variable.t list) : bexp =
  b_or_ex (List.map (fun x -> b_not (thread_eq (Var x))) idx)

let rec b_and_split : bexp -> bexp list =
  function
  | BRel (BAnd, b1, b2) ->
    b_and_split b1 @ b_and_split b2
  | b -> [b]

let rec b_or_split : bexp -> bexp list =
  function
  | BRel (BOr, b1, b2) ->
    b_or_split b1 @ b_or_split b2
  | b -> [b]


(* Checks if variable [x] is in the given expression *)
let rec n_exists (f:Variable.t -> bool) : nexp -> bool =
  function
  | CastInt b -> b_exists f b
  | Var x -> f x
  | Num _ -> false
  | Binary (_, e1, e2) ->
    n_exists f e1 || n_exists f e2
  | Unary (_, e) | NCall (_, e) | Other e -> n_exists f e
  | NIf (b, e1, e2) ->
    b_exists f b || n_exists f e1 || n_exists f e2

and b_exists (f:Variable.t -> bool) : bexp -> bool =
  function
  | Bool _ -> false
  | Pred (_, e) | CastBool e -> n_exists f e
  | NRel (_, e1, e2) -> n_exists f e1 || n_exists f e2
  | BRel (_, e1, e2) -> b_exists f e1 || b_exists f e2
  | BNot e -> b_exists f e

(* Checks if variable [x] is in the given expression *)
let n_mem (x:Variable.t) : nexp -> bool =
  n_exists (Variable.equal x)

let b_mem (x:Variable.t) : bexp -> bool =
  b_exists (Variable.equal x)

let rec n_par (n:nexp) : string =
  match n with
  | Num _
  | Var _
  | NCall _
  | Other _
  | CastInt _
    -> n_to_string n
  | NIf _
  | Unary _
  | Binary _
    -> "(" ^ n_to_string n ^ ")"

and n_to_string : nexp -> string =
  function
  | Num n -> string_of_int n
  | Var x -> Variable.name x
  | Unary (o, n) -> N_unary.to_string o ^ n_par n
  | Binary (b, a1, a2) ->
    n_par a1 ^ " " ^ N_binary.to_string b ^ " " ^ n_par a2
  | NCall (x, arg) ->
    x ^ "(" ^ n_to_string arg ^ ")"
  | NIf (b, n1, n2) ->
    b_par b ^ " ? " ^ n_par n1 ^ " : " ^ n_par n2
  | Other e -> "other(" ^ n_to_string e ^ ")"
  | CastInt b -> "int(" ^ b_to_string b ^ ")"

and b_to_string : bexp -> string = function
  | Bool b -> if b then "true" else "false"
  | CastBool e -> "bool(" ^ n_to_string e ^ ")"
  | NRel (b, n1, n2) ->
    n_to_string n1 ^ " " ^ N_rel.to_string b ^ " " ^ n_to_string n2
  | BRel (b, b1, b2) ->
    b_par b1 ^ " " ^ B_rel.to_string b ^ " " ^ b_par b2
  | BNot b -> "!" ^ b_par b
  | Pred (x, v) -> x ^ "(" ^ n_to_string v ^ ")"

and b_par (b:bexp) : string =
  match b with
  | Pred _
  | CastBool _
  | Bool _
  | BNot _
    -> b_to_string b
  | BRel _
  | NRel _
    -> "("  ^ b_to_string b ^ ")"

let b_to_s : bexp -> Indent.t list =
  let rec to_s (in_and:bool) (b:bexp) : Indent.t list =
    let open Indent in
    match b with
    | NRel _
    | Bool _
    | BNot _
    | CastBool _
    | Pred _
      -> [Line (b_to_string b)]
    | BRel (o, _, _) ->
      let op = B_rel.to_string o in
      b
      |> (if in_and then b_and_split else b_or_split)
      |> List.map (fun b ->
          match to_s (not in_and) b with
          | [Line b] -> Line b
          | l -> Block l
        )
      |> List.mapi (fun i ->
          let op = if i = 0 then "" else op ^ " " in
          function
          | Line s -> [Line (op ^ s)]
          | Block l -> [Line (op ^ "("); Block l; Line (")")]
          | Nil -> []
        )
      |> List.concat
  in
  to_s true

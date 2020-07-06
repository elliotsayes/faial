
type 'x exp =
  | Num of int
  | Var of 'x
  | Add of 'x exp * 'x exp
  | Mult of 'x exp * 'x exp

let add n1 n2 =
  match n1, n2 with
  | Num 0, n | n, Num 0 -> n
  | Num n1, Num n2 -> Num (n1 + n2)
  | _, _ -> Add (n1, n2)

let mul n1 n2 =
  match n1, n2 with
  | Num 0, _ | _, Num 0 -> Num 0
  | Num 1, n | Num 1, n -> n
  | Num n1, Num n2 -> Num (n1 * n2)
  | _, _ -> Mult (n1, n2)

let inc e = add (Num 1) e

let rec str_of e =
  match e with
  | Num x -> string_of_int x
  | Var x -> "?"
  | Add (x,y) -> (str_of x) ^ "+" ^ (str_of y)
  | Mult (x, y) -> (str_of x) ^ "*" ^ (str_of y)

type ('var, 's) prog =
  | Step of 's
  | Loop of ('var * 'var exp * ('var, 's) prog)
  | Sync
  | Seq of ('var, 's) prog * ('var, 's) prog
  | Skip

let rec infer_loop_steps e =
  match e with
  | Skip | Step _ -> Num 0
  | Sync -> Num 1
  | Loop (_, ub, body) -> mul ub (infer_loop_steps body)
  | Seq (e1, e2) -> add (infer_loop_steps e1) (infer_loop_steps e2)

type ('var, 'a) step = ('var exp * 'a)

(**
  Given a program, extracts the sequence of steps
  *)
let extract_steps e =
  let rec iter e (p,accum)  =
    let upper_bound e = iter e (Num 0, []) |> fst in
    match e with
    | Skip -> p, accum
    | Step d -> p, (p, d)::accum
    | Sync -> inc p, accum
    | Seq (e1, e2) ->
      iter e2 (iter e1 (p, accum))
    | Loop (var, ub, body) ->
      let loop_step = upper_bound body in
      let new_p = add p (mul (Var var) loop_step) in
      let (_, accum) = iter body (new_p, accum) in
      (add p (mul loop_step ub), accum)
  in
  iter e (Num 0, []) |> snd |> List.rev

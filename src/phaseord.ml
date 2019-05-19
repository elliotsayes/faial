type var = string

type exp =
  | Nat of int
  | Var of var
  | Add of exp * exp
  | Mul of exp * exp

let inc e = Add(Nat 1, e)

type 'a prog =
  | Step of 'a
  | Loop of (var * exp * 'a prog)
  | Sync
  | Seq of 'a prog * 'a prog
  | Skip

let rec infer_loop_steps e =
  match e with
  | Skip
  | Step _
    -> Nat 0
  | Sync -> Nat 1
  | Loop (_, ub, body) -> Mul (ub, infer_loop_steps body)
  | Seq (e1, e2) ->
    Add (infer_loop_steps e1, infer_loop_steps e2)

type 'a node = (exp * 'a)

(**
  Given a program, extracts the sequence
  *)
let extract_steps (e:'a prog) : ('a node) list =
  let rec iter (e:'a prog) (p,accum)  =
    match e with
    | Skip -> p, accum
    | Step d -> p, (p, d)::accum
    | Sync -> inc p, accum
    | Seq (e1, e2) ->
      iter e2 (iter e1 (p, accum))
    | Loop (var, ub, body) ->
      let step = infer_loop_steps body in
      let new_p = Add (p, Mul (Var var, step)) in
      let (_, accum) = iter body (new_p, accum) in
      Add (p, Mul(step, ub)), accum
  in
  iter e ((Nat 0), []) |> snd |> List.rev


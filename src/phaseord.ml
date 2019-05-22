type var = string

type exp =
  | Num of int
  | Var of var
  | Add of exp * exp
  | Mult of exp * exp

let inc e = Add (Num 1, e)

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
    -> Num 0
  | Sync -> Num 1
  | Loop (_, ub, body) -> Mult (ub, infer_loop_steps body)
  | Seq (e1, e2) -> Add (infer_loop_steps e1, infer_loop_steps e2)

type 'a step = (exp * 'a)

(**
  Given a program, extracts the sequence of steps
  *)
let extract_steps (e:'a prog) : ('a step) list =
  let rec iter (e:'a prog) (p,accum)  =
    match e with
    | Skip -> p, accum
    | Step d -> p, (p, d)::accum
    | Sync -> inc p, accum
    | Seq (e1, e2) ->
      iter e2 (iter e1 (p, accum))
    | Loop (var, ub, body) ->
      let loop_step = infer_loop_steps body in
      (match loop_step with
      | Num 0 -> raise (Failure "Loops must synchronize at least once.")
      | _ -> ());
      let new_p = Add (p, Mult (Var var, loop_step)) in
      let (_, accum) = iter body (new_p, accum) in
      Add (p, Mult(loop_step, ub)), accum
  in
  iter e (Num 0, []) |> snd |> List.rev


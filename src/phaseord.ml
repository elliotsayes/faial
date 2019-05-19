type var = string
type exp =
  | Nat of int
  | Var of var
  | Add of exp * exp
  | Mul of exp * exp

let inc e = Add(Nat 1, e)

let exp_subst ((x:var), (v:exp)) e : exp =
  let rec subst e =
    match e with
    | Nat _ -> e
    | Var y -> if String.equal x y then v else e
    | Add (e1, e2) -> Add (subst e1, subst e2)
    | Mul (e1, e2) -> Mul (subst e1, subst e2)
  in
  subst e

let rec opt_exp e =
  let simpl e =
    match e with
    | Add (Var _, Var _)
    | Mul (Var _, Var _)
      -> e
    | Mul (Nat x, Mul (Nat y, e))
      -> Mul (Nat (x * y), opt_exp e)
    | Add (Nat x, Add (Nat y, e))
      -> Add (Nat (x + y), opt_exp e)
    | Add (Var x, y) -> opt_exp (Add (y, Var x))
    | Mul (Var x, y) -> opt_exp (Mul (y, Var x))
    | Add (Nat 0, e)
    | Add (e, Nat 0)
    | Mul (Nat 1, e)
    | Mul (e, Nat 1)
      -> opt_exp e
    | Mul (_, Nat 0)
    | Mul (Nat 0, _)
      -> (Nat 0)
    | Add (Nat x, Nat y) -> Nat (x + y)
    | Mul (Nat x, Nat y) -> Nat (x * y)
    | _ -> e
  in
  match e with
  | Add (x, y) -> simpl (Add (opt_exp x, opt_exp y))
  | Mul (x, y) -> simpl (Mul (opt_exp x, opt_exp y))
  | Nat _
  | Var _
    -> e

let rec exp_to_string e : string =
  match e with
  | Nat x -> string_of_int x
  | Var x -> x
  | Add (x, y) -> "(" ^ exp_to_string x ^ " + " ^ exp_to_string y ^ ")"
  | Mul (x, y) -> "(" ^ exp_to_string x ^ " * " ^ exp_to_string y ^ ")"

module L0 = struct
  type 'a t =
    | Step of 'a
    | Loop of (var * exp * 'a t)
    | Sync
    | Seq of 'a t * 'a t
    | Skip

  let block es =
    List.fold_right
      (fun elem accum -> Seq(elem, accum))
      es
      Skip

  let fresh_name x xs =
    let rec do_fresh_name x n =
      let name = (x ^ string_of_int n) in
      if List.mem name xs
      then do_fresh_name x (n + 1)
      else name
    in
    if List.mem x xs then do_fresh_name x 1 else x

  let subst ((x:string), (v:exp)) e =
    let rec subst e =
      match e with
      | Step _
      | Sync
      | Skip
        -> e
      | Seq (e1, e2) -> Seq (subst e1, subst e2)
      | Loop (y, ub, e)
        ->
        let ub = exp_subst (x, v) ub in
        Loop (y, ub, if String.equal x y then e else subst e)
    in
    subst e

  let norm e =
    let rec norm e xs =
      match e with
      | Loop (x, ub, e) ->
        if List.mem x xs then (
          let new_x = fresh_name x xs in
          let new_xs = new_x::xs in
          let (e, new_xs) = norm (subst (x, Var new_x) e) new_xs in
          Loop (new_x, ub, e), new_xs
        ) else (
          let (e, new_xs) = norm e (x::xs) in
          Loop (x, ub, e), new_xs
        )
      | Seq (e1, e2) ->
        let (e1, xs) = norm e1 xs in
        let (e2, xs) = norm e2 xs in
        Seq (e1, e2), xs
      | Skip
      | Sync
      | Step _ -> e, xs
    in
    norm e []
end

module L1 = struct
  type 'a t =
    | Step of 'a
    | Loop of (var * exp * exp * 'a t)
    | Sync
    | Seq of 'a t * 'a t
    | Skip
end

let rec infer_loop_steps e =
  match e with
  | L0.Skip
  | L0.Step _
    -> Nat 0
  | L0.Sync -> Nat 1
  | L0.Loop (_, ub, body) -> Mul (ub, infer_loop_steps body)
  | L0.Seq (e1, e2) ->
    Add (infer_loop_steps e1, infer_loop_steps e2)

let rec generate_steps e =
  match e with
  | L0.Skip -> L1.Skip
  | L0.Step d -> L1.Step d
  | L0.Sync -> L1.Sync
  | L0.Loop (var, ub, body) ->
    L1.Loop (var, ub, infer_loop_steps body, generate_steps body)
  | L0.Seq (e1, e2) ->
    L1.Seq (generate_steps e1, generate_steps e2)

type 'a node = (exp * 'a)

let do_extract_steps (e:'a L0.t) : ('a node) list =
  let rec iter (e:'a L0.t) (p,accum)  =
    match e with
    | L0.Skip -> p, accum
    | L0.Step d -> p, (p, d)::accum
    | L0.Sync -> inc p, accum
    | L0.Seq (e1, e2) ->
      iter e2 (iter e1 (p, accum))
    | L0.Loop (var, ub, body) ->
      let step = infer_loop_steps body in
      let new_p = Add (p, Mul (Var var, step)) in
      let (_, accum) = iter body (new_p, accum) in
      Add (p, Mul(step, ub)), accum
  in
  snd (iter e ((Nat 0), []))


let extract_steps (e:'a L1.t) : ('a node) list =
  let rec iter (e:'a L1.t) (p,accum)  =
    match e with
    | L1.Skip -> p, accum
    | L1.Step d -> p, (p, d)::accum
    | L1.Sync -> inc p, accum
    | L1.Seq (e1, e2) ->
      iter e2 (iter e1 (p, accum))
    | L1.Loop (var, ub, step, body) ->
      let new_p = Add (p, Mul (Var var, step)) in
      let (_, accum) = iter body (new_p, accum) in
      Add (p, Mul(step, ub)), accum
  in
  snd (iter e ((Nat 0), []))

let flatten prog =
  do_extract_steps prog
  (*
  prog |> generate_steps |> extract_steps
  *)
let _ =
  let prog : int L0.t =
    Seq(
      Loop ("x", Var "UB1",
        L0.block [
          Step 1;
          Sync;
          (* 4 syncs *)
          Loop ("x", Var "UB2",
            Seq(Step 2, Sync)
          );
          Step 3;
          Sync
        ]
      ),
      Loop ("x", Var "UB1",
        L0.block [
          Step 4;
          Sync;
          (* 4 syncs *)
          Loop ("x", Var "UB2",
            Seq(Step 5, Sync)
          );
          Step 6;
          Sync
        ]
      ))
  in
  let prog, _ = L0.norm prog in
  let prog1 = prog |> generate_steps in
  let l = extract_steps prog1 in
  List.iter (
    fun (p, d) ->
    let p = opt_exp p in
    let p = exp_to_string p in
    print_string "phase: ";
    print_string p;
    print_string " datum: ";
    print_string (string_of_int d);
    print_string "\n"
  ) (List.rev l)

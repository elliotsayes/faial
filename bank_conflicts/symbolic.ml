open Protocols
open Stage0
(*
  1. Generates a summation from a slice.
  2. Flattens a summation expression as a single numeric expression.
  *)
open Exp

type t =
  | Const of int
  | Sum of Variable.t * Exp.nexp * t
  | Add of t list

let rec to_string : t -> string =
  function
  | Const x -> string_of_int x
  | Sum (x, n, s) -> "Σ_{1 ≤ " ^ Variable.name x ^ " ≤ " ^ Exp.n_to_string n ^ "} " ^ to_string s
  | Add l -> List.map to_string l |> Common.join " + "

module Make (S:Subst.SUBST) = struct
  module M = Subst.Make(S)
  let rec subst (s: S.t) : t -> t =
    function
    | Const k -> Const k
    | Add l -> Add (List.map (subst s) l)
    | Sum (x, ub, p) ->
      let ub = M.n_subst s ub in
      let p = M.add s x (function
        | Some s -> subst s p
        | None -> p
      )
      in
      Sum (x, ub, p)
end

module PSubstAssoc = Make(Subst.SubstAssoc)
module PSubstPair = Make(Subst.SubstPair)

let subst : (Variable.t * Exp.nexp) -> t -> t = PSubstPair.subst

let add (l:t list) : t = Add l

type factor = { power: int; divisor: int }

let rec is_zero : t -> bool =
  function
  | Const 0 -> true
  | Const _ -> false
  | Sum (_, _, s) -> is_zero s
  | Add l -> List.for_all is_zero l

let factor_to_n (e:nexp) (i: factor) : nexp =
  let rec pow (x:nexp) (n:int) : nexp =
    match n with
    | 0 -> Num 1
    | 1 -> x
    | _ -> n_mult x (pow x (n - 1))
  in
  n_div (pow e i.power) (Num i.divisor)

let sum power e : Exp.nexp =
  let rec formula : factor list -> nexp =
    function
    | [f] -> factor_to_n e f
    | f :: l -> n_plus (factor_to_n e f) (formula l)
    | [] -> Num 0
  in
  (* https://en.wikipedia.org/wiki/Faulhaber%27s_formula *)
  match power with
  | 0 -> e
  | 1 ->
    formula [
      {power=1; divisor=2};
      {power=2; divisor=2};
    ]
  | 2 ->
    formula [
      {power=1; divisor=6};
      {power=2; divisor=2};
      {power=3; divisor=3};
    ]
  | 3 ->
    formula [
      {power=2; divisor=4};
      {power=3; divisor=2};
      {power=4; divisor=4};
    ]
  | 4 ->
    formula [
      {power=1; divisor=(-30)};
      {power=3; divisor=3};
      {power=4; divisor=2};
      {power=5; divisor=5};
    ]
  | 5 ->
    formula [
      {power=2; divisor=(-12)};
      {power=4; divisor=12};
      {power=5; divisor=2};
      {power=6; divisor=6};
    ]
  | 6 ->
    formula [
      {power=1; divisor=42};
      {power=3; divisor=(-6)};
      {power=5; divisor=2};
      {power=6; divisor=2};
      {power=7; divisor=7};
    ]
  | _ -> failwith ("S_" ^ string_of_int power ^ " not implemented")

let n_fact (e:Exp.nexp) : Exp.nexp =
  NCall ("!", e)

let rec flatten : t -> (Exp.nexp, string) Result.t =
  let (let*) = Result.bind in
  function
  | Const k -> Ok (Num k)
  | Sum (x, ub, s) ->
    let* n = flatten s in
    (* When we give up, we convert our expr into a polynomial *)
    let handle_poly (n:Exp.nexp) : (Exp.nexp, string) Result.t =
      match Poly.from_nexp x n with
      | Some p ->
        Ok (
          p
          |> Poly.to_seq
          |> Seq.map (fun (coefficient, degree) ->
            n_mult coefficient (sum degree ub)
          )
          |> Seq.fold_left n_plus (Num 0)
        )
      | None ->
        Error ("Cannot convert to a polynomial of '" ^
          Variable.name x ^ "': "^
          Exp.n_to_string n)
    in
    (* Try to handle the easy cases: *)
    let rec handle_expr (e: Exp.nexp) : (Exp.nexp, string) Result.t =
      let open Exp in
      match e with
      | Bin (Plus, e1, e2) ->
        let* e1 = handle_expr e1 in
        let* e2 = handle_expr e2 in
        Ok (n_plus e1 e2)
      | Bin (Minus, e1, e2) ->
        let* e1 = handle_expr e1 in
        let* e2 = handle_expr e2 in
        Ok (n_minus e1 e2)
      | Bin (Mult, Num n, e)
      | Bin (Mult, e, Num n) ->
        let* e = handle_expr e in
        Ok (n_mult e (Num n))
      | Bin (Div, Num n, e) ->
        let* e = handle_expr e in
        Ok (Bin (Div, Num n, e))
      | Bin (Div, e, Num n) ->
        let* e = handle_expr e in
        Ok (Bin (Div, e, Num n))
      | NCall (f, Var y) when Variable.equal y x && String.starts_with ~prefix:"log" f ->
        let e = n_minus (n_mult ub (NCall (f, ub))) ub in
        prerr_endline ("WARNING:  Σ" ^ f ^ "(!" ^ Variable.name x ^ ") ≈ " ^ Exp.n_to_string e);
        Ok e
      | NCall (f, e) when String.starts_with ~prefix:"log" f && not (Freenames.mem_nexp x e) ->
        Ok (n_mult ub (NCall (f, e)))
      | e -> handle_poly e
    in
    handle_expr n

  | Add l ->
    let l = List.map flatten l in
    (match List.find_opt Result.is_error l with
    | Some r -> r
    | None ->
      Ok (
        List.map Result.get_ok l
        |> List.fold_left n_plus (Num 0)
      ))

let simplify (s : t) : string =
  let rec simpl e : string =
    let fvs = Freenames.free_names_nexp e Variable.Set.empty in
    if Variable.Set.is_empty fvs then
      Constfold.n_opt e |> Exp.n_to_string
    else
      let x = Variable.Set.choose fvs in
      let result =
        match Poly.from_nexp x e with
        | Some p ->
          p
          |> Poly.to_seq_ord
          |> Seq.filter_map (fun (coef, pow) ->
              let coef = simpl coef in
              if coef = "0" then None
              else Some (
                let x = Variable.name x in
                match pow with
                | 0 -> coef
                | 1 -> coef ^ "·" ^ x
                | _ ->
                  let pow = x ^ Poly.exponent_to_string pow in
                  coef ^ "·" ^ pow
              )
            )
          |> List.of_seq
          |> Common.join " + "
        | None ->
          prerr_endline (
            "WARNING: Could not rewrite the expression as a " ^
            "polynomial in terms of " ^ Variable.name x ^
            ": " ^ Exp.n_to_string e
          );
          (* We can't make a polynomial from this expression. *)
          Constfold.n_opt e |> Exp.n_to_string
      in
      if result = "" then
        "0"
      else
        "(" ^ result ^ ")"
  in
  match flatten s with
  | Ok e ->
    (try simpl e with
      Failure m ->
      prerr_endline ("WARNING: simplify: " ^ m ^ ": " ^ to_string s);
      to_string s)
  | Error m ->
    prerr_endline ("WARNING: simplify: " ^ m ^ ": " ^ to_string s);
    to_string s

let rec to_ra : t -> Ra.t =
  function
  | Const k -> Tick k
  | Sum (x, ub, s) ->
    Loop (Range.make ~lower_bound:(Num 1) x (Exp.n_inc ub), to_ra s)
  | Add l ->
    List.fold_right (fun i r -> Ra.Seq (to_ra i, r)) l Skip

let to_environ (s:t) : Environ.t =
  let rec fvs (env:Environ.Fvs.t) : t -> Environ.Fvs.t =
    function
    | Const _ -> env
    | Sum (x, ub, s) ->
      fvs env s
      |> Environ.Fvs.add_var x
      |> Environ.Fvs.add_exp ub
    | Add l ->
      List.fold_left fvs env l
  in
  fvs Environ.Fvs.empty s |> Environ.from_fvs

let pow_base (name:string) : int option =
  match Common.split 'w' name with
  | Some (_, k) -> int_of_string_opt k
  | None -> None

let log_base (name:string) : int option =
  match Common.split 'g' name with
  | Some (_, k) -> int_of_string_opt k
  | None -> None

let rec n_log ~base (e:Exp.nexp) : Exp.nexp =
  match e with
  | NCall (f, e) when pow_base f = Some base ->
    e
  | Num n when (n >= 1 && base = 2) -> Num (n |> float_of_int |> Float.log2 |> Float.ceil |> int_of_float)
  | Bin (RightShift, e', Num k) when base = 2 ->
    Bin (Minus, n_log ~base e', Num k)
  | Bin (Mult, e1, e2) ->
    n_plus (n_log ~base e1) (n_log ~base e2)
  | Bin (Div, e1, e2) ->
    n_minus (n_log ~base e1) (n_log ~base e2)
  | Bin (Plus, e', Num 1)
  | Bin (Minus, e', Num 1)
  | Bin (Minus, Num 1, e')
  | Bin (Plus, Num 1, e') ->
    let x = "log" ^ string_of_int base in
    prerr_endline ("WARNING: " ^ x ^ "(" ^ Exp.n_to_string e ^ ") ≈ " ^ x ^ "(" ^ Exp.n_to_string e' ^ ")");
    n_log ~base e'
  | _ ->
    NCall ("log" ^ string_of_int base, e)

let rec n_pow ~base (e:Exp.nexp) : Exp.nexp =
  match e with
  | Num n -> Num (Common.pow ~base n)
  | Bin (Plus, e1, e2) -> n_mult (n_pow ~base e1) (n_pow ~base e2)
  | Bin (Minus, e1, e2) -> n_div (n_pow ~base e1) (n_pow ~base e2)
  | _ -> NCall ("pow" ^ string_of_int base, e)

(* Given a range, try to build a Sum *)
let sum (r:Range.t) (s:t) : t option =
  if s = Const 0 then Some (Const 0)
  else
    match r with
    | {step = Plus k; _} ->
      let open Exp in
      (*
                ub - lb
        iters = -------
                    k
      *)
      let iters = n_div (n_minus r.upper_bound r.lower_bound) k in
      (* x := k (x + lb + 1) *)
      let new_range_var = n_mult (n_plus (Var r.var) (n_inc r.lower_bound)) k in
      let s = subst (r.var, new_range_var) s in
      Some (Sum (r.var, iters, s))

    | {
        var=x;
        step = Mult (Num k);
        _
      } when k >= 2 ->
        let open Exp in
        (*
          log_k (ub - lb)

          For instance,
            for (x = 3; x < 100; x *= 2) ->
              3, 6, 12, 24, 48, 96 <- log2 (100 / 3)
              3*2^0, 3*2^1, 3*2^2, ...

            for (x = 25; x < 100; x *= 2) ->
              25, 50 <- log2 (100 / 3) = log2(100) - log2(3)
              25*2^0, 25*2^1
          *)
      let iters =
        (* we use subtraction of logs, rather than division of args of logs
            because ultimately we want to simplify the logs. *)
        n_minus (n_log ~base:k r.upper_bound) (n_log ~base:k r.lower_bound)
      in
      (* In summations we start with base 1; we decrement 1 so that we start in base 2^0 *)
      let new_range_var = n_mult (r.lower_bound) (n_pow ~base:k (n_dec (Var r.var))) in
      let s = subst (r.var, new_range_var) s in
      Some (Sum (x, iters, s))
  | _ -> None

let rec from_ra : Ra.t -> t =
  function
  | Ra.Tick k -> Const k
  | Ra.Skip -> Const 0
  | Ra.Seq (p, q) -> Add [from_ra p; from_ra q]
  | Ra.Loop (r, p) ->
    match sum r (from_ra p) with
    | Some s -> s
    | None -> failwith ("Unsupported range: " ^ Range.to_string r)

let rec from_slice (num_banks:int) (thread_count:Vec3.t) (locs:Variable.Set.t) : Shared_access.t -> t =
  function
  | Index a -> Const (Index_analysis.analyze num_banks thread_count locs a.index)
  | Cond (_, p) -> from_slice num_banks thread_count locs p
  | Loop (r, p) ->
    match sum r (from_slice num_banks thread_count locs p) with
    | Some s -> s
    | None -> failwith ("Unsupported range: " ^ Range.to_string r)

open Protocols
open Stage0
(*
  1. Generates a summation from a slice.
  2. Flattens a summation expression as a single numeric expression.
  *)
open Exp

type t =
  | Const of int
  | Sum of Set_range.t * t
  | Add of t list

let rec to_string : t -> string =
  function
  | Const x -> string_of_int x
  | Sum (b, s) -> "Σ_" ^ Set_range.to_string b ^ " " ^ to_string s
  | Add l ->
    let l = List.map to_string l |> Common.join " + " in
    "(" ^ l ^ ")"

let subst ((x,v): Variable.t * Reals.t) : t -> t =
  let rec subst : t -> t =
    function
    | Const _ as e -> e
    | Add l -> Add (List.map subst l)
    | Sum (b, p)  ->
      let b = Set_range.subst (x, v) b in
      Sum (b,
        if Variable.equal b.var x then
          p
        else
          subst p
      )
  in
  subst

let add (l:t list) : t = Add l

type factor = { power: int; divisor: int }

let rec is_zero : t -> bool =
  function
  | Const 0 -> true
  | Const _ -> false
  | Sum (_, s) -> is_zero s
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

let adapt_error (r:('a, string) Result.t) : ('a, Errors.t) Result.t =
  r
  |> Result.map_error (fun e ->
    let open Errors in
    {output=e; reason=UnsupportedInput}
  )

module Make (L:Logger.Logger) = struct
  module I = Index_analysis.Make(L)
(*
let rec flatten : t -> (Exp.nexp, string) Result.t =
  let ( let* ) = Result.bind in
  function
  | Const k -> Ok (Num k)
  | Sum (b, s) ->
    let x = b.var in
    let (ub, s) =
      if Reals.is_one b.first_elem then
        (b.last_elem, s)
      else
        let ub = n_inc (n_minus b.last_elem b.first_elem) in
        let s = subst (b.var, n_plus (n_dec (Var b.var)) b.first_elem) s in
        (ub, s)
    in
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
        L.info ("Simplification: Σ" ^ f ^ "(!" ^ Variable.name x ^ ") ≈ " ^ Exp.n_to_string e);
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
          L.warning (
            "could not rewrite the expression as a " ^
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
      L.warning ("simplify: " ^ m ^ ": " ^ to_string s);
      to_string s)
  | Error m ->
    L.warning ("simplify: " ^ m ^ ": " ^ to_string s);
    to_string s

let rec to_ra : t -> Ra.t =
  function
  | Const k -> Tick k
  | Sum (b, s) ->
    Loop (Interval.to_range b, to_ra s)
  | Add l ->
    List.fold_right (fun i r -> Ra.Seq (to_ra i, r)) l Skip

let to_environ (s:t) : Environ.t =
  let rec fvs (env:Environ.Fvs.t) : t -> Environ.Fvs.t =
    function
    | Const _ -> env
    | Sum (b, s) ->
      fvs env s
      |> Environ.Fvs.add_var b.var
      |> Environ.Fvs.add_exp b.first_elem
      |> Environ.Fvs.add_exp b.last_elem
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
    L.info ("Simplification: " ^ x ^ "(" ^ Exp.n_to_string e ^ ") ≈ " ^ x ^ "(" ^ Exp.n_to_string e' ^ ")");
    n_log ~base e'
  | _ ->
    NCall ("log" ^ string_of_int base, e)

let rec n_pow ~base (e:Exp.nexp) : Exp.nexp =
  match e with
  | Num n -> Num (Common.pow ~base n)
  | Bin (Plus, e1, e2) -> n_mult (n_pow ~base e1) (n_pow ~base e2)
  | Bin (Minus, e1, e2) -> n_div (n_pow ~base e1) (n_pow ~base e2)
  | _ -> NCall ("pow" ^ string_of_int base, e)
*)
(* Given a range, try to build a Sum *)
let sum (r:Range.t) (s:t) : t option =
  if s = Const 0 then Some (Const 0)
  else
    match r with
    | {var; upper_bound; lower_bound; step = Plus (Num 1); _} ->
      let b =
        Set_range.make
        ~var
        ~lower_bound:(Reals.from_nexp lower_bound)
        ~upper_bound:(Reals.from_nexp upper_bound)
      in
      Some (Sum (b, s))
    | {var; step = Plus step; dir; lower_bound; upper_bound; _} ->
      let open Exp in
      let x = Var var in
      (*
                    ub - lb
        last_elem = -------
                      k
      *)
      let new_range_var =
        match dir with
        | Increase ->
          (* x := lb + step * x *)
          n_plus lower_bound (n_mult x step)
        | Decrease ->
          (* x := ub - (x * step) *)
          n_minus upper_bound (n_mult x step)
      in
      let new_range_var = Reals.from_nexp new_range_var in
      let iters =
        n_div (n_minus upper_bound lower_bound) step
      in
      let s = subst (var, new_range_var) s in
      let b = Set_range.make
        ~var
        ~lower_bound:(Reals.from_int 0)
        ~upper_bound:(Reals.from_nexp iters)
      in
      Some (Sum (b, s))
    | {
        step = Mult (Num k);
        _
      } when k >= 2 ->
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
      let i_lower_bound = Reals.from_nexp r.lower_bound in
      let i_upper_bound = Reals.from_nexp r.upper_bound in
      let f_lower_bound = i_lower_bound |> Reals.int_to_float in
      let f_upper_bound = i_upper_bound |> Reals.int_to_float in
      let iters =
        (* we use subtraction of logs, rather than division of args of logs
            because ultimately we want to simplify the logs. *)
        Reals.i_minus
          (Reals.floor (Reals.logarithm k f_upper_bound))
          (Reals.floor (Reals.logarithm k f_lower_bound))
      in
      (* In summations we start with base 1; we decrement 1 so that we start in base 2^0 *)
      let new_range_var =
        Reals.i_mult i_lower_bound (Reals.power_of k (Var r.var))
      in
      let s = subst (r.var, new_range_var) s in
      let b = Set_range.make
        ~var:r.var
        ~lower_bound:(Num 1)
        ~upper_bound:iters;
      in
      Some (Sum (b, s))
  | _ -> None

let rec from_ra : Ra.t -> (t, string) Result.t =
  let ( let* ) = Result.bind in
  function
  | Ra.Tick k -> Ok (Const k)
  | Ra.Skip -> Ok (Const 0)
  | Ra.Seq (p, q) ->
    let* p = from_ra p in
    let* q = from_ra q in
    Ok (Add [p; q])
  | Ra.Loop (r, p) ->
    let* s = from_ra p in
    match sum r s with
    | Some s -> Ok s
    | None -> Error ("Unsupported range: " ^ Range.to_string r)

let run_ra ~show_code (r: Ra.t) : (string, Errors.t) Result.t =
  from_ra r
  |> Result.map (fun s ->
    (if show_code then (to_string s |> print_endline) else ());
    (* simplify s *)
    to_string s
  )
  |> adapt_error

end



module Default = Make(Logger.Colors)
module Silent = Make(Logger.Silent)

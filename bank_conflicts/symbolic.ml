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
  | Sum (x, n, s) -> "Σ_{1 ≤ " ^ Variable.name x ^ " ≤ " ^ Serialize.PPrint.n_to_s n ^ "} " ^ to_string s
  | Add l -> List.map to_string l |> Common.join " + "

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

let rec to_maxima : t -> string =
  function
  | Const k -> string_of_int k
  | Sum (x, ub, s) -> "sum(" ^ to_maxima s ^ ", " ^ Variable.name x ^ ", 1, " ^ Serialize.PPrint.n_to_s ub ^ ")"
  | Add l -> List.map to_maxima l |> Common.join " + "

let rec flatten : t -> Exp.nexp =
  function
  | Const k -> Num k
  | Sum (x, ub, s) ->
    Poly.from_nexp x (flatten s)
    |> Poly.to_seq
    |> Seq.map (fun (coefficient, degree) ->
      n_mult coefficient (sum degree ub)
    )
    |> Seq.fold_left n_plus (Num 0)
  | Add l ->
    List.map flatten l
    |> List.fold_left n_plus (Num 0)

let cleanup_maxima_output (x:string) : string =
  let lines = String.split_on_char '\n' x in
  let max_len = List.map String.length lines
    |> List.fold_left max 0
  in
  let offsets =
    lines
    |> List.filter_map (fun line ->
      String.to_seqi line
      |> Seq.find (fun (_, a) -> a <> ' ')
      |> Option.map fst
    )
  in
  let min_offset = List.fold_left min max_len offsets in
  lines
  |> List.map (fun line ->
    Slice.from_start min_offset
    |> Slice.substring line
  )
  |> Common.join "\n"

let run ?(exe="maxima") (x:t) : string =
  let expr = to_maxima x ^ ",simpsum;" in
  let (_, txt) =
    let cmd = Filename.quote_command exe ["--very-quiet"; "--disable-readline"] in
    Unix.open_process cmd
    |> Common.with_process_in_out (fun (ic, oc) ->
      (* Send the expression to be processed *)
      output_string oc expr;
      (* Close output to ensure it is processed *)
      close_out oc;
      (* Receive the output *)
      Common.ic_to_string ic
    )
  in
  txt |> cleanup_maxima_output

let rec from_slice (num_banks:int) (thread_count:Vec3.t) (locs:Variable.Set.t) : Shared_access.t -> t =
  function
  | Index a -> Const (Index_analysis.analyze num_banks thread_count locs a.index)
  | Cond (_, p) -> from_slice num_banks thread_count locs p
  | Loop (r, p) ->
    match r with
    | {
        range_var=x;
        range_step = StepName "pow2";
        _
      } ->
      (match Predicates.r_eval_opt r with
      | Some l ->
        let l = List.map (fun i ->
          let p = Shared_access.subst (x, Num i) p in
          from_slice num_banks thread_count locs p
        ) l in
        Add l
      | None -> failwith ("Unsupported range: " ^ Serialize.PPrint.r_to_s r))
    | {
        range_var=x;
        range_lower_bound=Num 0;
        range_step = Default (Num 1);
        range_upper_bound=ub;
        _
      } ->
      Sum (x, ub, from_slice num_banks thread_count locs p)
    | {range_step = Default k; _} ->
      let open Exp in
      (* x := k (x + lb) *)
      let iters = n_minus r.range_upper_bound r.range_lower_bound in
      let new_range_var = n_mult (n_plus (Var r.range_var) (n_plus (Num 1) r.range_lower_bound)) k in
      let p = Shared_access.subst (r.range_var, new_range_var) p in
      (*  (ub-lb)/k *)
      Sum (r.range_var, n_div iters k, from_slice num_banks thread_count locs p)
    | _ -> failwith ("Unsupported range: " ^ Serialize.PPrint.r_to_s r)

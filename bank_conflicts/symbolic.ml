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
        Ok (NCall (f, n_fact (Var x)))
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

(* --------------------------------- Absynth ---------------------- *)

let rec to_maxima : t -> string =
  function
  | Const k -> string_of_int k
  | Sum (x, ub, s) -> "sum(" ^ to_maxima s ^ ", " ^ Variable.name x ^ ", 1, " ^ Exp.n_to_string ub ^ ")"
  | Add l -> List.map to_maxima l |> Common.join " + "

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

let run_prog ~out ~exe args =
  let (_, txt) =
    let cmd = Filename.quote_command exe args in
    Unix.open_process cmd
    |> Common.with_process_in_out (fun (ic, oc) ->
      (* Send the expression to be processed *)
      output_string oc out;
      (* Close output to ensure it is processed *)
      close_out oc;
      (* Receive the output *)
      Common.ic_to_string ic
    )
  in
  txt

let run_maxima ?(verbose=false) ?(exe="maxima") (x:t) : string =
  let expr = to_maxima x ^ ",simpsum;" in
  (if verbose
    then prerr_endline ("maxima output:\n" ^ expr ^ "\n")
    else ());
  run_prog ~out:expr ~exe ["--very-quiet"; "--disable-readline"]
  |> cleanup_maxima_output

(* --------------------------------- Absynth ---------------------- *)

let to_absynth : t -> string =
  let indent (depth:int) : string = String.make depth '\t' in
  let n_to_s = Exp.n_to_string in
  let rec translate (depth:int) : t -> string =
    function
    | Const k -> indent depth ^ "tick " ^ string_of_int k ^ "\n"
    | Sum (x, ub, s) ->
      indent depth ^ Variable.name x ^ " = 0\n" ^
      indent depth ^ "while " ^ Variable.name x ^ " < (" ^ n_to_s ub ^ "):\n" ^
      indent (depth + 1) ^ Variable.name x ^ " = " ^ Variable.name x ^ " + 1\n" ^
      translate (depth + 1) s
    | Add l -> List.map (translate depth) l |> Common.join ""
  in
  fun x ->
    "def f():\n" ^
    translate 1 x

let with_tmp ~prefix ~suffix (f:string -> 'a) : 'a =
  let fname = Filename.temp_file prefix suffix in
  try
      let res = f fname in
      Sys.remove fname;
      res
  with ex ->
      Sys.remove fname;
      raise ex

let write_string ~filename ~data : unit =
  let oc = open_out filename in
  try
    output_string oc data;
    close_out oc
  with ex ->
    close_out oc;
    raise ex

let cleanup_absynth (x:string) : string option =
  let (let*) = Option.bind in
  let* x =
    String.split_on_char '\n' x
    |> List.find_opt (String.starts_with ~prefix:"    Bound:")
  in
  let* (_, x) = Common.split ':' x in
  Some (String.trim x)

let run_absynth ?(verbose=false) ?(exe="absynth") (x:t) : string =
  let out = to_absynth x in
  (if verbose
    then prerr_endline ("Absynth output:\n" ^ out ^ "\n")
    else ());
  let data = with_tmp ~prefix:"absynth_" ~suffix:".imp" (fun filename ->
      write_string ~filename ~data:out;
      run_prog ~out ~exe [filename]
    )
  in
  match cleanup_absynth data with
  | Some x -> x
  | None ->
    if Common.contains ~substring:"Sorry, I could not find a bound" data then
      "No bound found."
    else (
      print_endline data;
      "???"
    )

(* --------------------------------- CoFloCo ---------------------- *)


module Fvs = struct
  type t = int Variable.Map.t

  let empty : t = Variable.Map.empty

  let get (x:Variable.t) (fvs:t) : int =
    Variable.Map.find x fvs

  let size (env:t) = Variable.Map.cardinal env

  let to_array (fvs:t) : Variable.t array =
    let vars : Variable.t option array = Array.make (size fvs) None in
    Variable.Map.to_seq fvs
    |> Seq.iter (fun (v, idx) ->
      Array.set vars idx (Some v)
    );
    vars |> Array.map Option.get

  let add_var (x:Variable.t) (env:t) : t =
    if Variable.Map.mem x env then
      env
    else
      let next_idx = Variable.Map.cardinal env in
      Variable.Map.add x next_idx env

  let add_exp (e:Exp.nexp) (env:t) : t =
    let fvs = Freenames.free_names_nexp e Variable.Set.empty in
    Variable.Set.fold add_var fvs env

end

module Environ = struct
  type t = {ctx: Fvs.t; data: string array}

  let var (idx:int) : string =
    "Arg_" ^ string_of_int idx

  let from_fvs (ctx:Fvs.t) : t =
    let data =
      Common.range (Fvs.size ctx)
      |> List.map var
      |> Array.of_list
    in
    let data =
      if Array.length data = 0 then
        (* CoFloCo does support empty environments. *)
        Array.of_list [var 0]
      else
        data
    in
    {ctx; data}

  let get (x:Variable.t) (env:t) : string =
    Fvs.get x env.ctx |> var

  let n_normalize (e:Exp.nexp) (env:t) : Exp.nexp =
    let fvs = Freenames.free_names_nexp e Variable.Set.empty in
    Variable.Set.fold (fun x e ->
      let new_x = Var (Variable.from_name (get x env)) in
      Subst.ReplacePair.n_subst (x, new_x) e
    ) fvs e

  let b_normalize (e:Exp.bexp) (env:t) : Exp.bexp =
    let fvs = Freenames.free_names_bexp e Variable.Set.empty in
    Variable.Set.fold (fun x e ->
      let new_x = Var (Variable.from_name (get x env)) in
      Subst.ReplacePair.b_subst (x, new_x) e
    ) fvs e

  let n_to_s (e:Exp.nexp) (env:t) : string =
    n_normalize e env |> Exp.n_to_string

  let b_to_s (e:Exp.bexp) (env:t) : string =
    b_normalize e env |> Exp.b_to_string

  let decode (data:string) (env:t) : string =
    Fvs.to_array env.ctx
    |> Array.to_seqi
    |> Seq.fold_left (fun data (idx, x) ->
      let substring = var idx in
      Common.replace ~substring ~by:(Variable.name x) data
    ) data

  let put (x:Variable.t) (e:Exp.nexp) (env:t) : t =
    let d = Array.copy env.data in
    let idx = Fvs.get x env.ctx in
    let e = n_to_s e env in
    Array.set d idx e;
    { env with data = d }

  let to_list (x:t) : string list =
    x.data
    |> Array.to_list
end


let rec fvs (s:t) (env:Fvs.t) : Fvs.t =
  match s with
  | Const _ -> env
  | Sum (x, ub, s) ->
    env
    |> fvs s
    |> Fvs.add_var x
    |> Fvs.add_exp ub
  | Add l ->
    List.fold_left (fun (env:Fvs.t) (e:t) ->
      fvs e env
    ) env l

let to_cofloco (env:Environ.t) (s: t) : string =
  (* Compute a map from variable to its identifier *)
  let call (x:int) (env:Environ.t) : string =
    let env = Environ.to_list env |> Common.join ", " in
    "inst_" ^ string_of_int x ^ "(" ^ env ^ ")"
  in
  let rule ~src ?(cost="0") ?(dst=[]) ?(cnd=[]) () : string =
    let b_to_s n = Environ.b_to_s n env in
    let cnd = List.map b_to_s cnd in
    let arr l = "[" ^ Common.join "," l ^ "]" in
    "eq(" ^ src ^ ", " ^ cost ^ ", " ^ arr dst ^ ", " ^ arr cnd ^ ")."
  in
  let rec translate (idx:int) : t -> int * string list =
    let self env : string = call idx env in
    function
    | Sum (x, ub, s) ->
      let (idx', rest) = translate (idx + 2) s in
      idx' + 1,
      [
        (* Initialize the loop *)
        "% init loop";
        rule
          ~src:(self env)
          ~dst:[call (idx + 1) (Environ.put x (Num 0) env)] ();
        "% next iter";
        (* Transition of next iteration *)
        rule ~src:(call (idx + 1) env) ~dst:[
          call (idx + 1) (Environ.put x (Exp.n_inc (Var x)) env); (* next iter *)
          call (idx + 2) env (* loop body *)
        ] ~cnd:[Exp.n_lt (Var x) ub] ();
        "% loop body";
      ] @
      rest
      @ [
        "% end of loop body";
        rule ~src:(call idx' env) ();
        (* Transition of end of loop: *)
        "% end loop";
        rule ~src:(call (idx + 1) env) ~cnd:[n_ge (Var x) ub]
            ~dst:[call (idx' + 1) env] ();
      ]
    | Add l ->
      List.fold_right (fun (s:t) ((idx:int), l1) ->
        let (idx, l2) = translate idx s in
        (idx, l1 @ l2)
      ) l (idx, [])
    | Const k ->
      idx + 1,
      [
        rule
          ~src:(self env)
          ~cost:(string_of_int k)
          ~dst:[call (idx+1) env] ();
      ]
  in
  let (idx, l) = translate 0 s in
  (l @ [rule ~src:(call idx env) ()])
  |> Common.join "\n"

let r_id = Str.regexp {|nat(\([A-Za-z_0-9-]+\))|}

let cleanup_cofloco (env:Environ.t) (x:string) : string option =
  let (let*) = Option.bind in
  let* x =
    String.split_on_char '\n' x
    |> List.find_opt (String.starts_with ~prefix:"###")
  in
  let* (_, x) = Common.split ':' x in
  let x = Environ.decode x env in
  Some (String.trim x)

let run_cofloco ?(verbose=false) ?(exe="cofloco") (s:t) : string =
  let env = fvs s Fvs.empty |> Environ.from_fvs in
  let expr = to_cofloco env s in
  (if verbose
    then prerr_endline ("CoFloCo output:\n" ^ expr ^ "\n")
    else ());
  let data = run_prog ~out:expr ~exe ["-v"; "0"; "-i"; "/dev/stdin"] in
  match data |> cleanup_cofloco env with
  | Some x -> x
  | None ->
    data

(* ---------------------------------------------------------- *)


let to_koat (env:Environ.t) (s: t) : string =
  let call (x:int) (env:Environ.t) : string =
    let env = Environ.to_list env |> Common.join ", " in
    "inst_" ^ string_of_int x ^ "(" ^ env ^ ")"
  in
  let open Exp in
  (* Compute a map from variable to its identifier *)
  let rule ~src ?(cost="0") ?(dst=[]) ?(cnd=[]) () : string =
    let arr l = "(" ^ Common.join "," l ^ ")" in
    let b_to_s e = Environ.b_to_s e env in
    let cnd = List.map b_to_s cnd in
    let cnd = if List.length cnd = 0 then "" else
      " :|: " ^ Common.join " && " cnd
    in
    if List.length dst = 0 then
      ""
    else
      Printf.sprintf "%s -{%s}> Com_%d%s %s"
        src cost (List.length dst) (arr dst) cnd
  in
  let rec translate (idx:int) : t -> int * string list =
    let self env : string = call idx env in
    function
    | Sum (x, ub, s) ->
      let (idx', rest) = translate (idx + 2) s in
      idx' + 1,
      [
        (* Initialize the loop *)
        rule
          ~src:(self env)
          ~dst:[call (idx + 1) (Environ.put x (Num 0) env)] ();
        (* Transition of next iteration *)
        rule ~src:(call (idx + 1) env) ~dst:[
          call (idx + 2) (Environ.put x (Exp.n_inc (Var x)) env); (* next iter *)
        ] ~cnd:[n_lt (Var x) ub] ();
      ] @
      rest
      @ [
        rule ~src:(call idx' env)
          ~dst:[call (idx + 1) env]
        ();
        (* Transition of end of loop: *)
        rule ~src:(call (idx + 1) env) ~cnd:[n_ge (Var x) ub]
            ~dst:[call (idx' + 1) env] ();
      ]
    | Add l ->
      List.fold_right (fun (s:t) ((idx:int), l1) ->
        let (idx, l2) = translate idx s in
        (idx, l1 @ l2)
      ) l (idx, [])
    | Const k ->
      idx + 1,
      [
        rule
          ~src:(self env)
          ~cost:(string_of_int k)
          ~dst:[call (idx+1) env] ();
      ]
  in
  let (idx, l) = translate 0 s in
  let rules = (l @ [rule ~src:(call idx env) ()])
  |> Common.join "\n"
  in
  "(GOAL COMPLEXITY)\n" ^
  "(STARTTERM (FUNCTIONSYMBOLS inst_0))\n" ^
  "(VAR " ^ Common.join " " (Array.to_list env.data) ^ ")\n" ^
  "(RULES\n" ^
  rules ^
  "\n)"


let cleanup_koat (env:Environ.t) (x:string) : string option =
  let (let*) = Option.bind in
  let* (x, _) = Common.split '{' x in
  let x = Environ.decode (String.trim x) env |> Str.global_replace r_id "\\1" in
  Some x

let run_koat ?(exe="koat2") ?(verbose=false) (s:t) : string =
  let env = fvs s Fvs.empty |> Environ.from_fvs in
  let expr = to_koat env s in
  (if verbose
    then prerr_endline ("KoAT output:\n" ^ expr ^ "\n")
    else ());
  let data = run_prog ~out:expr ~exe ["analyse"; "-i"; "/dev/stdin"] in
  match data |> cleanup_koat env with
  | Some x -> x
  | None ->
    data

(* ---------------------------- end of solvers ------------------------ *)

let n_log ~base (e:Exp.nexp) : Exp.nexp =
  match e with
  | Num n when (n >= 1 && base = 2) -> Num (n |> float_of_int |> Float.log2 |> Float.ceil |> int_of_float)
  | _ ->
    NCall ("log" ^ string_of_int base, e)

let n_pow ~base (e:Exp.nexp) : Exp.nexp =
  match e with
  | Num n -> Num (Common.pow ~base n)
  | _ -> NCall ("pow" ^ string_of_int base, e)

let rec from_slice (num_banks:int) (thread_count:Vec3.t) (locs:Variable.Set.t) : Shared_access.t -> t =
  function
  | Index a -> Const (Index_analysis.analyze num_banks thread_count locs a.index)
  | Cond (_, p) -> from_slice num_banks thread_count locs p
  | Loop (r, p) ->
    match r with
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
      let p = Shared_access.subst (r.var, new_range_var) p in
      (match from_slice num_banks thread_count locs p with
      | Const 0 -> Const 0
      | s -> Sum (x, iters, s))
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
      let p = Shared_access.subst (r.var, new_range_var) p in
      (*  (ub-lb)/k *)
      (match from_slice num_banks thread_count locs p with
      | Const 0 -> Const 0
      | s -> Sum (r.var, iters, s))
    | _ -> failwith ("Unsupported range: " ^ Range.to_string r)

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

let rec flatten : t -> Exp.nexp =
  function
  | Const k -> Num k
  | Sum (x, ub, s) ->
    Poly.from_nexp x (flatten s)
    |> Poly.N.to_seq
    |> Seq.map (fun (coefficient, degree) ->
      n_mult coefficient (sum degree ub)
    )
    |> Seq.fold_left n_plus (Num 0)
  | Add l ->
    List.map flatten l
    |> List.fold_left n_plus (Num 0)


let simplify (s : t) : string =
  let e = flatten s in
  let rec simpl e : string =
    let fvs = Freenames.free_names_nexp e Variable.Set.empty in
    if Variable.Set.is_empty fvs then
      Constfold.n_opt e |> Serialize.PPrint.n_to_s
    else
      let x = Variable.Set.choose fvs in
      let result = Poly.from_nexp x e
      |> Poly.N.to_seq_ord
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
      in
      if result = "" then
        "0"
      else
        "(" ^ result ^ ")"
  in
  simpl e

(* --------------------------------- Absynth ---------------------- *)

let rec to_maxima : t -> string =
  function
  | Const k -> string_of_int k
  | Sum (x, ub, s) -> "sum(" ^ to_maxima s ^ ", " ^ Variable.name x ^ ", 1, " ^ Serialize.PPrint.n_to_s ub ^ ")"
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

let run_maxima ?(exe="maxima") (x:t) : string =
  let expr = to_maxima x ^ ",simpsum;" in
  run_prog ~out:expr ~exe ["--very-quiet"; "--disable-readline"]
  |> cleanup_maxima_output

(* --------------------------------- Absynth ---------------------- *)

let to_absynth : t -> string =
  let indent (depth:int) : string = String.make depth '\t' in
  let n_to_s = Serialize.PPrint.n_to_s in
  let rec translate (depth:int) : t -> string =
    function
    | Const k -> indent depth ^ "tick " ^ string_of_int k ^ "\n"
    | Sum (x, ub, s) ->
      indent depth ^ Variable.name x ^ " = 0\n" ^
      indent depth ^ "while " ^ Variable.name x ^ " < " ^ n_to_s ub ^ ":\n" ^
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

let run_absynth ?(exe="absynth") (x:t) : string =
  let out = to_absynth x in
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
    n_normalize e env |> Serialize.PPrint.n_to_s

  let b_to_s (e:Exp.bexp) (env:t) : string =
    b_normalize e env |> Serialize.PPrint.b_to_s

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

let run_cofloco ?(exe="cofloco") (s:t) : string =
  let env = fvs s Fvs.empty |> Environ.from_fvs in
  let expr = to_cofloco env s in
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
      let (idx', rest) = translate (idx + 3) s in
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
        (* Transition of next iteration *)
        rule ~src:(call (idx + 2) env) ~dst:[
          call (idx + 3) env; (* next iter *)
        ] ();
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

let run_koat ?(exe="koat2") (s:t) : string =
  let env = fvs s Fvs.empty |> Environ.from_fvs in
  let expr = to_koat env s in
  let data = run_prog ~out:expr ~exe ["analyse"; "-i"; "/dev/stdin"] in
  match data |> cleanup_koat env with
  | Some x -> x
  | None ->
    data

(* ---------------------------- end of solvers ------------------------ *)

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
      (* iters = ub - lb *)
      let iters = n_minus r.range_upper_bound r.range_lower_bound in
      (* x := k (x + lb + 1) *)
      let new_range_var = n_mult (n_plus (Var r.range_var) (n_inc r.range_lower_bound)) k in
      let p = Shared_access.subst (r.range_var, new_range_var) p in
      (*  (ub-lb)/k *)
      Sum (r.range_var, n_div iters k, from_slice num_banks thread_count locs p)
    | _ -> failwith ("Unsupported range: " ^ Serialize.PPrint.r_to_s r)

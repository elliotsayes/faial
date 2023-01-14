open Stage0
open Protocols

module Rule = struct
  open Environ
  type t = rule
  let make ~src ~cost ~cnd ~dst : t =
    { src; cost; dst; cnd }

  let src (x:t) : call =
    {id = x.src; args =[] }

  let to_string (env:Environ.t) (x:t) =
    let arr l = "(" ^ Common.join "," l ^ ")" in
    let b_to_s = Environ.b_to_string env in
    let cnd = List.map b_to_s x.cnd in
    let cost = string_of_int x.cost in
    let src : string = Environ.c_to_string env (src x) in
    let cnd = if List.length cnd = 0 then "" else
      " :|: " ^ Common.join " && " cnd
    in
    let dst = List.map (Environ.c_to_string env) x.dst in
    if List.length dst = 0 then
      ""
    else
      Printf.sprintf "%s -{%s}> Com_%d%s %s"
        src cost (List.length dst) (arr dst) cnd

end

type inst =
  | Rule of Rule.t
  | Comment of string

type prog = {env:Environ.t; code:inst list}

let rule (src:int) ?(cost=0) ?(dst=[]) ?(cnd=[]) () : inst =
  Rule (Rule.make ~src ~cost ~dst ~cnd)

let from_symbolic (env:Environ.t) (s: Symbolic.t) : prog =
  let open Exp in
  (* Compute a map from variable to its identifier *)
  let rec translate (idx:int) : Symbolic.t -> int * inst list =
    function
    | Sum (b, s) ->
      let x = b.var in
      let (idx', rest) = translate (idx + 2) s in
      idx' + 1,
      [
        (* Initialize the loop *)
        rule
          idx
          ~dst:[{id=idx + 1; args=[(x, b.first_elem)]}] ();
        (* Transition of next iteration *)
        rule (idx + 1) ~dst:[
          {id=idx + 2; args=[(x, Exp.n_inc (Var x))]}; (* next iter *)
        ] ~cnd:[n_le (Var x) b.last_elem] ();
      ] @
      rest
      @ [
        rule idx'
          ~dst:[{id=idx + 1; args=[]}]
        ();
        (* Transition of end of loop: *)
        rule (idx + 1) ~cnd:[n_gt (Var x) b.last_elem]
            ~dst:[{id=idx' + 1; args=[]}] ();
      ]
    | Add l ->
      List.fold_right (fun (s:Symbolic.t) ((idx:int), l1) ->
        let (idx, l2) = translate idx s in
        (idx, l1 @ l2)
      ) l (idx, [])
    | Const k ->
      idx + 1,
      [
        rule idx
          ~cost:k
          ~dst:[{id=idx+1;args=[]}] ();
      ]
  in
  let (idx, l) = translate 0 s in
  let code = l @ [rule idx ()] in
  {env; code}


let from_ra (env:Environ.t) (s:Ra.t) : prog =
  let open Exp in
  (* Compute a map from variable to its identifier *)
  let rec translate (idx:int) : Ra.t -> int * inst list =
    function
    | Skip ->
      idx + 1,
      [
        rule idx
          ~cost:0
          ~dst:[{id=idx+1;args=[]}] ();
      ]
    | Tick k ->
      idx + 1,
      [
        rule idx
          ~cost:k
          ~dst:[{id=idx+1;args=[]}] ();
      ]
    | Seq (p, q) ->
      let (idx, l1) = translate idx p in
      let (idx, l2) = translate idx q in
      idx, l1 @ l2
    | Loop (r, s) ->
      let (idx', rest) = translate (idx + 2) s in
      let x = r.var in
      idx' + 1,
      [
        (* Initialize the loop *)
        rule
          idx
          ~dst:[{id=idx + 1; args=[(x, Range.while_init r)]}] ();
        (* Transition of next iteration *)
        rule (idx + 1) ~dst:[
          {id=idx + 2; args=[(x, Range.while_inc r)]}; (* next iter *)
        ] ~cnd:[Range.while_cond r] ();
      ] @
      rest
      @ [
        rule idx'
          ~dst:[{id=idx + 1; args=[]}]
        ();
        (* Transition of end of loop: *)
        rule (idx + 1) ~cnd:[Exp.b_not (Range.while_cond r) |> Constfold.b_opt]
            ~dst:[{id=idx' + 1; args=[]}] ();
      ]
  in
  let (idx, l) = translate 0 s in
  let code = l @ [rule idx ()] in
  {env; code}

let to_string (p:prog) : string =
  let rules =
    p.code
    |> List.map (function
        | Comment c -> "# " ^ c
        | Rule r -> Rule.to_string p.env r
    )
    |> String.concat "\n"
  in
  "(GOAL COMPLEXITY)\n" ^
  "(STARTTERM (FUNCTIONSYMBOLS inst_0))\n" ^
  "(VAR " ^ Common.join " " (Array.to_list p.env.data) ^ ")\n" ^
  "(RULES\n" ^
  rules ^
  "\n)"

let r_id = Str.regexp {|nat(\([A-Za-z_0-9-]+\))|}

let parse_koat (env:Environ.t) (x:string) : string option =
  let (let*) = Option.bind in
  let* (x, _) = Common.split '{' x in
  let x = Environ.decode (String.trim x) env |> Str.global_replace r_id "\\1" in
  Some x

let run ?(exe="koat2") ?(verbose=false) (env:Environ.t) (expr:string) : (string, Errors.t) Result.t =
  (if verbose
    then prerr_endline ("KoAT output:\n" ^ expr ^ "\n")
    else ());
  Common.run ~stdin:expr ~exe ["analyse"; "-i"; "/dev/stdin"]
  |> Errors.handle_result (parse_koat env)

let run_symbolic ?(exe="koat2") ?(verbose=false) (s:Symbolic.t) : (string, Errors.t) Result.t =
  let env = Symbolic.to_environ s in
  let expr = from_symbolic env s |> to_string in
  run ~exe ~verbose env expr

let run_ra ?(exe="koat2") ?(verbose=false) (s:Ra.t) : (string, Errors.t) Result.t =
  let env = Ra.to_environ s in
  let expr = from_ra env s |> to_string in
  run ~exe ~verbose env expr

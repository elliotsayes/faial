open Stage0
open Protocols

module Rule = struct
  type t = Environ.rule

  let src (x:t) : Environ.call =
    {id = x.src; args =[] }

  let to_string (env:Environ.t) (x:t) =
    let b_to_s (b:Exp.bexp) =
      b
      |> Environ.b_to_string env
      |> Common.replace ~substring:"<=" ~by:"=<"
    in
    let src : string = Environ.c_to_string env (src x) in
    let cnd = List.map b_to_s x.cnd in
    let cost = string_of_int x.cost in
    let dst = List.map (Environ.c_to_string env) x.dst in
    let arr l = "[" ^ Common.join "," l ^ "]" in
    "eq(" ^ src ^ ", " ^ cost ^ ", " ^ arr dst ^ ", " ^ arr cnd ^ ")."
end

type t =
  | Rule of Environ.rule
  | Comment of string

let rule (src:int) ?(cost=0) ?(dst=[]) ?(cnd=[]) () : t =
  Rule {src; cost; dst; cnd}

let from_ra (s: Ra.t) : t list =
  let rec translate (idx:int) : Ra.t -> int * t list =
    let open Ra in
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
      idx, (Comment "seq"::l1) @ l2
    | Loop (r, p) ->
      let (idx', rest) = translate (idx + 2) p in
      let init = Range.while_init r in
      let x = r.var in
      idx' + 1,
      [
        (* Initialize the loop *)
        Comment ("init loop: " ^ Variable.name x ^ " = " ^ Exp.n_to_string init);
        rule idx ~dst:[{id=idx + 1;args=[(x, init)]}] ();
        Comment ("next iter: inst_" ^ string_of_int (idx + 1));
        Comment ("loop_body: inst_" ^ string_of_int (idx + 2));
        (* Transition of next iteration *)
        rule (idx + 1) ~dst:[
          {id=idx + 1; args=[(x, Range.while_inc r)]}; (* next iter *)
          {id=idx + 2; args=[]}; (* loop body *)
        ] ~cnd:[Range.while_cond r] ();
        Comment ("loop body: inst_" ^ string_of_int (idx + 2));
      ] @
      rest
      @ [
        Comment ("end of loop body: inst_" ^ string_of_int (idx + 2));
        rule idx' ();
        (* Transition of end of loop: *)
        Comment ("end of loop inst_" ^ string_of_int (idx + 1));
        rule (idx + 1) ~cnd:[Exp.b_not (Range.while_cond r) |> Constfold.b_opt] ~dst:[{id=idx' + 1;args=[]}] ();
      ]
  in
  let (idx, l) = translate 0 s in
  (l @ [
    Comment "end of program ";
    rule idx ()
  ])

let to_string (env:Environ.t) (x:t list) : string =
  x
  |> List.map (function
      | Comment c -> "% " ^ c
      | Rule r -> Rule.to_string env r
  )
  |> String.concat "\n"

let parse_cost (env:Environ.t) (x:string) : string option =
  let (let*) = Option.bind in
  let* x =
    String.split_on_char '\n' x
    |> List.find_opt (String.starts_with ~prefix:"###")
  in
  let* (_, x) = Common.split ':' x in
  let x = Environ.decode x env in
  Some (String.trim x)

let parse_asympt (env:Environ.t) (x:string) : string option =
  let (let*) = Option.bind in
  let* x =
    String.split_on_char '\n' x
    |> List.find_opt (String.starts_with ~prefix:"Asymptotic")
  in
  let* (_, x) = Common.split ':' x in
  let x = Environ.decode x env |> String.trim in
  let x = if x = "constant" then "1" else x in
  Some ("O(" ^ x ^ ")")

let run
  ?(asympt=false)
  ?(verbose=false)
  ?(exe="cofloco")
  (env:Environ.t)
  (expr:string)
:
  (string, Errors.t) Result.t
=
  (if verbose
    then prerr_endline ("CoFloCo output:\n" ^ expr ^ "\n")
    else ());
  let parse = if asympt then parse_asympt else parse_cost in
  Subprocess.make exe ["-v"; "0"; "-i"; "/dev/stdin"]
  |> Subprocess.run_combine ~stdin:expr
  |> Errors.handle_result (parse env)

let run_ra ?(asympt=false) ?(verbose=false) ?(exe="cofloco") (s:Ra.t) : (string, Errors.t) Result.t =
  let env = Ra.to_environ s in
  let expr = from_ra s |> to_string env in
  run ~asympt ~verbose ~exe env expr

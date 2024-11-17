open Protocols

let ( let* ) = Option.bind

type t = {
  init: Stmt.t;
  cond: Exp.bexp;
  inc: Stmt.t;
}

let to_string : t -> string =
  function
  | {init; cond; inc} ->
    let stmt (s:Stmt.t) : string =
      s
      |> Stmt.to_string
      |> Stage0.Common.replace ~substring:"\n" ~by:" "
      |> String.trim
    in
    Printf.sprintf "(%s; %s; %s)"
      (stmt init)
      (Exp.b_to_string cond)
      (stmt inc)

module Increment = struct
  type t =
    | Plus
    | LeftShift
    | Mult
    | Minus
    | RightShift
    | Div

  let parse : N_binary.t -> t option =
    function
    | Minus -> Some Minus
    | Div -> Some Div
    | RightShift -> Some RightShift
    | Plus -> Some Plus
    | LeftShift -> Some LeftShift
    | Mult -> Some Mult
    | _ -> None

end

module Comparator = struct
  type t =
    | Lt
    | Le
    | Gt
    | Ge
    | RelMinus

  let parse : N_rel.t -> t option =
    function
    | Lt -> Some Lt
    | Gt -> Some Gt
    | Le -> Some Le
    | Ge -> Some Ge
    | _ -> None
end

module Infer = struct

  type 'a unop = {var: Variable.t; op: 'a; arg: Exp.nexp}

  type for_ = t

  type t = {
    name: Variable.t;
    cond: Comparator.t unop;
    inc: Increment.t unop;
    other_incs: Increment.t unop list;
    post_body: Stmt.t;
  }

  let extract_incs (r:Range.t) (l:Increment.t unop list) : Stmt.t =
    let l : Decl.t list =
      match r.step with
      | Plus step ->
        l
        |> List.map (fun i ->
            let open Increment in
            match i.op with
            | Plus | Minus ->
              let iters =
                Exp.n_div (Exp.n_minus (Var r.var) r.lower_bound)
                  step
              in
              let d =
                (* i = i.inc * ((r.var - r.init) / r.inc) + i.init *)
                let i_inc =
                  if i.op = Plus then i.arg else Exp.n_uminus i.arg
                in
                Decl.set r.var
                  (Exp.n_plus
                    (Exp.n_mult i_inc iters)
                    (Var i.var)
                  )
              in
              d
            | _ ->
              Decl.unset i.var
          )
      | Mult _ ->
        l
        |> List.map (fun i -> Decl.unset i.var)
    in
    if l = [] then
      Skip
    else
      Decl l


  let parse_init: Stmt.t -> (Variable.t * Exp.nexp * Stmt.t) option =
    function
    | Decl ({var; init=Some data; _}::l) ->
      Some (var, data, Decl l)
    | Seq (Decl [{var; init=Some data; _}], s) ->
      Some (var, data, s)
    | Assign {var; data; _} ->
      Some (var, data, Skip)
    | _ -> None

  let parse_cond : Exp.bexp -> Comparator.t unop option =
    function
    | NRel (o, Var var, arg) ->
      (match Comparator.parse o with
      | Some op -> Some {var; op; arg}
      | _ -> None)
    | CastBool (Binary (Minus, Var var, arg)) ->
      Some {var; op=RelMinus; arg}
    | _ -> None

  (**
    Find every increment that is possible to find.
    The remainding statements should be kept in order.
   *)
  let parse_inc : Stmt.t -> Increment.t unop list * Stmt.t =
    let parse (var:Variable.t) (o:N_binary.t) (arg:Exp.nexp) : Increment.t unop option =
      o
      |> Increment.parse
      |> Option.map (fun op -> {var; op; arg})
    in
    let rec loop (accum:Increment.t unop list) (s:Stmt.t) : Stmt.t list -> Increment.t unop list * Stmt.t =
      function
      | [] -> (accum, s)
      | s1 :: l ->
        let inc : Increment.t unop option =
          match s1 with
          | Assign {var=l; data=Binary (o, Var l1, Var l2); _} ->
            if Variable.equal l l1 then
              parse l o (Var l2)
            else if Variable.equal l l2 then
              parse l o (Var l1)
            else
              None
          | Assign {
              var=l;
              data=(Binary (o, r, Var l') | Binary (o, Var l', r));
            _}
          ->
            if Variable.equal l l' then
              parse l o r
            else
              None
          | _ -> None
        in
        let (s, accum) =
          match inc with
          | Some o -> (s, o :: accum)
          | None -> (Stmt.seq s1 s, accum)
        in
        loop accum s l
    in
    fun s ->
    loop [] Skip (Stmt.to_list s)


  let parse (loop: for_) : t option =
    let (incs, inc_stmt) = parse_inc loop.inc in
    let rec iter (skipped:Increment.t unop list) : Increment.t unop list -> t option =
      function
      | inc :: todo ->
        let name = inc.var in
        (* Try to find a range from this increment: *)
        (match
          let* cond = parse_cond loop.cond in
          Some {
            other_incs=skipped @ todo;
            post_body=Stmt.Skip;
            name; cond; inc;
          }
        with
        | Some _ as o ->
          (* This increment worked, return it *)
          o
        | None ->
          (* This increment didn't work, try again *)
          iter (inc::skipped) todo)
      | [] -> None (* Failed inference *)
    in
    incs
    (* Infer a range *)
    |> iter []
    (* And if we find it, add the non-increments to post_body *)
    |> Option.map (fun x ->
      {x with post_body=Stmt.seq x.post_body inc_stmt}
    )

  let infer_bounds : t -> Exp.nexp * Exp.nexp * Range.direction =
    function
    (* (int i = 0; i < 4; i++) *)
    | {name; cond={op=Lt; arg=ub; _}; _} ->
      (Var name, Binary (Minus, ub, Num 1), Range.Increase)
    (* (int i = 0; i <= 4; i++) *)
    | {name; cond={op=Le; arg=ub; _}; _} ->
      (Var name, ub, Increase)
    (* (int i = 4; i - k; i++) *)
    | {name; cond={op=RelMinus; arg=ub; _}; _} ->
      (Var name, ub, Range.Increase)
    (* (int i = 4; i >= 0; i--) *)
    | {name; cond={op=Ge; arg=lb; _}; _} ->
      (lb, Var name, Decrease)
    (* (int i = 4; i > 0; i--) *)
    | {name; cond={op=Gt; arg=lb; _}; _} ->
      (Binary (Plus, Num 1, lb), Var name, Decrease)


  let infer_step (r:t) : Range.Step.t option =
    match r.inc with
    | {op=Plus; arg=a; _}
    | {op=Minus; arg=a; _} -> Some (Range.Step.Plus a)
    | {op=Mult; arg=a; _}
    | {op=Div; arg=a; _} ->
      Some (Range.Step.Mult a)
    | {op=LeftShift; arg=Num a; _}
    | {op=RightShift; arg=Num a; _} ->
      Some (Range.Step.Mult (Num (Stage0.Common.pow ~base:2 a)))
    | _ -> None


  let to_range (r:t) : Range.t option =
    let (lower_bound, upper_bound, dir) = infer_bounds r in
    let* step = infer_step r in
    let dst = Range.{
      var=r.name; lower_bound; upper_bound; step; dir; ty=C_type.int;
    } in
    Some dst
end

let to_stmt (l:t) (body:Stmt.t) : Stmt.t =
  match
    let* inf = Infer.parse l in
    let* r = Infer.to_range inf in
    Some (inf, r)
  with
  | Some (inf, r) ->
    let body =
      Stmt.from_list [
        Infer.extract_incs r inf.other_incs;
        body;
        inf.post_body
      ]
    in
    Stmt.seq l.init (For (r, body))
  | None ->
    let body =
      Stmt.If (l.cond, Stmt.seq body l.init, Skip)
    in
    Stmt.seq l.init (Star body)

let infer_while (cond:Exp.bexp) (body:Stmt.t) : Stmt.t =
  let f = {
    init = Skip;
    inc = Stmt.last body;
    cond = cond
  } in
  to_stmt f (Stmt.skip_last body)

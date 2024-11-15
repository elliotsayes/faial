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

  type 'a unop = {op: 'a; arg: Exp.nexp}

  type for_ = t

  type t = {
    pre: Stmt.t;
    name: Variable.t;
    init: Exp.nexp;
    cond: Comparator.t unop;
    inc: Increment.t unop;
  }

  let parse_init: Stmt.t -> (Variable.t * Exp.nexp * Stmt.t) option =
    function
    | Decl ({var; init=Some data; _}::l) ->
      Some (var, data, Decl l)
    | Seq (Decl [{var; init=Some data; _}], s) ->
      Some (var, data, s)
    | Assign {var; data; _} ->
      Some (var, data, Skip)
    | _ -> None

  let parse_cond : Exp.bexp -> (Variable.t * Comparator.t unop) option =
    function
    | NRel (o, Var d, r) ->
      (match Comparator.parse o with
      | Some o -> Some (d, {op=o; arg=r})
      | _ -> None)
    | CastBool (Binary (Minus, Var d, r)) ->
      Some (d, {op=RelMinus; arg=r})
    | _ -> None

  let parse_inc (s:Stmt.t) : (Variable.t * Increment.t unop) option =
    let parse_inc (var:Variable.t) (o:N_binary.t) (arg:Exp.nexp) : (Variable.t * Increment.t unop) option =
      o
      |> Increment.parse
      |> Option.map (fun op ->
          (var, {op; arg})
        )
    in
    match Stmt.first s with
    | Assign {var=l; data=Binary (o, Var l1, Var l2); _} ->
      if Variable.equal l l1 then
        parse_inc l o (Var l2)
      else if Variable.equal l l2 then
        parse_inc l o (Var l1)
      else
        None
    | Assign {
        var=l;
        data=(Binary (o, r, Var l') | Binary (o, Var l', r));
      _}
    ->
      if Variable.equal l l' then
        parse_inc l o r
      else
        None
    | _ -> None

  let parse (loop: for_) : t option =
    let* (name, inc) = parse_inc loop.inc in
    let* (init, pre) : Exp.nexp * Stmt.t =
      match parse_init loop.init with
      | Some (x2, init, pre) ->
        if Variable.equal name x2 then Some (init, pre) else None
      | None -> Some (Var name, Stmt.Skip)
    in
    let* (_, cond) = parse_cond loop.cond in
    Some {pre; name; init; cond; inc;}

  let infer_bounds : t -> Exp.nexp * Exp.nexp * Range.direction =
    function
    (* (int i = 0; i < 4; i++) *)
    | {init=lb; cond={op=Lt; arg=ub; _}; _} ->
      (lb, Binary (Minus, ub, Num 1), Range.Increase)
    (* (int i = 0; i <= 4; i++) *)
    | {init=lb; cond={op=Le; arg=ub; _}; _} ->
      (lb, ub, Increase)
    (* (int i = 4; i - k; i++) *)
    | {init=lb; cond={op=RelMinus; arg=ub; _}; _} ->
      (lb, ub, Range.Increase)
    (* (int i = 4; i >= 0; i--) *)
    | {init=ub; cond={op=Ge; arg=lb; _}; _} ->
      (lb, ub, Decrease)
    (* (int i = 4; i > 0; i--) *)
    | {init=ub; cond={op=Gt; arg=lb; _}; _} ->
      (Binary (Plus, Num 1, lb), ub, Decrease)


  let infer_step (r:t) : Range.Step.t option =
    match r.inc with
    | {op=Plus; arg=a}
    | {op=Minus; arg=a} -> Some (Range.Step.Plus a)
    | {op=Mult; arg=a}
    | {op=Div; arg=a} ->
      Some (Range.Step.Mult a)
    | {op=LeftShift; arg=Num a}
    | {op=RightShift; arg=Num a} ->
      Some (Range.Step.Mult (Num (Stage0.Common.pow ~base:2 a)))
    | _ -> None


  let to_range (r:t) : (Stmt.t * Range.t) option =
    let (lower_bound, upper_bound, dir) = infer_bounds r in
    let* step = infer_step r in
    let dst = Range.{
      var=r.name; lower_bound; upper_bound; step; dir; ty=C_type.int;
    } in
    Some (r.pre, dst)
end

(* Given a for-loop range, output a protocol Range *)
let to_range (loop:t) : (Stmt.t * Range.t) option =
  let* inf = Infer.parse loop in
  Infer.to_range inf

let to_stmt (l:t) (body:Stmt.t) : Stmt.t =
  match to_range l with
  | Some (s, r) -> Stmt.seq s (For (r, body))
  | None -> Star body

let infer_while (cond:Exp.bexp) (body:Stmt.t) : Stmt.t =
  let f = {
    init = Skip;
    inc = Stmt.last body;
    cond = cond
  } in
  to_stmt f (Stmt.skip_last body)

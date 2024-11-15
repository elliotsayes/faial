open Protocols

let ( let* ) = Option.bind

type t = {
  init: Stmt.t option;
  cond: Exp.bexp option;
  inc: Stmt.t option;
}

let to_string : t -> string =
  function
  | {init; cond; inc} ->
    let init =
      init
      |> Option.map Stmt.to_string
      |> Option.value ~default:""
    in
    let cond =
      cond
      |> Option.map Exp.b_to_string
      |> Option.value ~default:""
    in
    let inc =
      inc
      |> Option.map Stmt.to_string
      |> Option.value ~default:""
    in
    "(" ^ init ^ "; " ^ cond ^ "; " ^ inc ^ ")"

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
    name: Variable.t;
    init: Exp.nexp;
    cond: Comparator.t unop;
    inc: Increment.t unop;
  }

  let parse_init: Stmt.t option -> (Variable.t * Exp.nexp) option =
    function
    | Some (Decl ({var; init=Some data; _}::_))
    | Some (Seq (Decl [{var; init=Some data; _}], _))
    | Some (Assign {var; data; _}) ->
      Some (var, data)
    | _ -> None

  let parse_cond : Exp.bexp option -> (Variable.t * Comparator.t unop) option =
    function
    | Some (NRel (o, Var d, r)) ->
      (match Comparator.parse o with
      | Some o -> Some (d, {op=o; arg=r})
      | _ -> None)
    | Some (CastBool (Binary (Minus, Var d, r))) ->
      Some (d, {op=RelMinus; arg=r})
    | _ -> None

  let parse_inc (s:Stmt.t option) : (Variable.t * Increment.t unop) option =
    let parse_inc (var:Variable.t) (o:N_binary.t) (arg:Exp.nexp) : (Variable.t * Increment.t unop) option =
      o
      |> Increment.parse
      |> Option.map (fun op ->
          (var, {op; arg})
        )
    in
    match Option.map Stmt.first s with
    | Some (Assign {var=l; data=Binary (o, Var l1, Var l2); _}) ->
      if Variable.equal l l1 then
        parse_inc l o (Var l2)
      else if Variable.equal l l2 then
        parse_inc l o (Var l1)
      else
        None
    | Some (Assign {
        var=l;
        data=(Binary (o, r, Var l') | Binary (o, Var l', r));
      _})
    ->
      if Variable.equal l l' then
        parse_inc l o r
      else
        None
    | _ -> None

  let parse (loop: for_) : t option =
    let* (x1, inc) = parse_inc loop.inc in
    let* init : Exp.nexp =
      match parse_init loop.init with
      | Some (x2, init) ->
        if Variable.equal x1 x2 then Some init else None
      | None -> Some (Var x1)
    in
    let* (_, cond) = parse_cond loop.cond in
    Some {
      name = x1;
      init = init;
      cond = cond;
      inc = inc;
    }

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


  let to_range (r:t) : Range.t option =
    let (lb, ub, d) = infer_bounds r in
    let* step = infer_step r in
    Some Range.{
      var=r.name;
      lower_bound=lb;
      upper_bound=ub;
      step=step;
      dir=d;
      ty=C_type.int;
    }
end

(* Given a for-loop range, output a protocol Range *)
let to_range (loop:t) : Range.t option =
  let* inf = Infer.parse loop in
  Infer.to_range inf

let to_stmt (l:t) (body:Stmt.t) : Stmt.t =
  match to_range l with
  | Some r -> For (r, body)
  | None -> Star body

let infer_while (cond:Exp.bexp) (body:Stmt.t) : Stmt.t =
  let f = {
    init = None;
    inc = Some (Stmt.last body);
    cond = Some cond
  } in
  to_stmt f (Stmt.skip_last body)

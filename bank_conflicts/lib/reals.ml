open Protocols

type nrel = Exp.nrel
type brel = Exp.brel

module BinOp = struct
  type t =
    | BitOr
    | BitXOr
    | BitAnd
    | LeftShift
    | RightShift
    | Plus
    | Minus
    | Mult
    | Div
    | Mod
    | Pow

  let from_nbin : Exp.nbin -> t =
    function
    | BitOr -> BitOr
    | BitXOr -> BitXOr
    | BitAnd -> BitAnd
    | LeftShift -> LeftShift
    | RightShift -> RightShift
    | Plus -> Plus
    | Minus -> Minus
    | Mult -> Mult
    | Div -> Div
    | Mod -> Mod

  let to_string : t -> string =
    function
    | BitOr -> "|"
    | BitXOr -> "^"
    | BitAnd -> "&"
    | LeftShift -> "<<"
    | RightShift -> ">>"
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Pow -> "^"
end

module TruncateOp = struct
  type t =
    | Floor
    | Ceiling

  let to_string : t -> string =
    function
    | Floor -> "floor"
    | Ceiling -> "ceiling"
end

type integer =
  | Var of Variable.t
  | Num of int
  | FloatToInt of TruncateOp.t * floating_point
  | Bin of BinOp.t * integer * integer
  | BitNot of integer
  | If of boolean * integer * integer
  | BoolToInt of boolean

and floating_point =
  | Float of float
  | Log of integer * floating_point
  | IntToFloat of integer

and boolean =
  | Bool of bool
  | NRel of nrel * integer * integer
  | BRel of brel * boolean * boolean
  | BNot of boolean
  | IntToBool of integer

type t = integer

let from_int (e:int) : integer =
  Num e

let int_to_float : integer -> floating_point =
  function
  | FloatToInt (_, e) -> e
  | e -> IntToFloat e

let float_to_int (o:TruncateOp.t) : floating_point -> integer =
  function
  | IntToFloat e -> e
  | e -> FloatToInt (o, e)

let bool_to_int : boolean -> integer =
  function
  | Bool true -> Num 1
  | Bool false -> Num 0
  | e -> BoolToInt e

let superscript (n:int) : string =
  String.fold_right (fun c a ->
    let c = match c with
    | '0' -> "⁰"
    | '1' -> "¹"
    | '2' -> "²"
    | '3' -> "³"
    | '4' -> "⁴"
    | '5' -> "⁵"
    | '6' -> "⁶"
    | '7' -> "⁷"
    | '8' -> "⁸"
    | '9' -> "⁹"
    | '-' -> "⁻"
    | c -> String.make 1 c
    in
    c ^ a
  ) (string_of_int n) ""

let subscript (n:int) : string =
  String.fold_right (fun c a ->
    let c = match c with
    | '0' -> "₀"
    | '1' -> "₁"
    | '2' -> "₂"
    | '3' -> "₃"
    | '4' -> "₄"
    | '5' -> "₅"
    | '6' -> "₆"
    | '7' -> "₇"
    | '8' -> "₈"
    | '9' -> "₉"
    | '-' -> "₋"
    | c -> String.make 1 c
    in
    c ^ a
  ) (string_of_int n) ""

let zero : integer =
  Num 0

let one : integer =
  Num 1

let pow (base:integer) : integer -> integer =
  match base with
  | Num 0 -> fun _ -> Num 1
  | Num 1 -> fun e -> e
  | _ -> fun arg -> Bin (Pow, base, arg)

let log (base:integer) (arg: floating_point) : floating_point =
  match base, arg with
  | Num base, IntToFloat (Num x) -> Float (log (Float.of_int x) /. log (Float.of_int base))
  | Num base, Float arg -> Float (log arg /. log (Float.of_int base))
  | _, _ -> Log (base, arg)

let div (lhs:integer) (rhs:integer) : integer =
  match lhs, rhs with
  | e, Num 1 -> e
  | Num lhs, Num rhs -> Num (lhs / rhs)
  | Num 0, _ -> Num 0
  | lhs, rhs -> Bin (Div, lhs, rhs)

let minus (lhs:integer) (rhs:integer) : integer =
  match lhs, rhs with
  | Num lhs, Num rhs -> Num (lhs - rhs)
  | e, Num 0 -> e
  | lhs, rhs -> Bin (Minus, lhs, rhs)

let uminus (e:integer) : integer =
  minus (Num 0) e

let mult (lhs:integer) (rhs:integer) : integer =
  match lhs, rhs with
  | Num 0, _ | _, Num 0 -> Num 0
  | Num 1, e | e, Num 1 -> e
  | lhs, rhs -> Bin (Mult, lhs, rhs)

let plus (lhs:integer) (rhs:integer) : integer =
  match lhs, rhs with
  | Num lhs, Num rhs -> Num (lhs + rhs)
  | Num 0, e | e, Num 0 -> e
  | _, _ ->
    Bin (Plus, lhs, rhs)

let inc (e:integer) : integer =
  plus e (Num 1)

let dec (e:integer) : integer =
  minus e (Num 1)

let bin (o:BinOp.t) (lhs:integer) (rhs:integer) : integer =
  match o with
  | Div -> div lhs rhs
  | Mult -> mult lhs rhs
  | Plus -> plus lhs rhs
  | Minus -> minus lhs rhs
  | Pow -> pow lhs rhs
  | o -> Bin (o, lhs, rhs)

let ceiling : floating_point -> integer =
  function
  | IntToFloat e -> e
  | Float f -> Num (Int.of_float (Float.ceil f))
  | e -> FloatToInt (Ceiling, e)

let floor : floating_point -> integer =
  function
  | IntToFloat e -> e
  | Float f -> Num (Int.of_float (Float.floor f))
  | e -> FloatToInt (Floor, e)

let if_ (cond: boolean) (e1:integer) (e2:integer) : integer =
  match cond with
  | Bool b -> if b then e1 else e2
  | _ -> If (cond, e1, e2)

let bit_not : integer -> integer =
  function
  | BitNot e -> e
  | e -> BitNot e

let is_one : t -> bool =
  function
  | Num 1 -> true
  | _ -> false

let n_rel (o:Exp.nrel) (e1:integer) (e2:integer) : boolean =
  NRel (o, e1, e2)

let b_rel (o:Exp.brel) (e1:boolean) (e2:boolean) : boolean =
  BRel (o, e1, e2)

let not_ (e:boolean) : boolean =
  BNot e

let int_to_bool : integer -> boolean =
  function
  | Num n -> Bool (n <> 0)
  | e -> IntToBool e

let rec from_nexp : Exp.nexp -> t =
  function
  | Var x -> Var x
  | Num x -> Num x
  | Bin (b, e1, e2) ->
    bin (BinOp.from_nbin b) (from_nexp e1) (from_nexp e2)
  | BitNot e -> BitNot (from_nexp e)
  | NCall _ -> failwith "NCall(_,_)"
  | Other _ -> failwith "Other _"
  | NIf (e1, e2, e3) ->
    if_ (from_bexp e1) (from_nexp e2) (from_nexp e3)
  | CastInt e -> BoolToInt (from_bexp e)

and from_bexp : Exp.bexp -> boolean =
  function
  | Bool b -> Bool b
  | NRel (o, e1, e2) -> n_rel o (from_nexp e1) (from_nexp e2)
  | BRel (o, e1, e2) -> b_rel o (from_bexp e1) (from_bexp e2)
  | BNot e -> not_ (from_bexp e)
  | Pred _ -> failwith "Pred _"
  | CastBool e -> IntToBool (from_nexp e)

let rec to_string : t -> string =
  function
  | Var x -> Variable.name x
  | Num x -> string_of_int x
  | FloatToInt (Ceiling, e) -> "⌈" ^ f_to_string e ^ "⌉"
  | FloatToInt (Floor, e) -> "⌊" ^ f_to_string e ^ "⌋"
  | Bin (o, e1, e2) ->
    "(" ^ to_string e1 ^ " " ^ BinOp.to_string o ^
    " " ^ to_string e2 ^ ")"
  | BitNot e ->
    "~(" ^ to_string e ^ ")"
  | If (b, e1, e2) ->
    "(" ^ b_to_string b ^ ")?(" ^ to_string e1 ^ "):(" ^
    to_string e2 ^")"
  | BoolToInt e -> "int(" ^ b_to_string e ^ ")"
and b_to_string : boolean -> string =
  function
  | Bool true -> "true"
  | Bool false -> "false"
  | NRel (o, e1, e2) ->
    "(" ^ to_string e1 ^ " " ^ Exp.nrel_to_string o ^
    " " ^ to_string e2 ^ ")"
  | BRel (o, e1, e2) ->
    "(" ^ b_to_string e1 ^ " " ^ Exp.brel_to_string o ^
    " " ^ b_to_string e2 ^ ")"
  | BNot e ->
    "!(" ^ b_to_string e ^ ")"
  | IntToBool e -> to_string e
and f_to_string : floating_point -> string =
  function
  | Float f -> string_of_float f
  | Log (Num b, e) ->
    "log" ^ subscript b ^"(" ^ f_to_string e ^ ")"
  | Log (b, e) ->
    "log(" ^ to_string b ^", " ^ f_to_string e ^ ")"
  | IntToFloat e -> to_string e

let subst ((x,v):Variable.t * t) : t -> t =
  let rec i_subst (e: integer) : integer =
    match e with
    | Var y -> if Variable.equal x y then v else e
    | Num _ -> e
    | FloatToInt (o, e) -> FloatToInt (o, f_subst e)
    | Bin (o, e1, e2) -> Bin (o, i_subst e1, i_subst e2)
    | BitNot e -> BitNot (i_subst e)
    | If (e1, e2, e3) -> If (b_subst e1, i_subst e2, i_subst e3)
    | BoolToInt e -> BoolToInt (b_subst e)
  and f_subst : floating_point -> floating_point =
    function
    | Float _ as f -> f
    | Log (i, e) -> Log (i_subst i, f_subst e)
    | IntToFloat e -> IntToFloat (i_subst e)
  and b_subst: boolean -> boolean =
    function
    | Bool _ as e -> e
    | NRel (o, e1, e2) -> NRel (o, i_subst e1, i_subst e2)
    | BRel (o, e1, e2) -> BRel (o, b_subst e1, b_subst e2)
    | BNot e -> BNot (b_subst e)
    | IntToBool e -> IntToBool (i_subst e)
  in
  i_subst

let optimize : t -> t =
  let rec i_opt : t -> t =
    function
    | Var _ as e -> e
    | Num _ as e -> e
    | FloatToInt (o, e) -> float_to_int o (f_opt e)
    | Bin (o, e1, e2) -> bin o (i_opt e1) (i_opt e2)
    | BitNot e -> bit_not (i_opt e)
    | If (e1, e2, e3) -> if_ (b_opt e1) (i_opt e2) (i_opt e3)
    | BoolToInt e -> bool_to_int (b_opt e)
  and f_opt : floating_point -> floating_point =
    function
    | Float _ as e -> e
    | Log (b, e) -> log b (f_opt e)
    | IntToFloat e -> int_to_float (i_opt e)
  and b_opt : boolean -> boolean =
    function
    | Bool _ as e -> e
    | NRel (o, e1, e2) -> n_rel o (i_opt e1) (i_opt e2)
    | BRel (o, e1, e2) -> b_rel o (b_opt e1) (b_opt e2)
    | BNot e -> not_ (b_opt e)
    | IntToBool e -> int_to_bool (i_opt e)
  in
  i_opt


let free_names : t -> Variable.Set.t -> Variable.Set.t =
  let rec i_fns : integer -> Variable.Set.t -> Variable.Set.t =
    function
    | Var x -> fun fns -> Variable.Set.add x fns
    | Num _ -> fun fns -> fns
    | FloatToInt (_, e) -> f_fns e
    | Bin (_, e1, e2) ->
      fun fns -> i_fns e1 fns |> i_fns e2
    | BitNot e -> i_fns e
    | If (e1, e2, e3) ->
      fun fns ->
        b_fns e1 fns |> i_fns e2 |> i_fns e3
    | BoolToInt e ->
      b_fns e
  and b_fns : boolean -> Variable.Set.t -> Variable.Set.t =
    function
    | Bool _ -> fun fns -> fns
    | NRel (_, e1, e2) ->
      fun fns ->
        i_fns e1 fns |> i_fns e2
    | BRel (_, e1, e2) ->
      fun fns ->
        b_fns e1 fns |> b_fns e2
    | BNot e ->
      b_fns e
    | IntToBool e ->
      i_fns e
  and f_fns : floating_point -> Variable.Set.t -> Variable.Set.t =
    function
    | Float _ -> fun fns -> fns
    | Log (b, e) ->
      fun fns ->
        i_fns b fns |> f_fns e
    | IntToFloat e ->
      i_fns e
  in
  i_fns

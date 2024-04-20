open Protocols

type nbin = Exp.nbin
type nrel = Exp.nrel
type brel = Exp.brel

type tr_op =
  | Floor
  | Ceiling

let tr_op_to_string : tr_op -> string =
  function
  | Floor -> "floor"
  | Ceiling -> "ceiling"

type fbin =
  | FMult
  | FDiv

let fbin_to_string : fbin -> string =
  function
  | FMult -> "*"
  | FDiv -> "/"

type f_unop =
  | Logarithm
  | Exponent

let f_unop_to_string : f_unop -> string =
  function
  | Logarithm -> "log"
  | Exponent -> "exp"

type integer =
  | Var of Variable.t
  | Num of int
  | PowerOf of (int * integer)
  | FloatToInt of tr_op * floating_point
  | Bin of nbin * integer * integer
  | BitNot of integer
  | NIf of boolean * integer * integer
  | BoolToInt of boolean

and floating_point =
  | FBin of fbin * floating_point * floating_point
  | FUnop of f_unop * int * floating_point
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

let int_to_float (e:t) : floating_point =
  IntToFloat e

let subscript_to_string (n:int) : string =
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

let rec from_nexp : Exp.nexp -> t =
  function
  | Var x -> Var x
  | Num x -> Num x
  | Bin (b, e1, e2) -> Bin (b, from_nexp e1, from_nexp e2)
  | BitNot e -> BitNot (from_nexp e)
  | NCall _ -> failwith "NCall(_,_)"
  | Other _ -> failwith "Other _"
  | NIf (e1, e2, e3) -> NIf (from_bexp e1, from_nexp e2, from_nexp e3)
  | CastInt e -> BoolToInt (from_bexp e)

and from_bexp : Exp.bexp -> boolean =
  function
  | Bool b -> Bool b
  | NRel (o, e1, e2) -> NRel (o, from_nexp e1, from_nexp e2)
  | BRel (o, e1, e2) -> BRel (o, from_bexp e1, from_bexp e2)
  | BNot e -> BNot (from_bexp e)
  | Pred _ -> failwith "Pred _"
  | CastBool e -> IntToBool (from_nexp e)

let logarithm (base:int) (arg:floating_point) : floating_point =
  FUnop (Logarithm, base, arg)

let power_of (base:int) (arg:integer) : integer =
  PowerOf (base, arg)

let i_minus (lhs:integer) (rhs:integer) : integer =
  Bin (Minus, lhs, rhs)

let i_mult (lhs:integer) (rhs:integer) : integer =
  Bin (Mult, lhs, rhs)

let ceiling (e:floating_point) : integer =
  FloatToInt (Ceiling, e)

let floor (e:floating_point) : integer =
  FloatToInt (Floor, e)

let is_one : t -> bool =
  function
  | Num 1 -> true
  | _ -> false

let rec to_string : t -> string =
  function
  | Var x -> Variable.name x
  | Num x -> string_of_int x
  | FloatToInt (Ceiling, e) -> "⌈" ^ f_to_string e ^ "⌉"
  | FloatToInt (Floor, e) -> "⌊" ^ f_to_string e ^ "⌋"
  | Bin (o, e1, e2) ->
    "(" ^ to_string e1 ^ " " ^ Exp.nbin_to_string o ^
    " " ^ to_string e2 ^ ")"
  | BitNot e ->
    "~(" ^ to_string e ^ ")"
  | NIf (b, e1, e2) ->
    "(" ^ b_to_string b ^ ")?(" ^ to_string e1 ^ "):(" ^
    to_string e2 ^")"
  | BoolToInt e -> "int(" ^ b_to_string e ^ ")"
  | PowerOf (base, e) ->
    "exponent(" ^ string_of_int base ^ ", " ^ to_string e ^ ")"
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
  | FBin (o, e1, e2) ->
    "(" ^ f_to_string e1 ^ " " ^ fbin_to_string o ^
    " " ^ f_to_string e2 ^ ")"
  | FUnop (Logarithm, b, e) ->
    "log" ^ subscript_to_string b ^"(" ^ f_to_string e ^ ")"
  | FUnop (Exponent, b, e) ->
    "(" ^ string_of_int b ^ ", " ^
    f_to_string e ^ ")" ^ Poly.exponent_to_string b
  | IntToFloat e -> to_string e

let subst ((x,v):Variable.t * t) : t -> t =
  let rec i_subst (e: integer) : integer =
    match e with
    | Var y -> if Variable.equal x y then v else e
    | Num _ -> e
    | FloatToInt (o, e) -> FloatToInt (o, f_subst e)
    | Bin (o, e1, e2) -> Bin (o, i_subst e1, i_subst e2)
    | BitNot e -> BitNot (i_subst e)
    | NIf (e1, e2, e3) -> NIf (b_subst e1, i_subst e2, i_subst e3)
    | BoolToInt e -> BoolToInt (b_subst e)
    | PowerOf (base, e) -> PowerOf (base, i_subst e)
  and f_subst : floating_point -> floating_point =
    function
    | FBin (o, e1, e2) -> FBin (o, f_subst e1, f_subst e2)
    | FUnop (o, i, e) -> FUnop (o, i, f_subst e)
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
(*
let optimize : t -> t =
  map Constfold.n_opt
*)
(*
let rec free_names (e:t) : Variable.Set.t -> Variable.Set.t =
  match e with
  | Int e -> Freenames.free_names_nexp e
  | Logarithm {arg; _}
  | Exponent {arg; _}
  | UnOp {arg; _} -> free_names arg
  | BinOp {lhs; rhs; _} ->
    fun fns ->
      free_names lhs fns
      |> free_names rhs
*)

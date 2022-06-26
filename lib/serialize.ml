open Exp
open Common

let s_list (elems:Smtlib.sexp list) : Smtlib.sexp =
  Smtlib.List elems

let symbol (s:string) = Smtlib.Atom (Smtlib.Symbol s)

let call (func:string) (args:Smtlib.sexp list) =
  let open Smtlib in
  s_list (symbol func::args)

let atoms : string list -> Smtlib.sexp list =
  List.map symbol

let flat_call func args = atoms args |> call func

let binop (f:string) arg1 arg2 = call f [arg1;arg2]
let unop (f:string) arg = call f [arg]

let s_map (f: 'a -> Smtlib.sexp) (l: 'a list) : Smtlib.sexp =
  List.map f l |> s_list

let s_print (s:Smtlib.sexp) : unit =
  Smtlib.output_sexp stdout s

module type NEXP_SERIALIZER = sig
  val n_ser: nexp -> Smtlib.sexp
  val b_ser: bexp -> Smtlib.sexp
end

let task_to_string t =
  match t with
    | Task1 -> "$T1"
    | Task2 -> "$T2"

let t_ser t =
  symbol (task_to_string t)

let brel_to_string (r:brel) =
  match r with
  | BOr -> "or"
  | BAnd -> "and"

let nbin_to_string : nbin -> string = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | LeftShift -> "<<"
  | RightShift -> ">>"
  | BitXOr -> "^"
  | BitOr -> "|"
  | BitAnd -> "&"



module StdNexp : NEXP_SERIALIZER = struct
  let nbin_to_string (m:nbin) : string =
    match m with
    | BitXOr
    | BitOr
    | BitAnd
    | LeftShift
    | RightShift
      ->
      let o = nbin_to_string m in
      prerr_endline ("WARNING: operator '" ^ o ^ "' unsupported; converting to '+'. Use bit-vector integers instead");
      "+"
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "div"
    | Mod -> "mod"

  let rec nrel_ser (r:nrel) =
    match r with
    | NEq -> binop "="
    | NLe -> binop "<="
    | NLt -> binop "<"
    | NGe -> binop ">="
    | NGt -> binop ">"
    | NNeq -> fun n1 n2 -> unop "not" (nrel_ser NEq n1 n2)

  let rec n_ser (a:nexp) : Smtlib.sexp =
    let open Smtlib in
    match a with
    | Proj (t, x) -> binop "proj" (t_ser t) (symbol (var_name x))
    | Num n -> Atom (Int n)
    | Var x -> Atom (Symbol (var_name x))
    | Bin (b, a1, a2) ->
      binop (nbin_to_string b) (n_ser a1) (n_ser a2)
    | NIf (b, n1, n2) -> call "if" [b_ser b; n_ser n1; n_ser n2]
    | NCall (x, n) -> call x [n_ser n]
  and b_ser (b:bexp) : Smtlib.sexp =
    match b with
    | Bool b -> Smtlib.Atom (Smtlib.Bool b)
    | NRel (b, a1, a2) ->
      nrel_ser b (n_ser a1) (n_ser a2)
    | BRel (b, b1, b2) ->
      binop (brel_to_string b) (b_ser b1) (b_ser b2)
    | BNot b -> unop "not" (b_ser b)
    | Pred (x, v) -> unop x (n_ser v)
end

module BvNexp : NEXP_SERIALIZER = struct
  let nbin_to_string (m:nbin) : string =
    match m with
    | BitOr -> "bvor"
    | BitAnd -> "bvand"
    | BitXOr -> "bvxor"
    | LeftShift -> "bvshl"
    | RightShift -> "bvshr"
    | Plus -> "bvadd"
    | Minus -> "bvusub"
    | Mult -> "bvmul"
    | Div -> "bvudiv"
    | Mod -> "bvurem"

  let rec nrel_ser (r:nrel) =
    match r with
    | NEq -> binop "="
    | NLe -> binop "bvule"
    | NLt -> binop "bvult"
    | NGe -> binop "bvuge"
    | NGt -> binop "bvugt"
    | NNeq -> fun n1 n2 -> unop "not" (nrel_ser NEq n1 n2)

  let rec n_ser (a:nexp) : Smtlib.sexp =
    let open Smtlib in
    match a with
    | Proj (t, x) -> binop "proj" (t_ser t) (symbol (var_name x))
    | Num n -> List [
        symbol "_";
        symbol ("bv" ^ (string_of_int n));
        symbol "32";
      ]
    | Var x -> symbol (var_name x)
    | Bin (b, a1, a2) ->
      binop (nbin_to_string b) (n_ser a1) (n_ser a2)
    | NIf (b, n1, n2) -> call "if" [b_ser b; n_ser n1; n_ser n2]
    | NCall (x, n) -> call x [n_ser n]

  and b_ser (b:bexp) : Smtlib.sexp =
    match b with
    | Bool b -> Smtlib.Atom (Smtlib.Bool b)
    | NRel (b, a1, a2) ->
      nrel_ser b (n_ser a1) (n_ser a2)
    | BRel (b, b1, b2) ->
      binop (brel_to_string b) (b_ser b1) (b_ser b2)
    | BNot b -> unop "not" (b_ser b)
    | Pred (x, v) -> unop x (n_ser v)
end

let n_ser = StdNexp.n_ser

let b_ser = StdNexp.b_ser

let m_ser m = match m with
  | Exp.R -> "ro"
  | Exp.W -> "rw"

let s_ser s =
  match s with
  | Default x -> n_ser x
  | StepName x -> symbol x

let r_ser r =
  call "range" [
    symbol (var_name r.range_var);
    n_ser r.range_lower_bound;
    n_ser r.range_upper_bound;
    s_ser r.range_step;
  ]

let a_ser a =
  let idx = Smtlib.List (List.map n_ser a.access_index) in
  call (m_ser a.access_mode) [idx]

let expr_acc_ser (x, a) : Smtlib.sexp =
  call "loc" [symbol (var_name x); a_ser a]

let acc_sym_ser (x, a, t) : Smtlib.sexp =
  call "loc" [symbol (var_name x); a_ser a; t_ser t]

let bexp_list pre = List.map b_ser pre

let bexp_list_ser name pre = bexp_list pre |> call name

let var_set_ser name (s:VarSet.t) =
  VarSet.elements s
    |> List.map var_name
    |> atoms
    |> call name

module PPrint = struct
  type t =
    | Line of string
    | Block of t list
    | Nil

  let doc_to_string ?indent:(p=4) (l: t list) : string =
    let b = Buffer.create 100 in
    let rec pp (accum:int) : t -> unit = function
      | Nil -> ()
      | Line s ->
        Common.repeat " " (p*accum) |> Buffer.add_string b;
        s |> Buffer.add_string b;
        "\n" |> Buffer.add_string b;
        ()
      | Block lines ->
        lines
        |> List.iter (pp (accum + 1))
    in
    List.iter (pp 0) l;
    Buffer.contents b

  let pp_doc (ppf:Format.formatter) ?indent:(p=4) : t list -> unit =
    let rec pp (accum:int) : t -> unit = function
      | Nil -> ()
      | Line s ->
        Format.fprintf ppf "%s%s\n" (Common.repeat " " (p*accum)) s
      | Block lines ->
        lines
        |> List.iter (pp (accum + 1))
    in
    List.iter (pp 0)

  let print_doc = pp_doc Format.std_formatter

  let ident: variable -> string = var_name

  let nrel_to_string (r:nrel) : string =
    match r with
    | NEq -> "=="
    | NLe -> "<="
    | NLt -> "<"
    | NGe -> ">="
    | NGt -> ">"
    | NNeq -> "!="

  let brel_to_string (r:brel) : string =
    match r with
    | BOr -> "||"
    | BAnd -> "&&"

  let rec n_par (n:nexp) : string =
    match n with
    | Proj _
    | Num _
    | Var _
    | NCall _
      -> n_to_s n
    | NIf _
    | Bin _
      -> "(" ^ n_to_s n ^ ")"
  and n_to_s : nexp -> string = function
    | Proj (t, x) ->
      "proj(" ^ task_to_string t ^ ", "  ^ ident x ^ ")"
    | Num n -> string_of_int n
    | Var x -> ident x
    | Bin (b, a1, a2) ->
      n_par a1 ^ " " ^ nbin_to_string b ^ " " ^ n_par a2
    | NCall (x, arg) ->
      x ^ "(" ^ n_to_s arg ^ ")"
    | NIf (b, n1, n2) ->
      b_par b ^ " ? " ^ n_par n1 ^ " : " ^ n_par n2
  and b_to_s : bexp -> string = function
    | Bool b -> if b then "true" else "false"
    | NRel (b, n1, n2) ->
      n_to_s n1 ^ " " ^nrel_to_string b ^ " " ^ n_to_s n2
    | BRel (b, b1, b2) ->
      b_par b1 ^ " " ^ brel_to_string b ^ " " ^ b_par b2
    | BNot b -> "!" ^ b_par b
    | Pred (x, v) -> x ^ "(" ^ n_to_s v ^ ")"
  and b_par (b:bexp) : string =
    match b with
    | Pred _
    | Bool _
    | NRel _ -> b_to_s b
    | BNot _
    | BRel _ -> "("  ^ b_to_s b ^ ")"

  let print_n (n:nexp) : unit =
    print_string (n_to_s n)

  (* ----------------- bool -------------------- *)

  let print_b (b:bexp) : unit =
    print_string (b_to_s b)

  let s_to_s s =
    match s with
    | Default x -> "+ " ^ n_to_s x
    | StepName "pow2" -> "* 2"
    | StepName x -> x

  let r_to_s (r : range) : string =
    let x = ident r.range_var in
    let lb = n_to_s r.range_lower_bound in
    let ub = n_to_s r.range_upper_bound in
    let s = match r.range_step with
    | Default (Num 1) -> ""
    | _ -> "; " ^ ident r.range_var ^ " " ^ s_to_s r.range_step
    in
    let d = match r.range_dir with
    | Increase -> "↑"
    | Decrease -> "↓"
    in
    x ^ " in " ^ lb ^ " .. " ^ ub ^ s ^ " " ^ d
    

  let mode_to_s: mode -> string = function
    | W -> "rw"
    | R -> "ro"

  let index_to_s (ns:nexp list) : string =
    match ns with
    | [] -> ""
    | _ ->
    let idx = ns
      |> List.map n_to_s
      |> join ", "
    in
     "[" ^ idx ^ "]"

  let acc_to_s (x, a) : string =
    mode_to_s a.access_mode ^ " " ^
    ident x ^ index_to_s a.access_index

  let acc_expr_to_s a : t list =
    [Line (acc_to_s a ^ ";")]

  let var_set_to_s (vs:VarSet.t) : string =
    VarSet.elements vs
    |> List.map ident
    |> Common.join ", "

  let array_to_s (a:array_t) : string =
    let ty = a.array_type |> Common.join " " in
    let ty = if ty = "" then "" else ty ^ "  "
    in
    let size =
      List.map string_of_int a.array_size
      |> Common.join ", "
    in
    let h = match a.array_hierarchy with
    | SharedMemory -> "shared"
    | GlobalMemory -> "global"
    in
    h ^ " " ^ ty ^ "[" ^ size ^ "]"

  let array_map_to_s (vs:array_t VarMap.t) : string =
    VarMap.bindings vs
    |> List.map (fun (k,v) -> var_name k ^ ": " ^ array_to_s v)
    |> Common.join ", "

end
open Exp
open Proto
open Common

let s_list elems =
  Sexplib.Sexp.List elems

let call func args =
  s_list ((Atom func)::args)

let atoms : string list -> Sexplib.Sexp.t list =
  List.map (fun x -> Sexplib.Sexp.Atom x)

let flat_call func args = atoms args |> call func

let binop f arg1 arg2 = call f [arg1;arg2]
let unop f arg = call f [arg]

let s_map (f: 'a -> Sexplib.Sexp.t) (l: 'a list) : Sexplib.Sexp.t =
  List.map f l |> s_list

let s_print (s:Sexplib.Sexp.t) : unit =
  Sexplib.Sexp.to_string_hum s
  |> print_endline

module type NEXP_SERIALIZER = sig
  val n_ser: nexp -> Sexplib.Sexp.t
end

let task_to_string t =
  match t with
    | Task1 -> "$T1"
    | Task2 -> "$T2"

let t_ser t =
  let open Sexplib in
  Sexp.Atom (task_to_string t)

module StdNexp : NEXP_SERIALIZER = struct
  let nbin_to_string (m:nbin) : string =
    match m with
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "div"
    | Mod -> "mod"

  let rec n_ser (a:nexp) : Sexplib.Sexp.t =
    let open Sexplib in
    match a with
    | Proj (t, x) -> binop "proj" (t_ser t) (Sexp.Atom x.var_name)
    | Num n -> Sexp.Atom (string_of_int n)
    | Var x -> Sexp.Atom x.var_name
    | Bin (b, a1, a2) ->
      binop (nbin_to_string b) (n_ser a1) (n_ser a2)
end

module BvNexp : NEXP_SERIALIZER = struct
  let nbin_to_string (m:nbin) : string =
    match m with
    | Plus -> "bvadd"
    | Minus -> "bvusub"
    | Mult -> "bvmul"
    | Div -> "bvudiv"
    | Mod -> "bvurem"

  let rec n_ser (a:nexp) : Sexplib.Sexp.t =
    let open Sexplib in
    match a with
    | Proj (t, x) -> binop "proj" (t_ser t) (Sexp.Atom x.var_name)
    | Num n -> Sexp.List [
        Sexp.Atom "_";
        Sexp.Atom ("bv" ^ (string_of_int n));
        Sexp.Atom "32";
      ]
    | Var x -> Sexp.Atom x.var_name
    | Bin (b, a1, a2) ->
      binop (nbin_to_string b) (n_ser a1) (n_ser a2)
end

let n_ser = StdNexp.n_ser

let brel_to_string (r:brel) =
  match r with
  | BOr -> "or"
  | BAnd -> "and"

module type BEXP_SERIALIZER = sig
  val b_ser: bexp -> Sexplib.Sexp.t
end

module StdBexp : BEXP_SERIALIZER =
  struct
    let rec nrel_ser (r:nrel) =
      match r with
      | NEq -> binop "="
      | NLe -> binop "<="
      | NLt -> binop "<"
      | NGe -> binop ">="
      | NGt -> binop ">"
      | NNeq -> fun n1 n2 -> unop "not" (nrel_ser NEq n1 n2)

    let rec b_ser (b:bexp) : Sexplib.Sexp.t =
      let open Sexplib in
      match b with
      | Bool b -> Sexp.Atom (if b then "true" else "false")
      | NRel (b, a1, a2) ->
        nrel_ser b (StdNexp.n_ser a1) (StdNexp.n_ser a2)
      | BRel (b, b1, b2) ->
        binop (brel_to_string b) (b_ser b1) (b_ser b2)
      | BNot b -> unop "not" (b_ser b)
      | Pred (x, v) -> unop x (n_ser v)
  end

module BvBexp : BEXP_SERIALIZER =
  struct
    let rec nrel_ser (r:nrel) =
      match r with
      | NEq -> binop "="
      | NLe -> binop "bvule"
      | NLt -> binop "bvult"
      | NGe -> binop "bvuge"
      | NGt -> binop "bvugt"
      | NNeq -> fun n1 n2 -> unop "not" (nrel_ser NEq n1 n2)

    let rec b_ser (b:bexp) : Sexplib.Sexp.t =
      let open Sexplib in
      match b with
      | Bool b -> Sexp.Atom (if b then "true" else "false")
      | NRel (b, a1, a2) ->
        nrel_ser b (BvNexp.n_ser a1) (BvNexp.n_ser a2)
      | BRel (b, b1, b2) ->
        binop (brel_to_string b) (b_ser b1) (b_ser b2)
      | BNot b -> unop "not" (b_ser b)
      | Pred (x, v) -> unop x (n_ser v)
  end

let b_ser = StdBexp.b_ser

let m_ser m = match m with
  | Exp.R -> "ro"
  | Exp.W -> "rw"

let s_ser s =
  match s with
  | Default x -> n_ser x
  | StepName x -> Sexplib.Sexp.Atom x

let r_ser r =
  call "range" [
    Sexplib.Sexp.Atom r.range_var.var_name;
    n_ser r.range_lower_bound;
    n_ser r.range_upper_bound;
    s_ser r.range_step;
  ]

let a_ser a =
  let idx = Sexplib.Sexp.List (List.map n_ser a.access_index) in
  call (m_ser a.access_mode) [idx]
(*
let rec base_inst_ser (f:'a -> Sexplib.Sexp.t) : 'a base_inst -> Sexplib.Sexp.t =
  let open Sexplib in
  function
  | Base a -> f a
  | Cond (b, p1) -> call "if" [
      b_ser b;
      base_prog_ser f p1;
    ]
  | Loop (r, p) -> binop "loop" (r_ser r) (base_prog_ser f p)

and base_prog_ser (f:'a -> Sexplib.Sexp.t) (p:('a base_inst) list) : Sexplib.Sexp.t =
  let open Sexplib in
  Sexp.List (List.map (base_inst_ser f) p)
*)
let expr_acc_ser (x, a) : Sexplib.Sexp.t =
  let open Sexplib in
  call "loc" [Sexp.Atom x.var_name; a_ser a]

let acc_sym_ser (x, a, t) : Sexplib.Sexp.t =
  let open Sexplib in
  call "loc" [Sexp.Atom x.var_name; a_ser a; t_ser t]
(*
let a_inst_ser (f: 'a -> Sexplib.Sexp.t) : 'a a_inst -> Sexplib.Sexp.t =
  function
    | Goal b -> unop "goal" (b_ser b)
    | Acc a -> f a

let acc_inst_ser : acc_inst -> Sexplib.Sexp.t =
  a_inst_ser expr_acc_ser

let sync_unsync_ser : sync_unsync -> Sexplib.Sexp.t =
  let open Sexplib in
  function
    | Sync -> Sexp.Atom "sync"
    | Unsync a -> acc_inst_ser a

let inst_ser : inst -> Sexplib.Sexp.t =
  base_inst_ser sync_unsync_ser

let prog_ser : prog -> Sexplib.Sexp.t =
  s_map inst_ser
*)

(*
let u_inst_ser : u_inst -> Sexplib.Sexp.t =
  base_inst_ser acc_inst_ser

let u_prog_ser : u_prog -> Sexplib.Sexp.t =
  s_map u_inst_ser

let s_inst_ser : s_inst -> Sexplib.Sexp.t =
  base_inst_ser (fun x -> call "phase" (List.map u_inst_ser x))

let s_prog_ser : s_prog -> Sexplib.Sexp.t =
  s_map s_inst_ser
*)
let bexp_list pre = List.map b_ser pre

let bexp_list_ser name pre = bexp_list pre |> call name

let var_set_ser name (s:VarSet.t) =
  VarSet.elements s
    |> List.map (fun x -> x.var_name)
    |> atoms
    |> call name

let kernel_ser (f:'a -> Sexplib.Sexp.t) (k:'a kernel) =
  let open Sexplib in
  Sexp.List [
    Sexp.Atom "kernel";
    unop "pre" (b_ser k.kernel_pre);
    var_set_ser "locations" k.kernel_locations;
    var_set_ser "locals" k.kernel_local_variables;
    var_set_ser "globals" k.kernel_global_variables;
    f k.kernel_code;
  ]


module PPrint = struct
  type t =
    | Line of string
    | Block of t list
    | Nil

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

  let ident (x:variable) : string = x.var_name

  let nbin_to_string : nbin -> string = function
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "%"

  let rec n_par (n:nexp) : string =
    match n with
    | Proj _
    | Num _
    | Var _ -> n_to_s n
    | Bin _ ->
      "(" ^ n_to_s n ^ ")"
  and n_to_s : nexp -> string = function
    | Proj (t, x) ->
      "proj(" ^ task_to_string t ^ ", "  ^ ident x ^ ")"
    | Num n -> string_of_int n
    | Var x -> ident x
    | Bin (b, a1, a2) ->
      n_par a1 ^ " " ^ nbin_to_string b ^ " " ^ n_par a2


  let print_n (n:nexp) : unit =
    print_string (n_to_s n)

  (* ----------------- bool -------------------- *)

  let rec nrel_to_string (r:nrel) : string =
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

  let rec b_to_s : bexp -> string = function
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

  let print_b (b:bexp) : unit =
    print_string (b_to_s b)

  let s_to_s s =
    match s with
    | Default x -> "+ " ^ n_to_s x
    | StepName "pow2" -> "* 2"
    | StepName x -> x

  let r_to_s (r : range) : string =
    ident r.range_var ^ " in " ^
    n_to_s r.range_lower_bound ^ " .. " ^
    n_to_s r.range_upper_bound ^
    (match r.range_step with
    | Default (Num 1) -> ""
    | _ -> "; " ^ ident r.range_var ^ " " ^ s_to_s r.range_step)

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

  let acc_expr_to_s (x, a) : t list =
    [Line (mode_to_s a.access_mode ^ " " ^
     ident x ^
     index_to_s a.access_index ^ ";")]

  let rec inst_to_s : inst -> t list =
    function
    | Sync -> [Line "sync;"]
    | Acc e -> acc_expr_to_s e
    | Cond (b, p1) -> [
        Line ("if (" ^ b_to_s b ^ ") {");
        Block (List.map inst_to_s p1 |> List.flatten);
        Line "}"
      ]
    | Loop (r, p) ->
      [
        Line ("foreach (" ^ r_to_s r ^ ") {");
        Block (List.map inst_to_s p |> List.flatten);
        Line "}"
      ]

  let prog_to_s (p: prog) : t list =
    List.map inst_to_s p |> List.flatten

(*
  let acc_inst_to_s : acc_inst -> t list =
    a_inst_ser expr_acc_ser

  let sync_unsync_to_doc : sync_unsync -> t list =
    function
      | Sync -> [Line "sync;"]
      | Unsync a -> acc_inst_to_s a

  let p_to_s: prog -> t list =
    base_p_to_s sync_unsync_to_doc
*)
  let print_p (p: prog) : unit =
    print_doc (prog_to_s p)

  let var_set_to_s (vs:VarSet.t) : string =
    VarSet.elements vs
    |> List.map ident
    |> Common.join ", "

  let kernel_to_s (f:'a -> t list) (k:'a kernel) : t list =
    [
      Line ("locations: " ^ var_set_to_s k.kernel_locations ^ ";");
      Line ("globals: " ^ var_set_to_s k.kernel_global_variables ^ ";");
      Line ("locals: " ^ var_set_to_s k.kernel_local_variables ^ ";");
      Line ("invariant: " ^ b_to_s k.kernel_pre ^";");
      Line "";
      Line "code {";
      Block (f k.kernel_code);
      Line "}"
    ]

  let print_kernel (f:'a -> t list) (k: 'a kernel) : unit =
    print_doc (kernel_to_s f k)

  let print_k (k:prog kernel) : unit =
    print_doc (kernel_to_s prog_to_s k)
end
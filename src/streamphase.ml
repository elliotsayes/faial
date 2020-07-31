open Proto
open Streamutil
(* ---------------- SECOND STAGE OF TRANSLATION ---------------------- *)

type locality = Global | Local

type 'a inst =
  | Base of 'a
  | Cond of bexp * ('a inst list)
  | Decl of variable * locality * ('a inst list)

type 'a prog = 'a inst list

type p_inst = acc_inst inst
type p_prog = acc_inst prog

let make_loop (l:locality) (r:range) (ls:'a prog) : 'a inst =
  Decl (r.range_var, l,
    [Cond (
      b_and
        (n_le r.range_lower_bound (Var r.range_var))
        (n_lt (Var r.range_var) r.range_upper_bound),
      ls
    )]
  )

let rec inst_to_p_inst (l:locality) (i: 'a base_inst) : 'a inst =
  match i with
  | Proto.Base x -> Base x
  | Proto.Cond (b, ls) -> Cond (b, prog_to_p_prog l ls)
  | Proto.Loop (r, ls) -> make_loop l r (prog_to_p_prog l ls)
and prog_to_p_prog (l:locality) (ls: 'a base_prog) : 'a prog =
  List.map (inst_to_p_inst l) ls

(* A synchronized-program has multiple goals to prove, so want to flatten
   each phased block into a list. Phased blocks may be nested inside a
   conditional or a variable declaration (loop), and in which case,
   we must ensure we preserve that structure. *)

let inline_vars (l:locality) (vars:VarSet.t) (p:p_prog) : p_prog =
  VarSet.elements vars
  |> List.fold_left (fun p x ->
    [Decl (x, l, p)]
  ) p

let rec inst_to_prog_stream (locals:VarSet.t) : s_inst -> p_prog Stream.t =
  function
  | Base p -> Stream.of_list [prog_to_p_prog Local p |> inline_vars Local locals]
  | Loop (r, l) ->
    prog_to_prog_stream locals l
    |> stream_map (fun p ->
      [make_loop Global r p]
    )
  | Cond (b, l) ->
    prog_to_prog_stream locals l
    |> stream_map (fun p ->
      [Cond (b, p)]
    )

and prog_to_prog_stream (locals:VarSet.t) (l: s_prog) : p_prog Stream.t =
  List.fold_left
    (fun s i -> inst_to_prog_stream locals i |> stream_seq s)
    (stream_make None)
    l

type p_kernel = {
  (* The shared locations that can be accessed in the kernel. *)
  p_kernel_locations: VarSet.t;
  (* The code of a kernel performs the actual memory accesses. *)
  p_kernel_code: p_prog;
}

let translate (k : s_prog kernel) : p_kernel Stream.t  =
  Streamutil.stream_map (fun p ->
      {
        p_kernel_locations = k.kernel_locations;
        p_kernel_code = inline_vars Global k.kernel_global_variables p
      }
    )
    (prog_to_prog_stream k.kernel_local_variables k.kernel_code)

(* --------------------SERIALIZE -------------------------- *)

open Serialize

let loc_ser : locality -> Sexplib.Sexp.t =
  let open Sexplib in
  function
  | Local -> Sexp.Atom "local"
  | Global -> Sexp.Atom "global"

let rec inst_ser (f : 'a -> Sexplib.Sexp.t) : 'a inst -> Sexplib.Sexp.t =
  function
  | Base a -> f a
  | Cond (b, ls) -> binop "if" (b_ser b) (prog_ser f ls)
  | Decl (x, l, ls) ->
    let open Sexplib in
    s_list [
      s_list [loc_ser l; Sexp.Atom x.var_name];
      prog_ser f ls
    ]
and prog_ser (f : 'a -> Sexplib.Sexp.t) (ls: 'a prog) : Sexplib.Sexp.t =
  List.map (inst_ser f) ls |> s_list

let kernel_ser (k:p_kernel) : Sexplib.Sexp.t =
  let open Sexplib in
  Sexp.List [
    Sexp.Atom "kernel";
    var_set_ser "locations" k.p_kernel_locations;
    unop "code" (prog_ser acc_inst_ser k.p_kernel_code);
  ]

let print_kernels (ks : p_kernel Stream.t) : unit =
  print_endline "; conc";
  let count = ref 0 in
  Stream.iter (fun (k:p_kernel) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; phase " ^ (string_of_int curr));
    kernel_ser k |> s_print
  ) ks;
  print_endline "; end of conc"

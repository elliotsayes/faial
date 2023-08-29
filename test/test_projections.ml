open Protocols

open Exp
open Variable

open OUnit2

type projection = {
  ydim: nexp;
  x: nexp;
  y: nexp;
}
let var name = Var (from_name name)

let find_projection thread_locals e =
  let thread_globals = Variable.Set.diff
    (Freenames.free_names_nexp e Variable.Set.empty)
    thread_locals
  in
  let rec is_constant_or_global = function
    | Var v -> Variable.Set.mem v thread_globals
    | Num _ -> true
    | Bin (_, l, r) -> is_constant_or_global l && is_constant_or_global r
    | NIf (p, t, f) -> b_is_constant_or_global p && is_constant_or_global t && is_constant_or_global f
    | _ -> false
  and b_is_constant_or_global = function
    | Bool _ -> true
    | NRel (_, l, r) -> is_constant_or_global l && is_constant_or_global r
    | BRel (_, l, r) -> b_is_constant_or_global l && b_is_constant_or_global r
    | BNot b -> b_is_constant_or_global b
    | _ -> false
  (* TODO: are Proj, Pred and NCall safe to assume constant if args are constant? *)
  in
  let infer_dim a b y = match a, b with
    (* Tf one is a constant or a thread global, assume the it is the dimension.
       This doesn't properly acount for block vs thread dim *)
    | ydim, x when is_constant_or_global ydim -> Some {ydim; x; y}
    | x, ydim when is_constant_or_global ydim -> Some {ydim; x; y}
    | _ -> Some {ydim = a; x = b; y}
  in
  let find_projection = function
    | Bin (Plus, Bin (Mult, a, b), c)
    | Bin (Plus, c, Bin (Mult, a, b)) -> infer_dim a b c
    | _ -> None
  in
  find_projection e


let p_to_string (p) =
  "{ ydim: " ^ n_to_string p.ydim ^ ", x: " ^ n_to_string p.x ^ ", y: " ^ n_to_string p.y ^ " }"

let assert_projection_opt (expected:projection option) (given:projection option) =
  let msg = match expected, given with
    | Some expected, Some given -> "Expected: " ^ p_to_string expected ^ "\nGiven: " ^ p_to_string given
    | Some expected, None -> "Expected: " ^ p_to_string expected ^ "\nGiven: None"
    | None, Some given -> "Expected: None" ^ "\nGiven: " ^ p_to_string given
    | None, None -> "Expected: None\nGiven: None"
  in
  assert_equal expected given ~msg

let globals = Variable.Set.of_list [
  from_name "blockDim.x";
  from_name "blockDim.y";
  from_name "blockDim.z";

  from_name "blockIdx.x";
  from_name "blockIdx.y";
  from_name "blockIdx.z";
]

let tests = "tests" >::: [
  "saxpy" >:: (fun _ -> assert_projection_opt
  (find_projection globals (Bin (
    Plus,
    Bin (Mult, var "blockIdx.x", var "blockDim.x"),
    var "threadIdx.x"
  ))) (* blockIdx.x*blockDim.x + threadIdx.x *)
  (Some {ydim = var "blockDim.x"; x = var "blockIdx.x"; y = var "threadIdx.x"}));

  "constant" >:: (fun _ -> (assert_projection_opt
  (find_projection globals (Bin (
    Plus,
    Bin (Mult, var "a", Num 10),
    var "c"
  ))) (* a*10 + c *)
  (Some {ydim = Num 10; x = var "a"; y = var "c"})));

  "fallback" >:: (fun _ -> assert_projection_opt
  (find_projection globals (Bin (
    Plus,
    Bin (Mult, var "a", var "b"),
    var "c"
  ))) (* a*b + c *)
  (Some {ydim = var "a"; x = var "b"; y = var "c"}))
]

let _ = run_test_tt_main tests

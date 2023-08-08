open Protocols

open Exp

open OUnit2

type projection = {
  ydim: nexp;
  x: nexp;
  y: nexp;
}
let var name = Var { name = name; location = None }

let rec is_constant = function
  | Num _ -> true
  | Bin (_, l, r) -> is_constant l && is_constant r
  | NIf (p, t, f) -> b_is_constant p && is_constant t && is_constant f
  | _ -> false
and b_is_constant = function
  | Bool _ -> true
  | NRel (_, l, r) -> is_constant l && is_constant r
  | BRel (_, l, r) -> b_is_constant l && b_is_constant r
  | BNot b -> b_is_constant b
  | _ -> false
(* TODO: are Proj, Pred and NCall safe to assume constant if args are constant? *)

let infer_dim a b y = match a, b with
  (* If one is explicitly named blockDim, assume it's the dimension *)
  | (Var {name; _} as ydim), x
  when String.starts_with ~prefix:"blockDim" name -> Some {ydim; x; y}
  | x, (Var {name; _} as ydim)
    when String.starts_with ~prefix:"blockDim" name -> Some {ydim; x; y}
  (* if one is a constant, assume the it is the dimension *)
  | ydim, x when is_constant ydim -> Some {ydim; x; y}
  | x, ydim when is_constant ydim -> Some {ydim; x; y}
  | _ -> Some {ydim = a; x = b; y}

let find_projection = function
  | Bin (Plus, Bin (Mult, a, b), c)
  | Bin (Plus, c, Bin (Mult, a, b)) -> infer_dim a b c
  | _ -> None


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

let tests = "tests" >::: [
  "saxpy" >:: (fun _ -> assert_projection_opt
  (find_projection (Bin (
    Plus,
    Bin (Mult, var "blockIdx.x", var "blockDim.x"),
    var "threadIdx.x"
  ))) (* blockIdx.x*blockDim.x + threadIdx.x *)
  (Some {ydim = var "blockDim.x"; x = var "blockIdx.x"; y = var "threadIdx.x"}));

  "constant" >:: (fun _ -> (assert_projection_opt
  (find_projection (Bin (
    Plus,
    Bin (Mult, var "a", Num 10),
    var "c"
  ))) (* a*10 + c *)
  (Some {ydim = Num 10; x = var "a"; y = var "c"})));

  "fallback" >:: (fun _ -> assert_projection_opt
  (find_projection (Bin (
    Plus,
    Bin (Mult, var "a", var "b"),
    var "c"
  ))) (* a*b + c *)
  (Some {ydim = var "a"; x = var "b"; y = var "c"}))
]

let _ = run_test_tt_main tests

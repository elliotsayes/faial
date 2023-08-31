open Protocols

open Exp


type projection = {
  ydim: nexp;
  x: nexp;
  y: nexp;
}

let find_projection (thread_locals : Variable.Set.t) (e : nexp) : projection option =
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

let translate (thread_locals : Variable.Set.t) (a : Access.t) : Access.t =
  let index = match a.index with
    | [index] -> (match find_projection thread_locals index with 
      | Some proj -> [proj.x; proj.y]
      | None -> a.index
    )
    | _ -> a.index
  in
  { a with index }
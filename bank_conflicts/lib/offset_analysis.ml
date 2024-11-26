open Stage0
open Protocols

(*
  Given a numeric expression try to remove any offsets in the form of
  `expression + constant` or `expression - constant`.

  The way we do this is by first getting all the free-names that are
  **not** tids. Secondly, we rearrange the expression as a polynomial
  in terms of each free variable. Third, we only keep polynomials that
  mention a tid, otherwise we can safely discard such a polynomial.
*)

type t =
  | Uniform of Exp.nexp
  | Any of Exp.nexp

let map (f:Exp.nexp -> Exp.nexp) : t -> t =
  function
  | Uniform e -> Uniform (f e)
  | Any e -> Any (f e)

let index_or (f:Exp.nexp -> Exp.nexp -> Exp.nexp) (e1: t) (e2: t) : t =
  match e1, e2 with
  | Any e, Uniform _
  | Uniform _, Any e -> Any e
  | Uniform e1, Uniform e2 -> Uniform (f e1 e2)
  | Any e1, Any e2 -> Any (f e1 e2)

let index_and (f:Exp.nexp -> Exp.nexp -> Exp.nexp) (e1: t) (e2: t) : t =
  match e1, e2 with
  | Any e1, Uniform e2
  | Uniform e1, Any e2
  | Any e1, Any e2 -> Any (f e1 e2)
  | Uniform e1, Uniform e2 -> Uniform (f e1 e2)

let from_nexp (cfg:Config.t) (locals:Variable.Set.t) : Exp.nexp -> t =
  let locals = Variable.Set.union locals Variable.tid_set in
  let rec from_nexp : Exp.nexp -> t =
    function
    | Num n -> Uniform (Num n)
    | Var x when Config.is_warp_uniform x cfg -> Uniform (Var x)
    | Var x when Variable.Set.mem x locals -> Any (Var x)
    | Var x -> Uniform (Var x)
    | Unary (o, e) ->
      map (fun e -> Unary (o, e)) (from_nexp e)
    | Binary (o, e1, e2) when o = Plus || o = Minus ->
      index_or (fun e1 e2 -> Binary (o, e1, e2)) (from_nexp e1) (from_nexp e2)
    | Binary (o, e1, e2) ->
      index_and (fun e1 e2 -> Binary (o, e1, e2)) (from_nexp e1) (from_nexp e2)
    | NCall (f, e) -> map (fun e -> NCall (f, e)) (from_nexp e)
    | Other e -> map (fun e -> Other e) (from_nexp e)
    | CastInt e ->
      if Exp.b_exists (fun x -> Variable.Set.mem x locals) e then
        Any (CastInt e)
      else
        Uniform (CastInt e)
    | NIf (c, n1, n2) ->
      if Exp.b_exists (fun x -> Variable.Set.mem x locals) c then
        Any (NIf (c, n1, n2))
      else
        index_and (fun n1 n2 -> NIf (c, n1, n2)) (from_nexp n1) (from_nexp n2)
  in
  from_nexp

let to_string : t -> string =
  function
  | Any e -> "index " ^ Exp.n_to_string e
  | Uniform e -> "unif " ^ Exp.n_to_string e

module Make (L:Logger.Logger) = struct
  open Exp

  let remove_offset (cfg:Config.t) (locals:Variable.Set.t) (n: Exp.nexp) : Exp.nexp =
    let after =
      match from_nexp cfg locals n with
      | Uniform _ -> Num 0
      | Any e -> e
    in
    (if n <> after then
      L.info ("Simplification: removed offset: " ^ Exp.n_to_string n ^ " 🡆 " ^ Exp.n_to_string after)
    );
    after
end
module Default = Make(Logger.Colors)
module Silent = Make(Logger.Silent)

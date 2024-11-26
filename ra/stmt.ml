open Stage0
open Protocols

type t =
  | Skip
  | Tick of int
  | Loop of {range: Range.t; body: t}
  | Seq of t * t
  | If of Exp.bexp * t * t
  | Choice of t * t

let rec is_zero : t -> bool =
  function
  | Skip
  | Tick 0 -> true
  | Tick _ -> false
  | Loop {body=r; _} -> is_zero r
  | Seq (p, q) | Choice (p, q) -> is_zero p && is_zero q
  | If (_, p, q) -> is_zero p && is_zero q

let to_environ (s:t) : Environ.t =
  let rec fvs (s:t) (env:Environ.Fvs.t) : Environ.Fvs.t =
    match s with
    | Skip
    | Tick _ -> env
    | Loop {range=r; body=p} ->
      env
      |> fvs p
      |> Environ.Fvs.add_r r
    | Seq (p, q) | Choice (p, q) ->
      env
      |> fvs p
      |> fvs q
    | If (b, p, q) ->
      env
      |> fvs p
      |> fvs q
      |> Environ.Fvs.add_b b
  in
  Environ.Fvs.empty
  |> fvs s
  |> Environ.from_fvs

module MakeSubst (S:Subst.SUBST) = struct
  module M = Subst.Make(S)
  let rec subst (s: S.t) : t -> t =
    function
    | Skip -> Skip
    | Tick k -> Tick k
    | Seq (p, q) -> Seq (subst s p, subst s q)
    | Choice (p, q) -> Choice (subst s p, subst s q)
    | If (b, p, q) -> If (M.b_subst s b, subst s p, subst s q)
    | Loop {range=r; body=p} ->
      let r = M.r_subst s r in
      M.add s r.var (function
        | Some s -> Loop {range=r; body=subst s p}
        | None -> Loop {range=r; body=p}
      )
end

module PSubstAssoc = MakeSubst(Subst.SubstAssoc)
module PSubstPair = MakeSubst(Subst.SubstPair)

let subst : (Variable.t * Exp.nexp) -> t -> t = PSubstPair.subst

let rec indent : t -> Indent.t list =
  let open Indent in
  function
  | Tick k -> [Line ("tick(" ^ string_of_int k ^ ");")]
  | Skip -> [Line "skip;"]
  | Seq (p, q) -> indent p @ indent q
  | Choice (p, q) ->
    [
      Line ("if <> {");
      Block (indent p);
      Line ("} else {");
      Block (indent q);
      Line ("}")
    ]
  | If (b, p, q) ->
    [
      Line ("if (" ^ Exp.b_to_string b ^ ") {");
      Block (indent p);
      Line ("} else {");
      Block (indent q);
      Line ("}")
    ]
  | Loop {range=r; body=p} ->
    [
      Line ("foreach (" ^ Range.to_string r ^ ") {");
      Block (indent p);
      Line "}"
    ]

module Opt = struct
  let skip : t = Skip

  let tick n : t =
    if n = 0 then Skip
    else Tick n

  let if_ (b:Exp.bexp) (p:t) (q:t) : t =
    match b, p, q with
    | Bool b, _, _ -> if b then p else q
    | _, Skip, Skip -> Skip
    | _, Skip, _ -> If (Exp.b_not b, p, Skip)
    | _, _, _ -> If (b, p, q)

  let seq (p:t) (q:t) : t =
    match p, q with
    | Skip, p | p, Skip -> p
    | _, _ -> Seq (p, q)

  let loop (r:Range.t) (p:t) : t =
    if p = Skip || Range.eval_is_empty r then
      Skip
    else
      Loop {range=r; body=p}

  let choice (p:t) (q:t) : t =
    Choice (p, q)
end

let rec simplify : t -> t =
  function
  | Skip -> Skip
  | Tick n -> Opt.tick n
  | If (b, p, q) ->
    Opt.if_ (Constfold.b_opt b) (simplify p) (simplify q)
  | Choice (p, q) -> Opt.choice (simplify p) (simplify q)
  | Loop {range=r; body=p} ->
    Opt.loop (Constfold.r_opt r) (simplify p)
  | Seq (p, q) ->
    Opt.seq (simplify p) (simplify q)

let to_string (x:t) : string =
  indent x |> Indent.to_string

let print (x:t) : unit =
  indent x |> Indent.print


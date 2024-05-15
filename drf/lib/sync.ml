open Stage0
open Protocols
open Subst
open Exp

type t =
  | Sync of Unsync.t
  | Loop of Unsync.t * Range.t * t * Unsync.t
  | Seq of t * t

let skip : t = Sync Skip

let rec to_s: t -> Indent.t list =
  function
  | Sync e -> Unsync.to_s e @ [Line "sync;"]
  | Loop (c1, r, p, c2) ->
    Unsync.to_s c1
    @
    [
      Line ("foreach* (" ^ Range.to_string r ^ ") {");
      Block (
        to_s p
        @
        Unsync.to_s c2
      );
      Line "}"
    ]
  | Seq (p1, p2) -> to_s p1 @ to_s p2

module Make (S:SUBST) = struct
  module M = Subst.Make(S)
  module U = Unsync.Make(S)
  let rec subst (s:S.t) : t -> t =
    function
    | Sync c -> Sync (U.subst s c)
    | Loop (c1, r, p, c2) ->
      let (p, c2) = M.add s r.var (function
        | Some s -> subst s p, U.subst s c2
        | None -> p, c2
      ) in
      Loop (U.subst s c1, M.r_subst s r, p, c2)
    | Seq (p, q) -> Seq (subst s p, subst s q)

end

module S1 = Make(SubstPair)

let subst = S1.subst

let inline_cond (b:bexp) (w:t) : t =
  let b = Constfold.b_opt b in
  let rec inline : t -> t =
    function
    | Sync c -> Sync (Seq (Assert b, c))
    | Loop (c1, r, w, c2) ->
      Loop (Seq (Assert b, c1), r, inline w, Seq (Assert b, c2))
    | Seq (p, q) -> Seq (inline p, inline q)
  in
  match b with
  | Bool true -> w
  | Bool false -> skip
  | _ -> inline w

(* Apply a function to the first unsync *)
let rec map_first (f: Unsync.t -> Unsync.t) : t -> t =
  function
  | Sync c -> Sync (f c)
  | Loop (c1, r, w1, c2) -> Loop (f c1, r, w1, c2)
  | Seq (p, q) -> Seq (map_first f p, q)

let add (u:Unsync.t) : t -> t =
  map_first (fun u2 -> Seq (u, u2))

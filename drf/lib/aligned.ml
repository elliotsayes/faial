open Stage0
open Protocols

let (@) = Common.append_tr

open Proto
open Subst

type t =
  | Sync of Unsync.t
  | Loop of t * Range.t * t
  | Seq of t * t

module Make (S:SUBST) = struct
  module M = Subst.Make(S)
  module U = Unsync.Make(S)

  let rec subst (s:S.t) (i:t) : t =
    match i with
    | Seq (p, q) -> Seq (subst s p, subst s q)
    | Sync c -> Sync (U.subst s c)
    | Loop (p, r, q) ->
      let q = M.add s r.var (function
        | Some s -> subst s q
        | None -> q
      ) in
      Loop (subst s p, M.r_subst s r, q)

end

module S1 = Make(SubstPair)
let subst = S1.subst

let rec seq (c:Unsync.t) : t -> t =
  function
  | Sync c' -> Sync (Unsync.seq c c')
  | Loop (p, r, q) -> Loop (seq c p, r, q)
  | Seq (p, q) -> Seq (seq c p, q)

let align (w:Sync.t) : t =
  let rec align : Sync.t -> t * Unsync.t =
    function
    | Sync c -> (Sync c, Skip)
    | Loop (c1, r, p, c2) ->
      let (q, c3) = align p in
      let q1 = seq c1 (subst (r.var, Range.first r) q) in
      let c = Unsync.seq c3 c2 in
      let r' = Range.next r in
      let x = r.var in
      let x_dec = Range.prev r in
      (Loop (q1, r', seq (Unsync.subst (x, x_dec) c) q),
        Unsync.subst (x, Range.lossy_last r) c)
    | Seq (i, p) ->
      let (i, c1) = align i in
      let (q, c2) = align p in
      Seq (i, seq c1 q), c2
  in
  match align w with
    (p, c) -> Seq (Sync c, p)

let translate (ks: Sync.t kernel Streamutil.stream) : t kernel Streamutil.stream =
  Streamutil.map (fun k -> { k with kernel_code = align k.kernel_code }) ks

let rec to_s: t -> Indent.t list =
  function
  | Seq (p, q) -> to_s p @ to_s q
  | Sync e -> Unsync.to_s e @ [Line "sync;"]
  | Loop (p, r, q) ->
    to_s p
    @
    [
      Line ("foreach* (" ^ Range.to_string r ^ ") {");
      Block (to_s q);
      Line "}"
    ]


(* ---------------------- SERIALIZATION ------------------------ *)
let print_kernel (k : t kernel) : unit =
  Proto.print_kernel to_s k

let print_kernels (ks : t kernel Streamutil.stream) : unit =
  print_endline "; a-lang";
  let count = ref 0 in
  Streamutil.iter (fun k ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; a-prog " ^ (string_of_int curr));
    print_kernel k
  ) ks;
  print_endline "; end of a-lang"

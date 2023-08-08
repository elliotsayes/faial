open Stage0
open Protocols

let (@) = Common.append_tr

open Proto
open Subst
open Wellformed

type inst =
  | Sync of Unsync.t
  | Loop of inst list * Range.t * inst list

type t = inst list

module Make (S:SUBST) = struct
  module M = Subst.Make(S)
  module U = Unsync.Make(S)

  let subst: S.t -> t -> t =
    let rec i_subst (s:S.t) (i:inst) : inst =
      match i with
      | Sync c -> Sync (U.subst s c)
      | Loop (p, r, q) ->
        let q = M.add s r.var (function
          | Some s -> p_subst s q
          | None -> q
        ) in
        Loop (p_subst s p, M.r_subst s r, q)
    and p_subst (s:S.t) : t -> t =
      List.map (i_subst s)
    in
    p_subst

end

module S1 = Make(SubstPair)
let subst = S1.subst

let rec seq (c:Unsync.t) (w:t) : t =
  match w with
  | Sync c' :: w -> Sync (Unsync.seq c c') :: w
  | Loop (p, r, q)::w -> Loop (seq c p, r, q)::w
  | [] -> failwith "UNEXPECTED!"

let align (w:Sync.t) : t =
  let rec align : Sync.t -> t * Unsync.t =
    function
    | Sync c -> ([Sync c], [])
    | Loop (c1, r, p, c2) ->
      let (q, c3) = align p in
      let q1 = seq c1 (subst (r.var, Range.first r) q) in
      let c = Unsync.seq c3 c2 in
      let r' = Range.next r in
      let x = r.var in
      let x_dec = Range.prev r in
      ([Loop (q1, r', seq (Unsync.subst (x, x_dec) c) q)],
        Unsync.subst (x, Range.lossy_last r) c)
    | Seq (i, p) ->
      let (i, c1) = align i in
      let (q, c2) = align p in
      i @ seq c1 q, c2
  in
  match align w with
    (p, c) -> Sync c :: p

let translate (ks: Sync.t kernel Streamutil.stream) : t kernel Streamutil.stream =
  let open Streamutil in
  ks
  |> map (fun k -> { k with kernel_code = align k.kernel_code })

let to_s: t -> Indent.t list =
  let rec inst_to_s : inst -> Indent.t list =
    function
    | Sync e -> Unsync.to_s e @ [Line "sync;"]
    | Loop (p, r, q) ->
      (List.map inst_to_s p |> List.flatten)
      @
      [
        Line ("foreach* (" ^ Range.to_string r ^ ") {");
        Block (
          List.map inst_to_s q |> List.flatten
        );
        Line "}"
      ]
  in
  let prog_to_s (p: t) : Indent.t list =
    List.map inst_to_s p |> List.flatten
  in
  prog_to_s


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

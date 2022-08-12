let (@) = Common.append_tr

open Exp
open Proto
open Common
open Serialize
open Subst
open Streamutil
open Wellformed

type a_inst =
  | ASync of u_prog
  | ALoop of a_inst list * range * a_inst list

type a_prog = a_inst list

module Make (S:SUBST) = struct
  module M = Subst.Make(S)
  module N = Wellformed.Make(S)

  let a_subst: S.t -> a_prog -> a_prog =
    let rec i_subst (s:S.t) (i:a_inst) : a_inst =
      match i with
      | ASync c -> ASync (N.u_subst s c)
      | ALoop (p, r, q) ->
        let q = M.add s r.range_var (function
          | Some s -> p_subst s q
          | None -> q
        ) in
        ALoop (p_subst s p, M.r_subst s r, q)
    and p_subst (s:S.t) : a_prog -> a_prog =
      List.map (i_subst s)
    in
    p_subst

end

module S1 = Make(SubstPair)
let a_subst = S1.a_subst

let align (w:w_prog) : a_prog =
  let rec seq (c:u_prog) (w:a_prog) : a_prog =
    match w with
    | ASync c' :: w -> ASync (u_seq c c') :: w
    | ALoop (p, r, q)::w -> ALoop (seq c p, r, q)::w
    | [] -> failwith "UNEXPECTED!"
  in
  let rec i_align (i:w_inst) : a_inst * u_prog =
    match i with
    | SSync c -> (ASync c, [])
    | SLoop (c1, r, p, c2) ->
      let (q, c3) = p_align p in
      let q1 = seq c1 (a_subst (r.range_var, r.range_lower_bound) q) in
      let c = u_seq c3 c2 in
      let r' = Predicates.range_inc r in
      let x = r.range_var in
      let x_dec = Predicates.step_dec r.range_step (Var r.range_var) in
      (ALoop (q1, r', seq (u_subst (x, x_dec) c) q),
        u_subst (x, Predicates.range_last r) c)
  and p_align (p:w_prog) : a_prog * u_prog =
    match p with
    | [i] ->
      let (i, c) = i_align i in
      [i], c
    | i :: p ->
      let (i, c1) = i_align i in
      let (q, c2) = p_align p in
      i :: seq c1 q, c2
    | [] -> failwith "Unexpected empty synchronized code!"
  in
  let open Streamutil in
  match p_align w with
    (p, c) -> ASync c :: p

let translate (ks: w_prog kernel Streamutil.stream) : a_prog kernel Streamutil.stream =
  let open Streamutil in
  ks
  |> map (fun k -> { k with kernel_code = align k.kernel_code })

let a_prog_to_s: a_prog -> PPrint.t list =
  let open PPrint in
  let rec inst_to_s : a_inst -> PPrint.t list =
    function
    | ASync e -> u_prog_to_s e @ [Line "sync;"]
    | ALoop (p, r, q) ->
      (List.map inst_to_s p |> List.flatten)
      @
      [
        Line ("foreach* (" ^ r_to_s r ^ ") {");
        Block (
          List.map inst_to_s q |> List.flatten
        );
        Line "}"
      ]
  in
  let prog_to_s (p: a_prog) : t list =
    List.map inst_to_s p |> List.flatten
  in
  prog_to_s


(* ---------------------- SERIALIZATION ------------------------ *)
let print_kernel (k : a_prog kernel) : unit =
  Proto.print_kernel a_prog_to_s k

let print_kernels (ks : a_prog kernel Streamutil.stream) : unit =
  let open Serialize in
  print_endline "; a-lang";
  let count = ref 0 in
  Streamutil.iter (fun k ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; a-prog " ^ (string_of_int curr));
    print_kernel k
  ) ks;
  print_endline "; end of a-lang"

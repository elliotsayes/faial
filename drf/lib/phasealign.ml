open Stage0
open Protocols

let (@) = Common.append_tr

open Proto
open Subst
open Wellformed

type a_inst =
  | ASync of u_prog
  | ALoop of a_inst list * Range.t * a_inst list

type a_prog = a_inst list

module Make (S:SUBST) = struct
  module M = Subst.Make(S)
  module N = Wellformed.Make(S)

  let a_subst: S.t -> a_prog -> a_prog =
    let rec i_subst (s:S.t) (i:a_inst) : a_inst =
      match i with
      | ASync c -> ASync (N.u_subst s c)
      | ALoop (p, r, q) ->
        let q = M.add s r.var (function
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
      let q1 = seq c1 (a_subst (r.var, Range.first r) q) in
      let c = u_seq c3 c2 in
      let r' = Range.next r in
      let x = r.var in
      let x_dec = Range.prev r in
      (ALoop (q1, r', seq (u_subst (x, x_dec) c) q),
        u_subst (x, Range.lossy_last r) c)
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
  match p_align w with
    (p, c) -> ASync c :: p

let translate (ks: w_prog kernel Streamutil.stream) : a_prog kernel Streamutil.stream =
  let open Streamutil in
  ks
  |> map (fun k -> { k with kernel_code = align k.kernel_code })

let a_prog_to_s: a_prog -> Indent.t list =
  let open Indent in
  let rec inst_to_s : a_inst -> Indent.t list =
    function
    | ASync e -> u_prog_to_s e @ [Line "sync;"]
    | ALoop (p, r, q) ->
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
  let prog_to_s (p: a_prog) : t list =
    List.map inst_to_s p |> List.flatten
  in
  prog_to_s


(* ---------------------- SERIALIZATION ------------------------ *)
let print_kernel (k : a_prog kernel) : unit =
  Proto.print_kernel a_prog_to_s k

let print_kernels (ks : a_prog kernel Streamutil.stream) : unit =
  print_endline "; a-lang";
  let count = ref 0 in
  Streamutil.iter (fun k ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; a-prog " ^ (string_of_int curr));
    print_kernel k
  ) ks;
  print_endline "; end of a-lang"

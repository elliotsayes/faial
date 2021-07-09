open Exp
open Proto
open Common

type 'a acc_t =
  | Var of variable * 'a acc_t
  | Range of range * 'a acc_t
  | Cond of bexp * 'a acc_t
  | Acc of 'a

let rec get_acc (acc: 'a acc_t) =
  match acc with
  | Var (_, acc)
  | Range (_, acc)
  | Cond (_, acc) -> get_acc acc
  | Acc a -> a

let proto_to_acc (x:variable) (f: access -> 'a) (p: prog) : 'a acc_t list = 
  let rec on_i (i:inst) : 'a acc_t list =
    match i with
    | Acc (y, e) -> if x = y then [Acc (f e)] else []
    | Sync -> []
    | Cond (b, is) -> on_p is |> List.map (fun i -> (Cond (b, i)))
    | Loop (r, is) -> on_p is |> List.map (fun i -> (Range (r, i)))

  and on_p (l:prog) : 'a acc_t list =
    List.concat_map on_i l
  in on_p p


type poly_ht = (int, nexp) Hashtbl.t

type poly_t =
  | One of nexp
  | Two of nexp * nexp
  | Many of poly_ht

let poly_to_string x (p:poly_t) =
  let open Serialize in
  let open PPrint in
  match p with
  | One n -> n_to_s n
  | Two (n1, n2) -> n_par n1 ^ " + " ^ n_par n2 ^ " * " ^ x
  | Many ht ->
    hashtbl_elements ht
    |> List.map (fun (k, v) -> n_par v ^ " * " ^ x ^ "^" ^ (string_of_int k))
    |> join " + "

let make_poly e n =
  if n = 0 then
    One e
  else if n = 1 then
    Two (Num 0, e)
  else
    let ht = Hashtbl.create 1 in
    Hashtbl.add ht n e;
    Many ht

let update_ht (ht:('a, 'b) Hashtbl.t) (k:'a)  (f:'b option -> 'b)  : unit =
  Hashtbl.replace ht k (f (Hashtbl.find_opt ht k))

let poly_update_ht (ht:poly_ht) (k:int) (f:nexp -> nexp) : unit =
  update_ht ht k (function | Some v -> f v | None -> f (Num 0))

let poly_add_ht (src:poly_ht) (dst:poly_ht) : unit =
  Hashtbl.iter (fun i n ->
    poly_update_ht dst i (n_plus n)
  ) src

let poly_add e1 e2 =
  match e1, e2 with
  | One n1, One n2 -> One (n_plus n1 n2)
  | One n1, Two (n2, n3)
  | Two (n2, n3), One n1 ->
    Two (n_plus n2 n1, n3)
  | Two (n1, n2), Two (n3, n4) -> Two (n_plus n1 n3, n_plus n2 n4)
  | One n1, Many ht
  | Many ht, One n1 ->
    let ht = Hashtbl.copy ht in
    poly_update_ht ht 0 (n_plus n1);
    Many ht
  | Two (n1, n2), Many ht
  | Many ht, Two (n1, n2) ->
    let ht = Hashtbl.copy ht in
    poly_update_ht ht 0 (n_plus n1);
    poly_update_ht ht 1 (n_plus n2);
    Many ht
  | Many ht1, Many ht2 ->
    let ht2 = Hashtbl.copy ht2 in
    poly_add_ht ht1 ht2;
    Many ht2


let rec poly_mult e1 e2 =
  let poly_mult_ht (src:poly_ht) ((i1,n1):int*nexp) : poly_ht =
    (* z * x * (a + b*x + c*x^2) = a * z * x + z * b * x ^ 2 ... *)
    let dst = Hashtbl.create (Hashtbl.length src) in
    Hashtbl.iter (fun i2 n2 ->
      Hashtbl.add dst (i1 + i2) (n_mult n1 n2)
    ) src;
    dst
  in
  let mk_poly_ht (n1:nexp) (n2:nexp) : poly_ht =
    let ht = Hashtbl.create 2 in
    Hashtbl.add ht 0 n1;
    Hashtbl.add ht 1 n2;
    ht
  in
  match e1, e2 with
  | One n1, One n2 -> One (n_mult n1 n2)
  | One n1, Two (n2, n3)
  | Two (n2, n3), One n1
    -> Two (n_mult n1 n2, n_mult n1 n3)
  | Two (n1, n2), Two (n3, n4) ->
      let ht' = poly_mult_ht (mk_poly_ht n3 n4) (1, n2) in
      poly_add (poly_mult (One n1) e2) (Many ht')
  | One n1, Many ht
  | Many ht, One n1 ->
    hashtbl_elements ht
    |> List.map (fun (i, n) -> (i, n_mult n n1))
    |> hashtbl_from_list
    |> (fun ht -> Many ht)
  | Two (n1, n2), Many ht
  | Many ht, Two (n1, n2)
    -> poly_mult (Many (mk_poly_ht n1 n2)) (Many ht)
  | Many ht1, Many ht2 ->
    let ht = Hashtbl.create ((Hashtbl.length ht1) * (Hashtbl.length ht2)) in
    hashtbl_elements ht1
    |> List.map (poly_mult_ht ht2)
    |> List.iter (fun src ->
      poly_add_ht src ht
    );
    Many ht

let poly_uminus (p:poly_t) : poly_t =
  let u_minus n = n_mult (Num (-1)) n in
  match p with
  | One n -> One (u_minus n)
  | Two (n1, n2) -> Two (u_minus n1, u_minus n2)
  | Many ht -> hashtbl_elements ht
    |> List.map (fun (k, v)-> (k, u_minus v))
    |> fun l -> Many (hashtbl_from_list l)

let rec n_to_poly v (n:nexp) : poly_t =
  match n with
  | Var x -> if x = v then Two (Num 0, Num 1) else One n
  | Num _ -> One n
  | Proj _
  | NCall _
  | NIf _ -> One (Num 0)
  | Bin (Plus, e1, e2) -> poly_add (n_to_poly v e1) (n_to_poly v e2)
  | Bin (Minus, e1, e2) -> poly_add (n_to_poly v e1) (poly_uminus (n_to_poly v e2))
  | Bin (Mult, e1, e2) -> poly_mult (n_to_poly v e1) (n_to_poly v e2)
  | Bin _ -> One (Num 0)


let proto_to_poly x v p : (poly_t list) acc_t list =
  proto_to_acc x (fun (a:access) -> List.map (n_to_poly v) (a.access_index)) p

let open_ic_with (fname:string option) (f : in_channel -> unit) : unit =
    let ic, (closer: in_channel -> unit) = match fname with
    | Some fname -> (open_in fname, close_in_noerr)
    | None -> (stdin, fun x -> ())
    in
    try (f ic; closer ic) with
    | e -> closer ic;
      raise e


let p_kernel_parser fname input : prog kernel =
  let fname = match fname with
  | Some x -> x
  | None -> "<STDIN>"
  in
  let filebuf = Lexing.from_channel input in
  Scan.set_filename filebuf fname;
  try Parse2.main Scan.read filebuf with
  | Parse2.Error ->
    let b = Buffer.create 1024 in
    let sloc = Sourceloc.of_lexbuf filebuf in
    Printf.bprintf b "%a: syntax error" Sourceloc.location_bprint_start sloc;
    (try
        Printf.bprintf b "%a" Sourceloc.location_bprint_title sloc
    with
        Sys_error _ -> ()
    );
    raise (Common.ParseError b)

let j_kernel_parser (_:string option) ic =
  try Yojson.Basic.from_channel ic |> Parsejs.parse_kernels.run with
  | Yojson.Json_error("Blank input data") ->
    (* If the input is blank, just ignore the input and err with -1 *)
    raise (Common.mk_parse_error_s "Empty input data. Blank file?\n")
  | Yojson.Json_error(e) ->
    raise (Common.mk_parse_error_s (Printf.sprintf "Error parsing JSON: %s\n" e))

type i_kernel =
  | JKernel of Imp.p_kernel list
  | PKernel of prog kernel

let parse_i_kernel (use_json:bool) (fname:string option) (ic:in_channel) : i_kernel =
  if use_json then
    JKernel (j_kernel_parser fname ic)
  else
    PKernel (p_kernel_parser fname ic)

let open_i_kernel_with (use_json:bool) (fname:string option) (f:i_kernel -> unit) : unit =
  open_ic_with fname (fun ic ->
    f (parse_i_kernel use_json fname ic)
  )

let i_kernel_to_p_kernel (k:i_kernel) : prog kernel list =
  match k with
  | JKernel ks -> List.map Imp.compile ks
  | PKernel p -> [p]

(* Return true/false whether we CAN analyze the expression, not if
   there are bank-conflicts. *)
let has_bank_conflicts (n:nexp) : bool =
  let handle_coefficient (n:nexp) : bool =
    (* TODO: handle variables. We need to check if the variable is
       a thread-local or thread-global. In case of the *)
    Freenames.is_closed_nexp n
  in
  match n_to_poly (var_make "threadIdx.x") n with
  | One n ->
    (* threadIdx.x is not in the expression *)
    handle_coefficient n
  | Two (c, k) ->
    (* The expression is of form:
       k * threadIdx + c
       *)
    handle_coefficient c && handle_coefficient k
  | Many _ -> false

let _ =
  try
    open_i_kernel_with true None (fun k ->
      let ks = i_kernel_to_p_kernel k in
      Printf.printf "L: Found %d kernels.\n" (List.length ks);
      ks |> List.iter (fun k ->
        let shared = kernel_shared_arrays k in
        Printf.printf "L: Kernel %s, has %d shared arrays.\n"
          k.kernel_name
          (shared |> VarSet.cardinal)
        ;
        VarSet.iter (fun v ->
          let accs = proto_to_acc v (fun x -> x) k.kernel_code in
          Printf.printf "L: Listing accesses for shared array %s. Found %d accesses.\n"
            v.var_name (List.length accs);
          List.iter (fun (x:access acc_t) ->
            let x = get_acc x in
            (* print_string "SOURCE: ";
            Serialize.PPrint.index_to_s x.access_index |> print_endline; *)
            x.access_index
            |> List.iter (fun n ->
              print_endline (
              (if has_bank_conflicts n then
                  "OK: "
                else
                  "SKIP: "
              )
              ^
              Serialize.PPrint.n_to_s n
              )
            )
          ) accs
        ) shared
      )
    )
  with
  | Common.ParseError b ->
    Buffer.output_buffer stderr b;
    exit (-1)

module PPrint = Serialize.PPrint 
module StackTrace = Common.StackTrace 
(* ------------------------------------------------------------------------ *)

let analyze j : Imp.p_kernel list =
  let open Indexflow in
  let open Cast in
  let open D_to_imp in
  match Cast.parse_kernels j with
  | Ok ks ->
      ks
      |> List.map (fun k ->
          Cast.print_kernel k;
          print_endline "------";
          let k = Dlang.rewrite_kernel k in
            Dlang.print_kernel k;
(*             (match Indexflow.types_stmt Indexflow.Typing.make k.code with
            | Ok _ -> 
              print_endline "OK!"
            | Error e -> Indexflow.print_s_error e;
              prerr_endline "CHECKME";
              exit (-1)
            );
 *)         print_endline "-------";
            (match D_to_imp.parse_kernel k with
            | Ok k -> k
            | Error e ->
              D_to_imp.print_error e;
              exit(-1)
            )
      )

  | Error e ->
    Rjson.print_error e;
    exit(-1)

type s_diff = string StackTrace.t option

let root_cause msg =
  let open StackTrace in
  Some (RootCause msg)


let list_diff f (l1: 'a list) (l2: 'a list) : s_diff =
  let open StackTrace in
  let rec list_diff (idx:int) (l1: 'a list) (l2: 'a list) : s_diff =
    let msg = "index #" ^ string_of_int (idx + 1) in
    match l1, l2 with
    | [], [] -> None
    | x::l1, y::l2 ->
      (match f x y with
      | Some e -> Some (Because (msg, e))
      | None -> list_diff (idx + 1) l1 l2)
    | _, _ -> failwith "unexpected"
  in
  let ll1 = List.length l1 in
  let ll2 = List.length l2 in
  if ll1 = ll2 then
    list_diff 0 l1 l2
  else
    let ll1 = string_of_int ll1 in
    let ll2 = string_of_int ll2 in
    root_cause ("lhs.lenght=" ^ ll1 ^ " != rhs.length=" ^ ll2)

let nexp_diff (n1:Exp.nexp) (n2:Exp.nexp) : s_diff =
  if n1 = n2 then
    None
  else
    root_cause (PPrint.n_to_s n1 ^ " != " ^ PPrint.n_to_s n2)

let bexp_diff (b1:Exp.bexp) (b2:Exp.bexp) : string StackTrace.t option =
  if b1 = b2 then
    None
  else
    root_cause (PPrint.b_to_s b1 ^ " != " ^ PPrint.b_to_s b2)

let stmt_to_s (s:Imp.stmt) : string =
  PPrint.doc_to_string (Imp.stmt_to_s s)

let rec stmt_diff (s1: Imp.stmt) (s2: Imp.stmt) : string StackTrace.t option =
  match s1, s2 with
  | Loop s1, Loop s2 ->
    (match stmt_diff s1 s2 with
    | Some e -> Some (Because ("loop", e))
    | None -> None)
  | Block s1, Block s2 -> list_diff stmt_diff s1 s2
  | For (r1, s1), For (r2, s2) ->
    if r1 = r2 then
      stmt_diff s1 s2
    else
      root_cause (PPrint.r_to_s r1 ^ " != " ^ PPrint.r_to_s r2)
  | If (b1, t1, e1), If (b2, t2, e2) ->
    (match bexp_diff b1 b2, stmt_diff t1 t2, stmt_diff e1 e2 with
    | None, None, None -> None
    | Some e, _, _ -> Some e
    | _, Some e, _ -> Some e
    | _, _, Some e -> Some e)
  | s1, s2 ->
    if s1 = s2 then None
    else
      root_cause (stmt_to_s s1 ^ "\n----------------\n" ^ stmt_to_s s2)

let print_stmt s =
  PPrint.print_doc (Imp.stmt_to_s s)


let () =
  let j = Yojson.Basic.from_channel stdin in
  let ss1 = analyze j in
  List.iter Imp.print_kernel ss1;
  ()

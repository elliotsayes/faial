module PPrint = Serialize.PPrint 
module StackTrace = Common.StackTrace 
(* ------------------------------------------------------------------------ *)


let parse_stmts_v1 j : Imp.stmt list =
  let open Cast in
  let open C_to_imp in
  match Cast.parse_kernels j with
  | Ok ks -> (
    match
      List.map (fun k -> Cast.print_kernel k; k.code) ks
      |> C_to_imp.cast_map C_to_imp.parse_stmt
    with
    | Ok ss -> List.map (fun s -> Imp.Block s) ss
    | Error es -> (
        C_to_imp.print_error es;
        exit(-1)
      )
    )
  | Error e ->
    Rjson.print_error e;
    exit(-1)

let parse_stmts_v2 j : Imp.stmt list =
  let open Imp in
  let open Parsejs in
  parse_kernels.run j
  |> List.map (fun k -> k.p_kernel_code)
(*
let parse_stmts_v3 j : Dlang.d_stmt list =
  let open Cast in
  let open D_to_imp in
  match Cast.parse_kernels j with
  | Ok ks ->
    List.map (fun k ->
      Cast.print_kernel k; k.code |> Dlang.rewrite_stmt) ks
  | Error e ->
    Rjson.print_error e;
    exit(-1)
*)

let parse_stmts_v3 j : Imp.stmt =
  let open Cast in
  let open D_to_imp in
  match Cast.parse_kernels j with
  | Ok ks -> (
    match
      List.map (fun k ->
          Cast.print_kernel k;
          let s = k.code |> Dlang.rewrite_stmt in
          Dlang.print_stmt s;
          s
      ) ks
      |> D_to_imp.cast_map D_to_imp.parse_stmt
    with
    | Ok (ss:Imp.stmt list) -> Imp.Block ss
    | Error es -> (
        C_to_imp.print_error es;
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

  (*
  | Acc a1, Acc a2 ->
  | Block s1, Block s2 ->
    match 
  *)
let print_stmt s =
  PPrint.print_doc (Imp.stmt_to_s s)


let () =
  let j = Yojson.Basic.from_channel stdin in
  let ss1 = parse_stmts_v3 j in
  (* let ss2 = parse_stmts_v2 j in *)
  print_stmt ss1;
  ()
(*
  match stmt_diff (Block ss1) (Block ss2) with
  | None -> ()
  | Some e ->
    StackTrace.iter print_endline e;
    print_endline ("---------");
    ;
    print_endline ("---------");
    print_stmt (Imp.Block ss2);
    exit (-1)
*)
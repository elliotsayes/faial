open Proto
open Program
open Common
(*
exception ParseError of (string list)
*)
let parse_error (cause:string list) msg data =
  raise (Parse.ParseError (( "Error parsing '" ^ msg ^"': " ^ Yojson.Basic.to_string data)::cause))

let abort_error msg data =
  raise (Parse.ParseError [msg ^ "\n" ^ Yojson.Basic.to_string data])

let call msg f data =
  let o = (try f data with Parse.ParseError l -> parse_error l msg data) in
  match o with
  | Some m -> m
  | None ->  parse_error [] msg data

let is_some o =
  match o with
  | Some _ -> true
  | None -> false

type 'a parser = {is_valid: Yojson.Basic.t -> bool; run: Yojson.Basic.t -> 'a}

let make name f = {
  is_valid = (fun x -> is_some (f x));
  run = call name f;
}

let parse_nbin = make "nbin" (fun m ->
  let open Yojson.Basic in
  match m with
  | `String "+" -> Some Plus
  | `String "-" -> Some Minus
  | `String "*"  -> Some Mult
  | `String "div" -> Some Div
  | `String "mod" -> Some Mod
  | _ -> None
)

let bind o1 f =
  match o1 with
  | Some x -> f x
  | None -> None

let bind_all l f =
  let rec aux l accum =
    match l with
    | [] -> f (List.rev accum)
    | x::l -> bind x (fun x -> aux l (x::accum))
  in
  aux l []

let member_opt k (j:Yojson.Basic.t) =
  let open Yojson.Basic.Util in
  match j with
  | `Assoc _ -> Some (member k j)
  | _ -> None

let get_kind_opt (j:Yojson.Basic.t) =
  let open Yojson.Basic.Util in
  match j with
  | `Assoc _ ->
    j |> member "kind" |> to_string_option
  | _ -> None

let get_kind (j:Yojson.Basic.t) : string =
  let open Yojson.Basic.Util in
  match j with
  | `Assoc _ ->
    begin match member "kind" j with
    | `String k -> k
    | _ -> abort_error "When getting the AST node kind from a JSON object, expecting the type to be a string, but got:" j
    end
  | _ -> abort_error "When getting the AST node kind from a JSON value, expecting a JSON object, but got:" j

let get_fields fields (j:Yojson.Basic.t) : Yojson.Basic.t list  =
  let open Yojson.Basic.Util in
  let kv = List.map (fun x -> (x, j |> member_opt x)) fields in
  let missing = List.filter (fun (x,y) -> y = None) kv |> List.split |> fst in
  if List.length missing > 0 then
    let fields = join ", " fields in
    let missing = join ", " missing in
    abort_error ("Getting fields [" ^ fields ^ "], but object is missing fields [" ^ missing ^ "]") j
  else
    List.split kv |> snd |> flatten_opt

type 'a choose_one_of_handler = string list * (Yojson.Basic.t list -> 'a option)

let choose_one_of (l:(string * 'a choose_one_of_handler) list) (j:Yojson.Basic.t) : 'a option =
  match List.assoc_opt (get_kind j) l with
  | Some ((fields:string list), kont) -> get_fields fields j |> kont
  | None ->
      let keys = List.split l |> fst |> join ", " in
      abort_error ("Expecting an AST 'kind' in [" ^ keys ^ "], but got:") j


let is_array_type o =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  match member "qualType" o with
  | `String x -> ends_with x " *"
  | _ -> false

let is_int_type o =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  match member "qualType" o with
  | `String "int" -> true
  | _ -> false

let has_type j ty =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  match j with
  | `Assoc _ -> ty (member "type" j)
  | _ -> false

let parse_task = function
  | 0 -> Some Task1
  | 1 -> Some Task2
  | _ -> None

let binary_operator f =
  "BinaryOperator", (["opcode"; "lhs"; "rhs"], function
    | [o; n1; n2] -> f o n1 n2
    | _ -> None
  )

let parse_var = make "variable" (fun j ->
    let open Yojson.Basic in
    choose_one_of [
      "VarDecl", (["name"],
        function [`String x] -> Some (var_make x) | _ -> None
      );
      "ParmVarDecl", (["name"],
        function [`String x] -> Some (var_make x) | _ -> None
      );
    ] j
  )

let rec parse_nexp n : nexp option =
  let open Yojson.Basic in
  choose_one_of [
    "VarDecl", (["name"],
      function [`String x] -> Some (Var (var_make x)) | _ -> None
    );
    "ParmVarDecl", (["name"],
      function [`String x] -> Some (Var (var_make x)) | _ -> None
    );
    binary_operator (fun o n1 n2 ->
      bind (parse_nexp n1) (fun n1 ->
        bind (parse_nexp n2) (fun n2 ->
            Some (Bin (parse_nbin.run o, n1, n2))
          ))
    );
    "IntegerLiteral", (["value"], function
      | [`Int n] -> Some (Num n)
      | _ -> None
    );
    "ProjExpr", (["task"; "child"], function
      | [`Int n; c] ->
        bind (parse_task n) (fun t ->
          bind (parse_nexp c) (fun c ->
            Some (Proj (t, c))))
      | _ -> None
    )
  ] n

let parse_nexp = make "nexp" parse_nexp

let parse_nrel = make "nrel" (fun m ->
  let open Yojson.Basic in
  match m with
  | `String "=" -> Some NEq
  | `String "<=" -> Some NLe
  | `String "<"  -> Some NLt
  | _ -> None
)

let parse_brel = make "brel" (fun m ->
  let open Yojson.Basic in
  match m with
  | `String "||" -> Some BOr
  | `String "&&" -> Some BAnd
  | _ -> None
)


let rec parse_bexp b : bexp option =
  let open Yojson.Basic in
  choose_one_of [
    binary_operator (fun o n1 n2 ->
      Some (NRel (parse_nrel.run o, parse_nexp.run n1, parse_nexp.run n2))
    );
    binary_operator (fun o b1 b2 ->
        bind (parse_bexp b1) (fun b1 ->
          bind (parse_bexp b2) (fun b2 ->
            Some (BRel (parse_brel.run o, b1, b2))))
    );
    "UnaryOperator", (["subExpr"; "opcode"], function
      | [b; `String "!"] ->
        bind (parse_bexp b) (fun b -> Some (BNot b))
      | _ -> None
    );
  ] b

let parse_bexp = make "bexp" parse_bexp

let parse_range_kind =
  let open Yojson.Basic in
  make "range kind" (fun s ->
    match s with
    | `String "+" -> Some Default
    | `String x -> Some (Pred x)
    | _ -> None
  )

let parse_range = make "range" (fun s ->
    choose_one_of [
      "RangeExpr", (["init"; "upper_bound"; "step"; "opcode"], function
      | [init; ub; step; k] -> Some {
          range_expr_start = parse_nexp.run init;
          range_expr_stop = parse_nexp.run ub;
          range_expr_step = parse_nexp.run step;
          range_expr_kind = parse_range_kind.run k;
        }
      | _ -> None
      )
    ] s
  )

let parse_mode =
  let open Yojson.Basic in
  make "mode" (fun j ->
    match j with
    | `String "ro" -> Some R
    | `String "rw" -> Some W
    | _ -> None
  )

let rec parse_program j =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  let do_parse_prog k msg =
    let p = try parse_program k with
      Parse.ParseError l -> parse_error l msg k
    in
    match p with
    | Some p -> p
    | None -> abort_error msg k
  in
  choose_one_of [
    "SyncStmt", ([], fun _ -> Some (Inst ISync));
    "AccessStmt", (["location"; "mode"; "index"], function
      | [`String loc; m; `List idx] -> Some (Inst (IAcc (var_make loc, {
          access_index = List.map parse_nexp.run idx;
          access_mode = parse_mode.run m;
          access_cond = Bool true
        })))
      | _ -> None);
    "AssertStmt", (["cond"], function
      | [b] -> Some (Inst (IAssert (parse_bexp.run b)))
      | _ -> None
    );
    "IfStmt", (["cond"], function
      | [cond] ->
        let get_branch (o:Yojson.Basic.t) k =
          let msg = "When parsing IfStmt, could not parse branch " ^ k in
          match member k j with
            | `Assoc _ as o ->
              do_parse_prog o k
            | `Null -> Block []
            | _ -> abort_error msg j
        in
        let then_stmt = get_branch j "thenStmt" in
        let else_stmt = get_branch j "elseStmt" in
        Some (If (parse_bexp.run cond, then_stmt, else_stmt))
      | _ -> None
    );
    "CompoundStmt", (["inner"], function
      | [`List l] ->
        let on_elem (idx, j) : program =
          let idx = string_of_int (idx + 1) in
          let msg = "When parsing CompoundStmt, could not parse #" ^ idx in
          do_parse_prog j msg
        in
        Some (Block (enumerate l |> List.map on_elem))
      | _ -> None
    );
    "ForStmt", (["var"; "range"; "body"], function
      | [v; r; body] ->
        bind (parse_program body) (fun body ->
          Some (For (parse_var.run v, parse_range.run r, body))
        )
      | _ -> None
    );
    "DeclStmt", (["inner"], function
      | [`List [(`Assoc _ ) as j] ] when get_kind_opt j = Some "VarDecl" && has_type j is_int_type ->
        let o = match member "inner" j with
          | `List [e] -> Some (parse_nexp.run e)
          | _ -> None
        in
        Some (Decl (member "name" j |> to_string |> var_make, Local, o))
      | _ ->
        (* XXX: Silently ignore any unrecognized declaration*)
        Some (Block [])
    )
  ] j

let parse_program = make "program" parse_program

let parse_kernel = make "kernel" (fun k ->
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  choose_one_of [
    "FunctionDecl", (["body"; "params"], function
      | [body; `List params] ->
        begin
          let is_param p l =
            match get_kind_opt l, member "isUsed" l, member "type" l with
            | Some "ParmVarDecl", `Bool true, ty -> p ty
            | _, _, _ -> false
          in
          let get_params p =
            List.filter (is_param p) params
              |> List.map parse_var.run
              |> VarSet.of_list
          in
          Some {
            p_kernel_locations = get_params is_array_type;
            p_kernel_params = get_params is_int_type;
            p_kernel_code = parse_program.run body;
          }
        end
      | _ -> None
    )
  ] k
  )

let parse_kernels = make "kernels" (fun s ->
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  choose_one_of [
    "TranslationUnitDecl", (["inner"], function
      | [`List l] ->
        let is_kernel x =
          match get_kind_opt x, member "is_kernel" x with
          | Some "FunctionDecl", `Bool true -> true
          | _, _ -> false
        in
        Some (List.map parse_kernel.run (List.filter is_kernel l))
      | _ -> None)
  ] s
)
